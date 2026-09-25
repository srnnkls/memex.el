;;; memex-herdr.el --- Resume an indexed session in a herdr tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-herdr-resume' takes a session memex indexed back to a live
;; agent: it opens a herdr tab in the session's working directory and
;; sends the resume command memex recorded for it.  A session memex knows
;; no resume command for, and one whose transcript is gone from disk, are
;; shown in the read-only viewer instead.
;;
;; The resume command is not part of the RPC surface, so it is read from
;; `memex sessions --json-array'.  That subcommand has no session filter:
;; it answers with a window of recent sessions, `memex-resume-lookup-limit'
;; long, which is searched here for the `session_id' at the `source_path'
;; asked for - a `session_id' names a session only along with the
;; transcript it was read from.
;;
;; `memex-herdr-open-agent-session' goes the other way: from the terminal
;; of an agent herdr attached, to the indexed transcript of the session it
;; is running.  herdr reports that session by id or by transcript path and
;; the viewer is keyed by both, so the same session window pairs them.
;;
;; herdr and the `+ws-pin' helpers of the user's Doom configuration are
;; both optional: each is probed for at call time and its absence costs
;; only the feature it carries.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-completion)
(require 'memex-view)
(require 'transient)
(require 'eieio)

(declare-function herdr-start-server-if-needed "herdr-core" ())
(declare-function herdr-open-tab "herdr" (&rest keys))
(declare-function memex-anchor--herdr-p "memex-anchor" ())
(declare-function memex-anchor-resume "memex-anchor" (record resume))
(declare-function herdr-api-agent-start "herdr-api"
                  (kind name pane-id &rest keys))
(declare-function +ws-pin-of "ext:+workspace-pins" (buffer))
(declare-function +ws-pin-buffer "ext:+workspace-pins"
                  (buffer &optional workspace))
(declare-function +ws-pin-follow "ext:+workspace-pins" (buffer))
(declare-function transient-append-suffix "transient" (prefix loc suffix &optional keep-other))
(declare-function herdr-read-agent "herdr" (prompt &optional entries))
(declare-function herdr--prune-session-targets "herdr" (entries))
(declare-function herdr-agent--send-candidates "herdr-agent" ())
(declare-function herdr-agent--workspace-entries "herdr-agent" (entries))
(declare-function transient-get-suffix "transient" (prefix loc))

(defcustom memex-resume-lookup-limit 5000
  "Number of recent sessions the resume lookup reads before matching.
The window has to be long enough to reach back to the session being
resumed, since it is the only filter `memex sessions' offers."
  :type 'natnum
  :group 'memex)

(defcustom memex-herdr-display-action nil
  "How the bridge shows the viewer it opens from a herdr terminal.
Nil leaves the choice to `display-buffer' and whatever the editor's own
window rules say, which is where a popup framework keeps its opinion.
A rule in `display-buffer-alist' outranks whatever is set here."
  :type 'sexp
  :group 'memex)

(defun memex-herdr--display (buffer)
  "Show BUFFER in the workspace it belongs to.
An unpinned buffer is pinned to the current workspace and one already
pinned is followed to its own.  Following is idempotent and so is the
`display-buffer' advice the pins install, so neither switches twice.
Where the viewer goes is `display-buffer''s to decide, so a window rule
of the editor's places it as it places any other buffer."
  (when (fboundp '+ws-pin-of)
    (if (+ws-pin-of buffer)
        (when (fboundp '+ws-pin-follow) (+ws-pin-follow buffer))
      (when (fboundp '+ws-pin-buffer) (+ws-pin-buffer buffer))))
  (display-buffer buffer memex-herdr-display-action))

;;;###autoload
(defun memex-herdr-open-session (session-id source-path &optional doc-id)
  "Show the session SESSION-ID at SOURCE-PATH and return its process.
Point lands on the record DOC-ID, or at the start of the transcript
without one.  The viewer goes up in the workspace the session is pinned
to where the `+ws-pin' helpers are installed, and in whatever window
`display-buffer' picks where they are not."
  (memex-view-session session-id source-path doc-id #'memex-herdr--display))

(defun memex-herdr--ready ()
  "Ready the session lookup, refusing without `memex-executable' on PATH.
The lookup shells it out, so a resume without it on PATH comes back with
no row and reads as a session out of the lookup window; refusing here is
what keeps the two apart."
  (unless (executable-find memex-executable)
    (user-error "Memex executable not found: %s" memex-executable)))

(defun memex-herdr--ready-server ()
  "Ready the herdr server a tab is about to be created against.
Only a resume that reaches an agent needs herdr; the branches that end in
the viewer are answered on a machine that has none, so the probe belongs
here rather than at the head of a resume."
  (unless (fboundp 'herdr-start-server-if-needed)
    (user-error "Herdr is not installed: no herdr-start-server-if-needed"))
  (condition-case failure (herdr-start-server-if-needed)
    (herdr-error (user-error "Cannot ready herdr: %s"
                             (error-message-string failure)))))

(defun memex-herdr--sessions (&rest filters)
  "Return the window of recent sessions FILTERS narrows, newest first.
FILTERS are the arguments that go between `sessions --json-array' and
the limit.  A shell-out that cannot run at all answers with no rows,
which every caller reports as the session it could not find."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory temporary-file-directory))
          (when (eq 0 (apply #'call-process
                             memex-executable nil '(t nil) nil
                             "sessions" "--json-array"
                             (append filters
                                     (list "--limit"
                                           (number-to-string
                                            memex-resume-lookup-limit)))))
            (memex--decode (buffer-string)))))
    (error nil)))

(defun memex-herdr--row (session-id source-path source)
  "Return what memex knows of the session SESSION-ID at SOURCE-PATH.
The window of recent SOURCE sessions is read in one shell-out and
matched here on the two fields together."
  (seq-find (lambda (row)
              (and (equal (alist-get 'session_id row) session-id)
                   (equal (alist-get 'source_path row) source-path)))
            (memex-herdr--sessions "--source" source)))

(defun memex-herdr--directory (row)
  "Return the directory a resume of ROW begins in.
The session's own working directory, else the root of the repository it
was recorded in, else the directory its transcript sits in."
  (or (alist-get 'cwd row)
      (alist-get 'git_root row)
      (file-name-directory (alist-get 'source_path row))))

(defcustom memex-herdr-start-timeout 20000
  "Milliseconds herdr waits for a resumed agent to come up.
herdr answers only once it has seen the agent it was asked for in the
pane, so this is also how long a refused resume takes to report."
  :type 'natnum
  :group 'memex)

(defun memex-herdr--arguments (command)
  "Return the resume COMMAND as (KIND ARGUMENT...), or nil for no agent.
memex records a shell line - a `cd' into the session's directory and
then the agent - and herdr starts an agent by kind and arguments rather
than by being typed at.  Splitting it is what lets herdr do the
starting, and what lets herdr answer whether the agent came up."
  (let* ((tail (if (string-match "&&[ \t]*" command)
                   (substring command (match-end 0))
                 command))
         (parts (ignore-errors
                  (split-string-and-unquote (string-trim tail)))))
    (when parts
      (cons (file-name-nondirectory (car parts)) (cdr parts)))))

(defun memex-herdr--start (row command &optional name)
  "Open a Herdr tab for ROW and start COMMAND's agent as NAME."
  (pcase-let ((`(,kind . ,arguments) (memex-herdr--arguments command)))
    (unless kind
      (user-error "Memex recorded no agent to resume this session with"))
    (memex-herdr--ready-server)
    (let* ((tab (herdr-open-tab :cwd (memex-herdr--directory row)))
           (pane (alist-get 'pane_id (alist-get 'root_pane tab))))
      (herdr-api-agent-start kind (or name kind) pane
                             :args arguments
                             :timeout-ms memex-herdr-start-timeout)
      pane)))

;;;###autoload
(defun memex-herdr-resume (record)
  "Resume the session RECORD belongs to in a Herdr terminal.
RECORD is the alist carried by Memex selectors and session buffers.  An
existing agent is attached by exact session reference; otherwise the
same anchor resolver starts it from Memex's recorded resume command.
Missing transcripts or resume commands open the indexed session."
  (interactive
   (list (if (derived-mode-p 'memex-session-mode)
             (list (cons 'session_id memex-view-session-id)
                   (cons 'source_path memex-view-source-path)
                   (cons 'source memex-view-source))
           (memex-read-session))))
  (let ((session-id (alist-get 'session_id record))
        (source-path (alist-get 'source_path record))
        (source (alist-get 'source record))
        (doc-id (alist-get 'doc_id record)))
    (memex-herdr--ready)
    (if (not (file-exists-p source-path))
        (progn
          (message "memex resume: nothing is left at %s, showing what memex indexed"
                   source-path)
          (memex-herdr-open-session session-id source-path doc-id))
      (let* ((row (memex-herdr--row session-id source-path source))
             (command (alist-get 'resume_cmd row)))
        (cond
         ((null row)
          (user-error
           "Memex found no session %s at %s in the last %d sessions; raise memex-resume-lookup-limit"
           session-id source-path memex-resume-lookup-limit))
         ((or (null command) (string-empty-p command))
          (message "memex resume: no %s resume command for this session, showing the transcript"
                   source)
          (memex-herdr-open-session session-id source-path doc-id))
         ((not (require 'memex-anchor nil t))
          (user-error "Memex anchor support is unavailable"))
         (t
          (unless (memex-anchor--herdr-p)
            (memex-herdr--ready-server))
          (memex-anchor-resume record (cons command row))))))))

;;;; Reading the session an attached agent is running

(defvar herdr-terminal-id)
(declare-function memex-anchor--agents "memex-anchor" ())

(defun memex-herdr--attached-agent (buffer)
  "Return the herdr agent whose terminal BUFFER shows, or nil.
herdr stamps `herdr-terminal-id' on every buffer it attaches, which is
what tells an agent's own terminal apart from any other buffer."
  (when-let* (((buffer-live-p buffer))
              ((local-variable-p 'herdr-terminal-id buffer))
              (terminal (buffer-local-value 'herdr-terminal-id buffer))
              ((require 'memex-anchor nil t)))
    (seq-find (lambda (agent)
                (equal (alist-get 'terminal_id agent) terminal))
              (memex-anchor--agents))))

(defun memex-herdr--ref-row (reference directory)
  "Return the session memex indexed for herdr's REFERENCE, or nil.
herdr names a session by id or by transcript path and the viewer needs
both, so the index is what pairs the one herdr reports with the other.
DIRECTORY narrows the window to the sessions of the directory the agent
works in; a session recorded elsewhere is looked for once more across
the whole window."
  (when-let* ((value (alist-get 'value reference))
              (field (pcase (alist-get 'kind reference)
                       ("id" 'session_id)
                       ("path" 'source_path))))
    (let ((match (lambda (rows)
                   (seq-find (lambda (row) (equal (alist-get field row) value))
                             rows))))
      (or (and directory
               (funcall match (memex-herdr--sessions "--cwd" directory)))
          (funcall match (memex-herdr--sessions))))))

;;;###autoload
(defun memex-herdr-session-scope (reference &optional directory)
  "Return the memex session scope for herdr's agent session REFERENCE, or nil.
REFERENCE is the `agent_session' record herdr reports for an agent.
DIRECTORY is where that agent works and narrows the lookup window.

The answer is a plist of `:source', `:session-id' and `:source-path',
which is one element of the scope `memex-search-in-sessions' takes."
  (when-let* ((row (memex-herdr--ref-row reference directory)))
    (list :source (alist-get 'source row)
          :session-id (alist-get 'session_id row)
          :source-path (alist-get 'source_path row))))

;;;###autoload
(defun memex-herdr-open-agent (agent)
  "Show memex's transcript of the session AGENT is running.
AGENT is a row of the agents herdr reports.  The transcript is the whole
conversation, including what the terminal has scrolled past, and reading
it leaves the agent alone.

A session herdr has not reported yet, or that memex has not indexed yet,
is refused by name rather than opened empty."
  (let ((reference (alist-get 'agent_session agent)))
    (unless reference
      (user-error "Herdr reports no session for %s"
                  (or (alist-get 'name agent) (alist-get 'agent agent)
                      "this agent")))
    (memex-herdr--ready)
    (let ((row (memex-herdr--ref-row reference (alist-get 'cwd agent))))
      (unless row
        (user-error "Memex has indexed no session %s" (alist-get 'value reference)))
      (memex-herdr-open-session (alist-get 'session_id row)
                                (alist-get 'source_path row)))))

(defun memex-herdr--running-agents ()
  "Return the agents herdr reports, refusing without herdr itself."
  (unless (or (fboundp 'herdr-agent--send-candidates)
              (require 'herdr-agent nil t))
    (user-error "Reading an agent requires herdr"))
  (let ((entries (herdr-agent--send-candidates)))
    (herdr--prune-session-targets entries)
    entries))

;;;###autoload
(defun memex-herdr-switch-session (&optional all)
  "Read one of this workspace's agents and show its transcript.
The agents are the ones `herdr-switch-agent' offers, in the same rows
and the same order; ALL, the prefix argument, offers every agent running
instead.  The agent keeps running and its terminal stays where it is:
this reads what it has said, not what it is doing."
  (interactive "P")
  (let* ((entries (memex-herdr--running-agents))
         (pool (if all entries (herdr-agent--workspace-entries entries))))
    (unless pool
      (user-error "No herdr agent is running"))
    (memex-herdr-open-agent
     (herdr-read-agent (if all "Transcript of agent: " "Transcript of agent here: ")
                       pool))))

;;;###autoload
(defun memex-herdr-open-agent-session (&optional buffer)
  "Show memex's transcript of the session the agent in BUFFER is running.
BUFFER defaults to the current one and is an attached herdr terminal."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (agent (memex-herdr--attached-agent buffer)))
    (unless agent
      (user-error "No herdr agent is attached to %s" (buffer-name buffer)))
    (memex-herdr-open-agent agent)))

;;;###autoload
(defun memex-herdr-setup ()
  "Offer an attached agent's transcript from herdr's own transient.
Absent herdr the command remains, reachable by name; this only puts it
where the rest of the agent commands are.  Herdr binds `x' to stopping an
agent, so the transcript goes under `v'."
  (when (and (fboundp 'transient-append-suffix)
             (not (ignore-errors (transient-get-suffix 'herdr-transient "v"))))
    (ignore-errors
      (transient-append-suffix 'herdr-transient "i"
        '("v" "memex transcript" memex-herdr-open-agent-session)))))

(with-eval-after-load 'herdr-transient (memex-herdr-setup))
;;;###autoload (with-eval-after-load 'herdr-transient (memex-herdr-setup))

;;;; The dashboard's own searches

(declare-function memex-search-in-sessions "memex" (scope &optional mode initial))
(declare-function herdr-status-visible-agents "ext:herdr-status" ())
(declare-function herdr-herd-member-entries "ext:herdr-herd" (name))
(declare-function herdr-entry-label "ext:herdr" (entry))
(declare-function magit-current-section "ext:magit-section" ())
(defvar herdr-status-mode-map)

(defun memex-herdr--dashboard-p ()
  "Return non-nil when point stands in herdr's dashboard."
  (and (fboundp 'herdr-status-visible-agents)
       (derived-mode-p 'herdr-status-mode)))

(defun memex-herdr--agents-at-point ()
  "Return the agent entries the dashboard section at point stands for.
An agent row is itself; a herd is its live members; anywhere else in the
dashboard is every agent the filters leave.  Outside the dashboard there
is nothing to narrow by and the answer is nil."
  (when-let* (((memex-herdr--dashboard-p))
              (section (magit-current-section)))
    (pcase (oref section type)
      ('herdr-status-agent (list (oref section value)))
      ('herdr-status-herd (herdr-herd-member-entries (oref section value)))
      (_ (herdr-status-visible-agents)))))

(defun memex-herdr--scope (entries)
  "Return the session scope covering ENTRIES.
An entry herdr reports no session for, or memex has not indexed, drops out."
  (delq nil
        (mapcar (lambda (entry)
                  (when-let* ((reference (alist-get 'agent_session entry)))
                    (memex-herdr-session-scope reference (alist-get 'cwd entry))))
                entries)))

(defun memex-herdr--scope-at-point ()
  "Return the scope for the section at point, refusing an empty narrowing.
Nil means a search across everything and is returned only where point
asked for one.  Agents whose sessions memex has all missed are an error,
not a widening."
  (let ((entries (memex-herdr--agents-at-point)))
    (if (null entries)
        nil
      (or (memex-herdr--scope entries)
          (user-error "Memex has indexed no session of %s"
                      (if (cdr entries) "these agents" "this agent"))))))

;;;###autoload
(defun memex-herdr-search (&optional mode)
  "Search memex over the sessions the dashboard section at point stands for.
MODE is `lexical', `semantic' or `hybrid'."
  (interactive)
  (memex-search-in-sessions (memex-herdr--scope-at-point) mode))

;;;###autoload
(defun memex-herdr-search-globally (&optional mode)
  "Search memex across every indexed session, ignoring point.
MODE is `lexical', `semantic' or `hybrid'."
  (interactive)
  (memex-search-in-sessions nil mode))

(defun memex-herdr--session-at-point ()
  "Return the scope entry of the single agent at point."
  (let ((entries (memex-herdr--agents-at-point)))
    (unless (and entries (null (cdr entries)))
      (user-error "Point is on no single agent"))
    (or (car (memex-herdr--scope entries))
        (user-error "Memex has indexed no session of this agent"))))

;;;###autoload
(defun memex-herdr-transcript-at-point ()
  "Show the transcript of the session the agent at point is running."
  (interactive)
  (let ((scope (memex-herdr--session-at-point)))
    (memex-herdr-open-session (plist-get scope :session-id)
                              (plist-get scope :source-path))))

;;;###autoload
(defun memex-herdr-resume-at-point ()
  "Resume the session the agent at point is running in a herdr tab."
  (interactive)
  (let ((scope (memex-herdr--session-at-point)))
    (memex-herdr-resume
     `((session_id . ,(plist-get scope :session-id))
       (source_path . ,(plist-get scope :source-path))
       (source . ,(plist-get scope :source))))))

(defun memex-herdr--scope-description ()
  "Return what a search from point would be narrowed to."
  (let ((entries (memex-herdr--agents-at-point)))
    (cond
     ((null entries) "search everything")
     ((null (cdr entries)) (format "search %s" (herdr-entry-label (car entries))))
     (t (format "search %d listed agents" (length entries))))))

;;;###autoload
(transient-define-prefix memex-herdr-dispatch ()
  "Search and read the history of the agents herdr runs."
  [["Scope"
    ("s" memex-herdr-search :description memex-herdr--scope-description)
    ("g" "search everything" memex-herdr-search-globally)]
   ["Mode"
    ("S" "semantic" (lambda () (interactive) (memex-herdr-search 'semantic)))
    ("L" "lexical" (lambda () (interactive) (memex-herdr-search 'lexical)))
    ("H" "hybrid" (lambda () (interactive) (memex-herdr-search 'hybrid)))]
   ["This agent"
    ("RET" "transcript" memex-herdr-transcript-at-point)
    ("r" "resume" memex-herdr-resume-at-point)]])

(defun memex-herdr--free-key-p (map key)
  "Return non-nil when KEY in MAP is unbound or already memex's own."
  (let ((bound (keymap-lookup map key)))
    (or (null bound)
        (and (symbolp bound)
             (string-prefix-p "memex-" (symbol-name bound))))))

(defun memex-herdr-install-keys ()
  "Bind the search keys in herdr's dashboard.
The dashboard knows nothing of memex; what it offers is a keymap, and
this is memex taking it up where both are installed.  A key someone else
has already bound is left alone, so loading memex after a configuration
has had its say does not undo it."
  (when (boundp 'herdr-status-mode-map)
    (dolist (binding '(("s" . memex-herdr-search) ("m" . memex-herdr-dispatch)))
      (when (memex-herdr--free-key-p herdr-status-mode-map (car binding))
        (keymap-set herdr-status-mode-map (car binding) (cdr binding))))))

(defun memex-herdr-install-dashboard ()
  "Offer memex's searches from herdr's dashboard, where herdr is installed.
The dashboard knows nothing of memex; what it offers is a keymap and a
transient, and this is memex taking both up."
  (memex-herdr-install-keys)
  (when (and (fboundp 'transient-append-suffix)
             (not (ignore-errors
                    (transient-get-suffix 'herdr-status-dispatch "m"))))
    (ignore-errors
      (transient-append-suffix 'herdr-status-dispatch '(0 -1)
        ["Search"
         ("s" "search" memex-herdr-search)
         ("m" "memex" memex-herdr-dispatch)]))))

(with-eval-after-load 'herdr-status (memex-herdr-install-dashboard))
;;;###autoload (with-eval-after-load 'herdr-status (memex-herdr-install-dashboard))

(provide 'memex-herdr)
;;; memex-herdr.el ends here
