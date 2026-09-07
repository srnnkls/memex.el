;;; memex-herdr.el --- Resume an indexed session in a herdr tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
(declare-function transient-get-suffix "transient" (prefix loc))

(defcustom memex-resume-lookup-limit 5000
  "Number of recent sessions the resume lookup reads before matching.
The window has to be long enough to reach back to the session being
resumed, since it is the only filter `memex sessions' offers."
  :type 'natnum
  :group 'memex)

(defun memex-herdr--display (buffer)
  "Show BUFFER in the workspace it belongs to.
An unpinned buffer is pinned to the current workspace and one already
pinned is followed to its own.  Following is idempotent and so is the
`display-buffer' advice the pins install, so neither switches twice."
  (when (fboundp '+ws-pin-of)
    (if (+ws-pin-of buffer)
        (when (fboundp '+ws-pin-follow) (+ws-pin-follow buffer))
      (when (fboundp '+ws-pin-buffer) (+ws-pin-buffer buffer))))
  (display-buffer buffer))

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
                             memex-executable nil t nil
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
(defun memex-herdr-open-agent-session (&optional buffer)
  "Show memex's transcript of the session the agent in BUFFER is running.
BUFFER defaults to the current one and is an attached herdr terminal.
The transcript is the whole conversation, including what the terminal
has scrolled past, and reading it leaves the agent alone.

A session herdr has not reported yet, or that memex has not indexed
yet, is refused by name rather than opened empty."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (agent (memex-herdr--attached-agent buffer))
         (reference (alist-get 'agent_session agent)))
    (cond
     ((null agent)
      (user-error "No herdr agent is attached to %s" (buffer-name buffer)))
     ((null reference)
      (user-error "Herdr reports no session for %s"
                  (or (alist-get 'name agent) (alist-get 'agent agent)
                      "this agent")))
     (t
      (memex-herdr--ready)
      (let ((row (memex-herdr--ref-row reference (alist-get 'cwd agent))))
        (unless row
          (user-error "Memex has indexed no session %s" (alist-get 'value reference)))
        (memex-herdr-open-session (alist-get 'session_id row)
                                  (alist-get 'source_path row)))))))

;;;###autoload
(defun memex-herdr-setup ()
  "Offer an attached agent's transcript from herdr's own transient.
Absent herdr the command remains, reachable by name; this only puts it
where the rest of the agent commands are."
  (when (and (fboundp 'transient-append-suffix)
             (not (ignore-errors (transient-get-suffix 'herdr-transient "x"))))
    (ignore-errors
      (transient-append-suffix 'herdr-transient "i"
        '("x" "memex transcript" memex-herdr-open-agent-session)))))

(with-eval-after-load 'herdr-transient (memex-herdr-setup))
;;;###autoload (with-eval-after-load 'herdr-transient (memex-herdr-setup))

(provide 'memex-herdr)
;;; memex-herdr.el ends here
