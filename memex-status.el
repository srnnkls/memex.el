;;; memex-status.el --- A dashboard over the sessions memex indexed -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "3.3"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-status' lists the sessions memex indexed, newest activity first,
;; as a Magit-style buffer: one collapsible row per session over its
;; project, source, size and working directory.
;;
;;   M-x memex-status
;;
;; Every action has a direct key and `?' shows a menu of the same keys.
;; `f' narrows the window memex answers with, `S' orders what came back,
;; and `L' says how many rows to ask for.
;;
;; Memex answers in its own order and offers no sort key, so `S' orders
;; the rows in the buffer and never the window they were drawn from:
;; ordering by size shows the largest of the sessions asked for, not the
;; largest memex knows.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'transient)
(require 'memex-api)
(require 'memex-completion)
(require 'memex-view)

(declare-function memex-herdr-resume "memex-herdr" (record))
(declare-function memex-search-in-sessions "memex" (scope &optional mode initial))

(defgroup memex-status nil
  "The dashboard over indexed sessions."
  :group 'memex)

;;;; Appearance

(defface memex-status-label '((t :inherit font-lock-function-name-face))
  "Face for what a session is about."
  :group 'memex-status)

(defface memex-status-project '((t :inherit font-lock-constant-face))
  "Face for the repository a session was recorded in."
  :group 'memex-status)

(defface memex-status-path '((t :inherit font-lock-comment-face))
  "Face for a working directory."
  :group 'memex-status)

(defface memex-status-meta '((t :inherit shadow))
  "Face for a session's size, age, and the separators around them."
  :group 'memex-status)

(defface memex-status-narrowing '((t :inherit font-lock-keyword-face))
  "Face for the filters shown in the sessions heading."
  :group 'memex-status)

(defcustom memex-status-limit 20
  "How many sessions the dashboard asks memex for."
  :type 'natnum
  :group 'memex-status)

(defcustom memex-status-buffer-name "*memex status*"
  "Name of the dashboard buffer."
  :type 'string
  :group 'memex-status)

(defcustom memex-status-display-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "How the dashboard is shown.
A rule in `display-buffer-alist' outranks this."
  :type 'sexp
  :group 'memex-status)

(defcustom memex-status-label-width 64
  "Widest a session's subject is drawn."
  :type 'natnum
  :group 'memex-status)

(defcustom memex-status-project-width 16
  "Widest a repository name is drawn."
  :type 'natnum
  :group 'memex-status)

(defcustom memex-status-origin nil
  "Which sessions memex is asked for.
Nil takes memex's own default, which leaves out permission reviews.
`all' includes them, and `interactive' and `subagent' pick out those
subsets."
  :type '(choice (const :tag "Memex's default" nil)
                 (const all) (const interactive) (const subagent))
  :group 'memex-status)

;;;; Ordering

(defun memex-status--milliseconds (timestamp)
  "Return the epoch milliseconds TIMESTAMP names, or nil."
  (when (stringp timestamp)
    (when-let* ((time (ignore-errors (date-to-time timestamp))))
      (* 1000 (float-time time)))))

(defun memex-status--key-recent (session)
  "Return SESSION's last activity as a sortable string."
  (or (alist-get 'last_at session) ""))

(defun memex-status--key-started (session)
  "Return when SESSION opened as a sortable string."
  (or (alist-get 'started_at session) ""))

(defun memex-status--key-messages (session)
  "Return how many messages SESSION holds as a sortable string."
  (format "%012d" (or (alist-get 'message_count session) 0)))

(defun memex-status--key-project (session)
  "Return the repository SESSION belongs to, folded for sorting."
  (downcase (or (alist-get 'repo_project session)
                (alist-get 'project session) "")))

(defun memex-status--key-source (session)
  "Return the harness SESSION was recorded from, folded for sorting."
  (downcase (or (alist-get 'source session) "")))

(defun memex-status--key-label (session)
  "Return what SESSION is about, folded for sorting."
  (downcase (or (memex-completion--clean (alist-get 'label session)) "")))

(defcustom memex-status-sorts
  '(("recent" memex-status--key-recent . descending)
    ("started" memex-status--key-started . descending)
    ("messages" memex-status--key-messages . descending)
    ("project" memex-status--key-project . ascending)
    ("source" memex-status--key-source . ascending)
    ("label" memex-status--key-label . ascending))
  "The orders the dashboard offers, keyed by the name they go under.
Each entry gives the function returning a session's sortable string and
the direction that reads as natural for it, which `\\[memex-status-sort-reverse]'
turns around."
  :type '(alist :key-type string
                :value-type (cons function (choice (const ascending)
                                                   (const descending))))
  :group 'memex-status)

;;;; State

(defvar-local memex-status--sessions nil
  "The sessions the last answer carried.")

(defvar-local memex-status--sort nil
  "Cons of the sort in force and whether it is turned around.
Nil leaves the rows in the order memex answered with.")

(defvar-local memex-status--narrowing nil
  "Plist of the filters the next request carries.")

(defvar-local memex-status--limit nil
  "How many sessions this dashboard asks for.")

(defvar-local memex-status--request nil
  "The request in flight, which a refresh cancels before starting another.")

(defvar-local memex-status--error nil
  "What memex refused the last request with, or nil.")

(defun memex-status--limit ()
  "Return how many sessions this dashboard asks for."
  (or memex-status--limit memex-status-limit))

(defun memex-status--ordered (sessions)
  "Return SESSIONS in the order in force, memex's own by default."
  (if-let* ((sort (car-safe memex-status--sort))
            (entry (cdr (assoc sort memex-status-sorts))))
      (let* ((key (car entry))
             (descending (eq (cdr entry) 'descending))
             (flipped (cdr memex-status--sort))
             (down (if flipped (not descending) descending)))
        (sort (copy-sequence sessions)
              (lambda (a b)
                (let ((one (funcall key a)) (other (funcall key b)))
                  (if down (string> one other) (string< one other))))))
    sessions))

;;;; Rendering

(defun memex-status--pad (value width)
  "Return VALUE, or the empty string, padded on the right to WIDTH."
  (truncate-string-to-width (or value "") width nil ?\s "…"))

(defun memex-status--source-column (session)
  "Return SESSION's harness behind the mark its vendor is drawn with."
  (let* ((source (or (alist-get 'source session) ""))
         (mark (cdr (assoc source memex-view-source-marks))))
    (concat (if mark
                (propertize (car mark) 'font-lock-face
                            (list (cdr mark) 'memex-view-source-glyph))
              (make-string (string-width "✳") ?\s))
            " "
            (propertize (memex-status--pad source 7)
                        'font-lock-face 'memex-status-meta))))

(defun memex-status--size-column (session)
  "Return how many messages SESSION holds, right-aligned."
  (propertize (format "%6s" (or (alist-get 'message_count session) "?"))
              'font-lock-face 'memex-status-meta))

(defun memex-status--age-column (session)
  "Return how long ago SESSION was last active."
  (propertize
   (format "%4s" (or (memex-completion--age
                      (memex-status--milliseconds (alist-get 'last_at session)))
                     "?"))
   'font-lock-face 'memex-status-meta))

(defun memex-status--row (session)
  "Return the single line drawn for SESSION."
  (concat
   " "
   (memex-status--age-column session) " "
   (memex-status--size-column session) "  "
   (memex-status--source-column session) "  "
   (propertize (memex-status--pad (or (alist-get 'repo_project session)
                                      (alist-get 'project session))
                                  memex-status-project-width)
               'font-lock-face 'memex-status-project)
   "  "
   (propertize (memex-status--pad
                (memex-completion--clean (alist-get 'label session))
                memex-status-label-width)
               'font-lock-face 'memex-status-label)))

(defconst memex-status--detail-fields
  '(session_id source source_path project repo_project cwd git_root
               started_at last_at message_count conversation_kind resume_cmd)
  "Session fields shown, in order, when a row is expanded.")

(defconst memex-status--detail-width
  (apply #'max (mapcar (lambda (field) (length (symbol-name field)))
                       memex-status--detail-fields))
  "Width of the key column in an expanded session.")

(defun memex-status--insert-field (name value)
  "Insert the detail line pairing NAME with VALUE, unless VALUE is empty."
  (when (and value (not (equal value "")))
    (insert "    "
            (propertize (memex-status--pad name memex-status--detail-width)
                        'font-lock-face 'memex-status-project)
            "  "
            (if (memq (intern name) '(cwd git_root source_path))
                (propertize (abbreviate-file-name (format "%s" value))
                            'font-lock-face 'memex-status-path)
              (format "%s" value))
            "\n")))

(defun memex-status--insert-session (session)
  "Insert SESSION as one collapsible row."
  (magit-insert-section (memex-status-session session t)
    (magit-insert-heading (memex-status--row session))
    (magit-insert-section-body
      (dolist (field memex-status--detail-fields)
        (memex-status--insert-field (symbol-name field)
                                    (alist-get field session))))))

(defun memex-status--narrowing-summary ()
  "Return the filters in force, rendered for the heading."
  (let (parts)
    (dolist (key '(:source :project :cwd :since :origin))
      (when-let* ((value (plist-get memex-status--narrowing key)))
        (push (format "%s %s" (substring (symbol-name key) 1) value) parts)))
    (when parts
      (concat "  " (propertize (string-join (nreverse parts) " ")
                               'font-lock-face 'memex-status-narrowing)))))

(defun memex-status--sort-summary ()
  "Return the order the rows are in, rendered for the heading."
  (if memex-status--sort
      (propertize (format "  · by %s%s" (car memex-status--sort)
                          (if (cdr memex-status--sort) " ↑" ""))
                  'font-lock-face 'memex-status-meta)
    (propertize "  · newest first" 'font-lock-face 'memex-status-meta)))

(defun memex-status--insert-sessions ()
  "Insert every session the last answer carried."
  (let ((sessions (memex-status--ordered memex-status--sessions)))
    (magit-insert-section (memex-status-sessions)
      (magit-insert-heading
        (concat (propertize (format "Sessions %d/%d"
                                    (length sessions) (memex-status--limit))
                            'font-lock-face 'magit-section-heading)
                (memex-status--sort-summary)
                (memex-status--narrowing-summary)))
      (cond
       (memex-status--error
        (insert (propertize (format "  %s\n" memex-status--error)
                            'font-lock-face 'error)))
       ((null sessions) (insert "  Nothing indexed matches.\n"))
       (t (mapc #'memex-status--insert-session sessions))))))

(defun memex-status--redraw ()
  "Redraw the dashboard in the current buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (magit-insert-section (memex-status-root)
      (memex-status--insert-sessions))
    (goto-char (point-min))
    (forward-line (1- line))))

;;;; Fetching

(defun memex-status--receive (buffer)
  "Return the callback filling BUFFER with the sessions memex answers."
  (lambda (sessions)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq memex-status--request nil
              memex-status--error nil
              memex-status--sessions sessions)
        (memex-status--redraw)))))

(defun memex-status--refuse (buffer)
  "Return the errback showing BUFFER what memex refused the request with."
  (lambda (failure)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq memex-status--request nil
              memex-status--error (error-message-string failure))
        (memex-status--redraw)))))

(defun memex-status-refresh ()
  "Ask memex for the sessions again and redraw when the answer lands."
  (interactive)
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (when memex-status--request
    (memex-cancel-rpc memex-status--request))
  (let ((buffer (current-buffer))
        (narrowing memex-status--narrowing))
    (setq memex-status--request
          (apply #'memex-api-sessions
                 (memex-status--receive buffer)
                 :errback (memex-status--refuse buffer)
                 :limit (memex-status--limit)
                 :origin (or (plist-get narrowing :origin) memex-status-origin)
                 (list :source (plist-get narrowing :source)
                       :project (plist-get narrowing :project)
                       :cwd (plist-get narrowing :cwd)
                       :since (plist-get narrowing :since))))))

;;;; Mode

(defvar-keymap memex-status-mode-map
  :doc "Keymap for `memex-status-mode'."
  :parent magit-section-mode-map
  "?" #'memex-status-dispatch
  "RET" #'memex-status-visit
  "o" #'memex-status-visit-other-window
  "r" #'memex-status-resume
  "w" #'memex-status-copy-command
  "s" #'memex-status-search
  "S" #'memex-status-search-dispatch
  "f" #'memex-status-filter
  "O" #'memex-status-sort
  "L" #'memex-status-set-limit
  "g" #'memex-status-refresh
  "q" #'quit-window)

(define-derived-mode memex-status-mode magit-section-mode "Memex Status"
  "Major mode for the memex session dashboard."
  :group 'memex-status
  (setq-local revert-buffer-function (lambda (&rest _) (memex-status-refresh))))

;;;###autoload
(defun memex-status ()
  "Show the sessions memex indexed, newest activity first."
  (interactive)
  (let ((buffer (get-buffer-create memex-status-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'memex-status-mode)
        (memex-status-mode))
      (memex-status-refresh))
    (let ((display-buffer-overriding-action memex-status-display-action))
      (pop-to-buffer buffer))))

;;;; Commands

(defun memex-status-session-at-point ()
  "Return the session the row at point stands for, or nil."
  (when-let* (((derived-mode-p 'memex-status-mode))
              (section (magit-current-section))
              ((eq (oref section type) 'memex-status-session)))
    (oref section value)))

(defun memex-status--session-at-point ()
  "Return the session the row at point stands for."
  (or (memex-status-session-at-point)
      (user-error "No session at point")))

(defun memex-status-visit ()
  "Open the transcript of the session at point."
  (interactive)
  (let ((session (memex-status--session-at-point)))
    (memex-view-session (alist-get 'session_id session)
                        (alist-get 'source_path session))))

(defun memex-status-visit-other-window ()
  "Open the transcript of the session at point in another window."
  (interactive)
  (let ((display-buffer-overriding-action
         '(display-buffer-use-some-window (inhibit-same-window . t))))
    (memex-status-visit)))

(defun memex-status-resume ()
  "Resume the session at point in a herdr tab."
  (interactive)
  (unless (require 'memex-herdr nil t)
    (user-error "Memex herdr support is unavailable"))
  (memex-herdr-resume (memex-status--session-at-point)))

(defun memex-status-copy-command ()
  "Copy the command that resumes the session at point."
  (interactive)
  (let ((command (or (alist-get 'resume_cmd (memex-status--session-at-point))
                     (user-error "Memex recorded no resume command"))))
    (kill-new command)
    (message "%s" command)))

;;;; Searching

(defun memex-status--scope (sessions)
  "Return the memex session scope covering SESSIONS.
A row carries its source, id and transcript path already, so the scope
follows from it without asking memex anything."
  (mapcar (lambda (session)
            (list :source (alist-get 'source session)
                  :session-id (alist-get 'session_id session)
                  :source-path (alist-get 'source_path session)))
          sessions))

(defun memex-status--scope-at-point ()
  "Return the sessions a search from point is narrowed to.
The row at point stands for itself, and anywhere else in the dashboard
stands for every session listed."
  (let ((sessions (if-let* ((session (memex-status-session-at-point)))
                      (list session)
                    memex-status--sessions)))
    (unless sessions
      (user-error "No session to search"))
    (memex-status--scope sessions)))

(defun memex-status--ready ()
  "Refuse by name where memex cannot search a named set of sessions."
  (unless (require 'memex nil t)
    (user-error "Memex search is unavailable"))
  (unless (fboundp 'memex-search-in-sessions)
    (user-error "Memex is too old: it has no `memex-search-in-sessions'")))

(defun memex-status-search (&optional mode)
  "Search the sessions the row at point stands for, in MODE.
Off a row that is every session listed.  MODE is `lexical', `semantic'
or `hybrid'."
  (interactive)
  (memex-status--ready)
  (memex-search-in-sessions (memex-status--scope-at-point) mode))

(defun memex-status-search-listed (&optional mode)
  "Search every session listed in MODE, ignoring the row at point."
  (interactive)
  (memex-status--ready)
  (unless memex-status--sessions
    (user-error "No session to search"))
  (memex-search-in-sessions (memex-status--scope memex-status--sessions) mode))

(defun memex-status-search-globally (&optional mode)
  "Search every indexed session in MODE, listed or not."
  (interactive)
  (memex-status--ready)
  (memex-search-in-sessions nil mode))

(defun memex-status--search-description ()
  "Return what a search from point would be narrowed to."
  (if-let* ((session (memex-status-session-at-point)))
      (format "search %s"
              (or (memex-completion--clean (alist-get 'label session))
                  (alist-get 'session_id session)))
    (format "search %d listed" (length memex-status--sessions))))

;;;; Narrowing

(defun memex-status--narrow (key value)
  "Make the next request carry VALUE under KEY and ask again."
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (setq-local memex-status--narrowing
              (plist-put (copy-sequence memex-status--narrowing) key value))
  (memex-status-refresh))

(defun memex-status-narrow-source (source)
  "Keep only the sessions SOURCE recorded."
  (interactive
   (list (completing-read "Source: "
                          '("claude" "codex" "cursor" "opencode" "pi" "omp"
                            "openclaw" "copilot" "grok" "hermes" "jcode" "muse")
                          nil nil)))
  (memex-status--narrow :source (unless (string-empty-p source) source)))

(defun memex-status-narrow-project (project)
  "Keep only the sessions of PROJECT."
  (interactive (list (read-string "Project: ")))
  (memex-status--narrow :project (unless (string-empty-p project) project)))

(defun memex-status-narrow-directory (directory)
  "Keep only the sessions recorded under DIRECTORY."
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (memex-status--narrow :cwd (expand-file-name directory)))

(defun memex-status-narrow-since (since)
  "Keep only the sessions active since SINCE."
  (interactive (list (read-string "Active since (date or RFC3339): ")))
  (memex-status--narrow :since (unless (string-empty-p since) since)))

(defun memex-status-narrow-origin (origin)
  "Ask memex for the ORIGIN subset of sessions."
  (interactive
   (list (intern (completing-read "Origin: "
                                  '("regular" "interactive" "subagent" "all")
                                  nil t))))
  (memex-status--narrow :origin origin))

(defun memex-status-clear-narrowing ()
  "Ask memex for every session again."
  (interactive)
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (setq-local memex-status--narrowing nil)
  (memex-status-refresh))

(defun memex-status-set-limit (limit)
  "Ask memex for LIMIT sessions."
  (interactive
   (list (read-number "Sessions: " (memex-status--limit))))
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (when (or (< limit 1) (> limit memex-api-max-sessions))
    (user-error "Memex lists between 1 and %d sessions" memex-api-max-sessions))
  (setq-local memex-status--limit limit)
  (memex-status-refresh))

;;;; Ordering commands

(defun memex-status-sort-by (order)
  "Put the rows in ORDER."
  (interactive
   (list (completing-read "Order by: " (mapcar #'car memex-status-sorts) nil t)))
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (setq-local memex-status--sort
              (if (equal (car-safe memex-status--sort) order)
                  (cons order (not (cdr memex-status--sort)))
                (cons order nil)))
  (memex-status--redraw))

(defun memex-status-sort-reverse ()
  "Turn the order of the rows around."
  (interactive)
  (unless memex-status--sort
    (user-error "The rows are in the order memex answered with"))
  (setq-local memex-status--sort (cons (car memex-status--sort)
                                       (not (cdr memex-status--sort))))
  (memex-status--redraw))

(defun memex-status-sort-clear ()
  "Return the rows to the order memex answered with."
  (interactive)
  (unless (derived-mode-p 'memex-status-mode)
    (user-error "Not a memex status buffer"))
  (setq-local memex-status--sort nil)
  (memex-status--redraw))

;;;; Menus

;;;###autoload
(transient-define-prefix memex-status-sort ()
  "Order the dashboard's rows."
  [["When"
    ("r" "last active" (lambda () (interactive) (memex-status-sort-by "recent")))
    ("s" "started" (lambda () (interactive) (memex-status-sort-by "started")))]
   ["What"
    ("n" "messages" (lambda () (interactive) (memex-status-sort-by "messages")))
    ("p" "project" (lambda () (interactive) (memex-status-sort-by "project")))
    ("k" "source" (lambda () (interactive) (memex-status-sort-by "source")))
    ("l" "label" (lambda () (interactive) (memex-status-sort-by "label")))]
   ["Order"
    ("x" "column" memex-status-sort-by)
    ("f" "flip" memex-status-sort-reverse)
    ("DEL" "clear" memex-status-sort-clear)]])

;;;###autoload
(transient-define-prefix memex-status-filter ()
  "Narrow the window of sessions memex answers with."
  [["Where"
    ("p" "project" memex-status-narrow-project)
    ("d" "directory" memex-status-narrow-directory)]
   ["What"
    ("k" "source" memex-status-narrow-source)
    ("o" "origin" memex-status-narrow-origin)
    ("s" "since" memex-status-narrow-since)]
   ["Manage"
    ("L" "how many" memex-status-set-limit)
    ("DEL" "clear" memex-status-clear-narrowing)]])

;;;###autoload
(transient-define-prefix memex-status-search-dispatch ()
  "Search the transcripts memex indexed."
  [["Scope"
    ("s" memex-status-search :description memex-status--search-description)
    ("l" "search every listed" memex-status-search-listed)
    ("g" "search everything" memex-status-search-globally)]
   ["Mode"
    ("x" "lexical" (lambda () (interactive) (memex-status-search 'lexical)))
    ("m" "semantic" (lambda () (interactive) (memex-status-search 'semantic)))
    ("y" "hybrid" (lambda () (interactive) (memex-status-search 'hybrid)))]])

(defun memex-status--limit-description ()
  "Return the row budget, for the menu."
  (format "how many (%d)" (memex-status--limit)))

;;;###autoload
(transient-define-prefix memex-status-dispatch ()
  "Show the dashboard's own keys.
Every suffix here is bound directly in `memex-status-mode-map' as well."
  [:description
   (lambda () (format "memex  ·  %d session%s"
                      (length memex-status--sessions)
                      (if (= 1 (length memex-status--sessions)) "" "s")))
   ["Session"
    ("RET" "transcript" memex-status-visit)
    ("o" "other window" memex-status-visit-other-window)
    ("r" "resume" memex-status-resume)
    ("w" "copy resume command" memex-status-copy-command)]
   ["Search"
    ("s" memex-status-search :description memex-status--search-description)
    ("S" "search menu" memex-status-search-dispatch)]
   ["List"
    ("f" "narrow" memex-status-filter)
    ("O" "order" memex-status-sort)
    ("L" memex-status-set-limit
     :description memex-status--limit-description)
    ("g" "refresh" memex-status-refresh)]]
  [:class transient-row
   ("?" "close" transient-quit-one)
   ("q" "quit dashboard" quit-window)])

(provide 'memex-status)
;;; memex-status.el ends here
