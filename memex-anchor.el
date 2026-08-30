;;; memex-anchor.el --- Read an indexed record where the live agent drew it -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:


;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-completion)
(require 'memex-entry)
(require 'memex-view)

(declare-function memex-herdr-resume "memex-herdr" (record))
(declare-function memex-herdr--row "memex-herdr" (session-id source-path source))
(declare-function memex-herdr--directory "memex-herdr" (row))
(declare-function memex-herdr--start "memex-herdr" (row command &optional name))

(declare-function herdr-agents "herdr" ())
(declare-function herdr-attach-entry "herdr" (entry))
(declare-function herdr-terminal-buffer "herdr" (terminal-id))
(declare-function herdr-session-for "herdr" (&optional directory))
(declare-function herdr-attach-ready-p "herdr" (&optional buffer))
(defvar herdr-attach-ready-hook)
(defvar herdr-attach-takeover)

(defgroup memex-anchor nil
  "Reading an indexed record in the live agent's scrollback."
  :group 'memex)

(defcustom memex-anchor-window 45
  "Letters of a record that must appear on screen together for a match.
Shorter windows match a different session in the same project often
enough to land the reader in the wrong conversation."
  :type 'natnum
  :group 'memex-anchor)

(defun memex-anchor--skeleton (text)
  "Return TEXT's letters as (SKELETON . POSITIONS).
POSITIONS maps each index of SKELETON to the index in TEXT it came
from.  Digits go along with the punctuation because diff gutters draw
line numbers into the middle of a span."
  (let* ((n (length text))
         (skeleton (make-string n ?\s))
         (positions (make-vector (max n 1) 0))
         (j 0))
    (dotimes (i n)
      (let ((c (downcase (aref text i))))
        (when (and (>= c ?a) (<= c ?z))
          (aset skeleton j c)
          (aset positions j i)
          (setq j (1+ j)))))
    (cons (substring skeleton 0 j) (substring positions 0 j))))

(defun memex-anchor--windows (skeleton width)
  "Return the windows of SKELETON WIDTH letters long, each with its offset.
Windows overlap by half so a record is probed along its whole length:
the terminal truncates long output, and what survives may be anywhere
but the head."
  (if (< (length skeleton) width)
      (and (>= (length skeleton) 24) (list (cons 0 skeleton)))
    (let ((stride (max 1 (/ width 2)))
          (i 0)
          (out nil))
      (while (<= (+ i width) (length skeleton))
        (push (cons i (substring skeleton i (+ i width))) out)
        (setq i (+ i stride)))
      (nreverse out))))

(defun memex-anchor--locate-reduced (text haystack positions)
  "Return where TEXT was drawn in an already reduced screen, or nil.
HAYSTACK is that screen's letters and POSITIONS maps them back to it.
Taking the reduction as an argument is what keeps a lookup over many
records from re-reducing the same screen once per record."
  (let ((needle (car (memex-anchor--skeleton text)))
        (found nil))
    (unless (string-empty-p haystack)
      (dolist (window (memex-anchor--windows needle memex-anchor-window))
        (unless found
          (let ((at (string-search (cdr window) haystack)))
            (when at (setq found (aref positions at)))))))
    found))

(defun memex-anchor--locate (text screen)
  "Return where in SCREEN the record TEXT was drawn, or nil.
Both sides are reduced to their letters, which is what survives a
terminal: the markdown it strips, the glyphs it prefixes and the
columns it breaks words across all fall out of the comparison."
  (pcase-let ((`(,haystack . ,positions) (memex-anchor--skeleton screen)))
    (memex-anchor--locate-reduced text haystack positions)))

(defun memex-anchor-on-screen-p (text screen)
  "Return non-nil when the record TEXT is drawn anywhere in SCREEN."
  (and (memex-anchor--locate text screen) t))

(defun memex-anchor--agent-session (record agent)
  "Return non-nil when AGENT reports RECORD's session."
  (when-let* ((reported (alist-get 'agent_session agent))
              (value (alist-get 'value reported)))
    (pcase (alist-get 'kind reported)
      ("id" (equal value (alist-get 'session_id record)))
      ("path" (equal value (alist-get 'source_path record))))))

(defun memex-anchor--by-session (record)
  "Return the Herdr pane whose agent reports RECORD's session."
  (seq-some (lambda (agent)
              (and (memex-anchor--agent-session record agent)
                   (alist-get 'pane_id agent)))
            (memex-anchor--agents)))

(defun memex-anchor--by-directory (record row)
  "Return ROW's sole Herdr agent of RECORD's kind, or nil when ambiguous."
  (when-let* ((directory (and row (alist-get 'cwd row)))
              (kind (alist-get 'source record))
              (matching (seq-filter
                         (lambda (agent)
                           (and (equal (alist-get 'cwd agent) directory)
                                (equal (alist-get 'agent agent) kind)))
                         (memex-anchor--agents))))
    (and (equal (length matching) 1)
         (alist-get 'pane_id (car matching)))))

(defvar-local memex-anchor-pane-id nil
  "The herdr pane whose terminal this buffer is attached to.")

(defvar-local memex-anchor-session-id nil
  "The `session_id' the agent in this buffer was running.")

(defun memex-anchor--session-buffer (session-id)
  "Return the live buffer already attached to SESSION-ID's agent, or nil.
Derived from `buffer-list' rather than stored, so a terminal the user
detached leaves no entry behind to go stale.  Finding one is what lets a
second jump into the same conversation skip both the request and the
sweep across every pane."
  (when session-id
    (seq-find (lambda (buffer)
                (and (buffer-live-p buffer)
                     (equal (buffer-local-value 'memex-anchor-session-id buffer)
                            session-id)))
              (buffer-list))))

(defun memex-anchor--herdr-p ()
  "Return non-nil once herdr's API is loaded, loading it if it is installed.
herdr autoloads its commands but not `herdr-api', so a bridge that only
probed `fboundp' would report herdr missing on a machine running it."
  (or (fboundp 'herdr-agents)
      (and (require 'herdr nil t) (fboundp 'herdr-agents))))

(defun memex-anchor--agents ()
  "Return herdr's live agent entries, or nil where herdr is not to be had."
  (when (memex-anchor--herdr-p)
    (condition-case nil
        (herdr-agents)
      (error nil))))

(defcustom memex-anchor-takeover t
  "Whether the attached terminal accepts input while its window has focus.
With Herdr's focus-following session stream, control and geometry return
to its foreground client as soon as focus leaves Emacs; the buffer keeps
observing and its searchable scrollback stays intact.  Nil keeps the
Emacs terminal read-only throughout."
  :type 'boolean
  :group 'memex-anchor)

(defun memex-anchor--attach (entry)
  "Attach the terminal of the agent ENTRY and return its buffer, or nil.
The live terminal is the session, so point lands in the conversation
rather than in a copy of it.  A terminal already attached is switched to
rather than attached twice."
  (when (and entry (require 'herdr nil t) (fboundp 'herdr-attach-entry))
    (let ((terminal (alist-get 'terminal_id entry))
          (herdr-attach-takeover memex-anchor-takeover))
      (or (and terminal (fboundp 'herdr-terminal-buffer)
               (herdr-terminal-buffer terminal))
          (condition-case nil
              (herdr-attach-entry
               (if (alist-get 'session entry)
                   entry
                 (cons (cons 'session (and (fboundp 'herdr-session-for)
                                           (herdr-session-for)))
                       entry)))
            (error nil))))))

(defun memex-anchor--goto (buffer text)
  "Put point in BUFFER where TEXT was drawn, returning non-nil when found."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((at (memex-anchor--locate text (buffer-string))))
        (goto-char (+ (point-min) at))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))
        t))))

(defun memex-anchor--resume-command (record)
  "Return (COMMAND . ROW) for RECORD, retaining ROW without a command."
  (when (fboundp 'memex-herdr--row)
    (when-let* ((row (memex-herdr--row (alist-get 'session_id record)
                                       (alist-get 'source_path record)
                                       (alist-get 'source record))))
      (let ((command (alist-get 'resume_cmd row)))
        (cons (and (stringp command)
                   (not (string-empty-p command))
                   command)
              row)))))

(defun memex-anchor--start (record command row)
  "Resume RECORD's session with COMMAND in a herdr pane for ROW."
  (memex-anchor--enter record (memex-herdr--start
                               row command (alist-get 'session_id record))))

(defun memex-anchor--resume (record resume)
  "Start RECORD's session from RESUME, or show its indexed transcript."
  (condition-case failure
      (if-let* ((command (car-safe resume)))
          (progn
            (message "memex anchor: no pane is running this session, resuming it")
            (memex-anchor--start record command (cdr resume)))
        (memex-anchor--fallback
         record "no pane is running this session and memex knows no way to"))
    (error (memex-anchor--fallback record (error-message-string failure)))))

(defun memex-anchor--fallback (record why)
  "Show RECORD in the viewer, saying WHY the live agent was not used."
  (message "memex anchor: %s, showing the indexed transcript" why)
  (memex-view-session (alist-get 'session_id record)
                      (alist-get 'source_path record)
                      (alist-get 'doc_id record)))

(defun memex-anchor--finish-land (buffer record pane-id)
  "Land on RECORD in ready BUFFER, falling back from PANE-ID when absent."
  (if (memex-anchor--goto buffer (alist-get 'text record))
      buffer
    (memex-anchor--fallback
     record (format "%s is running this session but has scrolled past this record"
                    pane-id))))

(defun memex-anchor--land-when-ready (buffer record pane-id)
  "Finish landing on RECORD once BUFFER has received PANE-ID's first frame."
  (let (finish)
    (setq finish
          (lambda ()
            (remove-hook 'herdr-attach-ready-hook finish t)
            (when (buffer-live-p buffer)
              (memex-anchor--finish-land buffer record pane-id))))
    (with-current-buffer buffer
      (add-hook 'herdr-attach-ready-hook finish nil t)))
  buffer)

(defun memex-anchor--land (buffer record session-id pane-id)
  "Show BUFFER with point where RECORD was drawn, remembering what it is.
SESSION-ID and PANE-ID are stamped on the buffer so a later jump into
the same conversation finds it without resolving anything."
  (with-current-buffer buffer
    (setq-local memex-anchor-session-id session-id)
    (setq-local memex-anchor-pane-id pane-id))
  (pop-to-buffer buffer)
  (cond
   ((memex-anchor--goto buffer (alist-get 'text record)) buffer)
   ((and (fboundp 'herdr-attach-ready-p)
         (not (herdr-attach-ready-p buffer)))
    (memex-anchor--land-when-ready buffer record pane-id))
   (t (memex-anchor--finish-land buffer record pane-id))))

(defcustom memex-anchor-target 'terminal
  "Where to read a record whose live Herdr pane is known.
`transcript' opens the indexed session.  `terminal' attaches its live
terminal and lands on the record when it remains in that buffer."
  :type '(choice (const :tag "The indexed transcript" transcript)
                 (const :tag "The agent's own terminal" terminal))
  :group 'memex-anchor)

(defun memex-anchor--enter (record pane-id)
  "Land on RECORD for the session PANE-ID is running.
`memex-anchor-target' says whether that is the viewer or the pane's own
terminal."
  (if (eq memex-anchor-target 'terminal)
      (let* ((entry (seq-find (lambda (agent)
                                (equal (alist-get 'pane_id agent) pane-id))
                              (memex-anchor--agents)))
             (buffer (memex-anchor--attach (or entry `((pane_id . ,pane-id))))))
        (if (buffer-live-p buffer)
            (memex-anchor--land buffer record (alist-get 'session_id record)
                                pane-id)
          (memex-anchor--fallback
           record (format "herdr would not attach %s" pane-id))))
    (message "memex anchor: %s is running this session" pane-id)
    (memex-view-session (alist-get 'session_id record)
                        (alist-get 'source_path record)
                        (alist-get 'doc_id record))))

(defun memex-anchor--finish-resolution (record pane-id resume)
  "Open RECORD from PANE-ID or precomputed RESUME data."
  (let ((pane-id (or pane-id
                     (memex-anchor--by-directory record (cdr-safe resume)))))
    (if pane-id
        (memex-anchor--enter record pane-id)
      (memex-anchor--resume record resume))))

(defun memex-anchor--resolve-with-resume (record resume)
  "Resolve RECORD through Herdr, using the precomputed RESUME data."
  (if (not (memex-anchor--herdr-p))
      (memex-anchor--fallback record "herdr is not available here")
    (memex-anchor--finish-resolution
     record (memex-anchor--by-session record) resume)))

(defun memex-anchor-resume (record resume)
  "Attach RECORD's live session or start it from RESUME data."
  (let ((memex-anchor-target 'terminal))
    (memex-anchor--resolve-with-resume record resume)))

(defun memex-anchor--resolve (record)
  "Resolve RECORD to a live pane, a resumed session, or its transcript."
  (if (not (memex-anchor--herdr-p))
      (memex-anchor--fallback record "herdr is not available here")
    (let ((pane-id (memex-anchor--by-session record)))
      (if pane-id
          (memex-anchor--enter record pane-id)
        (memex-anchor--finish-resolution
         record nil (memex-anchor--resume-command record))))))

;;;###autoload
(defun memex-anchor-show (record)
  "Open the agent still running RECORD's session, at RECORD.
RECORD is a record alist, which is what the selectors, the viewer and
memex's search all carry.  The agent's own terminal is attached and
point put where it drew RECORD, so the session is entered rather than
copied and can be typed at from where it is read.

A session already attached is switched to directly.  Otherwise Herdr's
reported session ID or transcript path selects its pane; an older
unreported session may use the sole matching agent in its directory.
A session nobody is running is resumed, and an unavailable one opens in
the indexed viewer.

A record outside the attached terminal's retained history cannot be
located there; the indexed viewer remains the complete history."
  (interactive
   (list (if (derived-mode-p 'memex-session-mode)
             (or (memex-view-record-at-point)
                 (user-error "No record at point"))
           (memex-read-record))))
  (let* ((session-id (alist-get 'session_id record))
         (open (memex-anchor--session-buffer session-id)))
    (if open
        (memex-anchor--land open record session-id
                            (buffer-local-value 'memex-anchor-pane-id open))
      (memex-anchor--resolve record))))

;;;###autoload
(defun memex-anchor-setup ()
  "Bind the anchor onto the viewer's map."
  (keymap-set memex-session-mode-map "a" #'memex-anchor-show))

(with-eval-after-load 'memex-view (memex-anchor-setup))
;;;###autoload (with-eval-after-load 'memex-view (memex-anchor-setup))

(provide 'memex-anchor)
;;; memex-anchor.el ends here
