;;; memex-view.el --- Whole-session transcript viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-view-session' asks memex for a whole session in one `session'
;; operation and renders every record of it into a read-only buffer.  A
;; record's whole region carries its alist in the `memex-record' text
;; property, the same property `memex-completion.el' puts on a candidate:
;; navigation, embark and the herdr bridge read the record under point
;; from there rather than from the text around it.
;;
;; A session is a `session_id' at a `source_path' and a viewer buffer is
;; keyed on both.  The registry of open sessions is derived from
;; `buffer-list' on every open rather than stored anywhere.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-api)
(require 'memex-completion)

(defvar so-long-predicate)

(defconst memex-view-tool-invisibility 'memex-view-tool
  "The `invisible' property the folded tool fields of a record carry.")

(defvar-local memex-view-session-id nil
  "The `session_id' of the session this buffer renders.")

(defvar-local memex-view-source-path nil
  "The `source_path' of the transcript this buffer renders.")

(defvar-local memex-view-source nil
  "The agent this buffer's session was recorded by, as memex names it.")

(defun memex-view--join (&rest fields)
  "Return the FIELDS carrying something as one line."
  (string-join (delq nil fields) "  "))

(defun memex-view--time (milliseconds)
  "Return the epoch MILLISECONDS as a local timestamp, or nil without one."
  (when (numberp milliseconds)
    (format-time-string "%F %T" (/ milliseconds 1000))))

(defun memex-view--header (record)
  "Return the line RECORD is rendered under."
  (let ((doc-id (alist-get 'doc_id record)))
    (memex-view--join (alist-get 'role record)
                      (memex-view--time (alist-get 'ts record))
                      (alist-get 'tool_name record)
                      (and doc-id (format "#%s" doc-id)))))

(defun memex-view--insert-tool-field (label text fold)
  "Insert TEXT under LABEL, folded out of sight when FOLD.
Does nothing when the record carried no such field."
  (when text
    (let ((start (point)))
      (insert label ": " text "\n")
      (when fold
        (put-text-property start (point) 'invisible
                           memex-view-tool-invisibility)))))

(defun memex-view--insert-record (record)
  "Insert RECORD, its whole region carrying it in `memex-record'."
  (let ((start (point))
        (fold (equal (alist-get 'role record) "tool_result")))
    (insert (propertize (memex-view--header record)
                        'face 'font-lock-comment-face)
            "\n")
    (when-let* ((text (alist-get 'text record)))
      (insert text "\n"))
    (memex-view--insert-tool-field "input" (alist-get 'tool_input record) fold)
    (memex-view--insert-tool-field "output" (alist-get 'tool_output record) fold)
    (insert "\n")
    (put-text-property start (point) 'memex-record record)))

(defun memex-view--boundary (position change)
  "Return the first record start CHANGE reaches from POSITION, or nil.
CHANGE is `next-single-property-change' or
`previous-single-property-change', which is the direction searched."
  (let ((found position))
    (while (and (setq found (funcall change found 'memex-record))
                (null (get-text-property found 'memex-record))))
    found))

(defun memex-view--record-start (position)
  "Return the start of the record covering POSITION, or POSITION in none."
  (if (and (get-text-property position 'memex-record)
           (> position (point-min))
           (eq (get-text-property position 'memex-record)
               (get-text-property (1- position) 'memex-record)))
      (or (previous-single-property-change position 'memex-record) (point-min))
    position))

(defun memex-view--record-position (doc-id)
  "Return the start of the region rendering the record DOC-ID, or nil."
  (let ((position (point-min))
        (found nil))
    (while (and position (not found))
      (let ((record (get-text-property position 'memex-record)))
        (if (equal (alist-get 'doc_id record) doc-id)
            (setq found position)
          (setq position (next-single-property-change position 'memex-record)))))
    found))

(defun memex-view-record-at-point ()
  "Return the record point is in, or nil when it is in none."
  (get-text-property (point) 'memex-record))

(defun memex-view-next-record ()
  "Move point to the start of the record after the one it is in."
  (interactive)
  (let ((position (memex-view--boundary (point) #'next-single-property-change)))
    (unless position (user-error "No next record"))
    (goto-char position)))

(defun memex-view-previous-record ()
  "Move point to the start of the record before the one it is in."
  (interactive)
  (let* ((start (memex-view--record-start (point)))
         (position (or (memex-view--boundary
                        start #'previous-single-property-change)
                       (and (> start (point-min))
                            (get-text-property (point-min) 'memex-record)
                            (point-min)))))
    (unless position (user-error "No previous record"))
    (goto-char position)))

(defun memex-view-toggle-tool-content ()
  "Fold or unfold the tool input and output of this session.
The text stays in the buffer either way, so a search reaches it while
it is folded."
  (interactive)
  (if (memq memex-view-tool-invisibility buffer-invisibility-spec)
      (remove-from-invisibility-spec memex-view-tool-invisibility)
    (add-to-invisibility-spec memex-view-tool-invisibility)))

(defun memex-view-jump-to-hit (record)
  "Move point to where this session renders RECORD.
RECORD is a record alist, which is what the selectors and memex's
search answer with; it is found again by its `doc_id'."
  (let* ((doc-id (alist-get 'doc_id record))
         (position (memex-view--record-position doc-id)))
    (unless position
      (user-error "This session renders no record %s" doc-id))
    (goto-char position)))

(defun memex-view-search-in-session (query)
  "Search this session alone for QUERY and move point to the hit read.
The search is narrowed by the session scope of this buffer - its source,
`session_id' and `source_path' together, since a `session_id' names a
session only along with the transcript it was read from.

The hit is read and point moved once the asynchronous response lands, so
the function itself returns the request process rather than the movement;
that process is what `memex-cancel-rpc' takes."
  (interactive (list (read-string "memex search in session: ")))
  (let ((buffer (current-buffer)))
    (memex-api-search
     query
     (lambda (matches)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if (null matches)
               (message "memex search: this session has no hit for %s" query)
             (memex-view-jump-to-hit
              (let ((enable-recursive-minibuffers t))
                (memex-read-record "memex hit: " (mapcar #'cadr matches))))))))
     :session-scope (list (list :source memex-view-source
                                :session-id memex-view-session-id
                                :source-path memex-view-source-path)))))

(defvar-keymap memex-session-mode-map
  :doc "Keymap for `memex-session-mode'."
  "n" #'memex-view-next-record
  "p" #'memex-view-previous-record
  "TAB" #'memex-view-toggle-tool-content
  "s" #'memex-view-search-in-session)

(define-derived-mode memex-session-mode special-mode "Memex Session"
  "Major mode for a memex session transcript.

\\{memex-session-mode-map}"
  (setq-local buffer-invisibility-spec (list memex-view-tool-invisibility))
  ;; so-long answers the long lines of minified output and base64 by
  ;; stripping this mode's keymap and fontification off the buffer.
  (setq-local so-long-predicate #'ignore))

(defun memex-view--buffer-name (session-id source-path)
  "Return the name a viewer buffer of SESSION-ID at SOURCE-PATH is made under."
  (format "*memex session %s (%s)*" session-id
          (abbreviate-file-name (or source-path ""))))

(defun memex-view--buffer (session-id source-path)
  "Return the live viewer buffer of SESSION-ID at SOURCE-PATH, or nil.
The registry is derived from `buffer-list' on every open and stored
nowhere, so it carries no entry a killed buffer could leave stale.  A nil
SESSION-ID keys no session and matches no buffer: both locals default to
nil outside a viewer, so an unkeyed lookup would otherwise adopt whatever
buffer the user is in and erase it."
  (when session-id
    (seq-find (lambda (buffer)
                (and (eq (buffer-local-value 'major-mode buffer)
                         'memex-session-mode)
                     (equal (buffer-local-value 'memex-view-session-id buffer)
                            session-id)
                     (equal (buffer-local-value 'memex-view-source-path buffer)
                            source-path)))
              (buffer-list))))

(defun memex-view--render (buffer context session-id source-path)
  "Render CONTEXT into BUFFER as the session SESSION-ID at SOURCE-PATH.
CONTEXT is the session context `memex-api-session' answers with.
The mode is entered before the session keys are set because entering it
kills the buffer-local values, so keys set beforehand are lost and
`memex-view--buffer' then matches the buffer no more.  A session's source
is read from its first record because every record of one session shares
it."
  (with-current-buffer buffer
    (let ((records (alist-get 'records context))
          (inhibit-read-only t))
      (memex-session-mode)
      (erase-buffer)
      (setq-local memex-view-session-id session-id)
      (setq-local memex-view-source-path source-path)
      (setq-local memex-view-source (alist-get 'source (car records)))
      (mapc #'memex-view--insert-record records)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

;;;###autoload
(defun memex-view-session (session-id source-path &optional doc-id)
  "Show the whole session SESSION-ID at SOURCE-PATH and return its process.
The session is fetched in one request and rendered whole.  Point lands
on the record DOC-ID, or at the start of the transcript without one.  A
session already open is rendered into the buffer it is open in."
  (interactive
   (let ((record (memex-read-session)))
     (list (alist-get 'session_id record) (alist-get 'source_path record))))
  (memex-api-session
   session-id source-path
   (lambda (context)
     (let ((buffer (or (memex-view--buffer session-id source-path)
                       (generate-new-buffer
                        (memex-view--buffer-name session-id source-path)))))
       (memex-view--render buffer context session-id source-path)
       (when doc-id
         (with-current-buffer buffer
           (when-let* ((position (memex-view--record-position doc-id)))
             (goto-char position))))
       (display-buffer buffer)))))

(provide 'memex-view)
;;; memex-view.el ends here
