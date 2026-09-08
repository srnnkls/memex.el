;;; memex-view.el --- Whole-session transcript viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "3.3"))
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

(require 'ansi-color)
(require 'eieio)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'memex-api)
(require 'memex-completion)
(require 'memex-entry)
(require 'memex-markdown)

(defvar so-long-predicate)
(defvar memex-entry--fields-cache)

(defcustom memex-view-display-action '(display-buffer-full-frame)
  "The `display-buffer' action a transcript is shown under.
A whole session read a few lines at a time is not read, so the viewer
takes the frame unless a caller passes a display function of its own."
  :type 'sexp
  :group 'memex)

(defcustom memex-view-markdown-roles '("user" "assistant")
  "Roles whose text is markdown an agent wrote, rather than terminal output.
Tool records are captured from a terminal, where an asterisk is a glob
and an underscore is part of a name; rendering those as markdown would
eat them."
  :type '(repeat string)
  :group 'memex)

(defconst memex-view--controls
  (rx (any "\0-\10" "\13" "\14" "\16-\37" "\177"))
  "The control characters a transcript shows as literal escapes.
Newline and tab are not among them: both carry the shape of the text.")

(defclass memex-view-transcript-section (magit-section) ()
  :documentation "The root section a whole transcript is inserted under.
Magit gives a section its `magit-section' text property only when it has
a parent, so the records need a root above them to be found at point.")

(defclass memex-view-record-section (magit-section) ()
  :documentation "A section holding one entry of a transcript.")

(defclass memex-view-tool-section (magit-section) ()
  :documentation "A section holding what an entry was given or returned.")

(defface memex-view-tool '((t :inherit font-lock-function-name-face))
  "Face for the name of the tool an entry called."
  :group 'memex)

(defface memex-view-warning '((t :inherit warning))
  "Face for an entry whose output reported that something went wrong."
  :group 'memex)

(defface memex-view-ok '((t :inherit success))
  "Face for an entry that returned without reporting anything wrong."
  :group 'memex)

(defface memex-view-human
  '((((background light)) :background "#e5e5e5" :extend t)
    (((background dark)) :background "#333333" :extend t))
  "Ground a person's turn is laid on.
The four grounds are neutral and differ only in how far they sit off the
buffer's own, so what colour there is on a line is the tool's and
nothing competes with it.  A person's turn sits furthest off: it is what
a transcript is scanned for."
  :group 'memex)

(defface memex-view-assistant
  '((((background light)) :background "#f1f1f1" :extend t)
    (((background dark)) :background "#282828" :extend t))
  "Ground the agent's turn is laid on."
  :group 'memex)

(defface memex-view-tool-ground
  '((((background light)) :background "#f8f8f8" :extend t)
    (((background dark)) :background "#212121" :extend t))
  "Ground a tool call is laid on."
  :group 'memex)

(defface memex-view-system
  '((((background light)) :background "#ededed" :extend t :inherit shadow)
    (((background dark)) :background "#2b2b2b" :extend t :inherit shadow))
  "Ground the harness's own injections are laid on."
  :group 'memex)

(defface memex-view-human-label
  '((((background light)) :foreground "#1e5fbe" :weight bold)
    (((background dark)) :foreground "#79aaf0" :weight bold))
  "Face for the marker a person's turn is headed by.
A transcript is scanned for where the person came back in, so that
marker carries more of the reader's weight than anything beside it."
  :group 'memex)

(defface memex-view-agent-label '((t :weight bold))
  "Face for the name of the agent a turn was written by.
Weight rather than colour: the agent writes most of a transcript, so
colouring its name puts colour almost everywhere and leaves none of it
meaning anything."
  :group 'memex)

(defface memex-view-glyph '((t :height 1.25))
  "Face the status glyph takes its size from.
A glyph is read at a glance down the left of the buffer rather than
read in a line, so it carries none of the text's proportions."
  :group 'memex)

(defconst memex-view--kind-faces
  '((human . memex-view-human)
    (assistant . memex-view-assistant)
    (tool . memex-view-tool-ground)
    (system . memex-view-system))
  "The ground an entry of each kind is laid on.")

(defcustom memex-view-metadata-width 12
  "Column the values of the metadata block are aligned at."
  :type 'natnum
  :group 'memex)

(defcustom memex-view-output-lines 12
  "Lines of a tool's output shown before the rest is folded away."
  :type 'natnum
  :group 'memex)

(defcustom memex-view-initial-states
  '((human . show) (assistant . show) (tool . collapse) (system . hide))
  "How much of each kind of entry a transcript opens showing.
`show' renders the entry whole, `collapse' leaves its heading and folds
the rest away, `hide' takes it out of the buffer's view altogether.  A
session is read for the conversation in it, and the calls that carried
the conversation out are worth a line each until one is asked for."
  :type '(alist :key-type (choice (const human) (const assistant)
                                  (const tool) (const system))
                :value-type (choice (const :tag "Whole" show)
                                    (const :tag "Heading only" collapse)
                                    (const :tag "Out of the way" hide)))
  :group 'memex)

(defcustom memex-view-details nil
  "Whether a transcript opens showing the numbers behind each entry.
How long a call took, when it ran, what it is called and how much it
returned: worth having on request, and worth nobody's eye on every
line of a session."
  :type 'boolean
  :group 'memex)

(defvar-local memex-view-states nil
  "How much of each kind of entry this buffer is showing.
An alist of the same shape as `memex-view-initial-states', which is
what it starts as and what `memex-view-filter' changes.")

(defcustom memex-view-chunk-size 200
  "Entries rendered per chunk, newest chunk first."
  :type 'natnum
  :group 'memex)

(defcustom memex-view-fill-delay 0.05
  "Seconds of idle time between the chunks of a transcript."
  :type 'number
  :group 'memex)

(defvar-local memex-view--problems 0
  "How many entries of this buffer's transcript reported something wrong.")

(defvar-local memex-view--pending nil
  "Pending entries and texts, both newest first, as (ENTRIES . TEXTS).")

(defvar-local memex-view--fill-timer nil
  "The timer drawing what is left of this buffer's transcript.")

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

(defface memex-view-tool-run
  '((((background light)) :foreground "#137a58" :weight bold)
    (((background dark)) :foreground "#4fc79b" :weight bold))
  "Face for a tool that ran something."
  :group 'memex)

(defface memex-view-tool-read
  '((((background light)) :foreground "#2b62c8" :weight bold)
    (((background dark)) :foreground "#7fb0f5" :weight bold))
  "Face for a tool that looked something up."
  :group 'memex)

(defface memex-view-tool-write
  '((((background light)) :foreground "#b8501d" :weight bold)
    (((background dark)) :foreground "#f0895a" :weight bold))
  "Face for a tool that changed a file."
  :group 'memex)

(defface memex-view-tool-agent
  '((((background light)) :foreground "#6a4fc0" :weight bold)
    (((background dark)) :foreground "#a892f0" :weight bold))
  "Face for a tool that handed work to another agent."
  :group 'memex)

(defface memex-view-tool-net
  '((((background light)) :foreground "#a03a86" :weight bold)
    (((background dark)) :foreground "#dd8fc8" :weight bold))
  "Face for a tool that went out to the network."
  :group 'memex)

(defconst memex-view--tool-faces
  '(("bash" . memex-view-tool-run)
    ("bashoutput" . memex-view-tool-run)
    ("killshell" . memex-view-tool-run)
    ("read" . memex-view-tool-read)
    ("glob" . memex-view-tool-read)
    ("grep" . memex-view-tool-read)
    ("toolsearch" . memex-view-tool-read)
    ("notebookread" . memex-view-tool-read)
    ("write" . memex-view-tool-write)
    ("edit" . memex-view-tool-write)
    ("multiedit" . memex-view-tool-write)
    ("notebookedit" . memex-view-tool-write)
    ("task" . memex-view-tool-agent)
    ("agent" . memex-view-tool-agent)
    ("sendmessage" . memex-view-tool-agent)
    ("workflow" . memex-view-tool-agent)
    ("webfetch" . memex-view-tool-net)
    ("websearch" . memex-view-tool-net))
  "The face each tool's name is written in.
A transcript is scanned down its left edge, so what a call was for has
to be legible as colour before it is legible as a word.")

(defun memex-view--tool-label (tool)
  "Return TOOL the way a reader writes it, or nil for no tool at all."
  (and tool (downcase tool)))

(defun memex-view--tool-face (label)
  "Return the face a tool named LABEL is shown in."
  (or (assoc-default label memex-view--tool-faces) 'memex-view-tool))

(defun memex-view--cut (text)
  "Return TEXT as (SHOWN . HIDDEN), cut to `memex-view-output-lines'.
HIDDEN is nil for text that already fits."
  (let ((lines (split-string text "\n")))
    (if (<= (length lines) memex-view-output-lines)
        (cons text nil)
      (cons (string-join (seq-take lines memex-view-output-lines) "\n")
            (- (length lines) memex-view-output-lines)))))

(defun memex-view--clock (milliseconds)
  "Return the epoch MILLISECONDS as a local time of day, or nil without one."
  (when (numberp milliseconds)
    (format-time-string "%T" (/ milliseconds 1000))))

(defcustom memex-view-heading-width 78
  "Longest a heading's description runs before it is cut."
  :type 'natnum
  :group 'memex)

(defun memex-view--summarize (text)
  "Return TEXT as one line short enough to head a section with."
  (when text
    (let* ((first (car (split-string (memex-view--clean text) "\n" t)))
           (line (string-join (split-string (or first "")) " ")))
      (if (> (length line) memex-view-heading-width)
          (concat (substring line 0 (- memex-view-heading-width 1)) "…")
        line))))

(defcustom memex-view-label-width 11
  "Column the description of every heading starts at.
A transcript is scanned down one edge, so what a call was reads at a
fixed offset whatever it was called."
  :type 'natnum
  :group 'memex)

(defun memex-view--detail (text)
  "Return a copy of TEXT marked as what `memex-view-toggle-details' hides.
The shadow goes on under whatever TEXT already carries, so marking a
line as detail does not cost it the faces it was drawn with."
  (let ((marked (copy-sequence text)))
    (put-text-property 0 (length marked) 'memex-detail t marked)
    (add-face-text-property 0 (length marked) 'shadow t marked)
    marked))

(defconst memex-view--glyphs
  '((ok . "○") (warn . "▲") (pending . "○"))
  "What each outcome of a call is drawn as.
A call is drawn hollow, so a turn someone took reads as the solid one;
only a call that reported trouble takes a shape of its own.")

(defface memex-view-source-claude
  '((((class color) (min-colors 88)) :foreground "#d97757")
    (t :inherit warning))
  "Face for the mark beside a turn Claude Code wrote."
  :group 'memex)

(defface memex-view-source-codex
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black")
    (t :inherit default))
  "Face for the mark beside a turn Codex wrote."
  :group 'memex)

(defface memex-view-source-glyph '((t :height 1.3))
  "Face lending the vendor marks their size, over their own colour."
  :group 'memex)

(defcustom memex-view-source-marks
  '(("claude" "✳" . memex-view-source-claude)
    ("codex" "⌬" . memex-view-source-codex))
  "Marks drawn before the name of the agent a turn was written by.
Each entry gives the glyph and the face it is drawn in, keyed by the
source memex recorded the session under.  A source without an entry is
drawn blank, so the names stay in the same column either way."
  :type '(alist :key-type string
                :value-type (cons (string :tag "Glyph") (face :tag "Face")))
  :group 'memex)

(defcustom memex-view-heading-clock t
  "Whether every heading says the time of day its entry was recorded."
  :type 'boolean
  :group 'memex)

(defcustom memex-view-message-padding 3
  "Pixels between the left edge of the window and every entry of a transcript.
Zero draws each entry flush against the edge.  Terminal frames measure
in whole columns and ignore this."
  :type 'natnum
  :group 'memex)

(defcustom memex-view-heading-indent 1
  "Columns between magit's fold indicator and the glyph of a heading."
  :type 'natnum
  :group 'memex)

(defun memex-view--source-mark (entry)
  "Return the glyph and face marking the agent that wrote ENTRY, or nil.
Only the agent's own turns carry it: a person's turn is the reader's and
a call is the tool's."
  (when (eq (memex-entry-kind entry) 'assistant)
    (cdr (assoc (alist-get 'source (memex-entry-call entry))
                memex-view-source-marks))))

(defun memex-view--header (entry label face &optional description)
  "Return the line ENTRY is headed with: its glyph, LABEL and DESCRIPTION.
LABEL is drawn in FACE and padded to `memex-view-label-width', so a run
of entries reads down a column rather than as a paragraph.  What the
call took and what it is called trail behind marked as detail, which
`memex-view-toggle-details' shows and hides."
  (pcase-let* ((`(,state . ,reason) (memex-entry-status entry))
               (_ (when (eq state 'warn)
                    (setq memex-view--problems (1+ memex-view--problems))))
               (call (memex-entry-call entry))
               (tool (memex-entry-tool entry))
               (took (memex-entry-duration entry))
               (doc-id (alist-get 'doc_id call)))
    (concat
     (make-string memex-view-heading-indent ?\s)
     (if-let* ((mark (and (not tool) (memex-view--source-mark entry))))
         (propertize (car mark)
                     'face (list 'memex-view-source-glyph (cdr mark)))
       (propertize (if tool (alist-get state memex-view--glyphs "·") "●")
                   'face (if tool
                             (pcase state
                               ('warn 'memex-view-warning)
                               ('ok 'memex-view-ok)
                               (_ 'shadow))
                           face)))
     " "
     (propertize label 'face face)
     (make-string (max 1 (- memex-view-label-width (string-width label))) ?\s)
     (if-let* ((clock (and memex-view-heading-clock
                           (memex-view--clock (alist-get 'ts call)))))
         (concat (propertize clock 'face 'shadow) "  ")
       "")
     (memex-view--join
      (memex-view--summarize description)
      (when reason (propertize (memex-view--summarize reason)
                               'face 'memex-view-warning)))
     (let ((detail (memex-view--join
                    (when took (if (< took 1000)
                                   (format "%dms" took)
                                 (format "%.1fs" (/ took 1000.0))))
                    (when doc-id (format "#%s" doc-id)))))
       (if (string-empty-p detail)
           ""
         (memex-view--detail (concat "  " detail)))))))

(defconst memex-view--dirty
  (rx (any "\0-\10" "\13" "\14" "\16-\37" "\r" "\177"))
  "Any character that would make `memex-view--clean' change the text.
Most fields hold none, and answering that in one scan is cheaper than
the three passes cleaning would otherwise cost each of them.")

(defun memex-view--clean (text)
  "Return TEXT with what drove a terminal taken out, or nil without any.
Tool output is captured from a terminal, so it arrives carrying the
escape sequences that drove one.  The colour codes go through
`ansi-color-filter-apply'; a carriage return becomes the line break it
stood for and the remaining control characters go."
  (when text
    (if (not (string-match-p memex-view--dirty text))
        text
      (thread-last text
                   (ansi-color-filter-apply)
                   (replace-regexp-in-string "\r\n?" "\n")
                   (replace-regexp-in-string memex-view--controls "")))))

(defcustom memex-view-fontify-tool-content t
  "Whether a tool's input and output are fontified as the code they are.
The mode comes from the tool's name and from the path it touched."
  :type 'boolean
  :group 'memex)

(defconst memex-view--tool-payload-keys
  '(("Bash" . "command")
    ("BashOutput" . "command")
    ("Write" . "content")
    ("Edit" . "new_string")
    ("MultiEdit" . "edits")
    ("NotebookEdit" . "new_source"))
  "The field of a tool's input that holds what it actually ran.
Tool input arrives as a Rust `Debug' rendering of a map, so the code is
nested inside it rather than being it.")

(defun memex-view--tool-path (record)
  "Return the file path RECORD's tool input names, or nil when it names none."
  (when-let* ((input (alist-get 'tool_input record))
              ((string-match "\"?file_path\"?[^\"]*\"\\([^\"]+\\)\"" input)))
    (match-string 1 input)))

(defun memex-view--tool-mode (record)
  "Return the major mode RECORD's tool content is written in, or nil.
A shell tool is a shell; a tool naming a file is whatever
`auto-mode-alist' makes of that name."
  (let ((tool (alist-get 'tool_name record)))
    (cond
     ((member tool '("Bash" "BashOutput" "KillShell")) 'sh-mode)
     ((memex-view--tool-path record)
      (assoc-default (memex-view--tool-path record) auto-mode-alist
                     #'string-match))
     (t nil))))

(defcustom memex-view-fontify-limit 40000
  "Longest tool field fontified, in characters.
Past this the field is shown plain: a minified bundle or a base64 blob
costs more to fontify than anyone gains from reading it in colour."
  :type 'natnum
  :group 'memex)

(defvar memex-view--fontify-buffers nil
  "Alist of major mode to the buffer kept for fontifying in that mode.")

(defun memex-view--fontify-buffer (mode)
  "Return the buffer kept in MODE, entering the mode only the first time.
Entering a major mode is the expensive part, and one transcript holds
hundreds of fields in the same handful of modes."
  (let ((buffer (alist-get mode memex-view--fontify-buffers)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer (format " *memex fontify %s*" mode) t))
      (with-current-buffer buffer
        (let ((inhibit-message t) (message-log-max nil))
          (delay-mode-hooks (funcall mode))))
      (setf (alist-get mode memex-view--fontify-buffers) buffer))
    buffer))

(defun memex-view--fontify (text mode)
  "Return TEXT carrying the faces MODE gives it, or TEXT under no MODE.
A mode that will not load costs the reader nothing: the text comes back
the way it went in."
  (if (or (null mode)
          (not (fboundp mode))
          (not memex-view-fontify-tool-content)
          (> (length text) memex-view-fontify-limit))
      text
    (condition-case nil
        (with-current-buffer (memex-view--fontify-buffer mode)
          (let ((inhibit-message t)
                (message-log-max nil)
                (inhibit-read-only t))
            (erase-buffer)
            (insert text)
            (font-lock-ensure))
          (buffer-string))
      (error text))))

(defconst memex-view--counted-keys '("lines" "bytes" "took")
  "The metadata a reader wants on request rather than on every entry.")

(defconst memex-view--languages
  '(("sh" . sh-mode) ("bash" . sh-mode) ("zsh" . sh-mode) ("shell" . sh-mode)
    ("console" . sh-mode) ("el" . emacs-lisp-mode) ("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode) ("lisp" . lisp-mode)
    ("py" . python-mode) ("python" . python-mode) ("rs" . rust-mode)
    ("rust" . rust-mode) ("js" . js-mode) ("javascript" . js-mode)
    ("ts" . js-mode) ("typescript" . js-mode) ("json" . js-mode)
    ("c" . c-mode) ("cpp" . c++-mode) ("go" . go-mode) ("rb" . ruby-mode)
    ("ruby" . ruby-mode) ("yaml" . yaml-mode) ("yml" . yaml-mode)
    ("toml" . conf-toml-mode) ("sql" . sql-mode) ("diff" . diff-mode)
    ("patch" . diff-mode) ("html" . html-mode) ("css" . css-mode))
  "The mode a fence's info string names, for the ones agents write in.")

(defun memex-view--code (language code)
  "Return CODE fontified as LANGUAGE, indented as the block it is."
  (memex-view--fontify
   (string-trim-right code)
   (or (assoc-default (downcase language) memex-view--languages)
       (and (string-match-p (rx bos (+ (in alnum "-+.")) eos) language)
            (let ((mode (intern (concat (downcase language) "-mode"))))
              (and (fboundp mode) mode))))))

(defun memex-view--insert-metadata (entry)
  "Insert ENTRY's arguments and counts as an aligned block of key and value.
An argument is drawn in the mode of the tool that took it, since the
one most worth reading is a shell command.  The counts go in under
`memex-detail': they are worth having and not worth reading past."
  (let ((mode (memex-view--tool-mode (memex-entry-call entry))))
    (dolist (field (memex-entry-metadata entry))
      (let* ((counted (member (car field) memex-view--counted-keys))
             (value (string-replace "\n" " " (cdr field)))
             (line (concat
                    "  "
                    (propertize
                     (format (format "%%-%ds" memex-view-metadata-width)
                             (car field))
                     'face 'shadow)
                    (if counted value (memex-view--fontify value mode))
                    "\n")))
        (insert (if counted (memex-view--detail line) line))))))

(defcustom memex-view-indent 2
  "Columns a section is set in from the one holding it.
magit indents nothing of its own, so a transcript's nesting is only as
readable as what its own headings are written at."
  :type 'natnum
  :group 'memex)

(defun memex-view--indent (text columns)
  "Return TEXT with COLUMNS of blanks down the front of every line."
  (let ((pad (make-string columns ?\s)))
    (concat pad (string-replace "\n" (concat "\n" pad)
                                (string-trim-right text "\n")))))

(defun memex-view--insert-body (label text mode entry)
  "Insert TEXT as a collapsed section headed LABEL, fontified in MODE.
The body is filled in when the section is opened rather than when the
transcript is built: most of them are never looked at.  ENTRY is put on
what the opening inserts, since the properties the renderer laid down
cannot reach text that did not exist yet.

Heading and body are both set in by `memex-view-indent', the body one
step further, so a fold reads as belonging to the entry above it whether
it is open or shut."
  (when (and text (not (string-empty-p text)))
    (let ((lines (length (split-string text "\n"))))
      (magit-insert-section (memex-view-tool-section (cons label text) t)
        (magit-insert-heading
          (memex-view--indent
           (propertize (format "%s (%d line%s)" label lines
                               (if (equal lines 1) "" "s"))
                       'face 'shadow)
           memex-view-indent))
        (magit-insert-section-body
          (let ((start (point)))
            (pcase-let ((`(,shown . ,hidden)
                         (memex-view--cut (memex-view--clean text))))
              (insert (memex-view--indent (memex-view--fontify shown mode)
                                          (* 2 memex-view-indent))
                      "\n")
              (when hidden
                (insert (memex-view--indent
                         (propertize (format "⋮ %d more" hidden) 'face 'shadow)
                         (* 2 memex-view-indent))
                        "\n")))
            (memex-view--dress start (point) entry)))))))

(defun memex-view--relock (start end)
  "Copy every face between START and END onto `font-lock-face' as well.
`magit-section-mode' sets `font-lock-defaults' to (nil t), which puts
font-lock in keywords-only mode, and that branch unfontifies the region
it is handed - stripping `face' off everything the renderer laid down.
The alias survives it.  An Emacs with font-lock switched off honours
`face' and ignores the alias, so a buffer carrying both reads in colour
either way."
  (let ((position start))
    (while (< position end)
      (let ((next (next-single-property-change position 'face nil end))
            (face (get-text-property position 'face)))
        (when face (put-text-property position next 'font-lock-face face))
        (setq position next)))))

(defun memex-view--pad (start end)
  "Set every line between START and END off the left edge by its padding."
  (when (> memex-view-message-padding 0)
    (let ((prefix (propertize
                   " " 'display
                   `(space :width (,memex-view-message-padding)))))
      (put-text-property start end 'line-prefix prefix)
      (put-text-property start end 'wrap-prefix prefix))))

(defun memex-view--dress (start end entry)
  "Claim START to END for ENTRY, lay its ground and keep its faces.
Washed-in text is dressed the same way as text the renderer wrote: it
belongs to the same entry and sits on the same ground, and it did not
exist when that ground was laid."
  (memex-view--claim start end entry)
  (memex-view--pad start end)
  (when-let* ((face (alist-get (memex-entry-kind entry)
                               memex-view--kind-faces)))
    (add-face-text-property start end face t))
  (memex-view--relock start end))

(defun memex-view--claim (start end entry)
  "Mark the region START to END as ENTRY's, and as its call's record.
The kind goes on as the region's `invisible' value, so filtering a kind
away is `buffer-invisibility-spec' rather than another render: the text
stays where it was and a search still reaches it.  A span already
marked as detail keeps that alongside its kind, since either one is
reason enough to hide it."
  (put-text-property start end 'memex-record (memex-entry-call entry))
  (put-text-property start end 'memex-entry entry)
  (let ((kind (memex-entry-kind entry))
        (position start))
    (while (< position end)
      (let ((next (next-single-property-change position 'memex-detail nil end)))
        (put-text-property
         position next 'invisible
         (if (get-text-property position 'memex-detail) (list kind 'detail) kind))
        (setq position next)))))

(defun memex-view--state (kind)
  "Return how much of an entry of KIND this buffer is showing."
  (or (alist-get kind memex-view-states) 'show))

(defconst memex-view--state-glyphs
  '((show . "") (collapse . "▸") (hide . "⊘"))
  "What each state is written as where the buffer says which it is in.")

(defun memex-view--next-state (state)
  "Return the state a kind in STATE is put into by asking for the next one."
  (pcase state ('show 'collapse) ('collapse 'hide) (_ 'show)))

(defun memex-view--filter-spec ()
  "Put `buffer-invisibility-spec' in step with what the states hide.
The spec keeps t throughout, which is what magit's own folds go under:
dropping it would unfold every section in the buffer."
  (setq-local buffer-invisibility-spec (list t))
  (unless memex-view-details (add-to-invisibility-spec 'detail))
  (pcase-dolist (`(,kind . ,state) memex-view-states)
    (when (eq state 'hide) (add-to-invisibility-spec kind))))

(defun memex-view--fold (sections)
  "Fold each of SECTIONS to the state its kind is in."
  (dolist (section sections)
    (if (eq (memex-view--state (memex-entry-kind (oref section value))) 'show)
        (magit-section-show section)
      (magit-section-hide section))))

(defun memex-view--apply-states ()
  "Fold every record of the transcript to the state its kind is in."
  (memex-view--filter-spec)
  (when magit-root-section
    (oset magit-root-section hidden nil)
    (memex-view--fold (oref magit-root-section children)))
  (force-mode-line-update))

(defun memex-view--set-state (kind state)
  "Show KIND at STATE from here on, and fold the buffer to match."
  (setf (alist-get kind memex-view-states) state)
  (memex-view--apply-states))

(defun memex-view--describe (kind)
  "Return KIND and the state it is in, as the filter menu lists it."
  (let ((state (memex-view--state kind)))
    (format "%-10s %s"
            (symbol-name kind)
            (propertize (symbol-name state)
                        'face (if (eq state 'show) 'memex-view-ok 'shadow)))))

(defmacro memex-view--define-cycle (kind)
  "Define the filter command putting KIND into the next state."
  (let ((name (intern (format "memex-view-cycle-%s" kind))))
    `(transient-define-suffix ,name ()
       ,(format "Show the whole, the heading or none of each %s entry." kind)
       :transient t
       :description (lambda () (memex-view--describe ',kind))
       (interactive)
       (memex-view--set-state
        ',kind (memex-view--next-state (memex-view--state ',kind))))))

(memex-view--define-cycle human)
(memex-view--define-cycle assistant)
(memex-view--define-cycle tool)
(memex-view--define-cycle system)

(transient-define-suffix memex-view-show-everything ()
  "Show every kind of entry whole."
  :transient t
  :description "show everything"
  (interactive)
  (setq-local memex-view-states
              (mapcar (lambda (cell) (cons (car cell) 'show))
                      memex-view-initial-states))
  (memex-view--apply-states))

(transient-define-suffix memex-view-reset-states ()
  "Put every kind back to the state a transcript opens in."
  :transient t
  :description "back to defaults"
  (interactive)
  (setq-local memex-view-states (copy-alist memex-view-initial-states))
  (memex-view--apply-states))

(defun memex-view-toggle-details ()
  "Show or hide the numbers behind every entry of this transcript."
  (interactive)
  (setq-local memex-view-details (not memex-view-details))
  (memex-view--filter-spec)
  (force-mode-line-update)
  (message "memex: details %s" (if memex-view-details "shown" "hidden")))

;;;###autoload (autoload 'memex-view-filter "memex-view" nil t)
(transient-define-prefix memex-view-filter ()
  "Choose how much of each kind of entry this transcript shows."
  [:description "Show the whole entry, its heading alone, or none of it"
   ("u" memex-view-cycle-human)
   ("a" memex-view-cycle-assistant)
   ("t" memex-view-cycle-tool)
   ("s" memex-view-cycle-system)]
  [("SPC" memex-view-show-everything)
   ("DEL" memex-view-reset-states)
   ("q" "done" transient-quit-one)])

(defun memex-view--label (entry)
  "Return the word ENTRY is headed by and the face it takes, as a cons.
A tool is named by itself.  A person is named `you', since a transcript
is scanned for where they came back in; an agent by the source memex
recorded it under, since a project is read across several of them and
which one wrote a passage is half of reading it."
  (if-let* ((tool (memex-view--tool-label (memex-entry-tool entry))))
      (cons tool (memex-view--tool-face tool))
    (pcase (memex-entry-kind entry)
      ('human (cons "you" 'memex-view-human-label))
      ('assistant (cons (or (alist-get 'source (memex-entry-call entry))
                            "assistant")
                        'memex-view-agent-label))
      (_ (cons "harness" 'shadow)))))

(defun memex-view--heading (entry)
  "Return the heading ENTRY is shown under.
A message is headed by who wrote it and nothing else: its description
is its own opening line, and printing that above the message prints the
message twice."
  (pcase-let ((`(,label . ,face) (memex-view--label entry)))
    (memex-view--header entry label face
                        (and (memex-entry-tool entry)
                             (memex-entry-description entry)))))

(defun memex-view--markdown-p (record)
  "Return non-nil when RECORD's text is markdown rather than terminal output."
  (member (alist-get 'role record) memex-view-markdown-roles))

(defun memex-view--texts (records)
  "Return the text each of RECORDS is drawn as, in order.
The prose goes through one markdown conversion for the whole session:
pandoc costs the same for a session as for a record of it, so a
conversion per record would take minutes over a long transcript."
  (let* ((cleaned (mapcar (lambda (record)
                            (memex-view--clean (alist-get 'text record)))
                          records))
         (prose (seq-mapn
                 (lambda (record text)
                   (and (memex-view--markdown-p record) text))
                 records cleaned))
         (drawn (memex-markdown-render-all prose #'memex-view--code)))
    (seq-mapn (lambda (text rendered) (or rendered text)) cleaned drawn)))

(defun memex-view--insert-record-body (entry text)
  "Insert and dress ENTRY's body, showing TEXT for a message."
  (let ((start (point))
        (call (memex-entry-call entry))
        (memex-entry--fields-cache
         (or memex-entry--fields-cache (make-hash-table :test #'eq))))
    (if (memex-entry-tool entry)
        (progn
          (memex-view--insert-metadata entry)
          (pcase-let ((`(,key ,mode . ,payload) (memex-entry-payload entry)))
            (when payload
              (memex-view--insert-body
               key payload (or mode (memex-view--tool-mode call)) entry)))
          (memex-view--insert-body "stdout" (memex-entry-output entry)
                                   (memex-view--tool-mode call) entry))
      (when-let* ((body (or text (memex-view--clean (alist-get 'text call)))))
        (insert body "\n")))
    (insert "\n")
    (memex-view--dress start (point) entry)))

(defun memex-view--insert-record (entry &optional text)
  "Insert ENTRY showing TEXT, its region carrying both entry and record.
`memex-record' spans the section as it always has, since embark, the
anchor and the herdr bridge read the record under point from it;
`memex-entry' carries the pair the heading and the status come off."
  (let* ((start (point))
         (kind (memex-entry-kind entry))
         (tool (memex-entry-tool entry))
         heading-end)
    (magit-insert-section (memex-view-record-section
                           entry (not (eq (memex-view--state kind) 'show)))
      (magit-insert-heading (memex-view--heading entry))
      (setq heading-end (point))
      (if tool
          (magit-insert-section-body
            (memex-view--insert-record-body entry text))
        (memex-view--insert-record-body entry text)))
    (memex-view--dress start heading-end entry)))

(defun memex-view--payload-buffer (text mode label)
  "Show TEXT in a buffer of its own named for LABEL, in MODE.
A file's contents are read in the mode they are written in."
  (let ((buffer (get-buffer-create (format "*memex %s*" label))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (when (and mode (fboundp mode))
          (condition-case nil (funcall mode) (error nil)))
        (goto-char (point-min))))
    buffer))

(defun memex-view-visit-payload ()
  "Open the body of the section point is in as a buffer of its own."
  (interactive)
  (let ((section (magit-section-at (point))))
    (unless (memex-view-tool-section-p section)
      (user-error "Point is in no payload"))
    (pcase-let* ((`(,label . ,text) (oref section value))
                 (entry (memex-view-entry-at-point))
                 (mode (or (cadr (memex-entry-payload entry))
                           (memex-view--tool-mode (memex-entry-call entry)))))
      (pop-to-buffer (memex-view--payload-buffer text mode label)))))

(defun memex-view-copy-command ()
  "Copy what the entry point is in was given, as it was given it."
  (interactive)
  (let* ((entry (or (memex-view-entry-at-point)
                    (user-error "Point is in no entry")))
         (fields (memex-entry-fields
                  (alist-get 'tool_input (memex-entry-call entry))))
         (value (or (alist-get "command" fields nil nil #'equal)
                    (cdar fields))))
    (unless value (user-error "This entry was given nothing to copy"))
    (kill-new value)
    (message "%s" value)))

(defun memex-view-entry-at-point ()
  "Return the entry point is in, or nil when it is in none."
  (get-text-property (point) 'memex-entry))

(defun memex-view--problem-p (position)
  "Return non-nil when the entry at POSITION reported something wrong."
  (when-let* ((entry (get-text-property position 'memex-entry))
              ((not (invisible-p position))))
    (eq (car (memex-entry-status entry)) 'warn)))

(defun memex-view--problem (position change)
  "Return the next position CHANGE reaches from POSITION reporting a problem."
  (let ((found position))
    (while (and (setq found (funcall change found 'memex-entry))
                (not (memex-view--problem-p found))))
    found))

(defun memex-view-next-problem ()
  "Move point to the next entry whose output reported something wrong."
  (interactive)
  (let ((position (memex-view--problem (point) #'next-single-property-change)))
    (unless position (user-error "No entry after this one went wrong"))
    (goto-char position)))

(defun memex-view-previous-problem ()
  "Move point to the previous entry whose output reported something wrong."
  (interactive)
  (let ((position (memex-view--problem (memex-view--record-start (point))
                                       #'previous-single-property-change)))
    (unless position (user-error "No entry before this one went wrong"))
    (goto-char position)))

(defun memex-view--imenu-index ()
  "Return the transcript's entries as an imenu index of their descriptions."
  (let ((position (point-min))
        (index nil))
    (while (setq position (next-single-property-change position 'memex-entry))
      (when-let* ((entry (get-text-property position 'memex-entry))
                  ((not (equal entry (caar index)))))
        (push (cons entry (copy-marker position)) index)))
    (nreverse
     (mapcar (lambda (cell)
               (cons (memex-entry-description (car cell)) (cdr cell)))
             index))))

(defun memex-view--header-line (&optional _position)
  "Return the line naming this buffer's transcript as a whole.
What the reader loses scrolling is the shape of the session, not which
entry is at the top of the window: the fold markers already say that."
  (let ((entries (and magit-root-section
                      (oref magit-root-section children))))
    (if (null entries)
        (or memex-view-session-id "")
      (let ((from (memex-view--clock
                   (alist-get 'ts (memex-entry-call (oref (car entries) value)))))
            (to (memex-view--clock
                 (alist-get 'ts (memex-entry-call
                                 (oref (car (last entries)) value))))))
        (memex-view--join
         (alist-get 'project (memex-entry-call (oref (car entries) value)))
         (format "%d entries" (length entries))
         (when (> memex-view--problems 0)
           (propertize (format "%d err" memex-view--problems)
                       'face 'memex-view-warning))
         (and from to (propertize (format "%s-%s" from to) 'face 'shadow))
         (memex-view--states-summary))))))

(defun memex-view--states-summary ()
  "Return the kinds this buffer is not showing whole, or nil showing all.
A reader who has hidden a kind needs the buffer to say so: a transcript
missing half its entries reads exactly like a short one."
  (when-let* ((folded (seq-remove (lambda (cell) (eq (cdr cell) 'show))
                                  memex-view-states)))
    (propertize
     (mapconcat (lambda (cell)
                  (format "%s%s" (alist-get (cdr cell) memex-view--state-glyphs)
                          (car cell)))
                folded " ")
     'face 'shadow)))

(defun memex-view--boundary (position change)
  "Return the first record start CHANGE reaches from POSITION, or nil.
CHANGE is `next-single-property-change' or
`previous-single-property-change', which is the direction searched."
  (let ((found position))
    (while (and (setq found (funcall change found 'memex-record))
                (or (null (get-text-property found 'memex-record))
                    (invisible-p found))))
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
  "Return the start of the region rendering the record DOC-ID, or nil.
A record still queued behind the chunks being drawn is drawn first."
  (let ((position (point-min))
        (found nil))
    (when (car memex-view--pending)
      (memex-view--fill-completely))
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

(defun memex-view--materialize-tool-sections ()
  "Insert the nested sections of tool records that are still deferred."
  (save-excursion
    (dolist (record (oref magit-root-section children))
      (when (and (memex-entry-tool (oref record value))
                 (oref record washer))
        (let ((hidden (oref record hidden)))
          (magit-section-show record)
          (when hidden (magit-section-hide record)))))))

(defun memex-view--tool-sections ()
  "Return every materialized tool section of this transcript."
  (seq-mapcat (lambda (record)
                (seq-filter #'memex-view-tool-section-p (oref record children)))
              (oref magit-root-section children)))

(defun memex-view-toggle-tool-content ()
  "Fold or unfold the tool input and output of the whole session at once.
Showing one open tool field is enough to close them all, so the command
answers what the buffer looks like rather than what it was last asked.
Once inserted, the text stays in the buffer folded so a search reaches it."
  (interactive)
  (memex-view--materialize-tool-sections)
  (let* ((sections (memex-view--tool-sections))
         (hide (seq-some (lambda (section) (not (oref section hidden))) sections)))
    (dolist (section sections)
      (if hide (magit-section-hide section) (magit-section-show section)))))

(defun memex-view--reveal (position)
  "Bring POSITION into view, whatever is folded or filtered over it.
A hit is worth nothing where the reader cannot see it, and most of a
transcript is folded away by the time one is searched for."
  (when-let* ((entry (get-text-property position 'memex-entry))
              (kind (memex-entry-kind entry))
              ((eq (memex-view--state kind) 'hide)))
    (memex-view--set-state kind 'collapse)
    (message "memex: showing the %s entries this hit is in" kind))
  (when-let* ((section (magit-section-at position)))
    (magit-section-reveal section)))

(defun memex-view--goto-position (position)
  "Move the buffer and every window showing it to POSITION."
  (goto-char position)
  (memex-view--reveal position)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-point window position)))

(defun memex-view-jump-to-hit (record)
  "Move point to where this session renders RECORD.
RECORD is a record alist, which is what the selectors and memex's
search answer with; it is found again by its `doc_id'."
  (let* ((doc-id (alist-get 'doc_id record))
         (position (memex-view--record-position doc-id)))
    (unless position
      (user-error "This session renders no record %s" doc-id))
    (memex-view--goto-position position)))

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
  "t" #'memex-view-toggle-tool-content
  "d" #'memex-view-toggle-details
  "e" #'memex-view-next-problem
  "f" #'memex-view-filter
  "w" #'memex-view-copy-command
  "RET" #'memex-view-visit-payload
  "M-e" #'memex-view-previous-problem
  "s" #'memex-view-search-in-session
  "q" #'memex-view-quit)

(defun memex-view-quit ()
  "Bury the transcript, leaving the window it was read in where it was."
  (interactive)
  (quit-restore-window nil 'bury))

(define-derived-mode memex-session-mode magit-section-mode "Memex Session"
  "Major mode for a memex session transcript.

\\{memex-session-mode-map}"
  (setq-local header-line-format '(:eval (memex-view--header-line)))
  (setq-local imenu-create-index-function #'memex-view--imenu-index)
  (setq-local memex-view-states (copy-alist memex-view-initial-states))
  (setq-local memex-view-details (default-value 'memex-view-details))
  (memex-view--filter-spec)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t)
  ;; so-long answers the long lines of minified output and base64 by
  ;; stripping this mode's keymap and fontification off the buffer.
  (setq-local so-long-predicate #'ignore))

(defun memex-view--buffer-name (session-id source-path)
  "Return the name a viewer buffer of SESSION-ID at SOURCE-PATH is made under."
  (format "*memex session %s (%s)*" session-id
          (abbreviate-file-name (or source-path ""))))

(defun memex-view-session-buffer (session-id source-path)
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

(defun memex-view--cancel-fill ()
  "Stop drawing what is left of this buffer's transcript."
  (when (timerp memex-view--fill-timer)
    (cancel-timer memex-view--fill-timer))
  (setq-local memex-view--fill-timer nil)
  (setq-local memex-view--pending nil))

(defun memex-view--schedule-fill ()
  "Draw the next chunk of this buffer's transcript once Emacs is idle."
  (when (and (car memex-view--pending) (not memex-view--fill-timer))
    (setq-local memex-view--fill-timer
                (run-with-idle-timer
                 (+ memex-view-fill-delay
                    (if-let* ((idle (current-idle-time))) (float-time idle) 0))
                 nil #'memex-view--fill (current-buffer)))))

(defun memex-view--fill-chunk ()
  "Prepend the next older chunk and return the entries still pending."
  (pcase-let ((`(,entries . ,texts) memex-view--pending))
    (when entries
      (let* ((inhibit-read-only t)
             (root magit-root-section)
             (children (oref root children))
             (take (min (max 1 memex-view-chunk-size) (length entries)))
             (position (copy-marker (point) t))
             (windows (mapcar
                       (lambda (window)
                         (list window (copy-marker (window-start window) t)
                               (copy-marker (window-point window) t)
                               (window-vscroll window t)))
                       (get-buffer-window-list (current-buffer) nil t)))
             (memex-entry--fields-cache (make-hash-table :test #'eq))
             (magit-insert-section--parent root)
             (magit-insert-section--current root)
             (magit-insert-section--oldroot nil))
        (unwind-protect
            (progn
              (oset root children nil)
              (goto-char (point-min))
              (seq-mapn #'memex-view--insert-record
                        (nreverse (seq-take entries take))
                        (nreverse (seq-take texts take)))
              (memex-view--fold (oref root children))
              (setq-local memex-view--pending
                          (cons (nthcdr take entries) (nthcdr take texts)))
              (set-buffer-modified-p nil)
              (force-mode-line-update))
          (oset root children (nconc (oref root children) children))
          (set-marker (oref root start) (point-min))
          (set-marker (oref root end) (point-max))
          (goto-char position)
          (set-marker position nil)
          (pcase-dolist (`(,window ,start ,point ,vscroll) windows)
            (when (and (window-live-p window)
                       (eq (window-buffer window) (current-buffer)))
              (set-window-start window start t)
              (set-window-point window point)
              (set-window-vscroll window vscroll t))
            (set-marker start nil)
            (set-marker point nil))))))
  (car memex-view--pending))

(defun memex-view-follow-end ()
  "Move to the last visible record's heading."
  (interactive)
  (when-let* ((section
               (seq-find (lambda (section)
                           (not (eq (memex-view--state
                                     (memex-entry-kind (oref section value)))
                                    'hide)))
                         (reverse (oref magit-root-section children)))))
    (magit-section-goto section)))

(defun memex-view--fill (buffer)
  "Draw the next chunk of BUFFER's transcript, and queue the one after it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp memex-view--fill-timer)
        (cancel-timer memex-view--fill-timer))
      (unwind-protect
          (memex-view--fill-chunk)
        (setq-local memex-view--fill-timer nil))
      (memex-view--schedule-fill))))

(defun memex-view--fill-completely ()
  "Draw whatever is left of this buffer's transcript now."
  (while (memex-view--fill-chunk))
  (memex-view--cancel-fill))

(defun memex-view--render (buffer context session-id source-path)
  "Render CONTEXT into BUFFER as the session SESSION-ID at SOURCE-PATH.
CONTEXT is the session context `memex-api-session' answers with.
The mode is entered before the session keys are set because entering it
kills the buffer-local values, so keys set beforehand are lost and
`memex-view-session-buffer' then matches the buffer no more.  A
session's source is read from its first record because every record of
one session shares it."
  (with-current-buffer buffer
    (let ((records (alist-get 'records context))
          (inhibit-read-only t))
      (memex-view--cancel-fill)
      (memex-session-mode)
      (add-hook 'kill-buffer-hook #'memex-view--cancel-fill nil t)
      (erase-buffer)
      (setq-local memex-view-session-id session-id)
      (setq-local memex-view-source-path source-path)
      (setq-local memex-view-source (alist-get 'source (car records)))
      (setq-local memex-view--problems 0)
      (let* ((entries (memex-entry-pair records))
             (memex-entry--fields-cache (make-hash-table :test #'eq))
             (texts (memex-view--texts (mapcar #'memex-entry-call entries)))
             (older (max 0 (- (length entries) (max 1 memex-view-chunk-size))))
             (magit-insert-section--parent nil))
        (magit-insert-section (memex-view-transcript-section session-id)
          (seq-mapn #'memex-view--insert-record
                    (nthcdr older entries) (nthcdr older texts)))
        (setq-local memex-view--pending
                    (cons (nreverse (seq-take entries older))
                          (nreverse (seq-take texts older))))
        (memex-view--apply-states))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (memex-view--schedule-fill))))

;;;###autoload
(defun memex-view-session (session-id source-path &optional doc-id display)
  "Show the whole session SESSION-ID at SOURCE-PATH and return its process.
The session is fetched in one request and rendered whole.  Point lands
on the record DOC-ID, or the last visible record's heading without one.  A
session already open is rendered into the buffer it is open in.
DISPLAY is called with the rendered buffer; without one the buffer goes
up under `memex-view-display-action'.  That seam is how the herdr bridge
puts the viewer in the workspace the session is pinned to."
  (interactive
   (let ((record (memex-read-session)))
     (list (alist-get 'session_id record) (alist-get 'source_path record))))
  (memex-api-session
   session-id source-path
   (lambda (context)
     (let ((buffer (or (memex-view-session-buffer session-id source-path)
                       (generate-new-buffer
                        (memex-view--buffer-name session-id source-path)))))
       (memex-view--render buffer context session-id source-path)
       (if display
           (funcall display buffer)
         (display-buffer buffer memex-view-display-action))
       (with-current-buffer buffer
         (if-let* ((position (and doc-id (memex-view--record-position doc-id))))
             (memex-view--goto-position position)
           (memex-view-follow-end)))))))

(provide 'memex-view)
;;; memex-view.el ends here
