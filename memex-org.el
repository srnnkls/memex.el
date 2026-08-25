;;; memex-org.el --- Org links and capture for memex records -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; The `memex' Org link type and `memex-org-capture', which excerpts a
;; record into Org text under a link back to it.
;;
;; A link names a record the way the rest of memex.el does: a
;; `session_id' at a `source_path', then a `doc_id' within it.  All
;; three travel percent-escaped through the link path, so the reserved
;; characters of Org's own syntax survive a transcript stored under a
;; path carrying them.  Following a link hands that triple to the herdr
;; bridge when it is loaded and to the viewer when it is not; this file
;; never requires the bridge, so capture works without herdr.
;;
;; Nothing here invents a capture target.  Without
;; `memex-org-capture-template' the excerpt is put on the kill ring and
;; up in a buffer, which asks an unconfigured Org setup for nothing.

;;; Code:

(require 'subr-x)
(require 'ol)
(require 'org)
(require 'org-capture)
(require 'memex-completion)
(require 'memex-view)

(declare-function memex-herdr-open-session "memex-herdr"
                  (session-id source-path &optional doc-id))

(defconst memex-org-link-escapes '(?% ?: ?\[ ?\] ?\\ ?\s ?\t ?\n ?\r)
  "Characters a link path carries percent-escaped.
`:' separates the fields of a path and the rest are what Org link
syntax reserves, `%' among them because it opens an escape itself.")

(defconst memex-org-excerpt-buffer-name "*memex excerpt*"
  "Name an excerpt buffer is made under.")

(defcustom memex-org-capture-template nil
  "Key of the `org-capture' template a memex excerpt is captured into.
Without one the excerpt goes to the kill ring and a buffer instead, so
capturing a record needs no capture target configured."
  :type '(choice (const :tag "Kill ring and a buffer" nil)
                 (string :tag "Capture template key"))
  :group 'memex)

(defun memex-org--escape (value)
  "Return VALUE escaped for a link path, or the empty path field without one."
  (org-link-encode (or value "") memex-org-link-escapes))

(defun memex-org-link-path (record)
  "Return the link path naming RECORD, without the `memex:' type prefix.
The path carries the session RECORD belongs to - a `session_id' at a
`source_path', since one id under two paths is two sessions - and then
its `doc_id', which a record memex sent without one omits."
  (let ((doc-id (alist-get 'doc_id record)))
    (string-join (delq nil
                       (list (memex-org--escape (alist-get 'session_id record))
                             (memex-org--escape (alist-get 'source_path record))
                             (and doc-id (number-to-string doc-id))))
                 ":")))

(defun memex-org-link-parse (path)
  "Return the record link PATH names as a plist.
The plist carries `:session-id', `:source-path' and `:doc-id', the last
of them the integer memex spells a `doc_id' as on the wire and nil for a
path naming a whole session."
  (let ((fields (split-string path ":"))
        (doc-id nil))
    (when (and (nth 2 fields) (not (string-empty-p (nth 2 fields))))
      (setq doc-id (string-to-number (nth 2 fields))))
    (list :session-id (org-link-decode (or (nth 0 fields) ""))
          :source-path (org-link-decode (or (nth 1 fields) ""))
          :doc-id doc-id)))

(defun memex-org--description (record)
  "Return the text a link to RECORD is shown under."
  (let ((doc-id (alist-get 'doc_id record)))
    (string-join (delq nil (list "memex"
                                 (alist-get 'project record)
                                 (alist-get 'role record)
                                 (and doc-id (format "#%s" doc-id))))
                 " ")))

(defun memex-org-link (record)
  "Return the Org link to RECORD."
  (org-link-make-string (concat "memex:" (memex-org-link-path record))
                        (memex-org--description record)))

;;;###autoload
(defun memex-org-follow (path &optional _arg)
  "Open the record the memex link PATH names.
The herdr bridge takes the record when it is loaded and the viewer takes
it when it is not."
  (let ((record (memex-org-link-parse path)))
    (funcall (if (fboundp 'memex-herdr-open-session)
                 #'memex-herdr-open-session
               #'memex-view-session)
             (plist-get record :session-id)
             (plist-get record :source-path)
             (plist-get record :doc-id))))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "memex" :follow #'memex-org-follow))

(defun memex-org--excerpt (record)
  "Return RECORD as Org text quoted under a link back to it.
The record's own text is escaped for the quote block, so a line of it
opening with `*' or `#+' stays inside the block instead of ending it and
writing itself into the outline it was filed under."
  (string-join (list (memex-org-link record)
                     "#+begin_quote"
                     (org-escape-code-in-string
                      (or (alist-get 'text record)
                          (alist-get 'tool_output record)
                          ""))
                     "#+end_quote")
               "\n"))

(defun memex-org--display (excerpt)
  "Put EXCERPT up in an Org buffer of its own."
  (let ((buffer (generate-new-buffer memex-org-excerpt-buffer-name)))
    (with-current-buffer buffer
      (org-mode)
      (insert excerpt)
      (goto-char (point-min)))
    (display-buffer buffer)))

;;;###autoload
(defun memex-org-capture (record)
  "Capture RECORD as an Org excerpt and return that excerpt.
`memex-org-capture-template' names the `org-capture' template the
excerpt is filed through; without one it lands on the kill ring and in a
buffer instead."
  (interactive (list (memex-read-record)))
  (let ((excerpt (memex-org--excerpt record)))
    (if memex-org-capture-template
        (org-capture-string excerpt memex-org-capture-template)
      (kill-new excerpt)
      (memex-org--display excerpt))
    excerpt))

(provide 'memex-org)
;;; memex-org.el ends here
