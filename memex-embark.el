;;; memex-embark.el --- Embark actions on memex candidates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; The verbs a memex candidate is acted on with, bound into two embark
;; keymaps: one for the record-shaped categories, `memex-record' and
;; `memex-session', and one for `memex-project', whose candidate is a
;; bare project name and carries no record.
;;
;; Every action is a plain function of the candidate rather than a
;; command.  Embark hands a command action the target stripped of its
;; text properties, and a memex candidate carries its record in one:
;; a command would be given the label alone and recover no record.
;; `memex-herdr-resume' is a command of its own for the viewer's key, so
;; it is wrapped here rather than bound.
;;
;; Nothing outside this file is invented here.  The one action with no
;; home module is `memex-embark-show-record', which puts a single record
;; up in an aside of its own: rendering it through the viewer would leave
;; a lone record in a buffer the session registry claims, and the next
;; open of that session would render the whole transcript over it.
;;
;; Neither embark nor marginalia is required.  `memex-embark-setup' is
;; reached when embark loads, and asks marginalia for nothing when
;; marginalia is absent.

;;; Code:

(require 'memex-completion)

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar marginalia-annotators)

(declare-function memex-herdr-open-session "memex-herdr"
                  (session-id source-path &optional doc-id))
(declare-function memex-herdr-resume "memex-herdr" (record))
(declare-function memex-usage "memex-usage" (&optional project))
(declare-function memex-org-capture "memex-org" (record))

(defconst memex-embark-record-buffer-name "*memex record*"
  "Name of the buffer a single record is shown in.")

(defun memex-embark-copy-record-id (candidate)
  "Put the `doc_id' of the record CANDIDATE carries on the kill ring.
The id is printed first: memex sends it as an integer and `kill-new'
signals on a number."
  (kill-new (format "%s" (alist-get 'doc_id
                                    (memex-completion-record-of candidate)))))

(defun memex-embark--insert-record (record)
  "Insert RECORD as one line per field it carries.
Each value is printed rather than inserted, since a `doc_id' and a `ts'
arrive as integers."
  (dolist (field record)
    (insert (format "%s: %s\n" (car field) (cdr field)))))

(defun memex-embark-show-record (candidate)
  "Show the record CANDIDATE carries in a buffer of its own.
The record's own fields go up, and its session is never fetched: a lone
record rendered into a viewer buffer would join the registry
`memex-view-session-buffer' derives, and the next open of that session
would render the whole transcript over it.  One shared buffer serves
every record, so acting on a second replaces the first."
  (let ((record (memex-completion-record-of candidate))
        (buffer (get-buffer-create memex-embark-record-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'special-mode) (special-mode))
        (erase-buffer)
        (memex-embark--insert-record record)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun memex-embark-open-transcript (candidate)
  "Open the transcript of the record CANDIDATE carries and return its process.
The session is named by its `session_id' and `source_path' together, and
opened at the record's own `doc_id' rather than at its start."
  (let ((record (memex-completion-record-of candidate)))
    (memex-herdr-open-session (alist-get 'session_id record)
                              (alist-get 'source_path record)
                              (alist-get 'doc_id record))))

(defun memex-embark-resume (candidate)
  "Resume the session the record CANDIDATE carries belongs to."
  (memex-herdr-resume (memex-completion-record-of candidate)))

(defun memex-embark-usage (candidate)
  "Report memex's usage for the project CANDIDATE names and return its process.
A `memex-project' candidate is that name itself and carries no record;
every other candidate names its project in the record it carries.  The
bare name arrives with the completion faces still on it, and the report
buffer is named after it."
  (let ((record (memex-completion-record-of candidate)))
    (memex-usage (if record
                     (alist-get 'project record)
                   (substring-no-properties candidate)))))

(defun memex-embark-org-capture (candidate)
  "Capture the record CANDIDATE carries as an Org excerpt."
  (memex-org-capture (memex-completion-record-of candidate)))

(defvar-keymap memex-embark-record-map
  :doc "Keymap of the memex actions on a record or session candidate.
The id goes on `y' rather than on `i', which the parent map binds to
`embark-insert' and which a binding here would shadow."
  "y" #'memex-embark-copy-record-id
  "v" #'memex-embark-show-record
  "t" #'memex-embark-open-transcript
  "r" #'memex-embark-resume
  "u" #'memex-embark-usage
  "c" #'memex-embark-org-capture)

(defvar-keymap memex-embark-project-map
  :doc "Keymap of the memex actions on a project candidate.
A project candidate carries no record, so the actions needing one are
not on offer for it."
  "u" #'memex-embark-usage)

(defun memex-embark--annotate ()
  "Offer memex's own annotation to marginalia for the record categories."
  (dolist (category '(memex-record memex-session))
    (add-to-list 'marginalia-annotators
                 (list category #'memex-completion-annotate 'builtin 'none))))

;;;###autoload
(defun memex-embark-setup ()
  "Offer memex's candidates to embark, and to marginalia where it is loaded.
Both keymaps take `embark-general-map' as their parent here rather than
at load time, since without embark that variable is void and this file
has to load anyway.

`marginalia-annotators' is written only where it is bound.  Reaching for
it unguarded signals out of the `with-eval-after-load' form embark runs
this from, which leaves embark itself half-loaded for a user who has
embark and not marginalia.  Marginalia loading after this ran is caught
by the deferred form instead, and `add-to-list' makes the pair
idempotent for the user whose marginalia was already up."
  (dolist (map (list memex-embark-record-map memex-embark-project-map))
    (set-keymap-parent map embark-general-map))
  (dolist (entry '((memex-record . memex-embark-record-map)
                   (memex-session . memex-embark-record-map)
                   (memex-project . memex-embark-project-map)))
    (add-to-list 'embark-keymap-alist entry))
  (when (boundp 'marginalia-annotators)
    (memex-embark--annotate))
  (with-eval-after-load 'marginalia
    (memex-embark--annotate)))

(with-eval-after-load 'embark (memex-embark-setup))

;;;###autoload (with-eval-after-load 'embark (memex-embark-setup))

(provide 'memex-embark)
;;; memex-embark.el ends here
