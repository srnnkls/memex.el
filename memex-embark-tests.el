;;; memex-embark-tests.el --- Tests for memex-embark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-embark-tests.el -f ert-run-tests-batch-and-exit
;;
;; embark and marginalia are external packages and are not on the batch load
;; path, so what `memex-embark-setup' registers is asserted against stubbed
;; registries rather than against a loaded embark.  The one test that does load
;; the embark installed on this machine drives `embark--act' for real: it is
;; what proves an action receives the candidate with its text properties still
;; on it, which every other test here assumes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)
(require 'memex-view)
(require 'memex-completion)

(defvar embark-keymap-alist)
(defvar embark-general-map)
(defvar marginalia-annotators)
(defvar nerd-icons-completion-category-icons)

(defconst memex-embark-tests--load
  (list :embark-before (featurep 'embark)
        :loaded (ignore-errors (require 'memex-embark nil t))
        :embark-after (featurep 'embark)
        :marginalia-after (featurep 'marginalia)
        :general-map (boundp 'embark-general-map))
  "What loading memex-embark.el did, captured as this file loads.
Taken here rather than in the test body because the end-to-end test
loads embark for real, and a later run of the degradation test would
otherwise read that as memex-embark.el having pulled embark in.")

(declare-function memex-embark-setup "memex-embark")
(declare-function memex-embark-copy-record-id "memex-embark")
(declare-function memex-embark-show-record "memex-embark")
(declare-function memex-embark-open-transcript "memex-embark")
(declare-function memex-embark-resume "memex-embark")
(declare-function memex-embark-usage "memex-embark")
(declare-function memex-embark-org-capture "memex-embark")
(declare-function embark--act "embark")

(defconst memex-embark-tests--actions
  '(memex-embark-copy-record-id
    memex-embark-show-record
    memex-embark-open-transcript
    memex-embark-resume
    memex-embark-usage
    memex-embark-org-capture)
  "AD-7's six actions, the whole of what a memex candidate is acted on with.
Close and rename are absent by decision: a memex candidate carries no
herdr identity to target them at.")

(defconst memex-embark-tests--record-buffer "*memex record*"
  "The buffer a single record is shown in, the transcript's own aside.")

(defconst memex-embark-tests--records
  (list '((source . "claude")
          (doc_id . 388401)
          (ts . 1787671116043)
          (project . "memex.el")
          (session_id . "sess-alpha-0001")
          (role . "assistant")
          (text . "alpha answer about the index")
          (source_path . "/tmp/memex-embark-tests/alpha.jsonl"))
        '((source . "codex")
          (doc_id . 9001)
          (ts . 1787671117099)
          (project . "herdr.el")
          (session_id . "sess-beta-0002")
          (role . "user")
          (text . "beta question about resume")
          (source_path . "/tmp/memex-embark-tests/beta.jsonl")))
  "Two records disagreeing on every field an action is asserted to carry.
An action answering with a constant is wrong for one of them.  Both
`doc_id's are integers because that is what memex sends on the wire, and
an action treating one as a string signals on every real record.")

(defconst memex-embark-tests--candidates
  (memex-completion-record-candidates memex-embark-tests--records)
  "The fixture records as the candidates memex's own selectors build.")

(defun memex-embark-tests--registration (&optional marginalia)
  "Return what `memex-embark-setup' registers, given stubbed registries.
The answer is a plist of the keymap alist, the marginalia annotators and
the stub general map the call leaves behind, which is the whole of the
request memex-embark makes of packages that are not installed here.
`marginalia-annotators' is bound only when MARGINALIA is non-nil; left
alone it stays void, as it is for the user who has embark installed and
marginalia not."
  (let ((embark-keymap-alist nil)
        (embark-general-map (make-sparse-keymap)))
    (cl-progv (and marginalia '(marginalia-annotators)) (and marginalia '(nil))
      (memex-embark-setup)
      (list :keymaps embark-keymap-alist
            :annotators (and marginalia marginalia-annotators)
            :general-map embark-general-map))))

(defun memex-embark-tests--maps-for (category registration)
  "Return the keymap symbols REGISTRATION registers for CATEGORY."
  (let ((entry (assq category (plist-get registration :keymaps))))
    (and entry (ensure-list (cdr entry)))))

(defun memex-embark-tests--annotators-for (category registration)
  "Return the marginalia entry REGISTRATION registers for CATEGORY."
  (assq category (plist-get registration :annotators)))

(defun memex-embark-tests--memex-bindings (map)
  "Return the memex actions MAP binds, without duplicates.
Bindings inherited from embark's own parent keymap are left out by
name: they are embark's, not memex's."
  (let ((bound nil))
    (map-keymap (lambda (_key definition)
                  (when (and (symbolp definition)
                             (string-prefix-p "memex-" (symbol-name definition)))
                    (push definition bound)))
                map)
    (seq-uniq (nreverse bound))))

(defun memex-embark-tests--calls (thunk)
  "Return what THUNK dispatched into, memex's targets replaced by recorders.
Every element is (SYMBOL . ARGUMENTS).  The viewer and the session
request are recorded alongside the dispatch targets, so an action
reaching for a whole transcript shows up as a call rather than as a
subprocess."
  (let ((calls nil))
    (cl-letf* ((recorder (lambda (symbol)
                           (lambda (&rest args) (push (cons symbol args) calls) nil)))
               ((symbol-function 'memex-herdr-open-session)
                (funcall recorder 'memex-herdr-open-session))
               ((symbol-function 'memex-herdr-resume)
                (funcall recorder 'memex-herdr-resume))
               ((symbol-function 'memex-usage)
                (funcall recorder 'memex-usage))
               ((symbol-function 'memex-org-capture)
                (funcall recorder 'memex-org-capture))
               ((symbol-function 'memex-view-session)
                (funcall recorder 'memex-view-session))
               ((symbol-function 'memex-api-session)
                (funcall recorder 'memex-api-session)))
      (funcall thunk))
    (nreverse calls)))

(defun memex-embark-tests--embark-or-skip ()
  "Require the embark installed on this machine, or skip the calling test."
  (unless (featurep 'embark)
    (dolist (name '("compat" "embark"))
      (let ((build (car (file-expand-wildcards
                         (expand-file-name
                          (format "~/.config/emacs/.local/straight/build-*/%s"
                                  name))))))
        (unless build (ert-skip "embark is not installed"))
        (add-to-list 'load-path build))))
  (unless (require 'embark nil t) (ert-skip "embark did not load")))

(ert-deftest memex-embark-actions-are-plain-functions-of-the-candidate ()
  "The six actions are functions, never commands, and the maps bind those only.
Embark hands a command action `substring-no-properties' of the target
\(embark.el:2091-2096), so a `commandp' action is given a bare string and
recovers no record - it fails at the keypress, not at load.  A
non-interactive action of one argument is handed the candidate itself.
`memex-herdr-resume' carries an interactive spec of its own for MXE-009,
so binding it into a map directly is that same defect: the map may bind
memex's wrapper and nothing else memex owns."
  (should (require 'memex-embark nil t))
  (memex-embark-tests--registration)
  (dolist (action memex-embark-tests--actions)
    (should (fboundp action))
    (should-not (commandp action))
    (should (equal (func-arity action) '(1 . 1))))
  (should (equal (sort (memex-embark-tests--memex-bindings
                        (symbol-value 'memex-embark-record-map))
                       #'string<)
                 (sort (copy-sequence memex-embark-tests--actions) #'string<)))
  (should (equal (memex-embark-tests--memex-bindings
                  (symbol-value 'memex-embark-project-map))
                 '(memex-embark-usage))))

(ert-deftest memex-embark-dispatches-the-candidate-record-into-each-target ()
  "Each action reads the record off the candidate and hands it on whole.
The transcript takes the session's compound key and the record's
`doc_id' - dropping the third argument opens the session at its start
instead of at the record acted on.  Resume and capture take the record
itself, usage the record's project.  The copied id is the printed form
of the integer memex sends, as `kill-new' signals on a number."
  (should (require 'memex-embark nil t))
  (cl-loop
   for record in memex-embark-tests--records
   for candidate in memex-embark-tests--candidates do
   (let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil))
     (memex-embark-copy-record-id candidate)
     (should (equal (car kill-ring) (format "%s" (alist-get 'doc_id record)))))
   (let ((calls (memex-embark-tests--calls
                 (lambda ()
                   (memex-embark-open-transcript candidate)
                   (memex-embark-resume candidate)
                   (memex-embark-usage candidate)
                   (memex-embark-org-capture candidate)))))
     (should (equal (alist-get 'memex-herdr-open-session calls)
                    (list (alist-get 'session_id record)
                          (alist-get 'source_path record)
                          (alist-get 'doc_id record))))
     (should (equal (alist-get 'memex-herdr-resume calls) (list record)))
     (should (equal (alist-get 'memex-usage calls)
                    (list (alist-get 'project record))))
     (should (equal (alist-get 'memex-org-capture calls) (list record))))))

(ert-deftest memex-embark-degrades-with-neither-embark-nor-marginalia-installed ()
  "memex-embark.el loads with neither package installed and registers nothing.
Nothing may be defined at load time that needs `embark-general-map': it
is void without embark, and the package has to load anyway.  The
registration is deferred instead, through the cookied form the autoload
file carries and the cookie on the entry point it calls - without the
second one that form reaches a function no one has loaded."
  (should (eq (plist-get memex-embark-tests--load :loaded) 'memex-embark))
  (should-not (plist-get memex-embark-tests--load :embark-before))
  (should-not (plist-get memex-embark-tests--load :embark-after))
  (should-not (plist-get memex-embark-tests--load :marginalia-after))
  (should-not (plist-get memex-embark-tests--load :general-map))
  (should (fboundp 'memex-embark-setup))
  (let ((source (with-temp-buffer
                  (insert-file-contents (locate-library "memex-embark.el"))
                  (buffer-string))))
    (should (string-match-p
             (concat ";;;###autoload[[:space:]]+(with-eval-after-load[[:space:]]+"
                     "'embark[[:space:]]+(memex-embark-setup))")
             source))
    (should (string-match-p ";;;###autoload[[:space:]]+(defun memex-embark-setup "
                            source))))

(ert-deftest memex-embark-registers-two-keymaps-and-the-record-annotator ()
  "Both record-shaped categories share one keymap; projects get their own.
A project candidate is a bare string, so the actions needing a record
are not on offer for it.  The annotation memex already renders for a
record candidate is handed to marginalia for the same two categories,
and to no other, and as the entry's first annotator: marginalia takes
`car' of the entry as the one it renders with and the rest as the order
cycling walks \(marginalia.el:463-467), so an entry led by `none'
annotates a memex candidate with nothing.  Both maps parent to
`embark-general-map', without which acting on a memex candidate loses
embark's own verbs - select, act-all, collect, export, insert, become
and the isearch pair.  The registration is also asked for with
`marginalia-annotators' void, which is how it stands for the user who
has embark and not marginalia: reaching for it unguarded signals out of
the `with-eval-after-load' form embark runs it from, and embark itself
then fails to load."
  (should (require 'memex-embark nil t))
  (dolist (marginalia '(nil t))
    (let ((registration (memex-embark-tests--registration marginalia)))
      (should (equal (memex-embark-tests--maps-for 'memex-record registration)
                     '(memex-embark-record-map)))
      (should (equal (memex-embark-tests--maps-for 'memex-session registration)
                     '(memex-embark-record-map)))
      (should (equal (memex-embark-tests--maps-for 'memex-project registration)
                     '(memex-embark-project-map)))
      (dolist (map '(memex-embark-record-map memex-embark-project-map))
        (should (keymapp (symbol-value map)))
        (should (eq (keymap-parent (symbol-value map))
                    (plist-get registration :general-map))))
      (when marginalia
        (dolist (category '(memex-record memex-session))
          (should (eq (cadr (memex-embark-tests--annotators-for
                             category registration))
                      'memex-completion-annotate)))
        (should-not (memex-embark-tests--annotators-for
                     'memex-project registration))))))

(ert-deftest memex-embark-reports-usage-for-the-project-candidate-itself ()
  "A `memex-project' candidate is its project name and carries no record.
Recovering a record from it answers nil, and a nil project reports on
every project instead of the one acted on."
  (should (require 'memex-embark nil t))
  (dolist (project '("memex.el" "sibling/other-project"))
    (let ((calls (memex-embark-tests--calls
                  (lambda () (memex-embark-usage project)))))
      (should (equal (alist-get 'memex-usage calls) (list project))))))

(ert-deftest memex-embark-shows-one-record-in-a-read-only-buffer-of-its-own ()
  "Showing a record renders the record itself, never fetching its session.
The record's own fields go up in one shared buffer, so acting on a
second record replaces the first rather than adding to it.  A lone
record must not land in a viewer buffer: that buffer would join the
registry `memex-view-session-buffer' derives, and the next open of the
session would render the whole transcript into it (AD-6).  The buffer
also has to reach a window - built, filled and handed back without being
shown, the keypress looks to the user like it did nothing."
  (should (require 'memex-embark nil t))
  (unwind-protect
      (cl-loop
       for record in memex-embark-tests--records
       for other in (reverse memex-embark-tests--records)
       for candidate in memex-embark-tests--candidates do
       (let* ((calls (memex-embark-tests--calls
                      (lambda () (memex-embark-show-record candidate))))
              (buffer (get-buffer memex-embark-tests--record-buffer)))
         (should-not calls)
         (should buffer)
         (should (get-buffer-window buffer t))
         (should-not (memex-view-session-buffer (alist-get 'session_id record)
                                                (alist-get 'source_path record)))
         (with-current-buffer buffer
           (should buffer-read-only)
           (let ((shown (buffer-string)))
             (dolist (field '(doc_id role text))
               (should (string-match-p
                        (regexp-quote (format "%s" (alist-get field record)))
                        shown)))
             (should-not (string-match-p (regexp-quote (alist-get 'text other))
                                         shown))))))
    (when (get-buffer memex-embark-tests--record-buffer)
      (kill-buffer memex-embark-tests--record-buffer))))

(ert-deftest memex-embark-with-embark-loaded-hands-the-action-the-record ()
  "Driven through the embark installed here, an action still gets the record.
This is the assumption the rest of the file rests on, taken from embark
rather than asserted about it: `embark--act' strips the properties off a
target bound for a command and leaves them on for a function, so the
`doc_id' reaching the kill ring is proof the record survived the trip."
  (memex-embark-tests--embark-or-skip)
  (should (require 'memex-embark nil t))
  (memex-embark-setup)
  (let ((record (car memex-embark-tests--records))
        (candidate (car memex-embark-tests--candidates))
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (interprogram-cut-function nil))
    (should (memq 'memex-embark-record-map
                  (ensure-list (alist-get 'memex-record embark-keymap-alist))))
    (embark--act 'memex-embark-copy-record-id
                 (list :type 'memex-record :target candidate))
    (should (equal (car kill-ring) (format "%s" (alist-get 'doc_id record))))))

(ert-deftest memex-embark-gives-each-category-an-icon-where-nerd-icons-is-up ()
  "Every memex category reaches nerd-icons under a spec it can draw.
The registry is stubbed the way the marginalia one is, and the entry is
asserted down to the shape nerd-icons reads it in: an icon function, an
icon name and a face, taken as the first three of the entry's `cdr'
\(nerd-icons-completion.el:111-117).  The names are asserted rather
than resolved, since nerd-icons is no more on the batch load path than
embark is."
  (should (require 'memex-embark nil t))
  (let ((embark-keymap-alist nil)
        (embark-general-map (make-sparse-keymap)))
    (cl-progv '(nerd-icons-completion-category-icons) '(nil)
      (memex-embark-setup)
      (dolist (category '(memex-record memex-session memex-project))
        (let ((spec (cdr (assq category nerd-icons-completion-category-icons))))
          (should (string-prefix-p "nerd-icons-" (symbol-name (nth 0 spec))))
          (should (string-prefix-p "nf-" (nth 1 spec)))
          (should (string-prefix-p "nerd-icons-" (symbol-name (nth 2 spec)))))))))

(provide 'memex-embark-tests)
;;; memex-embark-tests.el ends here
