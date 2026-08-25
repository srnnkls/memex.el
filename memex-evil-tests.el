;;; memex-evil-tests.el --- Tests for memex-evil.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-evil-tests.el -f ert-run-tests-batch-and-exit
;;
;; evil is an external package and is not on the batch load path, so the
;; bindings are asserted as a request rather than as a keymap lookup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pcase)
(require 'memex-core)
(require 'memex-api)
(require 'memex-view)

(ignore-errors (require 'memex-evil nil t))

(declare-function memex-evil-setup "memex-evil")
(declare-function memex-view-next-record "memex-view")
(declare-function memex-view-previous-record "memex-view")
(declare-function memex-view-search-in-session "memex-view")
(declare-function memex-session-mode "memex-view")
(declare-function evil-define-key* "evil-core")
(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")

(defconst memex-evil-tests--resume-command 'memex-herdr-resume
  "The MXE-007 entry point resume is expected to dispatch into.
MXE-007 carries an interactive spec on it, so a binding target that is
merely bound is not enough - it has to be `commandp'.")

(defconst memex-evil-tests--verbs
  (list #'memex-view-next-record
        #'memex-view-previous-record
        #'memex-view-search-in-session
        #'quit-window)
  "The viewer verbs evil normal state must reach in a session buffer.
Motion is evil's own, and `memex-view-jump-to-hit' is the movement
helper the search verb calls rather than a verb of its own.  Resume is
asserted apart from these, under the assumed name of MXE-007's entry
point.")

(defun memex-evil-tests--requests ()
  "Return every `evil-define-key*' request `memex-evil-setup' issues.
Each element is (STATE KEYMAP BINDINGS), BINDINGS being the flat
key-then-definition tail the function takes."
  (let ((calls nil))
    (cl-letf (((symbol-function 'evil-define-key*)
               (lambda (state keymap &rest bindings)
                 (push (list state keymap bindings) calls))))
      (memex-evil-setup))
    (nreverse calls)))

(defun memex-evil-tests--bindings (state)
  "Return the bindings `memex-evil-setup' requests for STATE.
Each element is (KEY-DESCRIPTION KEYMAP . DEFINITION)."
  (let ((result nil))
    (pcase-dolist (`(,states ,keymap ,bindings) (memex-evil-tests--requests))
      (when (memq state (if (listp states) states (list states)))
        (while bindings
          (push (cons (key-description (pop bindings))
                      (cons keymap (pop bindings)))
                result))))
    (nreverse result)))

(defun memex-evil-tests--evil-or-skip ()
  "Require the evil installed on this machine, or skip the calling test."
  (unless (featurep 'evil)
    (let ((build (car (file-expand-wildcards
                       (expand-file-name
                        "~/.config/emacs/.local/straight/build-*/evil")))))
      (unless build
        (ert-skip "evil is not installed"))
      (add-to-list 'load-path build)))
  (unless (require 'evil nil t)
    (ert-skip "evil did not load")))

(ert-deftest memex-evil-loads-and-degrades-without-evil ()
  "memex-evil.el loads with no evil installed and leaves the viewer working.
It pulls in neither evil nor the herdr bridge, and the viewer's own
keymap keeps the plain bindings `memex-view' put there - the major mode
stays evil-agnostic.  The herdr assertion also keeps the resume test's
own `require' from reaching this one: ERT runs the batch alphabetically,
so this test sorts first, and if that ever stops holding the assertion
fails loudly instead of quietly passing on a pre-loaded feature."
  (should (eq (require 'memex-evil nil t) 'memex-evil))
  (should (fboundp 'memex-evil-setup))
  (should-not (featurep 'memex-herdr))
  (unless (locate-library "evil")
    (should-not (featurep 'evil)))
  (let ((buffer (generate-new-buffer " *memex-evil-tests*")))
    (unwind-protect
        (with-current-buffer buffer
          (memex-session-mode)
          (should (eq (keymap-lookup memex-session-mode-map "n")
                      #'memex-view-next-record))
          (should (eq (keymap-lookup memex-session-mode-map "p")
                      #'memex-view-previous-record))
          (should (eq (keymap-lookup memex-session-mode-map "s")
                      #'memex-view-search-in-session)))
      (kill-buffer buffer))))

(ert-deftest memex-evil-requests-a-resume-binding-into-the-herdr-entry-point ()
  "Resume gets a normal-state key dispatching into MXE-007's command.
The target has to be invocable, not merely named: a plain `defun' over a
required RECORD argument is bindable and errors on the first keypress.
The bridge is required outright rather than probed, so an absent one
fails the assertion instead of retiring it."
  (should (require 'memex-evil nil t))
  (should (require 'memex-herdr nil t))
  (should (commandp memex-evil-tests--resume-command))
  (should (rassoc (cons memex-session-mode-map
                        memex-evil-tests--resume-command)
                  (memex-evil-tests--bindings 'normal))))

(ert-deftest memex-evil-requests-no-binding-for-search-repeat ()
  "Evil's `n' and `N' keep repeating a search in a session buffer."
  (should (require 'memex-evil nil t))
  (let ((keys (mapcar #'car (memex-evil-tests--bindings 'normal))))
    (should keys)
    (should-not (member "n" keys))
    (should-not (member "N" keys))))

(ert-deftest memex-evil-requests-normal-state-bindings-for-the-viewer-verbs ()
  "Every viewer verb gets a normal-state key on the session mode's own keymap.
The keymap is the argument, not the mode symbol: a mode symbol routes to
`evil-define-minor-mode-key', which never applies to a major mode."
  (should (require 'memex-evil nil t))
  (let ((bindings (memex-evil-tests--bindings 'normal)))
    (dolist (binding bindings)
      (should (eq (cadr binding) memex-session-mode-map)))
    (dolist (verb memex-evil-tests--verbs)
      (should (commandp verb))
      (should (rassoc (cons memex-session-mode-map verb) bindings)))))

(ert-deftest memex-evil-with-evil-loaded-reaches-the-verbs-in-normal-state ()
  "With evil installed, the requested keys really do run the verbs.
`where-is-internal' drops a key another active map shadows, so a verb
answers here only if evil normal state leaves memex's key alone."
  (memex-evil-tests--evil-or-skip)
  (should (require 'memex-evil nil t))
  (memex-evil-setup)
  (let ((buffer (generate-new-buffer " *memex-evil-tests-live*")))
    (unwind-protect
        (with-current-buffer buffer
          (memex-session-mode)
          (evil-local-mode 1)
          (evil-normal-state)
          (dolist (verb memex-evil-tests--verbs)
            (let ((key (where-is-internal verb (current-active-maps t) t)))
              (should key)
              (should (eq (key-binding key) verb))))
          (dolist (key '("n" "N"))
            (let ((command (key-binding (kbd key))))
              (should command)
              (should-not (string-prefix-p "memex-" (symbol-name command))))))
      (kill-buffer buffer))))

(provide 'memex-evil-tests)
;;; memex-evil-tests.el ends here
