;;; memex-tests-support.el --- Load path for the suites -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; The viewer requires magit-section, which lives wherever the user's
;; package manager put it rather than beside these files.  Every suite
;; that reaches the viewer loads this first so `emacs -Q --batch -L .'
;; keeps working as the README documents it.

;;; Code:

(defconst memex-tests-support-packages
  '("magit-section" "compat" "dash" "cond-let" "llama" "transient" "seq")
  "Packages the suites put on `load-path' before loading the viewer.
Only these: putting every installed package on the path costs the suite
more than naming them does.")

(defconst memex-tests-support-roots
  (list (expand-file-name "~/.config/emacs/.local/straight")
        (expand-file-name "~/.emacs.d/.local/straight")
        (expand-file-name "straight" user-emacs-directory)
        (expand-file-name "~/.emacs.d/elpa"))
  "Directories the suites look for those packages under.")

(defun memex-tests-support-add-packages ()
  "Put `memex-tests-support-packages' on `load-path' where they are found."
  (dolist (name memex-tests-support-packages)
    (dolist (root memex-tests-support-roots)
      (dolist (dir (file-expand-wildcards
                    (expand-file-name (concat "build-*/" name) root)))
        (when (file-directory-p dir) (add-to-list 'load-path dir))))))

(memex-tests-support-add-packages)

(provide 'memex-tests-support)
;;; memex-tests-support.el ends here
