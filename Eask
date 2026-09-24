;; -*- mode: eask; lexical-binding: t -*-

(package "memex"
         "0.1.0"
         "Read the sessions your agents leave behind")

(website-url "https://github.com/srnnkls/memex.el")
(license "GPL-3.0-or-later")

(keywords "convenience" "tools" "matching")

(package-file "memex.el")

(source 'gnu)
(source 'nongnu)
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "lectio" "0.1.0")
(depends-on "magit-section" "4.0.0")
(depends-on "transient" "0.9.0")

(load-paths ".")

(files "memex*.el" "LICENSE"
       '(:exclude "memex*-tests.el"))

(defconst memex-test-file-spec
  '("memex*-tests.el")
  "File spec for ERT suites excluded from the package files.")

(script "test"
        (let* ((default-directory (file-name-directory load-file-name))
               (files (eask-expand-file-specs memex-test-file-spec)))
          (unless files
            (error "No test file matches %S under %s"
                   memex-test-file-spec default-directory))
          (mapconcat #'identity
                     (cons "eask test ert"
                           (mapcar #'shell-quote-argument files))
                     " ")))

(add-hook 'eask-before-compile-hook
          (lambda ()
            (setq byte-compile-error-on-warn t)))
