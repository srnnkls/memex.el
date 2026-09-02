;; -*- mode: eask; lexical-binding: t -*-

(package "memex"
         "0.1.0"
         "Search indexed agent conversation history")

(website-url "https://github.com/srnnkls/memex.el")

(keywords "convenience" "tools" "matching")

(package-file "memex.el")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")

;; The aggregate suite exercises the fallback where Consult cannot be loaded.
;; Installing it in that test environment changes five tests to the live path.
(development
 (depends-on "embark")
 (depends-on "evil")
 (depends-on "marginalia"))

(files "memex*.el" '(:exclude "memex*-tests.el"))

(defconst memex-test-file-spec
  '("memex-tests.el")
  "File spec for the aggregate ERT suite, which `files' does not ship.")

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
