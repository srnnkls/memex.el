;;; memex-markdown.el --- Aliases over lectio -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (lectio "0.1.0"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; The renderer that lived here moved to lectio.el, where memex and the
;; packages that read the same markdown share it.  These aliases stay for
;; a caller that looks the old names up - herdr-status finds
;; `memex-markdown-render' by name - and go once those callers have moved.

;;; Code:

(require 'lectio)

(define-obsolete-function-alias 'memex-markdown-render
  #'lectio-render "0.2.0")
(define-obsolete-function-alias 'memex-markdown-render-all
  #'lectio-render-all "0.2.0")
(define-obsolete-function-alias 'memex-markdown-html
  #'lectio-html "0.2.0")
(define-obsolete-face-alias 'memex-markdown-code
  'lectio-code "0.2.0")

(provide 'memex-markdown)
;;; memex-markdown.el ends here
