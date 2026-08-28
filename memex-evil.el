;;; memex-evil.el --- Evil bindings for the session viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-session-mode' reaches its verbs on plain keys, all of which evil
;; normal state shadows.  `memex-evil-setup' gives each verb a normal-state
;; key of its own in the auxiliary keymap evil keeps inside
;; `memex-session-mode-map', so the major mode stays evil-agnostic and its
;; plain keys keep answering wherever evil is not.
;;
;; Motion stays evil's own, and so does every key the viewer is given:
;; `n' and `N' go on repeating a search, and no other normal-state binding
;; is displaced either.  Neither evil nor the herdr bridge is loaded by
;; this file - the bindings are made when evil loads, and resume is reached
;; through an autoload.

;;; Code:

(require 'memex-view)

(declare-function evil-define-key* "evil-core"
                  (state keymap key def &rest bindings))

(autoload 'memex-herdr-resume "memex-herdr" nil t)
(autoload 'memex-anchor-show "memex-anchor" nil t)

(defun memex-evil-setup ()
  "Give the session viewer's verbs a key in evil normal state.
`M-n' and `M-p' walk the records, keeping Emacs's own sense of a next
and a previous element, since evil's `g n' and `g p' are match motions.
The rest sit under evil's `g' prefix, which is where a mode's own verbs
belong: `g TAB' folds a tool result, `g f' chooses what the transcript
shows, `g s' searches the session, `g r' resumes it in herdr and `g Q'
quits the viewer."
  (evil-define-key* 'normal memex-session-mode-map
                    (kbd "M-n") #'memex-view-next-record
                    (kbd "M-p") #'memex-view-previous-record
                    (kbd "g TAB") #'memex-view-toggle-tool-content
                    (kbd "g f") #'memex-view-filter
                    (kbd "g s") #'memex-view-search-in-session
                    (kbd "g r") #'memex-herdr-resume
                    (kbd "g a") #'memex-anchor-show
                    (kbd "g Q") #'quit-window))

(with-eval-after-load 'evil (memex-evil-setup))

(provide 'memex-evil)
;;; memex-evil.el ends here
