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
shows, `g s' searches the session and `g r' resumes it in herdr.  `g U',
`g A', `g T' and `g S' put one kind of entry in or out of the view,
taking evil's `g' prefix rather than the bare keys the viewer binds them
on, which normal state spends on its own verbs.

`q' and `g Q' quit the viewer through `memex-view-quit'.  The `q' that
magit-section's evil bindings would otherwise lend the viewer is
`quit-window', and a popup framework that remaps it closes the window
the transcript borrowed rather than handing it back."
  (evil-define-key* 'normal memex-session-mode-map
                    (kbd "M-n") #'memex-view-next-record
                    (kbd "M-p") #'memex-view-previous-record
                    (kbd "g TAB") #'memex-view-toggle-tool-content
                    (kbd "g f") #'memex-view-filter
                    (kbd "g U") #'memex-view-toggle-human
                    (kbd "g A") #'memex-view-toggle-assistant
                    (kbd "g T") #'memex-view-toggle-tool
                    (kbd "g S") #'memex-view-toggle-system
                    (kbd "g s") #'memex-view-search-in-session
                    (kbd "g r") #'memex-herdr-resume
                    (kbd "g a") #'memex-anchor-show
                    (kbd "q") #'memex-view-quit
                    (kbd "g Q") #'memex-view-quit))

(with-eval-after-load 'evil (memex-evil-setup))

(provide 'memex-evil)
;;; memex-evil.el ends here
