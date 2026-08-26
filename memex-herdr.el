;;; memex-herdr.el --- Resume an indexed session in a herdr tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-herdr-resume' takes a session memex indexed back to a live
;; agent: it opens a herdr tab in the session's working directory and
;; sends the resume command memex recorded for it.  A session memex knows
;; no resume command for, and one whose transcript is gone from disk, are
;; shown in the read-only viewer instead.
;;
;; The resume command is not part of the RPC surface, so it is read from
;; `memex sessions --json-array'.  That subcommand has no session filter:
;; it answers with a window of recent sessions, `memex-resume-lookup-limit'
;; long, which is searched here for the `session_id' at the `source_path'
;; asked for - a `session_id' names a session only along with the
;; transcript it was read from.
;;
;; herdr and the `+ws-pin' helpers of the user's Doom configuration are
;; both optional: each is probed for at call time and its absence costs
;; only the feature it carries.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-completion)
(require 'memex-view)

(declare-function herdr-start-server-if-needed "herdr-core" ())
(declare-function herdr-api-tab-create "herdr-api" (&rest keys))
(declare-function herdr-api-pane-send-text "herdr-api" (pane-id text))
(declare-function +ws-pin-of "ext:+workspace-pins" (buffer))
(declare-function +ws-pin-buffer "ext:+workspace-pins"
                  (buffer &optional workspace))
(declare-function +ws-pin-follow "ext:+workspace-pins" (buffer))

(defcustom memex-resume-lookup-limit 5000
  "Number of recent sessions the resume lookup reads before matching.
The window has to be long enough to reach back to the session being
resumed, since it is the only filter `memex sessions' offers."
  :type 'natnum
  :group 'memex)

(defun memex-herdr--display (buffer)
  "Show BUFFER in the workspace it belongs to.
An unpinned buffer is pinned to the current workspace and one already
pinned is followed to its own.  Following is idempotent and so is the
`display-buffer' advice the pins install, so neither switches twice."
  (when (fboundp '+ws-pin-of)
    (if (+ws-pin-of buffer)
        (when (fboundp '+ws-pin-follow) (+ws-pin-follow buffer))
      (when (fboundp '+ws-pin-buffer) (+ws-pin-buffer buffer))))
  (display-buffer buffer))

;;;###autoload
(defun memex-herdr-open-session (session-id source-path &optional doc-id)
  "Show the session SESSION-ID at SOURCE-PATH and return its process.
Point lands on the record DOC-ID, or at the start of the transcript
without one.  The viewer goes up in the workspace the session is pinned
to where the `+ws-pin' helpers are installed, and in whatever window
`display-buffer' picks where they are not."
  (memex-view-session session-id source-path doc-id #'memex-herdr--display))

(defun memex-herdr--ready ()
  "Ready the session lookup, refusing without `memex-executable' on PATH.
The lookup shells it out, so a resume without it on PATH comes back with
no row and reads as a session out of the lookup window; refusing here is
what keeps the two apart."
  (unless (executable-find memex-executable)
    (user-error "Memex executable not found: %s" memex-executable)))

(defun memex-herdr--ready-server ()
  "Ready the herdr server a tab is about to be created against.
Only a resume that reaches an agent needs herdr; the branches that end in
the viewer are answered on a machine that has none, so the probe belongs
here rather than at the head of a resume."
  (unless (fboundp 'herdr-start-server-if-needed)
    (user-error "Herdr is not installed: no herdr-start-server-if-needed"))
  (condition-case failure (herdr-start-server-if-needed)
    (herdr-error (user-error "Cannot ready herdr: %s"
                             (error-message-string failure)))))

(defun memex-herdr--row (session-id source-path source)
  "Return what memex knows of the session SESSION-ID at SOURCE-PATH.
The window of recent SOURCE sessions is read in one shell-out and
matched here on the two fields together.  A shell-out that cannot run at
all - no SOURCE to ask about - answers with no rows, which the caller
reports as the session it could not find."
  (let ((rows (condition-case nil
                  (with-temp-buffer
                    (let ((default-directory temporary-file-directory))
                      (when (eq 0 (call-process
                                   memex-executable nil t nil
                                   "sessions" "--json-array"
                                   "--source" source
                                   "--limit" (number-to-string
                                              memex-resume-lookup-limit)))
                        (memex--decode (buffer-string)))))
                (error nil))))
    (seq-find (lambda (row)
                (and (equal (alist-get 'session_id row) session-id)
                     (equal (alist-get 'source_path row) source-path)))
              rows)))

(defun memex-herdr--directory (row)
  "Return the directory a resume of ROW begins in.
The session's own working directory, else the root of the repository it
was recorded in, else the directory its transcript sits in."
  (or (alist-get 'cwd row)
      (alist-get 'git_root row)
      (file-name-directory (alist-get 'source_path row))))

(defun memex-herdr--start (row command)
  "Open a herdr tab for ROW and send COMMAND to the agent in it.
The tab is created without a workspace, which puts it in the focused
one."
  (let* ((tab (herdr-api-tab-create :cwd (memex-herdr--directory row)))
         (pane (alist-get 'pane_id (alist-get 'root_pane tab))))
    (herdr-api-pane-send-text pane (concat command "\n"))))

;;;###autoload
(defun memex-herdr-resume (record)
  "Resume the session RECORD belongs to in a herdr tab, returning nothing.
RECORD is a record alist, which is what the selectors, the viewer and
memex's search all carry: its `session_id', `source_path' and `source'
name the session.  Called in a viewer buffer it resumes that buffer's
session without prompting.

The session is shown in the viewer instead when its transcript is gone
from disk or memex recorded no resume command for it; what the branches
answer with differs and none of it is meant to be read."
  (interactive
   (list (if (derived-mode-p 'memex-session-mode)
             (list (cons 'session_id memex-view-session-id)
                   (cons 'source_path memex-view-source-path)
                   (cons 'source memex-view-source))
           (memex-read-session))))
  (let ((session-id (alist-get 'session_id record))
        (source-path (alist-get 'source_path record))
        (source (alist-get 'source record))
        (doc-id (alist-get 'doc_id record)))
    (memex-herdr--ready)
    (if (not (file-exists-p source-path))
        (progn
          (message "memex resume: nothing is left at %s, showing what memex indexed"
                   source-path)
          (memex-herdr-open-session session-id source-path doc-id))
      (let* ((row (memex-herdr--row session-id source-path source))
             (command (alist-get 'resume_cmd row)))
        (cond
         ((null row)
          (user-error
           "Memex found no session %s at %s in the last %d sessions; raise memex-resume-lookup-limit"
           session-id source-path memex-resume-lookup-limit))
         ((or (null command) (string-empty-p command))
          (message "memex resume: no %s resume command for this session, showing the transcript"
                   source)
          (memex-herdr-open-session session-id source-path doc-id))
         (t (memex-herdr--ready-server)
            (memex-herdr--start row command)))))))

(provide 'memex-herdr)
;;; memex-herdr.el ends here
