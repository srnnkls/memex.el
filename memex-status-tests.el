;;; memex-status-tests.el --- Tests for memex-status.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-status-tests.el -f ert-run-tests-batch-and-exit
;;
;; `memex-api-sessions' is replaced for the duration of a refresh, so the
;; dashboard draws without a memex install or an indexed corpus, and the
;; request it would have sent is asserted on directly.  Row assertions read
;; the session off its section rather than out of the rendered line, since
;; that is the seam every command goes through.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)

(require 'memex-tests-support)

(require 'magit-section nil t)
(require 'memex-status nil t)

(declare-function memex-status-mode "memex-status")
(declare-function memex-status-refresh "memex-status")
(declare-function memex-status-session-at-point "memex-status")
(declare-function memex-status-sort-by "memex-status")
(declare-function memex-status-sort-reverse "memex-status")
(declare-function memex-status-sort-clear "memex-status")
(declare-function memex-status-narrow-source "memex-status")
(declare-function memex-status-narrow-project "memex-status")
(declare-function memex-status-clear-narrowing "memex-status")
(declare-function memex-status-set-limit "memex-status")
(declare-function memex-status-copy-command "memex-status")
(declare-function memex-status-visit "memex-status")
(declare-function memex-status-search "memex-status")
(declare-function memex-status-search-listed "memex-status")
(declare-function memex-status-search-globally "memex-status")
(declare-function magit-current-section "magit-section")

(defvar memex-status-tests--requests nil
  "Keyword arguments of every stubbed request, newest first.")

(defvar memex-status-tests--count-requests nil
  "Keyword arguments of every stubbed count, newest first.")

(defvar memex-status-tests--total 347
  "The total the stubbed count answers with.")

(defun memex-status-tests--sessions ()
  "Return the sessions the stub answers with, newest activity first."
  (list '((session_id . "s1") (source . "claude")
          (source_path . "/tmp/s1.jsonl")
          (project . "srnnkls-memex") (repo_project . "memex")
          (cwd . "/tmp/memex") (git_root . "/tmp/memex")
          (started_at . "2026-09-09T10:00:00Z")
          (last_at . "2026-09-09T22:00:00Z")
          (message_count . 122) (conversation_kind . "main")
          (label . "index building cost model")
          (resume_cmd . "cd '/tmp/memex' && claude --resume s1"))
        '((session_id . "s2") (source . "codex")
          (source_path . "/tmp/s2.jsonl")
          (project . "sira-compaction") (repo_project . "sira")
          (cwd . "/tmp/sira") (git_root . "/tmp/sira")
          (started_at . "2026-09-08T09:00:00Z")
          (last_at . "2026-09-09T21:00:00Z")
          (message_count . 10924) (conversation_kind . "main")
          (label . "on \\nzr\u001b[31m")
          (resume_cmd . "codex resume s2"))
        '((session_id . "s3") (source . "claude")
          (source_path . "/tmp/s3.jsonl")
          (project . "dotfiles") (repo_project . "dotfiles")
          (cwd . "/tmp/dotfiles") (git_root . "/tmp/dotfiles")
          (started_at . "2026-09-07T08:00:00Z")
          (last_at . "2026-09-09T20:00:00Z")
          (message_count . 435) (conversation_kind . "main")
          (label . "Doom Emacs cooked package")
          (resume_cmd . "cd '/tmp/dotfiles' && claude --resume s3"))))

(defmacro memex-status-tests--with-dashboard (answer &rest body)
  "Draw a dashboard over ANSWER and run BODY inside it.
ANSWER is the session list the stub hands back, or a cons of `:error' and
the error object it refuses with instead."
  (declare (indent 1) (debug (form body)))
  `(let ((memex-status-tests--requests nil)
         (memex-status-tests--count-requests nil)
         (buffer (generate-new-buffer " *memex-status-test*"))
         (answer ,answer))
     (unwind-protect
         (cl-letf (((symbol-function 'memex-api-sessions)
                    (lambda (callback &rest keys)
                      (push keys memex-status-tests--requests)
                      (if (eq (car-safe answer) :error)
                          (funcall (plist-get keys :errback) (cdr answer))
                        (funcall callback answer))
                      nil))
                   ((symbol-function 'memex-api-session-count)
                    (lambda (callback &rest keys)
                      (push keys memex-status-tests--count-requests)
                      (funcall callback memex-status-tests--total)
                      nil)))
           (with-current-buffer buffer
             (memex-status-mode)
             (memex-status-refresh)
             ,@body))
       (kill-buffer buffer))))

(defun memex-status-tests--sent (&optional key)
  "Return the last request, or its KEY."
  (let ((request (car memex-status-tests--requests)))
    (if key (plist-get request key) request)))

(defun memex-status-tests--rows ()
  "Return the session of every row drawn, top to bottom."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((section (magit-current-section))
                    ((eq (oref section type) 'memex-status-session))
                    (session (oref section value))
                    ((not (memq session rows))))
          (push session rows))
        (forward-line 1)))
    (nreverse rows)))

(defun memex-status-tests--ids ()
  "Return the session id of every row drawn, top to bottom."
  (mapcar (lambda (session) (alist-get 'session_id session))
          (memex-status-tests--rows)))

;;;; Drawing

(ert-deftest memex-status-heads-the-list-with-the-rows-over-the-total ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (goto-char (point-min))
    (should (looking-at-p "Sessions 3/347"))
    (should (string-match-p "newest first" (thing-at-point 'line t)))))

(ert-deftest memex-status-heads-the-list-with-the-rows-alone-without-a-total ()
  (skip-unless (featurep 'magit-section))
  (let ((memex-status-tests--total nil))
    (memex-status-tests--with-dashboard (memex-status-tests--sessions)
      (goto-char (point-min))
      (should (looking-at-p "Sessions 3 ")))))

(ert-deftest memex-status-counts-what-its-filters-match ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-narrow-source "codex")
    (let ((sent (car memex-status-tests--count-requests)))
      (should (equal "codex" (plist-get sent :source)))
      (should-not (plist-member sent :limit)))))

(ert-deftest memex-status-draws-one-row-per-session-carrying-it ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (should (equal '("s1" "s2" "s3") (memex-status-tests--ids)))))

(ert-deftest memex-status-rows-say-project-source-size-and-subject ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "memex" text))
      (should (string-match-p "claude" text))
      (should (string-match-p "122" text))
      (should (string-match-p "index building cost model" text)))))

(ert-deftest memex-status-reads-a-subject-past-its-terminal-colour ()
  "A label carries the escapes and colour of the transcript it was read
from, so a row that printed them raw would show `\\n' and an SGR run."
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "on zr" text))
      (should-not (string-match-p "\\\\n" text))
      (should-not (string-match-p "\u001b" text)))))

(ert-deftest memex-status-says-so-when-nothing-matches ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard nil
    (goto-char (point-min))
    (should (string-match-p "Nothing indexed matches"
                            (buffer-substring-no-properties (point-min)
                                                            (point-max))))))

(ert-deftest memex-status-shows-what-memex-refused-the-request-with ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard
      (cons :error '(memex-transport-error "memex is not on PATH"))
    (should (string-match-p "memex is not on PATH"
                            (buffer-substring-no-properties (point-min)
                                                            (point-max))))
    (should (null (memex-status-tests--ids)))))

;;;; The row at point

(ert-deftest memex-status-session-at-point-is-the-row-it-is-on ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (goto-char (point-min))
    (should (search-forward "on zr" nil t))
    (should (equal "s2" (alist-get 'session_id (memex-status-session-at-point))))))

(ert-deftest memex-status-session-at-point-is-nil-off-a-row ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (goto-char (point-min))
    (should-not (memex-status-session-at-point)))
  (with-temp-buffer
    (should-not (memex-status-session-at-point))))

(ert-deftest memex-status-copies-the-command-memex-recorded ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (goto-char (point-min))
    (should (search-forward "on zr" nil t))
    (let ((kill-ring nil))
      (memex-status-copy-command)
      (should (equal "codex resume s2" (current-kill 0))))))

(ert-deftest memex-status-opens-the-transcript-of-the-row-at-point ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (let (opened)
      (cl-letf (((symbol-function 'memex-view-session)
                 (lambda (id path &rest _) (setq opened (list id path)))))
        (goto-char (point-min))
        (should (search-forward "Doom Emacs" nil t))
        (memex-status-visit))
      (should (equal '("s3" "/tmp/s3.jsonl") opened)))))

;;;; Ordering

(ert-deftest memex-status-orders-by-size-largest-first ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-sort-by "messages")
    (should (equal '("s2" "s3" "s1") (memex-status-tests--ids)))))

(ert-deftest memex-status-orders-by-project-and-by-subject-alphabetically ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-sort-by "project")
    (should (equal '("s3" "s1" "s2") (memex-status-tests--ids)))
    (memex-status-sort-by "label")
    (should (equal '("s3" "s1" "s2") (memex-status-tests--ids)))))

(ert-deftest memex-status-asking-for-the-same-order-turns-it-around ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-sort-by "messages")
    (memex-status-sort-by "messages")
    (should (equal '("s1" "s3" "s2") (memex-status-tests--ids)))
    (memex-status-sort-reverse)
    (should (equal '("s2" "s3" "s1") (memex-status-tests--ids)))))

(ert-deftest memex-status-clearing-the-order-returns-memex-s-own ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-sort-by "project")
    (memex-status-sort-clear)
    (should (equal '("s1" "s2" "s3") (memex-status-tests--ids)))))

(ert-deftest memex-status-ordering-asks-memex-for-nothing-new ()
  "Memex offers no sort key, so an order is applied to the window already
answered with and must not spend a request."
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (should (equal 1 (length memex-status-tests--requests)))
    (memex-status-sort-by "messages")
    (memex-status-sort-reverse)
    (memex-status-sort-clear)
    (should (equal 1 (length memex-status-tests--requests)))))

(ert-deftest memex-status-refuses-an-order-outside-a-dashboard ()
  (with-temp-buffer
    (should-error (memex-status-sort-by "messages") :type 'user-error)
    (should-error (memex-status-sort-clear) :type 'user-error)
    (should-error (memex-status-refresh) :type 'user-error)))

;;;; Narrowing

(ert-deftest memex-status-sends-the-limit-and-no-narrowing-at-first ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (should (equal 20 (memex-status-tests--sent :limit)))
    (dolist (key '(:source :project :cwd :since))
      (should-not (memex-status-tests--sent key)))))

(ert-deftest memex-status-narrowing-goes-into-the-next-request ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-narrow-source "codex")
    (should (equal "codex" (memex-status-tests--sent :source)))
    (memex-status-narrow-project "memex")
    (should (equal "memex" (memex-status-tests--sent :project)))
    (should (equal "codex" (memex-status-tests--sent :source)))
    (memex-status-clear-narrowing)
    (should-not (memex-status-tests--sent :source))
    (should-not (memex-status-tests--sent :project))))

(ert-deftest memex-status-an-empty-narrowing-answer-drops-the-filter ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-narrow-source "")
    (should-not (memex-status-tests--sent :source))))

(ert-deftest memex-status-narrowing-shows-in-the-heading ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-narrow-source "codex")
    (goto-char (point-min))
    (should (string-match-p "source codex" (thing-at-point 'line t)))))

(ert-deftest memex-status-a-new-limit-goes-into-the-next-request ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-set-limit 5)
    (should (equal 5 (memex-status-tests--sent :limit)))))

(ert-deftest memex-status-refuses-a-limit-memex-will-not-serve ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (should-error (memex-status-set-limit 0) :type 'user-error)
    (should-error (memex-status-set-limit (1+ memex-api-max-sessions))
                  :type 'user-error)
    (should (equal 20 (memex-status-tests--sent :limit)))))


;;;; Searching

(defmacro memex-status-tests--capturing-search (&rest body)
  "Run BODY with the scope every search was narrowed to in `scopes'."
  (declare (indent 0) (debug (body)))
  `(let ((scopes nil))
     (cl-letf (((symbol-function 'memex-status--ready) #'ignore)
               ((symbol-function 'memex-search-in-sessions)
                (lambda (scope &optional mode _initial)
                  (push (cons scope mode) scopes)
                  scope)))
       ,@body)))

(defun memex-status-tests--scoped (scopes)
  "Return the session ids the newest of SCOPES was narrowed to."
  (mapcar (lambda (entry) (plist-get entry :session-id)) (car (car scopes))))

(ert-deftest memex-status-searches-the-session-the-row-at-point-holds ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-tests--capturing-search
      (goto-char (point-min))
      (should (search-forward "on zr" nil t))
      (memex-status-search)
      (should (equal '("s2") (memex-status-tests--scoped scopes)))
      (should (equal '(:source "codex" :session-id "s2"
                               :source-path "/tmp/s2.jsonl")
                     (car (car (car scopes))))))))

(ert-deftest memex-status-searches-every-listed-session-off-a-row ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-tests--capturing-search
      (goto-char (point-min))
      (memex-status-search)
      (should (equal '("s1" "s2" "s3") (memex-status-tests--scoped scopes))))))

(ert-deftest memex-status-searching-everything-ignores-the-row-at-point ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-tests--capturing-search
      (goto-char (point-min))
      (should (search-forward "on zr" nil t))
      (memex-status-search-globally)
      (should-not (car (car scopes)))
      (memex-status-search-listed)
      (should (equal '("s1" "s2" "s3") (memex-status-tests--scoped scopes))))))

(ert-deftest memex-status-a-search-narrowed-by-a-filter-follows-it ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (list (car (memex-status-tests--sessions)))
    (memex-status-tests--capturing-search
      (goto-char (point-min))
      (memex-status-search)
      (should (equal '("s1") (memex-status-tests--scoped scopes))))))

(ert-deftest memex-status-search-modes-reach-memex ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard (memex-status-tests--sessions)
    (memex-status-tests--capturing-search
      (goto-char (point-min))
      (memex-status-search 'semantic)
      (should (eq 'semantic (cdr (car scopes)))))))

(ert-deftest memex-status-refuses-a-search-with-nothing-listed ()
  (skip-unless (featurep 'magit-section))
  (memex-status-tests--with-dashboard nil
    (memex-status-tests--capturing-search
      (should-error (memex-status-search) :type 'user-error)
      (should-error (memex-status-search-listed) :type 'user-error)
      (should-not scopes))))

;;;; Keys

(defun memex-status-tests--suffix-command (prefix key)
  "Return the command PREFIX runs for KEY, or nil when it binds none."
  (when-let* ((suffix (ignore-errors (transient-get-suffix prefix key))))
    (plist-get (cdr suffix) :command)))

(ert-deftest memex-status-dispatch-mirrors-the-keymap ()
  (skip-unless (featurep 'magit-section))
  (dolist (key '("RET" "o" "r" "w" "s" "S" "f" "O" "L" "g" "q"))
    (let ((bound (keymap-lookup memex-status-mode-map key))
          (offered (memex-status-tests--suffix-command
                    'memex-status-dispatch key)))
      (should (commandp bound))
      (should (eq bound offered)))))

(ert-deftest memex-status-dispatch-closes-on-a-second-question-mark ()
  (skip-unless (featurep 'magit-section))
  (should (eq 'transient-quit-one
              (memex-status-tests--suffix-command 'memex-status-dispatch "?")))
  (should (eq #'memex-status-dispatch (keymap-lookup memex-status-mode-map "?"))))

(ert-deftest memex-status-leaves-section-navigation-to-magit ()
  "The dashboard inherits `magit-section-mode-map', so `n' and `p' walk
the rows and must not be rebound."
  (skip-unless (featurep 'magit-section))
  (should (eq #'magit-section-forward (keymap-lookup memex-status-mode-map "n")))
  (should (eq #'magit-section-backward
              (keymap-lookup memex-status-mode-map "p"))))

(ert-deftest memex-status-menus-offer-only-real-commands ()
  (skip-unless (featurep 'magit-section))
  (dolist (key '("r" "s" "n" "p" "k" "l" "x" "f" "DEL"))
    (should (commandp (memex-status-tests--suffix-command
                       'memex-status-sort key))))
  (dolist (key '("s" "l" "g" "x" "m" "y"))
    (should (commandp (memex-status-tests--suffix-command
                       'memex-status-search-dispatch key))))
  (dolist (key '("p" "d" "k" "o" "s" "L" "DEL"))
    (should (commandp (memex-status-tests--suffix-command
                       'memex-status-filter key)))))

(ert-deftest memex-status-is-an-interactive-command ()
  (should (commandp 'memex-status))
  (should (commandp 'memex-status-refresh))
  (should (commandp 'memex-status-dispatch)))

(provide 'memex-status-tests)
;;; memex-status-tests.el ends here
