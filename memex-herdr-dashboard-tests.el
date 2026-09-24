;;; memex-herdr-dashboard-tests.el --- Searches from herdr's dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; The searches memex offers from herdr's dashboard.  Herdr is optional,
;; so the whole suite stands aside where it is not on the load path; the
;; dashboard itself runs over a stubbed server.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst memex-herdr-dashboard-tests--ready
  (and (require 'herdr-status nil t) (require 'herdr-herd nil t)
       (require 'memex-herdr nil t))
  "Whether herdr is installed beside memex.")

(when memex-herdr-dashboard-tests--ready

(defun memex-herdr-dashboard-tests--entry (name session &optional cwd)
  "Return an agent entry called NAME running SESSION in CWD."
  `((kind . "herdr") (session . "alpha") (server_key . "/tmp/alpha.sock")
    (agent . "claude") (agent_status . "idle")
    (agent_session . ((agent . "claude") (kind . "id")
                      (source . "herdr:claude") (value . ,session)))
    (name . ,name) (terminal_title_stripped . ,name)
    (terminal_id . ,(concat "term-" session)) (pane_id . "w1:p1")
    (workspace_id . "w1") (tab_id . "w1:t1")
    (cwd . ,(or cwd "/tmp/proj/"))))

(defvar memex-herdr-dashboard-tests--entries
  (list (memex-herdr-dashboard-tests--entry "one" "s1")
        (memex-herdr-dashboard-tests--entry "two" "s2")
        (memex-herdr-dashboard-tests--entry "three" "s3"))
  "The agents the fake server reports.")

(defvar memex-herdr-dashboard-tests--searches nil
  "Scopes the stubbed search was called with, newest first.")

(defmacro memex-herdr-dashboard-tests--with-dashboard (&rest body)
  "Render a dashboard over stubbed agents and run BODY inside it."
  (declare (indent 0) (debug (body)))
  `(let ((herdr--recent-session-targets nil)
         (memex-herdr-dashboard-tests--searches nil)
         (buffer (generate-new-buffer " *herdr-memex-test*")))
     (unwind-protect
         (cl-letf (((symbol-function 'herdr-all-sessions) (lambda () '("alpha")))
                   ((symbol-function 'herdr-server-key)
                    (lambda () "/tmp/alpha.sock"))
                   ((symbol-function 'herdr-available-p) (lambda () t))
                   ((symbol-function 'herdr-snapshot)
                    (lambda () '((version . "1.2.3")
                                 (workspaces . (((workspace_id . "w1")
                                                 (label . "proj"))))
                                 (tabs . (((tab_id . "w1:t1") (label . "main"))))
                                 (panes . (((pane_id . "w1:p1")
                                            (workspace_id . "w1")))))))
                   ((symbol-function 'herdr-sessions)
                    (lambda () memex-herdr-dashboard-tests--entries))
                   ((symbol-function 'herdr--entry-buffer) (lambda (_) nil))
                   ((symbol-function 'herdr-api-agent-read)
                    (lambda (&rest _) '((type . "pane_read"))))
                   ((symbol-function 'herdr-api-pane-read)
                    (lambda (&rest _) '((type . "pane_read"))))
                   ((symbol-function 'memex-herdr-session-scope)
                    (lambda (reference _directory)
                      (list :source "claude"
                            :session-id (alist-get 'value reference)
                            :source-path (concat "/tmp/"
                                                 (alist-get 'value reference)
                                                 ".jsonl"))))
                   ((symbol-function 'memex-search-in-sessions)
                    (lambda (scope &optional _mode _initial)
                      (push scope memex-herdr-dashboard-tests--searches)
                      scope)))
           (with-current-buffer buffer
             (herdr-status-mode)
             (herdr-status-refresh)
             ,@body))
       (kill-buffer buffer))))

(defun memex-herdr-dashboard-tests--goto (text)
  "Move point to the line holding TEXT."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "No line holding %s in the dashboard" text))
  (beginning-of-line))

(defun memex-herdr-dashboard-tests--scoped-sessions ()
  "Return the session ids the last search was narrowed to."
  (mapcar (lambda (entry) (plist-get entry :session-id))
          (car memex-herdr-dashboard-tests--searches)))

;;;; Keys

(ert-deftest memex-herdr-takes-up-the-keys-the-dashboard-offers ()
  (let ((map (copy-keymap herdr-status-mode-map)))
    (cl-letf ((herdr-status-mode-map map))
      (memex-herdr-install-keys)
      (should (eq #'memex-herdr-search (keymap-lookup map "s")))
      (should (eq #'memex-herdr-dispatch (keymap-lookup map "m"))))))

(defun memex-herdr-dashboard-tests--suffix (key)
  "Return the command KEY runs in `memex-herdr-dispatch'."
  (plist-get (cdr (transient-get-suffix 'memex-herdr-dispatch key)) :command))

(ert-deftest memex-herdr-dispatch-reads-the-agent-and-searches-by-mode ()
  (should (eq #'memex-herdr-transcript-at-point
              (memex-herdr-dashboard-tests--suffix "RET")))
  (dolist (binding '(("S" . semantic) ("L" . lexical) ("H" . hybrid)))
    (let (mode)
      (cl-letf (((symbol-function 'memex-herdr-search)
                 (lambda (&optional searched) (setq mode searched))))
        (funcall (memex-herdr-dashboard-tests--suffix (car binding))))
      (should (eq (cdr binding) mode)))))

;;;; Scope

(ert-deftest memex-herdr-searches-one-session-from-an-agent-row ()
  (memex-herdr-dashboard-tests--with-dashboard
    (memex-herdr-dashboard-tests--goto "two")
    (memex-herdr-search)
    (should (equal '("s2") (memex-herdr-dashboard-tests--scoped-sessions)))))

(ert-deftest memex-herdr-searches-every-listed-agent-from-the-heading ()
  (memex-herdr-dashboard-tests--with-dashboard
    (memex-herdr-dashboard-tests--goto "Agents")
    (memex-herdr-search)
    (should (equal '("s1" "s2" "s3") (memex-herdr-dashboard-tests--scoped-sessions)))))

(ert-deftest memex-herdr-searches-a-herd-from-its-own-section ()
  (let ((memex-herdr-dashboard-tests--entries
         (mapcar (lambda (entry)
                   (if (member (alist-get 'name entry) '("one" "three"))
                       (cons '(pane_label . "herd:refactor") entry)
                     entry))
                 memex-herdr-dashboard-tests--entries)))
    (memex-herdr-dashboard-tests--with-dashboard
      (memex-herdr-dashboard-tests--goto "refactor")
      (memex-herdr-search)
      (should (equal '("s1" "s3") (memex-herdr-dashboard-tests--scoped-sessions))))))

(ert-deftest memex-herdr-searches-everything-outside-the-dashboard ()
  (with-temp-buffer
    (let ((memex-herdr-dashboard-tests--searches nil))
      (cl-letf (((symbol-function 'memex-search-in-sessions)
                 (lambda (scope &optional _mode _initial)
                   (push scope memex-herdr-dashboard-tests--searches)
                   scope)))
        (memex-herdr-search)
        (should (equal '(nil) memex-herdr-dashboard-tests--searches))))))

(ert-deftest herdr-memex-refuses-an-agent-memex-has-not-indexed ()
  (memex-herdr-dashboard-tests--with-dashboard
    (cl-letf (((symbol-function 'memex-herdr-session-scope) (lambda (&rest _) nil)))
      (memex-herdr-dashboard-tests--goto "two")
      (should-error (memex-herdr-search) :type 'user-error)
      (should-not memex-herdr-dashboard-tests--searches))))

)

(provide 'memex-herdr-dashboard-tests)
;;; memex-herdr-dashboard-tests.el ends here
