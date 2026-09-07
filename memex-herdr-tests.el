;;; memex-herdr-tests.el --- Tests for memex-herdr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-herdr-tests.el -f ert-run-tests-batch-and-exit
;;
;; herdr is an external package and the `+ws-pin' helpers live in the
;; user's Doom configuration, so neither is loaded here: every one of
;; them is replaced for the duration of a run and the bridge is measured
;; by what it called and in what order.  The resume lookup is a shell-out
;; rather than an RPC, so `call-process' and `process-file' are replaced
;; too and the argument vector is asserted whole - the CLI has no
;; session-id filter, which is what makes the limit and the client-side
;; match part of the contract rather than an implementation detail.
;;
;; The replacements carry the names and arities herdr.el exports:
;; `herdr-start-server-if-needed' (herdr-core.el:377),
;; `herdr-open-tab' (herdr.el:149) and
;; `herdr-api-agent-start' (herdr-api.el:421).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)

(require 'memex-herdr nil t)
(require 'memex-anchor nil t)

(declare-function memex-herdr-resume "memex-herdr")
(declare-function memex-herdr-open-session "memex-herdr")

(defvar memex-resume-lookup-limit)

(define-error 'herdr-error "herdr error")

(defconst memex-herdr-tests--session-id "caea32e0-f5ad-4906-9f1f-b9b7047bd7a1"
  "The `session_id' of the session the tests resume.")

(defconst memex-herdr-tests--other-session-id
  "3b71c8de-2a04-4d59-8f7c-1c0b6d2e5a11"
  "A second `session_id', carried by rows the lookup must not match.")

(defconst memex-herdr-tests--tab
  '((tab_id . "tab-1")
    (root_pane . ((pane_id . "pane-7") (terminal_id . "term-3"))))
  "The result `herdr-open-tab' answers with in these tests.")

(defvar memex-herdr-tests--calls nil
  "Every call the bridge made during a run, newest first.
An entry is (ensure), (shell PROGRAM . ARGUMENTS), (tab-create . KEYS),
\(agent-start KIND NAME PANE-ID KEYS) or
\(view SESSION-ID SOURCE-PATH DOC-ID DISPLAY).")

(defvar memex-herdr-tests--messages nil
  "Every message the bridge reported during a run, newest first.")

(defvar memex-herdr-tests--unreachable nil
  "When non-nil, no herdr server answers for the duration of a run.")

(defvar memex-herdr-tests--installed "/usr/bin/memex"
  "Where `executable-find' answers `memex-executable' is, nil for nowhere.
Whether the machine running the suite has memex installed is not a fact
any of these tests is about, so the probe is answered from here rather
than from the filesystem.")

(defun memex-herdr-tests--json (value)
  "Return VALUE as JSON text: a string quoted, nil as null."
  (if value (format "%S" value) "null"))

(defun memex-herdr-tests--row (session-id source-path cwd git-root resume-cmd
                                          &optional source)
  "Return one `memex sessions --json-array' row as JSON text.
SESSION-ID and SOURCE-PATH key the row.  CWD and RESUME-CMD are each a
string or nil, nil written as the JSON null memex sends for a session it
knows no such value for.  A nil GIT-ROOT is left out of the object
entirely, which is the other shape a missing field arrives in.  SOURCE
defaults to \"codex\"."
  (format
   "{\"session_id\":%s,\"source_path\":%s,\"source\":%s,\"cwd\":%s%s,\"resume_cmd\":%s}"
   (memex-herdr-tests--json session-id)
   (memex-herdr-tests--json source-path)
   (memex-herdr-tests--json (or source "codex"))
   (memex-herdr-tests--json cwd)
   (if git-root
       (format ",\"git_root\":%s" (memex-herdr-tests--json git-root))
     "")
   (memex-herdr-tests--json resume-cmd)))

(defun memex-herdr-tests--rows (&rest rows)
  "Return ROWS as the JSON array `memex sessions --json-array' prints."
  (concat "[" (string-join rows ",") "]"))

(defun memex-herdr-tests--shell (json)
  "Return a `call-process' replacement recording its argv and printing JSON."
  (lambda (program &optional _infile destination _display &rest arguments)
    (push (cons 'shell (cons program arguments)) memex-herdr-tests--calls)
    (let ((target (if (consp destination) (car destination) destination)))
      (with-current-buffer (if (bufferp target) target (current-buffer))
        (insert json)))
    0))

(defun memex-herdr-tests--which ()
  "Return an `executable-find' replacement answering from this file alone."
  (lambda (command &rest _)
    (and (equal command memex-executable) memex-herdr-tests--installed)))

(defmacro memex-herdr-tests--letf (symbols bindings &rest body)
  "Run BODY under BINDINGS, leaving SYMBOLS unbound afterwards.
`cl-letf' restores a function cell it found void to nil rather than to
void, and `fboundp' answers true for a nil cell, so a later test probing
for an absent package would otherwise find one."
  (declare (indent 2))
  `(unwind-protect (cl-letf ,bindings ,@body)
     (dolist (memex-herdr-tests--symbol ,symbols)
       (fmakunbound memex-herdr-tests--symbol))))

(defmacro memex-herdr-tests--run (json &rest body)
  "Run BODY with the session lookup answering JSON and herdr recorded.
A non-nil `memex-herdr-tests--unreachable' makes the readiness step signal
`herdr-error' the way herdr does when no server answers."
  (declare (indent 1))
  `(let ((memex-herdr-tests--calls nil)
         (memex-herdr-tests--messages nil))
     (memex-herdr-tests--letf
         '(herdr-start-server-if-needed herdr-open-tab
                                        herdr-api-agent-start)
         (((symbol-function 'call-process) (memex-herdr-tests--shell ,json))
          ((symbol-function 'process-file) (memex-herdr-tests--shell ,json))
          ((symbol-function 'executable-find) (memex-herdr-tests--which))
          ((symbol-function 'herdr-start-server-if-needed)
                (lambda ()
                  (push (list 'ensure) memex-herdr-tests--calls)
                  (if memex-herdr-tests--unreachable
                      (signal 'herdr-error
                              (list "no herdr server answering on /tmp/herdr.sock"))
                    t)))
          ((symbol-function 'herdr-open-tab)
           (lambda (&rest keys)
             (push (cons 'tab-create keys) memex-herdr-tests--calls)
             memex-herdr-tests--tab))
          ((symbol-function 'herdr-api-agent-start)
           (lambda (kind name pane-id &rest keys)
             (push (list 'agent-start kind name pane-id keys)
                   memex-herdr-tests--calls)
             nil))
          ((symbol-function 'memex-anchor--herdr-p) (lambda () t))
          ((symbol-function 'memex-anchor-resume)
           (lambda (record resume)
             (memex-herdr--start (cdr resume) (car resume)
                                 (alist-get 'session_id record))))
          ((symbol-function 'memex-view-session)
           (lambda (session-id source-path &optional doc-id display)
             (push (list 'view session-id source-path doc-id display)
                   memex-herdr-tests--calls)
             nil))
          ((symbol-function 'message)
           (lambda (format-string &rest arguments)
             (push (if format-string
                       (apply #'format format-string arguments)
                     "")
                   memex-herdr-tests--messages)
             nil)))
       ,@body)))

(defmacro memex-herdr-tests--reporting (&rest body)
  "Run BODY, recording a `user-error' it signals as a reported message.
A refusal reaches the user as either one, so the tests read the text and
leave the mechanism open."
  `(condition-case memex-herdr-tests--signal (progn ,@body)
     (user-error
      (push (error-message-string memex-herdr-tests--signal)
            memex-herdr-tests--messages))))

(defconst memex-herdr-tests--pin-helpers
  '(+ws-pin-of +ws-pin-buffer +ws-pin-follow)
  "The workspace-pin helpers the bridge probes for at call time.")

(defun memex-herdr-tests--transcript ()
  "Return the path of a transcript that exists on disk."
  (make-temp-file "memex-herdr-tests-" nil ".jsonl"))

(defun memex-herdr-tests--record (source-path &optional source)
  "Return the session record the bridge is asked to resume, at SOURCE-PATH.
SOURCE defaults to \"codex\"."
  (list (cons 'source (or source "codex"))
        (cons 'doc_id 8801)
        (cons 'project "memex.el")
        (cons 'session_id memex-herdr-tests--session-id)
        (cons 'source_path source-path)))

(defun memex-herdr-tests--of (operation)
  "Return every recorded call of OPERATION, oldest first."
  (seq-filter (lambda (call) (eq (car call) operation))
              (reverse memex-herdr-tests--calls)))

(defun memex-herdr-tests--reported-p (text)
  "Return non-nil when a message of the run named TEXT."
  (seq-find (lambda (message)
              (string-match-p (regexp-quote text) message))
            memex-herdr-tests--messages))

(ert-deftest memex-herdr-resume-delegates-the-looked-up-session-to-the-anchor ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path))
         (row-json (memex-herdr-tests--row
                    memex-herdr-tests--session-id path
                    "/tmp/memex-herdr-tests/proj" nil "codex resume 7f"))
         resolved)
    (unwind-protect
        (memex-herdr-tests--run (memex-herdr-tests--rows row-json)
          (cl-letf (((symbol-function 'memex-anchor--herdr-p)
                     (lambda () t))
                    ((symbol-function 'memex-anchor-resume)
                     (lambda (given-record resume)
                       (setq resolved (list given-record resume)))))
            (memex-herdr-resume record))
          (should (equal (car resolved) record))
          (should (equal (car (cadr resolved)) "codex resume 7f"))
          (should (equal (alist-get 'session_id (cdr (cadr resolved)))
                         memex-herdr-tests--session-id))
          (should (null (memex-herdr-tests--of 'tab-create)))
          (should (null (memex-herdr-tests--of 'agent-start))))
      (delete-file path))))

(ert-deftest memex-herdr-resume-looks-the-session-up-with-one-limited-shell-out ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path))
         (memex-resume-lookup-limit 42))
    (unwind-protect
        (memex-herdr-tests--run
            (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--session-id path
                                     "/tmp/memex-herdr-tests/proj" nil
                                     "codex resume 7f"))
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (let ((shells (memex-herdr-tests--of 'shell)))
            (should (equal (length shells) 1))
            (should (equal (cdr (car shells))
                           (list memex-executable
                                 "sessions" "--json-array"
                                 "--source" "codex"
                                 "--limit" "42")))
            (should (commandp 'memex-herdr-resume))))
      (delete-file path))))

(ert-deftest memex-herdr-resume-readies-herdr-then-opens-the-tab-and-sends-the-command ()
  "The ordering that carries a guarantee is `ensure' before `tab-create':
a tab must never be created against a server that was never readied.
`ensure' before `shell' guarantees nothing - the memex lookup has no
relationship to herdr - and requiring it is what would force herdr to be
probed for on the three branches that never touch it."
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path)))
    (unwind-protect
        (memex-herdr-tests--run
            (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--session-id path
                                     "/tmp/memex-herdr-tests/proj" nil
                                     "codex resume 7f"))
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (let ((log (seq-remove (lambda (call) (eq (car call) 'view))
                                 (reverse memex-herdr-tests--calls))))
            (should (equal (mapcar #'car log)
                           '(shell ensure tab-create agent-start)))
            (let ((keys (cdr (nth 2 log))))
              (should (equal (plist-get keys :cwd)
                             "/tmp/memex-herdr-tests/proj"))
              (should (null (plist-get keys :workspace-id))))
            (pcase-let ((`(,_ ,kind ,name ,pane ,keys) (nth 3 log)))
              (should (equal kind "codex"))
              (should (equal name memex-herdr-tests--session-id))
              (should (equal pane "pane-7"))
              (should (equal (plist-get keys :args) '("resume" "7f"))))))
      (delete-file path))))

(ert-deftest memex-herdr-resume-falls-back-from-cwd-to-git-root-to-the-transcript-directory ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path)))
    (unwind-protect
        (dolist (ladder (list (list "/tmp/memex-herdr-tests/cwd"
                                    "/tmp/memex-herdr-tests/git"
                                    "/tmp/memex-herdr-tests/cwd")
                              (list nil
                                    "/tmp/memex-herdr-tests/git"
                                    "/tmp/memex-herdr-tests/git")
                              (list nil nil (file-name-directory path))))
          (memex-herdr-tests--run
              (memex-herdr-tests--rows
               (memex-herdr-tests--row memex-herdr-tests--session-id path
                                       (nth 0 ladder) (nth 1 ladder)
                                       "codex resume 7f"))
            (memex-herdr-tests--reporting (memex-herdr-resume record))
            (let ((create (car (memex-herdr-tests--of 'tab-create))))
              (should create)
              (should (equal (file-name-as-directory
                              (plist-get (cdr create) :cwd))
                             (file-name-as-directory (nth 2 ladder)))))))
      (delete-file path))))

(ert-deftest memex-herdr-resume-reports-the-lookup-failure-naming-the-session ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path)))
    (unwind-protect
        (memex-herdr-tests--run
            (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--other-session-id path
                                     "/tmp/memex-herdr-tests/proj" nil
                                     "codex resume other")
             (memex-herdr-tests--row memex-herdr-tests--session-id
                                     "/tmp/memex-herdr-tests/elsewhere.jsonl"
                                     "/tmp/memex-herdr-tests/proj" nil
                                     "codex resume decoy"))
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (should (null (memex-herdr-tests--of 'tab-create)))
          (should (null (memex-herdr-tests--of 'agent-start)))
          (should (null (memex-herdr-tests--of 'view)))
          (should (memex-herdr-tests--reported-p
                   memex-herdr-tests--session-id)))
      (delete-file path))))

(ert-deftest memex-herdr-resume-of-a-row-without-a-template-views-the-session ()
  (let ((path (memex-herdr-tests--transcript))
        (memex-resume-lookup-limit 42))
    (unwind-protect
        (dolist (template (list nil ""))
          (memex-herdr-tests--run
              (memex-herdr-tests--rows
               (memex-herdr-tests--row memex-herdr-tests--session-id path
                                       "/tmp/memex-herdr-tests/proj" nil
                                       template "openclaw"))
            (memex-herdr-tests--reporting
             (memex-herdr-resume (memex-herdr-tests--record path "openclaw")))
            (should (equal (cdr (car (memex-herdr-tests--of 'shell)))
                           (list memex-executable
                                 "sessions" "--json-array"
                                 "--source" "openclaw"
                                 "--limit"
                                 (number-to-string memex-resume-lookup-limit))))
            (should (null (memex-herdr-tests--of 'tab-create)))
            (should (null (memex-herdr-tests--of 'agent-start)))
            (should (memex-herdr-tests--reported-p "openclaw"))
            (let ((view (car (memex-herdr-tests--of 'view))))
              (should view)
              (should (equal (nth 1 view) memex-herdr-tests--session-id))
              (should (equal (nth 2 view) path))
              (should (functionp (nth 4 view))))))
      (delete-file path))))

(ert-deftest memex-herdr-resume-of-a-vanished-transcript-views-it-and-creates-no-tab ()
  (let* ((path "/tmp/memex-herdr-tests/vanished.jsonl")
         (record (memex-herdr-tests--record path)))
    (should-not (file-exists-p path))
    (memex-herdr-tests--run
        (memex-herdr-tests--rows
         (memex-herdr-tests--row memex-herdr-tests--session-id path
                                 "/tmp/memex-herdr-tests/proj" nil
                                 "codex resume 7f"))
      (memex-herdr-tests--reporting (memex-herdr-resume record))
      (should (null (memex-herdr-tests--of 'shell)))
      (should (null (memex-herdr-tests--of 'tab-create)))
      (should (null (memex-herdr-tests--of 'agent-start)))
      (should (memex-herdr-tests--reported-p path))
      (let ((view (car (memex-herdr-tests--of 'view))))
        (should view)
        (should (equal (nth 1 view) memex-herdr-tests--session-id))
        (should (equal (nth 2 view) path))
        (should (functionp (nth 4 view)))))))

(ert-deftest memex-herdr-resume-reports-an-unreachable-herdr-and-starts-nothing ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path))
         (memex-herdr-tests--unreachable t))
    (unwind-protect
        (memex-herdr-tests--run
            (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--session-id path
                                     "/tmp/memex-herdr-tests/proj" nil
                                     "codex resume 7f"))
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (should (memex-herdr-tests--of 'ensure))
          (should (null (memex-herdr-tests--of 'tab-create)))
          (should (null (memex-herdr-tests--of 'agent-start)))
          (should (memex-herdr-tests--reported-p "no herdr server answering")))
      (delete-file path))))

(ert-deftest memex-herdr-resume-without-herdr-names-it-instead-of-failing ()
  (let* ((path (memex-herdr-tests--transcript))
         (record (memex-herdr-tests--record path))
         (memex-herdr-tests--calls nil)
         (memex-herdr-tests--messages nil))
    (should-not (fboundp 'herdr-open-tab))
    (should-not (fboundp 'herdr-api-agent-start))
    (unwind-protect
        (cl-letf (((symbol-function 'call-process)
                   (memex-herdr-tests--shell
                    (memex-herdr-tests--rows
                     (memex-herdr-tests--row memex-herdr-tests--session-id path
                                             "/tmp/memex-herdr-tests/proj" nil
                                             "codex resume 7f"))))
                  ((symbol-function 'executable-find) (memex-herdr-tests--which))
                  ((symbol-function 'memex-view-session)
                   (lambda (session-id source-path &optional doc-id display)
                     (push (list 'view session-id source-path doc-id display)
                           memex-herdr-tests--calls)
                     nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (push (apply #'format format-string arguments)
                           memex-herdr-tests--messages)
                     nil)))
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (should (or (memex-herdr-tests--reported-p
                       "herdr-start-server-if-needed")
                      (memex-herdr-tests--reported-p "not installed"))))
      (delete-file path))))

(ert-deftest memex-herdr-open-session-pins-an-unpinned-buffer-then-displays-it ()
  (let ((buffer (generate-new-buffer "*memex herdr tests session*"))
        (view nil)
        (pinned nil)
        (displayed nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'memex-view-session)
                     (lambda (session-id source-path &optional doc-id display)
                       (setq view (list session-id source-path doc-id display))
                       nil)))
            (memex-herdr-open-session memex-herdr-tests--session-id
                                      "/tmp/memex-herdr-tests/a.jsonl" 8803))
          (should (equal (seq-take view 3)
                         (list memex-herdr-tests--session-id
                               "/tmp/memex-herdr-tests/a.jsonl" 8803)))
          (should (functionp (nth 3 view)))
          (memex-herdr-tests--letf memex-herdr-tests--pin-helpers
              (((symbol-function '+ws-pin-of) (lambda (_buffer) nil))
               ((symbol-function '+ws-pin-buffer)
                (lambda (target &optional workspace)
                  (push (cons target workspace) pinned)
                  "current"))
               ((symbol-function '+ws-pin-follow) (lambda (_buffer) nil))
               ((symbol-function 'display-buffer)
                (lambda (target &rest _) (push target displayed) nil)))
            (funcall (nth 3 view) buffer))
          (should (equal (mapcar #'car pinned) (list buffer)))
          (should (equal displayed (list buffer))))
      (kill-buffer buffer))))

(ert-deftest memex-herdr-open-session-follows-a-pinned-buffer-without-repinning-it ()
  (let ((buffer (generate-new-buffer "*memex herdr tests pinned*"))
        (display nil)
        (pinned nil)
        (followed nil)
        (displayed nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'memex-view-session)
                     (lambda (_session-id _source-path &optional _doc-id fn)
                       (setq display fn)
                       nil)))
            (memex-herdr-open-session memex-herdr-tests--session-id
                                      "/tmp/memex-herdr-tests/a.jsonl"))
          (should (functionp display))
          (memex-herdr-tests--letf memex-herdr-tests--pin-helpers
              (((symbol-function '+ws-pin-of) (lambda (_buffer) "workspace-a"))
               ((symbol-function '+ws-pin-buffer)
                (lambda (target &optional _workspace) (push target pinned) nil))
               ((symbol-function '+ws-pin-follow)
                (lambda (target) (push target followed) "workspace-a"))
               ((symbol-function 'display-buffer)
                (lambda (target &rest _) (push target displayed) nil)))
            (funcall display buffer))
          (should (null pinned))
          (should (equal followed (list buffer)))
          (should (equal displayed (list buffer))))
      (kill-buffer buffer))))

(ert-deftest memex-herdr-open-session-displays-plainly-without-the-workspace-pins ()
  (let ((buffer (generate-new-buffer "*memex herdr tests unpinnable*"))
        (display nil)
        (displayed nil))
    (dolist (symbol memex-herdr-tests--pin-helpers) (fmakunbound symbol))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'memex-view-session)
                     (lambda (_session-id _source-path &optional _doc-id fn)
                       (setq display fn)
                       nil)))
            (memex-herdr-open-session memex-herdr-tests--session-id
                                      "/tmp/memex-herdr-tests/a.jsonl"))
          (should (functionp display))
          (should-not (fboundp '+ws-pin-of))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (target &rest _) (push target displayed) nil)))
            (funcall display buffer))
          (should (equal displayed (list buffer))))
      (kill-buffer buffer))))

(defconst memex-herdr-tests--agent-cwd "/tmp/memex-herdr-tests/proj"
  "The directory herdr reports the attached agent working in.")

(defun memex-herdr-tests--agent (reference)
  "Return a herdr agent entry on terminal `term-3' reporting REFERENCE."
  `((terminal_id . "term-3")
    (name . "review")
    (agent . "claude")
    (pane_id . "pane-7")
    (cwd . ,memex-herdr-tests--agent-cwd)
    ,@(when reference (list (cons 'agent_session reference)))))

(defun memex-herdr-tests--reference (kind value)
  "Return the session reference herdr reports as KIND naming VALUE."
  `((source . "herdr:claude") (agent . "claude")
    (kind . ,kind) (value . ,value)))

(defun memex-herdr-tests--queue (answers)
  "Return a `call-process' replacement answering ANSWERS in turn.
The last answer is repeated once the queue runs dry, so a test says only
as many answers as it is about."
  (lambda (program &optional _infile destination _display &rest arguments)
    (push (cons 'shell (cons program arguments)) memex-herdr-tests--calls)
    (let ((json (if (cdr answers) (pop answers) (car answers)))
          (target (if (consp destination) (car destination) destination)))
      (with-current-buffer (if (bufferp target) target (current-buffer))
        (insert json)))
    0))

(defmacro memex-herdr-tests--attached (agent answers &rest body)
  "Run BODY in a buffer herdr attached, with AGENT live and ANSWERS queued."
  (declare (indent 2))
  `(let ((memex-herdr-tests--calls nil)
         (memex-herdr-tests--messages nil)
         (memex-resume-lookup-limit 42)
         (buffer (generate-new-buffer "*memex herdr tests terminal*")))
     (unwind-protect
         (cl-letf (((symbol-function 'call-process)
                    (memex-herdr-tests--queue ,answers))
                   ((symbol-function 'executable-find)
                    (memex-herdr-tests--which))
                   ((symbol-function 'memex-anchor--agents)
                    (lambda () (delq nil (list ,agent))))
                   ((symbol-function 'memex-view-session)
                    (lambda (session-id source-path &optional doc-id display)
                      (push (list 'view session-id source-path doc-id display)
                            memex-herdr-tests--calls)
                      nil)))
           (with-current-buffer buffer
             (setq-local herdr-terminal-id "term-3")
             ,@body))
       (kill-buffer buffer))))

(ert-deftest memex-herdr-open-agent-session-views-the-session-herdr-reports-by-id ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent
       (memex-herdr-tests--reference "id" memex-herdr-tests--session-id))
      (list (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--other-session-id
                                     "/tmp/memex-herdr-tests/other.jsonl"
                                     memex-herdr-tests--agent-cwd nil nil)
             (memex-herdr-tests--row memex-herdr-tests--session-id
                                     "/tmp/memex-herdr-tests/a.jsonl"
                                     memex-herdr-tests--agent-cwd nil nil)))
    (memex-herdr-open-agent-session)
    (let ((shells (memex-herdr-tests--of 'shell))
          (view (car (memex-herdr-tests--of 'view))))
      (should (equal (length shells) 1))
      (should (equal (cdr (car shells))
                     (list memex-executable "sessions" "--json-array"
                           "--cwd" memex-herdr-tests--agent-cwd
                           "--limit" "42")))
      (should (equal (seq-take (cdr view) 2)
                     (list memex-herdr-tests--session-id
                           "/tmp/memex-herdr-tests/a.jsonl")))
      (should (commandp 'memex-herdr-open-agent-session)))))

(ert-deftest memex-herdr-open-agent-session-views-the-session-herdr-reports-by-path ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent
       (memex-herdr-tests--reference "path" "/tmp/memex-herdr-tests/a.jsonl"))
      (list (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--session-id
                                     "/tmp/memex-herdr-tests/a.jsonl"
                                     memex-herdr-tests--agent-cwd nil nil)))
    (memex-herdr-open-agent-session)
    (should (equal (seq-take (cdr (car (memex-herdr-tests--of 'view))) 2)
                   (list memex-herdr-tests--session-id
                         "/tmp/memex-herdr-tests/a.jsonl")))))

(ert-deftest memex-herdr-open-agent-session-widens-past-the-agent-directory ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent
       (memex-herdr-tests--reference "id" memex-herdr-tests--session-id))
      (list (memex-herdr-tests--rows)
            (memex-herdr-tests--rows
             (memex-herdr-tests--row memex-herdr-tests--session-id
                                     "/tmp/memex-herdr-tests/a.jsonl"
                                     "/tmp/memex-herdr-tests/elsewhere" nil nil)))
    (memex-herdr-open-agent-session)
    (let ((shells (memex-herdr-tests--of 'shell)))
      (should (equal (length shells) 2))
      (should (equal (cdr (nth 1 shells))
                     (list memex-executable "sessions" "--json-array"
                           "--limit" "42"))))
    (should (equal (nth 1 (car (memex-herdr-tests--of 'view)))
                   memex-herdr-tests--session-id))))

(ert-deftest memex-herdr-open-agent-session-refuses-a-session-memex-has-not-indexed ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent
       (memex-herdr-tests--reference "id" memex-herdr-tests--session-id))
      (list (memex-herdr-tests--rows))
    (memex-herdr-tests--reporting (memex-herdr-open-agent-session))
    (should (null (memex-herdr-tests--of 'view)))
    (should (memex-herdr-tests--reported-p memex-herdr-tests--session-id))))

(ert-deftest memex-herdr-open-agent-session-refuses-an-agent-with-no-session-yet ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent nil)
      (list (memex-herdr-tests--rows))
    (memex-herdr-tests--reporting (memex-herdr-open-agent-session))
    (should (null (memex-herdr-tests--of 'shell)))
    (should (null (memex-herdr-tests--of 'view)))
    (should (memex-herdr-tests--reported-p "review"))))

(ert-deftest memex-herdr-open-agent-session-refuses-a-buffer-herdr-never-attached ()
  (memex-herdr-tests--attached
      (memex-herdr-tests--agent
       (memex-herdr-tests--reference "id" memex-herdr-tests--session-id))
      (list (memex-herdr-tests--rows))
    (kill-local-variable 'herdr-terminal-id)
    (memex-herdr-tests--reporting (memex-herdr-open-agent-session))
    (should (null (memex-herdr-tests--of 'shell)))
    (should (null (memex-herdr-tests--of 'view)))
    (should (memex-herdr-tests--reported-p "memex herdr tests terminal"))))

(provide 'memex-herdr-tests)
;;; memex-herdr-tests.el ends here
