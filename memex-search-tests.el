;;; memex-search-tests.el --- Tests for memex-search -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-search-tests.el -f ert-run-tests-batch-and-exit
;;
;; `memex-search--async' is curried the way `consult--async-pipeline'
;; composes its functions, so a closure recording the actions it is
;; handed stands in for the sink and neither consult nor a minibuffer is
;; needed.  `memex-api-search' is replaced by a stand-in answering with a
;; live `sleep' process, which is what tells a superseded request from a
;; killed one, and its callback is invoked by hand so the order two
;; requests complete in is the test's to choose.
;;
;; Consult is not on the load path under `emacs -Q', so the fallback
;; tests reach the consult-free branch with nothing stubbed out.  They
;; call the command as `(memex-search MODE INITIAL)', the arguments a
;; mode switch re-invokes it with once it has exited its session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)
(require 'memex-completion)

(require 'memex nil t)

(declare-function memex-search--async "memex")
(declare-function memex-search "memex")

(defvar memex-search--consult-noted)

(defconst memex-search-tests--sink-answer (list 'candidates)
  "What the stub sink answers a nil action with.
Its identity is what tells a forwarded nil action apart from one the
async function answered on the sink's behalf.")

(defconst memex-search-tests--cancel-rpc (symbol-function 'memex-cancel-rpc)
  "`memex-cancel-rpc' as memex-core defines it, saved before it is spied on.")

(defvar memex-search-tests--requests nil
  "The searches the stub `memex-api-search' was handed, newest first.")

(defvar memex-search-tests--actions nil
  "The actions the stub sink was handed, newest first.")

(defvar memex-search-tests--cancelled nil
  "The processes `memex-cancel-rpc' was handed, newest first.")

(defun memex-search-tests--sleeper ()
  "Return a live process standing in for a memex request."
  (make-process :name "memex-search-stub" :command (list "sleep" "30")
                :noquery t :connection-type 'pipe))

(defun memex-search-tests--stub-search ()
  "Return a stand-in for `memex-api-search' recording what it is handed.
Each call records a plist of `:query', `:mode', `:callback' and
`:process' and answers with a live process, so a request that was
superseded can be told from one that is still running."
  (lambda (query callback &rest arguments)
    (let ((process (memex-search-tests--sleeper)))
      (push (list :query query :mode (plist-get arguments :mode)
                  :callback callback :process process)
            memex-search-tests--requests)
      process)))

(defun memex-search-tests--answering-stub (records)
  "Return a stand-in for `memex-api-search' answering with RECORDS at once.
The callback runs before the call returns, which is what the bounded
synchronous fetch of the static picker waits for."
  (lambda (query callback &rest arguments)
    (push (list :query query :mode (plist-get arguments :mode))
          memex-search-tests--requests)
    (funcall callback (memex-search-tests--hits records))
    (memex-search-tests--sleeper)))

(defun memex-search-tests--requests ()
  "Return the recorded search requests, oldest first."
  (reverse memex-search-tests--requests))

(defun memex-search-tests--live ()
  "Return the recorded requests whose process is still running."
  (seq-filter (lambda (request) (process-live-p (plist-get request :process)))
              memex-search-tests--requests))

(defun memex-search-tests--mode (request)
  "Return the search mode REQUEST carries, as a symbol."
  (let ((mode (plist-get request :mode)))
    (if (stringp mode) (intern mode) mode)))

(defun memex-search-tests--cancel-spy ()
  "Return a stand-in for `memex-cancel-rpc' recording what it abandons.
The request is abandoned through memex-core itself, which marks the
process so its sentinel reports nothing: a request killed by a bare
`delete-process' instead reaches the sentinel and fails with exit
status 9 on every keystroke."
  (lambda (process)
    (push process memex-search-tests--cancelled)
    (funcall memex-search-tests--cancel-rpc process)))

(defun memex-search-tests--cancelled-p (request)
  "Return non-nil when REQUEST was abandoned through `memex-cancel-rpc'."
  (and (memq (plist-get request :process) memex-search-tests--cancelled) t))

(defun memex-search-tests--complete (request records)
  "Hand RECORDS to REQUEST's callback as memex's scored matches."
  (funcall (plist-get request :callback) (memex-search-tests--hits records)))

(defun memex-search-tests--sink ()
  "Return a sink recording every action it is handed.
A nil action answers with `memex-search-tests--sink-answer', the way
consult's own sink answers one with its candidate list."
  (lambda (action)
    (push action memex-search-tests--actions)
    (and (null action) memex-search-tests--sink-answer)))

(defun memex-search-tests--actions ()
  "Return the actions the stub sink was handed, oldest first."
  (reverse memex-search-tests--actions))

(defun memex-search-tests--drive (mode &rest actions)
  "Return `memex-search--async' for MODE, after ACTIONS have reached it.
The async function is composed over the recording sink and handed each
of ACTIONS in turn."
  (let ((async (funcall (memex-search--async mode) (memex-search-tests--sink))))
    (dolist (action actions async)
      (funcall async action))))

(defun memex-search-tests--cleanup ()
  "Delete every stub request process and forget what was recorded."
  (dolist (request memex-search-tests--requests)
    (let ((process (plist-get request :process)))
      (when (process-live-p process)
        (delete-process process))))
  (setq memex-search-tests--requests nil
        memex-search-tests--actions nil
        memex-search-tests--cancelled nil))

(defun memex-search-tests--record (doc-id project text)
  "Return a record alist carrying DOC-ID, PROJECT and TEXT."
  (list (cons 'source "codex")
        (cons 'doc_id doc-id)
        (cons 'ts 1787140243301)
        (cons 'project project)
        (cons 'session_id "s-search")
        (cons 'role "assistant")
        (cons 'text text)
        (cons 'source_path "/tmp/search.jsonl")))

(defun memex-search-tests--hits (records)
  "Return RECORDS paired with descending scores, memex's wire shape."
  (cl-loop for record in records
           for score downfrom 9.0 by 1.0
           collect (list score record)))

(defun memex-search-tests--matches ()
  "Return two records whose labels run against alphabetical order."
  (list (memex-search-tests--record 9003 "zebra" "zulu hit")
        (memex-search-tests--record 9001 "alpha" "alfa hit")))

(defun memex-search-tests--stale-matches ()
  "Return the records of a query that has since been superseded."
  (list (memex-search-tests--record 7001 "outdated" "stale hit")))

(defun memex-search-tests--doc-ids (candidates)
  "Return the `doc_id' of the record each of CANDIDATES carries."
  (mapcar (lambda (candidate)
            (alist-get 'doc_id (memex-completion-record-of candidate)))
          candidates))

(defun memex-search-tests--about-consult (messages)
  "Return the MESSAGES naming consult."
  (seq-filter (lambda (text) (string-match-p "consult" text)) messages))

(ert-deftest memex-search-async-publishes-flush-candidates-then-refresh ()
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((records (memex-search-tests--matches))
               (async (memex-search-tests--drive 'semantic 'setup "alfa"))
               (requests (memex-search-tests--requests)))
          (should (functionp async))
          (should (equal (length requests) 1))
          (should (equal (plist-get (car requests) :query) "alfa"))
          (should (eq (memex-search-tests--mode (car requests)) 'semantic))
          (setq memex-search-tests--actions nil)
          (memex-search-tests--complete (car requests) records)
          (let* ((actions (memex-search-tests--actions))
                 (candidates (nth 1 actions)))
            (should (equal (length actions) 3))
            (should (eq (nth 0 actions) 'flush))
            (should (eq (nth 2 actions) 'refresh))
            (should (consp candidates))
            (should (equal (length candidates) 2))
            (dolist (candidate candidates)
              (should (stringp candidate))
              (should-not (string-empty-p (substring-no-properties candidate))))
            (should (equal (length (delete-dups
                                    (mapcar #'substring-no-properties
                                            candidates)))
                           2))
            (should (equal (memex-search-tests--doc-ids candidates) '(9003 9001)))
            (should (equal (memex-completion-record-of (car candidates))
                           (car records))))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-async-kills-the-superseded-request ()
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search))
                ((symbol-function 'memex-cancel-rpc)
                 (memex-search-tests--cancel-spy)))
        (memex-search-tests--drive 'lexical 'setup "alf" "alfa")
        (let ((requests (memex-search-tests--requests)))
          (should (equal (length requests) 2))
          (should (equal (mapcar (lambda (request) (plist-get request :query))
                                 requests)
                         '("alf" "alfa")))
          (should (memex-search-tests--cancelled-p (nth 0 requests)))
          (should-not (memex-search-tests--cancelled-p (nth 1 requests)))
          (should-not (process-live-p (plist-get (nth 0 requests) :process)))
          (should (process-live-p (plist-get (nth 1 requests) :process)))
          (should (equal (length (memex-search-tests--live)) 1))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-async-discards-the-stale-generation ()
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((records (memex-search-tests--matches))
               (stale (memex-search-tests--stale-matches)))
          (memex-search-tests--drive 'lexical 'setup "old" "new")
          (let ((requests (memex-search-tests--requests)))
            (should (equal (length requests) 2))
            (setq memex-search-tests--actions nil)
            (memex-search-tests--complete (nth 0 requests) stale)
            (should-not (memex-search-tests--actions))
            (memex-search-tests--complete (nth 1 requests) records)
            (let ((actions (memex-search-tests--actions)))
              (should (equal (length actions) 3))
              (should (eq (nth 0 actions) 'flush))
              (should (eq (nth 2 actions) 'refresh))
              (should (equal (memex-search-tests--doc-ids (nth 1 actions))
                             '(9003 9001)))))
          (memex-search-tests--cleanup)
          (memex-search-tests--drive 'lexical 'setup "old" "new")
          (let ((requests (memex-search-tests--requests)))
            (should (equal (length requests) 2))
            (setq memex-search-tests--actions nil)
            (memex-search-tests--complete (nth 1 requests) records)
            (should (equal (length (memex-search-tests--actions)) 3))
            (setq memex-search-tests--actions nil)
            (memex-search-tests--complete (nth 0 requests) stale)
            (should-not (memex-search-tests--actions)))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-async-cancel-and-destroy-kill-the-request ()
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search))
                ((symbol-function 'memex-cancel-rpc)
                 (memex-search-tests--cancel-spy)))
        (dolist (action '(cancel destroy))
          (let ((async (memex-search-tests--drive 'lexical 'setup "alfa"))
                (request (car memex-search-tests--requests)))
            (should (equal (length (memex-search-tests--live)) 1))
            (setq memex-search-tests--actions nil)
            (funcall async action)
            (should (equal (memex-search-tests--actions) (list action)))
            (should (memex-search-tests--cancelled-p request))
            (should-not (memex-search-tests--live))))
        (should (equal (length (memex-search-tests--requests)) 2)))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-async-forwards-every-other-action-to-the-sink ()
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((curried (memex-search--async 'lexical))
               (async (funcall curried (memex-search-tests--sink)))
               (appended (list "one" "two")))
          (should (equal (func-arity curried) '(1 . 1)))
          (should (equal (func-arity async) '(1 . 1)))
          (setq memex-search-tests--actions nil)
          (funcall async 'setup)
          (funcall async 'flush)
          (funcall async 'refresh)
          (funcall async appended)
          (should (eq (funcall async nil) memex-search-tests--sink-answer))
          (should (equal (memex-search-tests--actions)
                         (list 'setup 'flush 'refresh appended nil)))
          (should-not (memex-search-tests--requests))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-without-consult-reads-one-query-into-the-picker ()
  (unwind-protect
      (let ((records (memex-search-tests--matches))
            (memex-search--consult-noted nil)
            (supplied 'not-called)
            (messages nil))
        (should-not (featurep 'consult))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "alfa")))
                  ((symbol-function 'memex-read-record)
                   (lambda (&optional _prompt records)
                     (setq supplied records)
                     (car records)))
                  ((symbol-function 'message)
                   (lambda (format &rest arguments)
                     (push (apply #'format format arguments) messages)
                     nil)))
          (memex-search)
          (should (equal (length (memex-search-tests--requests)) 1))
          (should (equal (plist-get (car (memex-search-tests--requests)) :query)
                         "alfa"))
          (should (equal supplied records))
          (should (equal (length (memex-search-tests--about-consult messages)) 1))
          (memex-search)
          (should (equal (length (memex-search-tests--requests)) 2))
          (should (equal (length (memex-search-tests--about-consult messages)) 1))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-without-consult-refuses-a-query-with-no-hits ()
  (unwind-protect
      (let ((memex-search--consult-noted nil))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub nil))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _)
                     (or initial "nothing at all")))
                  ((symbol-function 'memex-read-record)
                   (lambda (&rest _)
                     (error "A search with no hits must not reach the recent window")))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (let ((failure (should-error (memex-search) :type 'user-error)))
            (should (string-match-p "nothing at all"
                                    (error-message-string failure))))
          (should (equal (length (memex-search-tests--requests)) 1))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-carries-the-mode-and-the-input-of-a-re-invocation ()
  (unwind-protect
      (let ((records (memex-search-tests--matches))
            (memex-search--consult-noted nil)
            (supplied 'not-called))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "")))
                  ((symbol-function 'memex-read-record)
                   (lambda (&optional _prompt records)
                     (setq supplied records)
                     (car records)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (memex-search 'hybrid "alfa")
          (let ((request (car (memex-search-tests--requests))))
            (should (equal (plist-get request :query) "alfa"))
            (should (eq (memex-search-tests--mode request) 'hybrid)))
          (should (equal supplied records))))
    (memex-search-tests--cleanup)))

(provide 'memex-search-tests)
;;; memex-search-tests.el ends here
