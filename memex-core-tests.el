;;; memex-core-tests.el --- Tests for memex-core.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-core-tests.el -f ert-run-tests-batch-and-exit
;;
;; The transport tests run against a stub executable written to a temp
;; directory, so neither a memex install nor an indexed corpus is needed.
;; The live-contract test skips itself when memex is absent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'memex-core)

(defvar memex-core-tests--dir nil)

(defun memex-core-tests--tempdir ()
  "Return this test's temporary directory, creating it once."
  (or memex-core-tests--dir
      (setq memex-core-tests--dir (make-temp-file "memex-core-test" t))))

(defun memex-core-tests--cleanup ()
  "Remove the temporary directory and any stub process left behind."
  (dolist (process (process-list))
    (when (string-match-p "memex" (process-name process))
      (delete-process process)))
  (when (and memex-core-tests--dir (file-directory-p memex-core-tests--dir))
    (delete-directory memex-core-tests--dir t))
  (setq memex-core-tests--dir nil))

(defun memex-core-tests--stub-named (name body)
  "Write BODY as a stub memex executable called NAME and return its path."
  (let ((path (expand-file-name name (memex-core-tests--tempdir))))
    (with-temp-file path (insert "#!/bin/sh\n" body))
    (set-file-modes path #o755)
    path))

(defun memex-core-tests--stub (body)
  "Write BODY as a stub memex executable and return its path."
  (memex-core-tests--stub-named "memex-stub" body))

(defun memex-core-tests--pong-stub (name delay version)
  "Write stub NAME answering `ping' with VERSION after DELAY seconds."
  (memex-core-tests--stub-named
   name
   (format "cat > /dev/null\nsleep %s\nprintf '%%s' %s\n"
           delay
           (shell-quote-argument
            (json-serialize `((protocol . 1)
                              (response . ((kind . "pong") (version . ,version)))))))))

(defun memex-core-tests--write (name content)
  "Write CONTENT to NAME inside the test directory and return its path."
  (let ((path (expand-file-name name (memex-core-tests--tempdir))))
    (with-temp-file path (insert content))
    path))

(defun memex-core-tests--read (path)
  "Return the contents of PATH."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun memex-core-tests--running-p ()
  "Return non-nil while a memex process of this run is still alive."
  (cl-some (lambda (process)
             (and (process-live-p process)
                  (string-match-p "memex" (process-name process))))
           (process-list)))

(defun memex-core-tests--wait (predicate &optional timeout)
  "Pump process output until PREDICATE is non-nil or TIMEOUT elapses.
TIMEOUT bounds the wait for a memex that has already exited: a stub
still running pushes the deadline back, so a loaded machine that merely
delays every stub does not fail a test about something else.  The
extension is clamped to a ceiling far above any stub's runtime, because
every stub here blocks on EOF and a transport that stops sending one
would otherwise never release the wait."
  (let* ((bound (or timeout 10.0))
         (ceiling (+ (float-time) (* 30 bound)))
         (deadline (min ceiling (+ (float-time) bound))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (when (memex-core-tests--running-p)
        (setq deadline (min ceiling (+ (float-time) bound)))))
    (funcall predicate)))

(defun memex-core-tests--pump (seconds)
  "Pump process output for SECONDS without waiting on any condition."
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(defun memex-core-tests--rpc-buffers ()
  "Return how many transport buffers of `memex-rpc' are alive right now."
  (cl-count-if (lambda (buffer)
                 (string-prefix-p " *memex-rpc" (buffer-name buffer)))
               (buffer-list)))

(defun memex-core-tests--session-json ()
  "Return a session response large enough to reach Emacs in several chunks."
  (json-serialize
   `((protocol . 1)
     (response
      . ((kind . "session")
         (context
          . ((records
              . ,(vconcat
                  (mapcar (lambda (index)
                            `((source . "claude-code")
                              (doc_id . ,index)
                              (ts . 1787140243301)
                              (session_id . "caea32e0")
                              (source_path . "/tmp/caea32e0.jsonl")
                              (role . "assistant")
                              (text . "padding so the payload spans more than one filter call")))
                          (number-sequence 1 40))))
             (cwd . :null))))))))

(ert-deftest memex-core-defines-the-group-and-the-executable-custom ()
  (should (get 'memex 'custom-group))
  (should (custom-variable-p 'memex-executable))
  (should (equal (eval (car (get 'memex-executable 'standard-value)) t) "memex"))
  (should (memq 'memex-executable (mapcar #'car (get 'memex 'custom-group))))
  (should (equal memex-protocol-version 1)))

(ert-deftest memex-core-encodes-the-internally-tagged-request-envelope ()
  (let* ((json (memex--encode-request
                "search" '((spec . ((query . "hello") (limit . 20) (mode . "lexical"))))))
         (decoded (json-parse-string json :object-type 'alist :array-type 'list))
         (request (alist-get 'request decoded)))
    (should (equal (alist-get 'protocol decoded) 1))
    (should (equal (alist-get 'op request) "search"))
    (should (equal (alist-get 'query (alist-get 'spec request)) "hello"))
    (should (equal (alist-get 'limit (alist-get 'spec request)) 20))
    (should-not (assq 'Search decoded))
    (should-not (assq 'Search request))
    (should-not (assq 'type request))
    (should-not (assq 'op decoded)))
  (let* ((decoded (json-parse-string (memex--encode-request "ping")
                                     :object-type 'alist :array-type 'list))
         (request (alist-get 'request decoded)))
    (should (equal (alist-get 'protocol decoded) 1))
    (should (equal (mapcar #'car request) '(op)))
    (should (equal (alist-get 'op request) "ping"))))

(ert-deftest memex-core-request-fields-are-siblings-of-op ()
  (let* ((json (memex--encode-request
                "session" '((session_id . "caea32e0") (source_path . "/tmp/a.jsonl"))))
         (request (alist-get 'request (json-parse-string
                                       json :object-type 'alist :array-type 'list))))
    (should (equal (alist-get 'op request) "session"))
    (should (equal (alist-get 'session_id request) "caea32e0"))
    (should (equal (alist-get 'source_path request) "/tmp/a.jsonl"))
    (should-not (assq 'fields request))
    (should-not (assq 'params request))
    (should-not (assq 'session request))))

(ert-deftest memex-core-decode-collapses-null-and-false-to-nil ()
  (let* ((decoded (memex--decode
                   (concat "{\"context\":{\"cwd\":null,\"next_offset\":null},"
                           "\"partial\":false,"
                           "\"records\":[[0.5,{\"source\":\"open-claw\",\"doc_id\":7}]]}")))
         (context (alist-get 'context decoded))
         (records (alist-get 'records decoded))
         (pair (car records)))
    (should (assq 'cwd context))
    (should-not (alist-get 'cwd context))
    (should-not (eq (alist-get 'cwd context) :null))
    (should (assq 'next_offset context))
    (should-not (alist-get 'next_offset context))
    (should-not (eq (alist-get 'next_offset context) :null))
    (should (assq 'partial decoded))
    (should-not (alist-get 'partial decoded))
    (should-not (eq (alist-get 'partial decoded) :false))
    (should-not (vectorp records))
    (should-not (vectorp pair))
    (should (equal (car pair) 0.5))
    (should (equal (alist-get 'source (cadr pair)) "open-claw"))
    (should (equal (alist-get 'doc_id (cadr pair)) 7))))

(ert-deftest memex-core-error-classes-are-distinguishable ()
  (dolist (symbol '(memex-rpc-error memex-transport-error memex-protocol-error))
    (should (memq 'memex-error (get symbol 'error-conditions))))
  (should-not (memq 'memex-transport-error (get 'memex-rpc-error 'error-conditions)))
  (should-not (memq 'memex-protocol-error (get 'memex-rpc-error 'error-conditions)))
  (should-not (memq 'memex-rpc-error (get 'memex-transport-error 'error-conditions)))
  (should-not (memq 'memex-protocol-error (get 'memex-transport-error 'error-conditions)))
  (should-not (memq 'memex-rpc-error (get 'memex-protocol-error 'error-conditions)))
  (should-not (memq 'memex-transport-error (get 'memex-protocol-error 'error-conditions))))

(ert-deftest memex-core-fails-fast-when-the-executable-is-missing ()
  (let ((memex-executable "memex-that-is-not-installed"))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "Should not spawn a missing executable"))))
      (let ((err (should-error (memex-rpc "ping" nil #'ignore) :type 'memex-error)))
        (should (string-match-p "memex-that-is-not-installed" (format "%S" err)))))))

(ert-deftest memex-core-sends-the-envelope-on-stdin-and-returns-the-payload ()
  (unwind-protect
      (let* ((request-file (expand-file-name "request.json" (memex-core-tests--tempdir)))
             (args-file (expand-file-name "args" (memex-core-tests--tempdir)))
             (memex-executable
              (memex-core-tests--stub
               (format "printf '%%s' \"$*\" > %s\ncat > %s\nprintf '%%s' %s\n"
                       (shell-quote-argument args-file)
                       (shell-quote-argument request-file)
                       (shell-quote-argument
                        "{\"protocol\":1,\"response\":{\"kind\":\"pong\",\"version\":\"0.11.6\"}}"))))
             (payload 'pending)
             (failure nil))
        (memex-rpc "ping" nil
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should-not failure)
        (should (equal (alist-get 'kind payload) "pong"))
        (should (equal (alist-get 'version payload) "0.11.6"))
        (should-not (assq 'protocol payload))
        (should-not (assq 'response payload))
        (should (equal (memex-core-tests--read args-file) "rpc"))
        (let ((sent (json-parse-string (memex-core-tests--read request-file)
                                       :object-type 'alist :array-type 'list)))
          (should (equal (alist-get 'protocol sent) 1))
          (should (equal (alist-get 'op (alist-get 'request sent)) "ping"))))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-accumulates-a-chunked-response-and-parses-it-once ()
  (unwind-protect
      (let* ((json (memex-core-tests--session-json))
             (response-file (memex-core-tests--write "response.json" json))
             (memex-executable
              (memex-core-tests--stub
               (format "cat > /dev/null\nhead -c 1000 %s\nsleep 0.3\ntail -c +1001 %s\n"
                       (shell-quote-argument response-file)
                       (shell-quote-argument response-file))))
             (payload 'pending)
             (failure nil)
             (decodes 0)
             (parse (symbol-function 'json-parse-string)))
        (should (> (length json) 2000))
        (cl-letf (((symbol-function 'json-parse-string)
                   (lambda (&rest args) (cl-incf decodes) (apply parse args))))
          (memex-rpc "session" '((session_id . "caea32e0") (source_path . "/tmp/a.jsonl"))
                     (lambda (value) (setq payload value))
                     (lambda (err) (setq failure err)))
          (should (memex-core-tests--wait
                   (lambda () (or failure (not (eq payload 'pending)))))))
        (should-not failure)
        (should (equal (alist-get 'kind payload) "session"))
        (should (equal decodes 1))
        (let ((context (alist-get 'context payload)))
          (should (equal (length (alist-get 'records context)) 40))
          (should (equal (alist-get 'doc_id (car (last (alist-get 'records context)))) 40))
          (should (assq 'cwd context))
          (should-not (alist-get 'cwd context))
          (should-not (eq (alist-get 'cwd context) :null))))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-error-payload-on-zero-exit-is-not-a-transport-failure ()
  (unwind-protect
      (let* ((memex-executable
              (memex-core-tests--stub
               (format "cat > /dev/null\nprintf '%%s' %s\nexit 0\n"
                       (shell-quote-argument
                        (concat "{\"protocol\":1,\"response\":"
                                "{\"kind\":\"error\",\"message\":\"no session caea32e0\"}}")))))
             (payload 'pending)
             (failure nil))
        (memex-rpc "session" '((session_id . "caea32e0") (source_path . "/tmp/a.jsonl"))
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should (eq payload 'pending))
        (should (eq (car failure) 'memex-rpc-error))
        (should (equal (plist-get (cdr failure) :message) "no session caea32e0")))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-unparsable-output-fails-the-transport ()
  (unwind-protect
      (let* ((memex-executable
              (memex-core-tests--stub
               (concat "cat > /dev/null\n"
                       "printf '%s\\n' 'panic: index out of bounds'\n")))
             (payload 'pending)
             (failure nil))
        (memex-rpc "search" '((spec . ((limit . 20))))
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should (eq payload 'pending))
        (should (eq (car failure) 'memex-transport-error))
        (should (equal (plist-get (cdr failure) :exit-status) 0)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-non-zero-exit-attaches-stderr-verbatim ()
  (unwind-protect
      (let* ((memex-executable
              (memex-core-tests--stub
               (concat "cat > /dev/null\n"
                       "printf '%s\\n' 'Error: missing field `query` at line 1 column 61' >&2\n"
                       "exit 1\n")))
             (payload 'pending)
             (failure nil))
        (memex-rpc "search" '((spec . ((limit . 20))))
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should (eq payload 'pending))
        (should (eq (car failure) 'memex-transport-error))
        (should (equal (plist-get (cdr failure) :exit-status) 1))
        (should (equal (plist-get (cdr failure) :stderr)
                       "Error: missing field `query` at line 1 column 61\n")))
    (memex-core-tests--cleanup)))

(defun memex-core-tests--mismatch-stub (response version-body)
  "Return a stub answering `--version' with VERSION-BODY and `rpc' with RESPONSE."
  (memex-core-tests--stub
   (format (concat "case \"$1\" in\n"
                   "  --version) %s ;;\n"
                   "  *) cat > /dev/null; printf '%%s' %s ;;\n"
                   "esac\n")
           version-body
           (shell-quote-argument response))))

(ert-deftest memex-core-protocol-mismatch-rejects-the-response-as-data ()
  (unwind-protect
      (pcase-dolist (`(,response ,op ,fields)
                     (list (list (concat "{\"protocol\":2,\"response\":"
                                         "{\"kind\":\"pong\",\"version\":\"9.9.9\"}}")
                                 "ping" nil)
                           (list (concat "{\"protocol\":2,\"response\":"
                                         "{\"kind\":\"records\",\"records\":[]}}")
                                 "search"
                                 '((spec . ((query . "x") (limit . 20) (mode . "lexical")))))))
        (let* ((memex-executable
                (memex-core-tests--mismatch-stub response "printf 'memex 7.7.7\\n'"))
               (payload 'pending)
               (failure nil))
          (memex-rpc op fields
                     (lambda (value) (setq payload value))
                     (lambda (err) (setq failure err)))
          (should (memex-core-tests--wait
                   (lambda () (or failure (not (eq payload 'pending))))))
          (should (eq payload 'pending))
          (should (eq (car failure) 'memex-protocol-error))
          (should (equal (plist-get (cdr failure) :expected) 1))
          (should (equal (plist-get (cdr failure) :received) 2))
          (let ((version (plist-get (cdr failure) :version)))
            (should (stringp version))
            (should (string-match-p "7\\.7\\.7" version))
            (should-not (string-match-p "9\\.9\\.9" version)))))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-protocol-mismatch-survives-a-failing-version-probe ()
  (unwind-protect
      (let* ((memex-executable
              (memex-core-tests--mismatch-stub
               "{\"protocol\":2,\"response\":{\"kind\":\"records\",\"records\":[]}}"
               "printf 'boom\\n' >&2; exit 127"))
             (payload 'pending)
             (failure nil))
        (memex-rpc "search" '((spec . ((query . "x") (limit . 20) (mode . "lexical"))))
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should (eq payload 'pending))
        (should (eq (car failure) 'memex-protocol-error))
        (should (equal (plist-get (cdr failure) :expected) 1))
        (should (equal (plist-get (cdr failure) :received) 2))
        (should-not (plist-get (cdr failure) :version)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-version-probe-runs-only-on-protocol-mismatch ()
  (unwind-protect
      (let* ((args-file (expand-file-name "argv" (memex-core-tests--tempdir)))
             (memex-executable
              (memex-core-tests--stub
               (format (concat "printf '%%s\\n' \"$*\" >> %s\n"
                               "case \"$1\" in\n"
                               "  --version) printf 'memex 7.7.7\\n' ;;\n"
                               "  *) cat > /dev/null; printf '%%s' %s ;;\n"
                               "esac\n")
                       (shell-quote-argument args-file)
                       (shell-quote-argument
                        (concat "{\"protocol\":1,\"response\":"
                                "{\"kind\":\"records\",\"records\":[]}}")))))
             (payload 'pending)
             (failure nil))
        (memex-rpc "search" '((spec . ((query . "x") (limit . 20) (mode . "lexical"))))
                   (lambda (value) (setq payload value))
                   (lambda (err) (setq failure err)))
        (should (memex-core-tests--wait
                 (lambda () (or failure (not (eq payload 'pending))))))
        (should-not failure)
        (should (equal (alist-get 'kind payload) "records"))
        (should (equal (memex-core-tests--read args-file) "rpc\n")))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-delivers-neither-callback-nor-errback ()
  (unwind-protect
      (let* ((memex-executable (memex-core-tests--pong-stub "memex-slow" "1.0" "0.11.6"))
             (payload 'pending)
             (failure 'pending)
             (process (memex-rpc "ping" nil
                                 (lambda (value) (setq payload value))
                                 (lambda (err) (setq failure err)))))
        (should (memex-cancel-rpc process))
        (memex-core-tests--pump 2.5)
        (should (eq payload 'pending))
        (should (eq failure 'pending)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-leaves-no-rpc-buffers-behind ()
  (unwind-protect
      (let ((memex-executable (memex-core-tests--pong-stub "memex-slow" "1.0" "0.11.6"))
            (baseline (memex-core-tests--rpc-buffers)))
        (dotimes (_ 5)
          (let ((process (memex-rpc "ping" nil #'ignore #'ignore)))
            (should (> (memex-core-tests--rpc-buffers) baseline))
            (should (memex-cancel-rpc process))
            (should (memex-core-tests--wait
                     (lambda () (not (process-live-p process))) 5.0))))
        (should (memex-core-tests--wait
                 (lambda () (= (memex-core-tests--rpc-buffers) baseline)) 5.0))
        (should (equal (memex-core-tests--rpc-buffers) baseline)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-reports-nothing-to-the-user ()
  (unwind-protect
      (let* ((memex-executable (memex-core-tests--pong-stub "memex-slow" "1.0" "0.11.6"))
             (announced nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format-message format-string args) announced))))
          (let ((process (memex-rpc "ping" nil #'ignore)))
            (memex-cancel-rpc process)
            (memex-core-tests--pump 2.5)))
        (should (equal announced nil)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-after-completion-is-a-no-op ()
  (unwind-protect
      (let* ((memex-executable (memex-core-tests--pong-stub "memex-quick" "0" "0.11.6"))
             (payloads nil)
             (failure nil)
             (process (memex-rpc "ping" nil
                                 (lambda (value) (push value payloads))
                                 (lambda (err) (setq failure err)))))
        (should (memex-core-tests--wait (lambda () (or failure payloads))))
        (should-not failure)
        (should (equal (length payloads) 1))
        (should (equal (alist-get 'version (car payloads)) "0.11.6"))
        (should-not (memex-cancel-rpc process))
        (memex-core-tests--pump 0.5)
        (should (equal (length payloads) 1))
        (should (equal (alist-get 'version (car payloads)) "0.11.6"))
        (should-not failure))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-leaves-a-concurrent-request-untouched ()
  (unwind-protect
      (let ((doomed-payload 'pending)
            (doomed-failure 'pending)
            (kept-payload 'pending)
            (kept-failure nil)
            (doomed nil))
        (let ((memex-executable (memex-core-tests--pong-stub "memex-slow" "1.0" "doomed")))
          (setq doomed (memex-rpc "ping" nil
                                  (lambda (value) (setq doomed-payload value))
                                  (lambda (err) (setq doomed-failure err)))))
        (let ((memex-executable (memex-core-tests--pong-stub "memex-quick" "0.4" "kept")))
          (memex-rpc "ping" nil
                     (lambda (value) (setq kept-payload value))
                     (lambda (err) (setq kept-failure err))))
        (should (memex-cancel-rpc doomed))
        (should (memex-core-tests--wait
                 (lambda () (or kept-failure (not (eq kept-payload 'pending))))))
        (should-not kept-failure)
        (should (equal (alist-get 'kind kept-payload) "pong"))
        (should (equal (alist-get 'version kept-payload) "kept"))
        (memex-core-tests--pump 1.5)
        (should (eq doomed-payload 'pending))
        (should (eq doomed-failure 'pending)))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-cancel-return-value-distinguishes-live-from-finished ()
  (unwind-protect
      (progn
        (should-not (memex-cancel-rpc nil))
        (let* ((memex-executable (memex-core-tests--pong-stub "memex-slow" "1.0" "0.11.6"))
               (process (memex-rpc "ping" nil #'ignore #'ignore)))
          (should (memex-cancel-rpc process))
          (should-not (memex-cancel-rpc process)))
        (let* ((memex-executable (memex-core-tests--pong-stub "memex-quick" "0" "0.11.6"))
               (payload 'pending)
               (process (memex-rpc "ping" nil
                                   (lambda (value) (setq payload value))
                                   #'ignore)))
          (should (memex-core-tests--wait (lambda () (not (eq payload 'pending)))))
          (should-not (memex-cancel-rpc process))))
    (memex-core-tests--cleanup)))

(ert-deftest memex-core-live-ping-answers-with-protocol-1 ()
  (unless (executable-find "memex")
    (ert-skip "memex is not installed"))
  (let ((memex-executable "memex")
        (payload 'pending)
        (failure nil))
    (memex-rpc "ping" nil
               (lambda (value) (setq payload value))
               (lambda (err) (setq failure err)))
    (should (memex-core-tests--wait
             (lambda () (or failure (not (eq payload 'pending)))) 30.0))
    (should-not failure)
    (should (equal (alist-get 'kind payload) "pong"))
    (should (stringp (alist-get 'version payload)))))

(provide 'memex-core-tests)
;;; memex-core-tests.el ends here
