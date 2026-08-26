;;; memex-api-tests.el --- Tests for memex-api.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-api-tests.el -f ert-run-tests-batch-and-exit
;;
;; The wrapper tests run against a stub executable that records the request
;; it was handed on stdin, so neither a memex install nor an indexed corpus
;; is needed.  The live-contract test skips itself when memex is absent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'memex-core)

(require 'memex-api)

(defvar memex-api-tests--dir nil)

(defconst memex-api-tests--wrappers
  '(memex-api-ping
    memex-api-search
    memex-api-recent
    memex-api-session
    memex-api-show
    memex-api-session-page
    memex-api-session-batch
    memex-api-index
    memex-api-usage
    memex-api-usage-activity
    memex-api-session-activity)
  "Wrapper expected for each of memex's eleven RPC operations.")

(defconst memex-api-tests--search-optional-fields
  '(project role tool session_id session_scope cwd source since until
            min_score project_grouping)
  "Wire name of every optional `SearchSpec' field.")

(defconst memex-api-tests--usage-optional-fields
  '(source project session_keys machine_session_keys since_ms until_ms)
  "Wire name of every optional `UsageSpec' field.")

(defconst memex-api-tests--session-activity-optional-fields
  '(source project since_ms until_ms)
  "Wire name of every optional `SessionActivitySpec' field.")

(defconst memex-api-tests--omitted-record-fields
  '(tool_name tool_input tool_output
              event_id parent_event_id logical_parent_event_id
              parent_session_id thread_source conversation_kind
              parent_tool_use_id source_tool_use_id
              source_tool_assistant_uuid)
  "Wire name of every `Record' field the `open-claw' fixture omits.")

(defun memex-api-tests--tempdir ()
  "Return this test's temporary directory, creating it once."
  (or memex-api-tests--dir
      (setq memex-api-tests--dir (make-temp-file "memex-api-test" t))))

(defun memex-api-tests--cleanup ()
  "Remove the temporary directory and any stub process left behind."
  (dolist (process (process-list))
    (when (string-match-p "memex" (process-name process))
      (delete-process process)))
  (when (and memex-api-tests--dir (file-directory-p memex-api-tests--dir))
    (delete-directory memex-api-tests--dir t))
  (setq memex-api-tests--dir nil))

(defun memex-api-tests--stub (body)
  "Write BODY as a stub memex executable and return its path."
  (let ((path (expand-file-name "memex-stub" (memex-api-tests--tempdir))))
    (with-temp-file path (insert "#!/bin/sh\n" body))
    (set-file-modes path #o755)
    path))

(defun memex-api-tests--read (path)
  "Return the contents of PATH."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun memex-api-tests--running-p ()
  "Return non-nil while a memex process of this run is still alive."
  (cl-some (lambda (process)
             (and (process-live-p process)
                  (string-match-p "memex" (process-name process))))
           (process-list)))

(defun memex-api-tests--wait (predicate &optional timeout)
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
      (when (memex-api-tests--running-p)
        (setq deadline (min ceiling (+ (float-time) bound)))))
    (funcall predicate)))

(defun memex-api-tests--request-file ()
  "Return the path the stub records the request it received in."
  (expand-file-name "request.json" (memex-api-tests--tempdir)))

(defun memex-api-tests--response (kind &rest fields)
  "Return a memex response of KIND carrying FIELDS as a JSON string."
  (json-serialize `((protocol . 1)
                    (response . ,(cons (cons 'kind kind) fields)))))

(defun memex-api-tests--recording-stub (response)
  "Return a stub saving the request it is handed and printing RESPONSE."
  (memex-api-tests--stub
   (format "cat > %s\nprintf '%%s' %s\n"
           (shell-quote-argument (memex-api-tests--request-file))
           (shell-quote-argument response))))

(defun memex-api-tests--exchange-with (stub invoke &optional timeout)
  "Run INVOKE against the memex STUB and return the exchange.
INVOKE is called with a callback and an errback and must start one
request.  TIMEOUT bounds the wait.  The exchange is a plist of
`:request', the request memex received decoded as an alist, `:payload',
what the callback was handed, and `:error', what the errback was
handed."
  (let ((memex-executable stub)
        (payload 'pending)
        (failure nil))
    (funcall invoke
             (lambda (value) (setq payload value))
             (lambda (err) (setq failure err)))
    (should (memex-api-tests--wait
             (lambda () (or failure (not (eq payload 'pending)))) timeout))
    (list :request (when (file-exists-p (memex-api-tests--request-file))
                     (json-parse-string
                      (memex-api-tests--read (memex-api-tests--request-file))
                      :object-type 'alist :array-type 'array))
          :payload (unless (eq payload 'pending) payload)
          :error failure)))

(defun memex-api-tests--exchange (response invoke &optional timeout)
  "Run INVOKE against a stub answering RESPONSE and return the exchange.
TIMEOUT bounds the wait.  See `memex-api-tests--exchange-with'."
  (memex-api-tests--exchange-with
   (memex-api-tests--recording-stub response) invoke timeout))

(defun memex-api-tests--sent (exchange)
  "Return the request object memex received in EXCHANGE."
  (alist-get 'request (plist-get exchange :request)))

(defun memex-api-tests--codex-record ()
  "Return a record whose flattened link fields are all populated."
  '((source . "codex")
    (doc_id . 388401)
    (ts . 1787140243301)
    (project . "memex.el")
    (session_id . "01a0380e-ec8f-7b63-bce1-10e5a205e000")
    (turn_id . 41)
    (role . "tool_result")
    (text . "Script completed")
    (tool_name . "shell")
    (source_path . "/tmp/01a0380e.jsonl")
    (event_id . "ev-41")
    (parent_event_id . "ev-40")
    (logical_parent_event_id . "ev-39")
    (parent_session_id . "0000-parent")
    (thread_source . "main")
    (conversation_kind . "session")
    (parent_tool_use_id . "tu-7")
    (source_tool_use_id . "tu-8")
    (source_tool_assistant_uuid . "uuid-9")))

(defun memex-api-tests--open-claw-record ()
  "Return a record from the kebab-case `open-claw' source."
  '((source . "open-claw")
    (doc_id . 12)
    (ts . 1787140243302)
    (project . "memex.el")
    (session_id . "caea32e0")
    (turn_id . 1)
    (role . "user")
    (text . "second")
    (source_path . "/tmp/caea32e0.jsonl")))

(defun memex-api-tests--records-response ()
  "Return a records payload pairing two scores with two records."
  (memex-api-tests--response
   "records"
   (cons 'records
         (vector (vector 24.105473 (memex-api-tests--codex-record))
                 (vector 0.5 (memex-api-tests--open-claw-record))))))

(defun memex-api-tests--page-context ()
  "Return a session page context."
  '((session_id . "caea32e0")
    (source_path . "/tmp/caea32e0.jsonl")
    (records . [])
    (cwd . :null)
    (offset . 0)
    (total . 0)
    (next_offset . :null)))

(defun memex-api-tests--operations ()
  "Return the operation tag, response and invocation of every wrapper."
  (list
   (list "ping"
         (memex-api-tests--response "pong" '(version . "0.11.6"))
         (lambda (cb eb) (memex-api-ping cb :errback eb)))
   (list "search"
         (memex-api-tests--records-response)
         (lambda (cb eb) (memex-api-search "emacs" cb :errback eb)))
   (list "recent"
         (memex-api-tests--records-response)
         (lambda (cb eb) (memex-api-recent cb :errback eb)))
   (list "session"
         (memex-api-tests--response
          "session" '(context . ((records . []) (cwd . :null))))
         (lambda (cb eb)
           (memex-api-session "caea32e0" "/tmp/caea32e0.jsonl" cb :errback eb)))
   (list "show"
         (memex-api-tests--response
          "record" (cons 'record (memex-api-tests--codex-record)))
         (lambda (cb eb) (memex-api-show 388401 cb :errback eb)))
   (list "session_page"
         (memex-api-tests--response
          "session_page" (cons 'context (memex-api-tests--page-context)))
         (lambda (cb eb)
           (memex-api-session-page "caea32e0" "/tmp/caea32e0.jsonl" cb :errback eb)))
   (list "session_batch"
         (memex-api-tests--response
          "session_batch"
          (cons 'contexts (vector (memex-api-tests--page-context))))
         (lambda (cb eb)
           (memex-api-session-batch
            (list (list :session-id "caea32e0" :source-path "/tmp/caea32e0.jsonl"))
            cb :errback eb)))
   (list "index"
         (memex-api-tests--response
          "index" '(records_added . 3) '(records_embedded . 2)
          '(files_scanned . 9) '(files_skipped . 1))
         (lambda (cb eb) (memex-api-index cb :errback eb)))
   (list "usage"
         (memex-api-tests--response
          "usage" '(report . ((authority . "local") (events . 12))))
         (lambda (cb eb) (memex-api-usage cb :errback eb)))
   (list "usage_activity"
         (memex-api-tests--response
          "usage_activity"
          (cons 'points (vector '((machine . "local") (source . "claude")
                                  (timestamp_ms . 1787140243301)
                                  (total_tokens . 900))))
          '(partial . :false))
         (lambda (cb eb) (memex-api-usage-activity cb :errback eb)))
   (list "session_activity"
         (memex-api-tests--response
          "session_activity"
          (cons 'points (vector '((machine . "local") (source . "claude")
                                  (timestamp_ms . 1787140243301)))))
         (lambda (cb eb) (memex-api-session-activity cb :errback eb)))))

(ert-deftest memex-api-defines-a-non-interactive-wrapper-per-operation ()
  (should (equal (length memex-api-tests--wrappers) 11))
  (dolist (wrapper memex-api-tests--wrappers)
    (should (fboundp wrapper))
    (should-not (commandp wrapper))))

(ert-deftest memex-api-sends-the-operation-tag-of-every-wrapper ()
  (unwind-protect
      (pcase-dolist (`(,op ,response ,invoke) (memex-api-tests--operations))
        (let* ((exchange (memex-api-tests--exchange response invoke))
               (request (memex-api-tests--sent exchange)))
          (should-not (plist-get exchange :error))
          (should (equal (alist-get 'op request) op))
          (should-not (assq 'type request))
          (should-not (assq 'kind request))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-search-carries-its-spec-inline-under-spec ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--records-response)
                        (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
             (request (memex-api-tests--sent exchange))
             (spec (alist-get 'spec request)))
        (should (equal (alist-get 'op request) "search"))
        (should (equal (alist-get 'query spec) "emacs"))
        (should-not (assq 'request request))
        (should-not (assq 'query request))
        (should-not (assq 'spec spec)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-session-page-nests-its-request-under-request ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--response
                         "session_page" (cons 'context (memex-api-tests--page-context)))
                        (lambda (cb eb)
                          (memex-api-session-page
                           "caea32e0" "/tmp/caea32e0.jsonl" cb
                           :offset 100 :limit 250 :errback eb))))
             (request (memex-api-tests--sent exchange))
             (page (alist-get 'request request)))
        (should (equal (alist-get 'op request) "session_page"))
        (should (equal (alist-get 'session_id page) "caea32e0"))
        (should (equal (alist-get 'source_path page) "/tmp/caea32e0.jsonl"))
        (should (equal (alist-get 'offset page) 100))
        (should (equal (alist-get 'limit page) 250))
        (should-not (assq 'spec request))
        (should-not (assq 'session_id request))
        (should-not (assq 'limit request)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-session-page-defaults-to-the-first-full-page ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--response
                         "session_page" (cons 'context (memex-api-tests--page-context)))
                        (lambda (cb eb)
                          (memex-api-session-page
                           "caea32e0" "/tmp/caea32e0.jsonl" cb :errback eb))))
             (page (alist-get 'request (memex-api-tests--sent exchange))))
        (should (equal (alist-get 'offset page) 0))
        (should (equal (alist-get 'limit page) memex-api-max-session-page-size)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-session-sends-both-ids-as-siblings-of-op ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--response
                         "session" '(context . ((records . []) (cwd . :null))))
                        (lambda (cb eb)
                          (memex-api-session "caea32e0" "/tmp/caea32e0.jsonl"
                                             cb :errback eb))))
             (request (memex-api-tests--sent exchange)))
        (should (equal (alist-get 'op request) "session"))
        (should (equal (alist-get 'session_id request) "caea32e0"))
        (should (equal (alist-get 'source_path request) "/tmp/caea32e0.jsonl"))
        (should-not (assq 'spec request))
        (should-not (assq 'request request))
        (should-not (assq 'session request))
        (should-not (assq 'context request)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-recent-and-show-send-their-fields-as-siblings-of-op ()
  (unwind-protect
      (progn
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--records-response)
                          (lambda (cb eb) (memex-api-recent cb :errback eb))))
               (request (memex-api-tests--sent exchange)))
          (should (equal (alist-get 'op request) "recent"))
          (should (equal (alist-get 'limit request) 20))
          (should-not (assq 'spec request))
          (should-not (assq 'project_grouping request)))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--records-response)
                          (lambda (cb eb)
                            (memex-api-recent cb :limit 500
                                              :project-grouping 'repository
                                              :errback eb))))
               (request (memex-api-tests--sent exchange)))
          (should (equal (alist-get 'limit request) 500))
          (should (equal (alist-get 'project_grouping request) "repository")))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "record" (cons 'record (memex-api-tests--codex-record)))
                          (lambda (cb eb) (memex-api-show 388401 cb :errback eb))))
               (request (memex-api-tests--sent exchange)))
          (should (equal (alist-get 'op request) "show"))
          (should (equal (alist-get 'doc_id request) 388401))
          (should-not (assq 'spec request))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-ping-and-index-send-nothing-beside-the-op ()
  (unwind-protect
      (progn
        (let ((request (memex-api-tests--sent
                        (memex-api-tests--exchange
                         (memex-api-tests--response "pong" '(version . "0.11.6"))
                         (lambda (cb eb) (memex-api-ping cb :errback eb))))))
          (should (equal (mapcar #'car request) '(op))))
        (let ((request (memex-api-tests--sent
                        (memex-api-tests--exchange
                         (memex-api-tests--response
                          "index" '(records_added . 0) '(records_embedded . 0)
                          '(files_scanned . 0) '(files_skipped . 0))
                         (lambda (cb eb) (memex-api-index cb :errback eb))))))
          (should (equal (mapcar #'car request) '(op)))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-search-always-sends-the-mandatory-spec-fields ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--records-response)
                        (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
             (spec (alist-get 'spec (memex-api-tests--sent exchange))))
        (should (equal (alist-get 'query spec) "emacs"))
        (should (assq 'limit spec))
        (should (= (alist-get 'limit spec) 20))
        (should (equal (alist-get 'mode spec) "lexical"))
        (should (assq 'recency_weight spec))
        (should (floatp (alist-get 'recency_weight spec)))
        (should (= (alist-get 'recency_weight spec) 1.0))
        (should (assq 'recency_half_life_days spec))
        (should (floatp (alist-get 'recency_half_life_days spec)))
        (should (= (alist-get 'recency_half_life_days spec) 30.0))
        (dolist (omitted memex-api-tests--search-optional-fields)
          (should (equal (list omitted (assq omitted spec))
                         (list omitted nil)))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-search-keywords-reach-the-spec-as-wire-values ()
  (unwind-protect
      (progn
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--records-response)
                          (lambda (cb eb)
                            (memex-api-search
                             "emacs" cb
                             :mode 'semantic :limit 5 :source 'open-claw
                             :project "memex.el" :min-score 0.25
                             :recency-weight 0.0 :recency-half-life-days 7.0
                             :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange))))
          (should (equal (alist-get 'mode spec) "semantic"))
          (should (equal (alist-get 'source spec) "open-claw"))
          (should (equal (alist-get 'project spec) "memex.el"))
          (should (= (alist-get 'limit spec) 5))
          (should (= (alist-get 'min_score spec) 0.25))
          (should (= (alist-get 'recency_weight spec) 0.0))
          (should (= (alist-get 'recency_half_life_days spec) 7.0)))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--records-response)
                          (lambda (cb eb)
                            (memex-api-search
                             "emacs" cb
                             :mode 'hybrid :limit 5 :source 'open-claw
                             :project "memex.el" :min-score 0.25
                             :recency-weight 0.0 :recency-half-life-days 7.0
                             :role "assistant" :tool "shell"
                             :session-id "caea32e0"
                             :session-scope
                             (list (list :source 'open-claw
                                         :session-id "caea32e0"
                                         :source-path "/tmp/caea32e0.jsonl"))
                             :cwd "/home/user/memex.el"
                             :since 1787140243000 :until 1787140244000
                             :project-grouping 'repository
                             :include-reasoning t
                             :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange)))
               (scope (alist-get 'session_scope spec)))
          (should (equal (alist-get 'mode spec) "hybrid"))
          (should (equal (alist-get 'role spec) "assistant"))
          (should (equal (alist-get 'tool spec) "shell"))
          (should (equal (alist-get 'session_id spec) "caea32e0"))
          (should (equal (alist-get 'cwd spec) "/home/user/memex.el"))
          (should (equal (alist-get 'since spec) 1787140243000))
          (should (equal (alist-get 'until spec) 1787140244000))
          (should (equal (alist-get 'project_grouping spec) "repository"))
          (should (eq (alist-get 'include_reasoning spec) t))
          (should (vectorp scope))
          (should (equal (length scope) 1))
          (should (equal (alist-get 'source (aref scope 0)) "open-claw"))
          (should (equal (alist-get 'session_id (aref scope 0)) "caea32e0"))
          (should (equal (alist-get 'source_path (aref scope 0))
                         "/tmp/caea32e0.jsonl"))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-usage-always-sends-the-mandatory-spec-fields ()
  (unwind-protect
      (dolist (wrapper '(memex-api-usage memex-api-usage-activity))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "usage" '(report . ((authority . "local"))))
                          (lambda (cb eb) (funcall wrapper cb :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange))))
          (should (equal (alist-get 'project_grouping spec) "flat"))
          (should (equal (alist-get 'cost_mode spec) "auto"))
          (should (assq 'include_events spec))
          (should (eq (alist-get 'include_events spec) :false))
          (should (assq 'memo_ttl_ms spec))
          (should (= (alist-get 'memo_ttl_ms spec) 0))
          (dolist (omitted memex-api-tests--usage-optional-fields)
            (should (equal (list wrapper omitted (assq omitted spec))
                           (list wrapper omitted nil)))))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "usage" '(report . ((authority . "local"))))
                          (lambda (cb eb)
                            (funcall wrapper cb
                                     :source 'open-claw
                                     :project "memex.el"
                                     :project-grouping 'repository
                                     :session-keys
                                     (list (list :source 'codex
                                                 :session-id "01a0380e"))
                                     :machine-session-keys
                                     (list (list :machine "local"
                                                 :source 'open-claw
                                                 :session-id "caea32e0"))
                                     :since-ms 1787140243000
                                     :until-ms 1787140244000
                                     :cost-mode 'reprice
                                     :include-events t
                                     :memo-ttl-ms 60000
                                     :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange))))
          (should (equal (alist-get 'source spec) "open-claw"))
          (should (equal (alist-get 'project spec) "memex.el"))
          (should (equal (alist-get 'project_grouping spec) "repository"))
          (should (equal (alist-get 'session_keys spec)
                         (vector (vector "codex" "01a0380e"))))
          (should (equal (alist-get 'machine_session_keys spec)
                         (vector (vector "local" "open-claw" "caea32e0"))))
          (should (equal (alist-get 'since_ms spec) 1787140243000))
          (should (equal (alist-get 'until_ms spec) 1787140244000))
          (should (equal (alist-get 'cost_mode spec) "reprice"))
          (should (eq (alist-get 'include_events spec) t))
          (should (equal (alist-get 'memo_ttl_ms spec) 60000))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-session-activity-always-sends-its-project-grouping ()
  (unwind-protect
      (progn
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "session_activity" '(points . []))
                          (lambda (cb eb) (memex-api-session-activity cb :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange))))
          (should (equal (alist-get 'project_grouping spec) "flat"))
          (should-not (assq 'cost_mode spec))
          (dolist (omitted memex-api-tests--session-activity-optional-fields)
            (should (equal (list omitted (assq omitted spec))
                           (list omitted nil)))))
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "session_activity" '(points . []))
                          (lambda (cb eb)
                            (memex-api-session-activity
                             cb
                             :source 'open-claw
                             :project "memex.el"
                             :project-grouping 'repository
                             :since-ms 1787140243000
                             :until-ms 1787140244000
                             :errback eb))))
               (spec (alist-get 'spec (memex-api-tests--sent exchange))))
          (should (equal (alist-get 'source spec) "open-claw"))
          (should (equal (alist-get 'project spec) "memex.el"))
          (should (equal (alist-get 'project_grouping spec) "repository"))
          (should (equal (alist-get 'since_ms spec) 1787140243000))
          (should (equal (alist-get 'until_ms spec) 1787140244000))
          (should-not (assq 'cost_mode spec))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-session-batch-sends-an-array-of-page-requests ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--response
                         "session_batch"
                         (cons 'contexts (vector (memex-api-tests--page-context))))
                        (lambda (cb eb)
                          (memex-api-session-batch
                           (list (list :session-id "a" :source-path "/tmp/a.jsonl")
                                 (list :session-id "b" :source-path "/tmp/b.jsonl"
                                       :offset 10 :limit 250))
                           cb :errback eb))))
             (request (memex-api-tests--sent exchange))
             (requests (alist-get 'requests request)))
        (should (equal (alist-get 'op request) "session_batch"))
        (should (vectorp requests))
        (should (equal (length requests) 2))
        (should-not (assq 'spec request))
        (should-not (assq 'request request))
        (let ((first (aref requests 0))
              (second (aref requests 1)))
          (should (equal (alist-get 'session_id first) "a"))
          (should (equal (alist-get 'source_path first) "/tmp/a.jsonl"))
          (should (equal (alist-get 'offset first) 0))
          (should (equal (alist-get 'limit first) memex-api-max-session-page-size))
          (should (equal (alist-get 'session_id second) "b"))
          (should (equal (alist-get 'offset second) 10))
          (should (equal (alist-get 'limit second) 250))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-refuses-requests-above-the-server-caps ()
  (should (equal memex-api-max-session-page-size 500))
  (should (equal memex-api-max-session-batch-size 32))
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'memex-rpc)
                   (lambda (&rest _)
                     (error "The API layer must not send an over-cap request"))))
          (should-error (memex-api-session-page
                         "caea32e0" "/tmp/a.jsonl" #'ignore :limit 501)
                        :type 'user-error)
          (should-error (memex-api-session-page
                         "caea32e0" "/tmp/a.jsonl" #'ignore :limit 0)
                        :type 'user-error)
          (should-error (memex-api-session-batch
                         (cl-loop for index below 33
                                  collect (list :session-id (number-to-string index)
                                                :source-path "/tmp/a.jsonl"))
                         #'ignore)
                        :type 'user-error)
          (should-error (memex-api-session-batch
                         (list (list :session-id "a" :source-path "/tmp/a.jsonl"
                                     :limit 501))
                         #'ignore)
                        :type 'user-error)
          (should-error (memex-api-session-batch
                         (list (list :session-id "a" :source-path "/tmp/a.jsonl"
                                     :limit 0))
                         #'ignore)
                        :type 'user-error))
        (let ((exchange (memex-api-tests--exchange
                         (memex-api-tests--response
                          "session_page" (cons 'context (memex-api-tests--page-context)))
                         (lambda (cb eb)
                           (memex-api-session-page "caea32e0" "/tmp/a.jsonl" cb
                                                   :limit 500 :errback eb)))))
          (should-not (plist-get exchange :error))
          (should (equal (alist-get 'limit
                                    (alist-get 'request
                                               (memex-api-tests--sent exchange)))
                         500)))
        (let ((exchange (memex-api-tests--exchange
                         (memex-api-tests--response
                          "session_batch"
                          (cons 'contexts (vector (memex-api-tests--page-context))))
                         (lambda (cb eb)
                           (memex-api-session-batch
                            (cl-loop for index below 32
                                     collect (list :session-id (number-to-string index)
                                                   :source-path "/tmp/a.jsonl"))
                            cb :errback eb)))))
          (should-not (plist-get exchange :error))
          (should (equal (length (alist-get 'requests
                                            (memex-api-tests--sent exchange)))
                         32))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-hands-records-as-score-and-record-pairs ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--records-response)
                        (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
             (records (plist-get exchange :payload)))
        (should-not (plist-get exchange :error))
        (should (listp records))
        (should-not (vectorp records))
        (should (equal (length records) 2))
        (let ((first (nth 0 records))
              (second (nth 1 records)))
          (should (numberp (car first)))
          (should (< (abs (- (car first) 24.105473)) 1e-6))
          (should (listp (cadr first)))
          (should (consp (car (cadr first))))
          (should (equal (alist-get 'doc_id (cadr first)) 388401))
          (should (equal (alist-get 'source (cadr first)) "codex"))
          (should (numberp (car second)))
          (should (< (abs (- (car second) 0.5)) 1e-6))
          (should (equal (alist-get 'doc_id (cadr second)) 12))
          (should (equal (alist-get 'source (cadr second)) "open-claw"))
          (should (equal (alist-get 'role (cadr second)) "user"))
          (should (equal (alist-get 'text (cadr second)) "second"))
          (dolist (omitted memex-api-tests--omitted-record-fields)
            (should (equal (list omitted (assq omitted (cadr second)))
                           (list omitted nil))))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-record-link-fields-stay-top-level ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--response
                         "record" (cons 'record (memex-api-tests--codex-record)))
                        (lambda (cb eb) (memex-api-show 388401 cb :errback eb))))
             (record (plist-get exchange :payload)))
        (should-not (plist-get exchange :error))
        (should (equal (alist-get 'doc_id record) 388401))
        (should (equal (alist-get 'event_id record) "ev-41"))
        (should (equal (alist-get 'parent_event_id record) "ev-40"))
        (should (equal (alist-get 'logical_parent_event_id record) "ev-39"))
        (should (equal (alist-get 'parent_session_id record) "0000-parent"))
        (should (equal (alist-get 'thread_source record) "main"))
        (should (equal (alist-get 'conversation_kind record) "session"))
        (should (equal (alist-get 'parent_tool_use_id record) "tu-7"))
        (should (equal (alist-get 'source_tool_use_id record) "tu-8"))
        (should (equal (alist-get 'source_tool_assistant_uuid record) "uuid-9"))
        (should-not (assq 'links record))
        (should-not (assq 'record record))
        (should-not (assq 'kind record)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-record-timestamp-stays-epoch-milliseconds ()
  (unwind-protect
      (let* ((exchange (memex-api-tests--exchange
                        (memex-api-tests--records-response)
                        (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
             (record (cadr (car (plist-get exchange :payload)))))
        (should (integerp (alist-get 'ts record)))
        (should (equal (alist-get 'ts record) 1787140243301)))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-unwraps-single-field-payloads ()
  (unwind-protect
      (progn
        (let ((exchange (memex-api-tests--exchange
                         (memex-api-tests--response "pong" '(version . "0.11.6"))
                         (lambda (cb eb) (memex-api-ping cb :errback eb)))))
          (should (equal (plist-get exchange :payload) "0.11.6")))
        (let ((context (plist-get
                        (memex-api-tests--exchange
                         (memex-api-tests--response
                          "session"
                          (cons 'context
                                (list (cons 'records
                                            (vector (memex-api-tests--codex-record)))
                                      '(cwd . :null))))
                         (lambda (cb eb)
                           (memex-api-session "caea32e0" "/tmp/a.jsonl" cb :errback eb)))
                        :payload)))
          (should (assq 'cwd context))
          (should-not (alist-get 'cwd context))
          (should (equal (length (alist-get 'records context)) 1))
          (should-not (assq 'kind context)))
        (let ((context (plist-get
                        (memex-api-tests--exchange
                         (memex-api-tests--response
                          "session_page" (cons 'context (memex-api-tests--page-context)))
                         (lambda (cb eb)
                           (memex-api-session-page "caea32e0" "/tmp/a.jsonl"
                                                   cb :errback eb)))
                        :payload)))
          (should (equal (alist-get 'session_id context) "caea32e0"))
          (should (equal (alist-get 'total context) 0))
          (should (assq 'next_offset context))
          (should-not (alist-get 'next_offset context))
          (should-not (assq 'kind context)))
        (let ((contexts (plist-get
                         (memex-api-tests--exchange
                          (memex-api-tests--response
                           "session_batch"
                           (cons 'contexts (vector (memex-api-tests--page-context))))
                          (lambda (cb eb)
                            (memex-api-session-batch
                             (list (list :session-id "caea32e0"
                                         :source-path "/tmp/a.jsonl"))
                             cb :errback eb)))
                         :payload)))
          (should (listp contexts))
          (should (equal (length contexts) 1))
          (should (equal (alist-get 'session_id (car contexts)) "caea32e0")))
        (let ((report (plist-get
                       (memex-api-tests--exchange
                        (memex-api-tests--response
                         "usage" '(report . ((authority . "local") (events . 12))))
                        (lambda (cb eb) (memex-api-usage cb :errback eb)))
                       :payload)))
          (should (equal (alist-get 'authority report) "local"))
          (should (equal (alist-get 'events report) 12))
          (should-not (assq 'report report))
          (should-not (assq 'kind report)))
        (let ((points (plist-get
                       (memex-api-tests--exchange
                        (memex-api-tests--response
                         "session_activity"
                         (cons 'points
                               (vector '((machine . "local") (source . "claude")
                                         (timestamp_ms . 1787140243301)))))
                        (lambda (cb eb) (memex-api-session-activity cb :errback eb)))
                       :payload)))
          (should (listp points))
          (should (equal (length points) 1))
          (should (equal (alist-get 'timestamp_ms (car points)) 1787140243301))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-hands-multi-field-payloads-without-the-kind-tag ()
  (unwind-protect
      (progn
        (let ((report (plist-get
                       (memex-api-tests--exchange
                        (memex-api-tests--response
                         "index" '(records_added . 3) '(records_embedded . 2)
                         '(files_scanned . 9) '(files_skipped . 1))
                        (lambda (cb eb) (memex-api-index cb :errback eb)))
                       :payload)))
          (should (equal (alist-get 'records_added report) 3))
          (should (equal (alist-get 'records_embedded report) 2))
          (should (equal (alist-get 'files_scanned report) 9))
          (should (equal (alist-get 'files_skipped report) 1))
          (should-not (assq 'kind report)))
        (let ((activity (plist-get
                         (memex-api-tests--exchange
                          (memex-api-tests--response
                           "usage_activity"
                           (cons 'points
                                 (vector '((machine . "local") (source . "claude")
                                           (timestamp_ms . 1787140243301)
                                           (total_tokens . 900))))
                           '(partial . :false))
                          (lambda (cb eb) (memex-api-usage-activity cb :errback eb)))
                         :payload)))
          (should (equal (length (alist-get 'points activity)) 1))
          (should (equal (alist-get 'total_tokens (car (alist-get 'points activity)))
                         900))
          (should (assq 'partial activity))
          (should-not (alist-get 'partial activity))
          (should-not (assq 'kind activity))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-propagates-every-error-class-unchanged ()
  (unwind-protect
      (progn
        (let* ((exchange (memex-api-tests--exchange
                          (memex-api-tests--response
                           "error" '(message . "no session caea32e0"))
                          (lambda (cb eb)
                            (memex-api-session "caea32e0" "/tmp/a.jsonl"
                                               cb :errback eb))))
               (failure (plist-get exchange :error)))
          (should-not (plist-get exchange :payload))
          (should (eq (car failure) 'memex-rpc-error))
          (should (equal (plist-get (cdr failure) :message) "no session caea32e0")))
        (let* ((stub (memex-api-tests--stub
                      (concat "cat > /dev/null\n"
                              "printf '%s\\n' 'Error: missing field `query`' >&2\n"
                              "exit 1\n")))
               (exchange (memex-api-tests--exchange-with
                          stub
                          (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
               (failure (plist-get exchange :error)))
          (should-not (plist-get exchange :payload))
          (should (eq (car failure) 'memex-transport-error))
          (should (equal (plist-get (cdr failure) :exit-status) 1))
          (should (equal (plist-get (cdr failure) :stderr)
                         "Error: missing field `query`\n")))
        (let* ((stub (memex-api-tests--stub
                      (format (concat "case \"$1\" in\n"
                                      "  --version) printf 'memex 7.7.7\\n' ;;\n"
                                      "  *) cat > /dev/null; printf '%%s' %s ;;\n"
                                      "esac\n")
                              (shell-quote-argument
                               (concat "{\"protocol\":2,\"response\":"
                                       "{\"kind\":\"records\",\"records\":[]}}")))))
               (exchange (memex-api-tests--exchange-with
                          stub
                          (lambda (cb eb) (memex-api-search "emacs" cb :errback eb))))
               (failure (plist-get exchange :error)))
          (should-not (plist-get exchange :payload))
          (should (eq (car failure) 'memex-protocol-error))
          (should (equal (plist-get (cdr failure) :expected) 1))
          (should (equal (plist-get (cdr failure) :received) 2))
          (should (string-match-p "7\\.7\\.7" (plist-get (cdr failure) :version)))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-hands-back-the-process-memex-cancel-rpc-takes ()
  (unwind-protect
      (let* ((memex-executable (memex-api-tests--stub "sleep 30\n"))
             (process (memex-api-search "emacs" #'ignore :errback #'ignore)))
        (should (processp process))
        (should (process-live-p process))
        (should (memex-cancel-rpc process))
        (should-not (process-live-p process))
        (should-not (memex-cancel-rpc process)))
    (memex-api-tests--cleanup)))

(defun memex-api-tests--mentions-p (form symbols)
  "Return non-nil when any of SYMBOLS appears in FORM."
  (cond ((memq form symbols) t)
        ((consp form) (or (memex-api-tests--mentions-p (car form) symbols)
                          (memex-api-tests--mentions-p (cdr form) symbols)))))

(defun memex-api-tests--forms (path)
  "Return every top-level form of the Lisp file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest memex-api-never-reaches-into-the-ui-layer ()
  (let ((path (locate-library "memex-api")))
    (should path)
    (when (string-suffix-p ".elc" path)
      (setq path (substring path 0 -1)))
    (should (memex-api-tests--mentions-p (memex-api-tests--forms path)
                                         '(memex-rpc)))
    (should-not (memex-api-tests--mentions-p
                 (memex-api-tests--forms path)
                 '(interactive called-interactively-p
                               completing-read completing-read-multiple
                               read-string read-from-minibuffer y-or-n-p
                               display-buffer pop-to-buffer switch-to-buffer
                               with-output-to-temp-buffer))))
  (unwind-protect
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (error "The API layer must not prompt")))
                ((symbol-function 'display-buffer)
                 (lambda (&rest _) (error "The API layer must not display a buffer"))))
        (let ((exchange (memex-api-tests--exchange
                         (memex-api-tests--records-response)
                         (lambda (cb eb) (memex-api-search "emacs" cb :errback eb)))))
          (should-not (plist-get exchange :error))
          (should (equal (length (plist-get exchange :payload)) 2))))
    (memex-api-tests--cleanup)))

(ert-deftest memex-api-live-requests-are-accepted-by-memex ()
  (unless (executable-find "memex")
    (ert-skip "memex is not installed"))
  (let ((memex-executable "memex"))
    (pcase-dolist (`(,label ,invoke)
                   (list
                    (list "ping" (lambda (cb eb) (memex-api-ping cb :errback eb)))
                    (list "search"
                          (lambda (cb eb)
                            (memex-api-search "emacs" cb :limit 1 :errback eb)))
                    (list "recent"
                          (lambda (cb eb) (memex-api-recent cb :limit 1 :errback eb)))
                    (list "session"
                          (lambda (cb eb)
                            (memex-api-session "no-such-session" "/tmp/nope.jsonl"
                                               cb :errback eb)))
                    (list "session_page"
                          (lambda (cb eb)
                            (memex-api-session-page "no-such-session" "/tmp/nope.jsonl"
                                                    cb :limit 10 :errback eb)))
                    (list "session_batch"
                          (lambda (cb eb)
                            (memex-api-session-batch
                             (list (list :session-id "no-such-session"
                                         :source-path "/tmp/nope.jsonl" :limit 10))
                             cb :errback eb)))
                    (list "session_activity"
                          (lambda (cb eb)
                            (memex-api-session-activity cb :errback eb)))))
      (let ((payload 'pending)
            (failure nil))
        (funcall invoke
                 (lambda (value) (setq payload value))
                 (lambda (err) (setq failure err)))
        (should (memex-api-tests--wait
                 (lambda () (or failure (not (eq payload 'pending)))) 60.0))
        (should (equal (list label failure) (list label nil)))
        (should-not (eq payload 'pending))))))

(provide 'memex-api-tests)
;;; memex-api-tests.el ends here
