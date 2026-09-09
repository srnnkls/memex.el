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
;;
;; A search answers with sessions, one candidate to a session, the way
;; memex's own `SessionSummary' does; `memex-search-group-by-session'
;; nil is the record-level path the older tests here bind.  A selection
;; opens the session it stands for, so a test that carries one through
;; binds both openers away from the real ones.

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
(declare-function memex-herdr-open-session "memex-herdr")

(defvar consult--completion-candidate-hook)

(defvar memex-search--consult-noted)
(defvar memex-search-group-by-session)
(defvar memex-search-snippet-width)

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

(defvar memex-search-tests--opened nil
  "The sessions an opener was asked to open, newest first.")

(defun memex-search-tests--sleeper ()
  "Return a live process standing in for a memex request."
  (make-process :name "memex-search-stub" :command (list "sleep" "30")
                :noquery t :connection-type 'pipe))

(defun memex-search-tests--stub-search ()
  "Return a stand-in for `memex-api-search' recording what it is handed.
Each call records a plist of `:query', `:mode', `:limit', `:callback'
and `:process' and answers with a live process, so a request that was
superseded can be told from one that is still running."
  (lambda (query callback &rest arguments)
    (let ((process (memex-search-tests--sleeper)))
      (push (list :query query :mode (plist-get arguments :mode)
                  :limit (plist-get arguments :limit)
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

(defun memex-search-tests--limit (request)
  "Return the number of matches REQUEST asked memex for.
A request that names no limit is answered with `memex-api-search's own
default of 20, which is the cap that then reaches the index."
  (or (plist-get request :limit) 20))

(defun memex-search-tests--opener (name)
  "Return a stand-in for an opener recording the session it is handed.
NAME is the opener it stands for, `herdr' or `view'."
  (lambda (session-id source-path &optional doc-id &rest _)
    (push (list :opener name :session-id session-id
                :source-path source-path :doc-id doc-id)
          memex-search-tests--opened)
    nil))

(defun memex-search-tests--opened ()
  "Return the sessions an opener was asked to open, oldest first."
  (reverse memex-search-tests--opened))

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

(defun memex-search-tests--deliver (request hits)
  "Hand the already scored HITS to REQUEST's callback."
  (funcall (plist-get request :callback) hits))

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
        memex-search-tests--cancelled nil
        memex-search-tests--opened nil))

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

(cl-defun memex-search-tests--session-record (&key doc-id ts session-id text
                                                   source-path)
  "Return a record of one session's hit.
DOC-ID, TS, SESSION-ID, TEXT and SOURCE-PATH are its wire fields."
  (list (cons 'source "codex")
        (cons 'doc_id doc-id)
        (cons 'ts ts)
        (cons 'project "memex.el")
        (cons 'session_id session-id)
        (cons 'role "assistant")
        (cons 'text text)
        (cons 'source_path source-path)))

(defun memex-search-tests--session-matches ()
  "Return scored hits of three sessions, two of them one `session_id'.
A session is a `session_id' under a `source_path': memex's own server
filters a session's records by both, so one id spanning two paths is two
conversations and not one.  The alpha id spans two paths here, and the
four hits under the first of them carry the rest of the contract: two
tie for the best score and the second of the two is neither the group's
leading hit nor its newest, so the tie tells memex's `>=' from a `>',
the newest hit is where `last_ts' has to come from, and the leading hit
is what a summary built from the head of the list would take."
  (list (list 7.0 (memex-search-tests--session-record
                   :doc-id 103 :ts 1787140243200 :session-id "s-alpha"
                   :text "top alpha" :source-path "/tmp/alpha-3.jsonl"))
        (list 7.0 (memex-search-tests--session-record
                   :doc-id 105 :ts 1787140243250 :session-id "s-alpha"
                   :text "later tie alpha" :source-path "/tmp/alpha-3.jsonl"))
        (list 6.0 (memex-search-tests--session-record
                   :doc-id 104 :ts 1787140243300 :session-id "s-alpha"
                   :text "split alpha" :source-path "/tmp/alpha-4.jsonl"))
        (list 5.0 (memex-search-tests--session-record
                   :doc-id 102 :ts 1787140243500 :session-id "s-alpha"
                   :text "newest alpha" :source-path "/tmp/alpha-3.jsonl"))
        (list 4.0 (memex-search-tests--session-record
                   :doc-id 201 :ts 1787140243400 :session-id "s-beta"
                   :text "only beta" :source-path "/tmp/beta.jsonl"))
        (list 3.0 (memex-search-tests--session-record
                   :doc-id 101 :ts 1787140243100 :session-id "s-alpha"
                   :text "oldest alpha" :source-path "/tmp/alpha-3.jsonl"))))

(defconst memex-search-tests--sprawl
  (concat "  lead\n\tin  " (make-string 300 ?x))
  "Text of a hit too long and too ragged to stand in a candidate line.
Its whitespace runs, one of them leading, collapse to a single space
each, and what is left is longer than any snippet width the tests use.")

(defun memex-search-tests--sprawling-matches ()
  "Return one scored hit whose text is `memex-search-tests--sprawl'."
  (list (list 6.0 (memex-search-tests--session-record
                   :doc-id 301 :ts 1787140243600 :session-id "s-sprawl"
                   :text memex-search-tests--sprawl
                   :source-path "/tmp/sprawl.jsonl"))))

(defconst memex-search-tests--haystack
  (concat "Script completed Wall time 0.4 seconds Output: "
          (string-join (make-list 20 "padding") " ")
          " leading-context EMBARK-general-map trailing-context "
          (string-join (make-list 20 "filler") " ")
          " define-keymap here "
          (string-join (make-list 20 "tail") " "))
  "Text of a tool hit whose matches sit far past its opening boilerplate.
It opens the way every `codex exec' record in the index does, and the
first 160 characters of it are padding: a snippet taken from character
zero reaches neither `EMBARK-general-map' at 223 nor `define-keymap' at
399.  The two are 183 characters apart, so a 160-column excerpt holds
one of them and never both, and the earlier of the two is spelled in
another case than any query would be.")

(defun memex-search-tests--haystack-matches ()
  "Return one scored hit whose text is `memex-search-tests--haystack'."
  (list (list 6.0 (memex-search-tests--session-record
                   :doc-id 401 :ts 1787140243700 :session-id "s-haystack"
                   :text memex-search-tests--haystack
                   :source-path "/tmp/haystack.jsonl"))))

(cl-defun memex-search-tests--tool-record (&key doc-id session-id text tool-output
                                                source-path)
  "Return a tool hit of one session, the shape most of the index has.
DOC-ID, SESSION-ID, TEXT, TOOL-OUTPUT and SOURCE-PATH are its wire
fields.  Its `tool_name' is the same on every one of them, so what an
annotation makes of `tool_output' is never read off the name."
  (list (cons 'source "codex")
        (cons 'doc_id doc-id)
        (cons 'ts 1787140243700)
        (cons 'project "feat-emacs-client")
        (cons 'session_id session-id)
        (cons 'role "tool_result")
        (cons 'text text)
        (cons 'tool_name "exec")
        (cons 'tool_output tool-output)
        (cons 'source_path source-path)))

(defconst memex-search-tests--echoed-output
  (concat "Script completed Wall time 0.4 seconds Output: "
          "---RESULT 1--- ran the command and printed nothing anyone reads "
          "---RESULT 2--- the preamble every codex exec record opens with "
          (make-string 400 ?z)
          " EMBARK-general-map "
          (make-string 400 ?z))
  "Output a tool record carries as both its `text' and its `tool_output'.
memex fills a tool record's `text' from the output it reports, so the
two agree on every tool record in the index.  The match sits past a head
of `codex exec' boilerplate longer than a snippet, and the filler on
either side of it appears nowhere in that head: the window centred on
the match and the window taken from character zero share none of their
content, so nothing an annotation compares against the rendered label
can tell that the two are the same field.")

(defun memex-search-tests--echoing-matches ()
  "Return scored hits of two tool sessions, the first of them echoing itself.
The first carries `memex-search-tests--echoed-output' as both its
`text' and its `tool_output', which is the shape the corpus is four
fifths made of.  The second reports an exit line its `text' does not, so
its `tool_output' is content no label showed and is worth annotating."
  (list (list 6.0 (memex-search-tests--tool-record
                   :doc-id 501 :session-id "s-echo"
                   :text memex-search-tests--echoed-output
                   :tool-output memex-search-tests--echoed-output
                   :source-path "/tmp/echo.jsonl"))
        (list 5.0 (memex-search-tests--tool-record
                   :doc-id 502 :session-id "s-exit"
                   :text "the EMBARK-general-map binding landed"
                   :tool-output "exit status 0 wrote 3 files"
                   :source-path "/tmp/exit.jsonl"))))

(defun memex-search-tests--published (mode query hits)
  "Return the candidates a MODE search for QUERY publishes for HITS.
They are taken off the async function's own publication, which is where
a completion UI and every consumer of a candidate find them."
  (memex-search-tests--drive mode 'setup query)
  (setq memex-search-tests--actions nil)
  (memex-search-tests--deliver (car memex-search-tests--requests) hits)
  (nth 1 (memex-search-tests--actions)))

(defun memex-search-tests--rows-for (mode query hits)
  "Return the session rows a MODE search for QUERY draws from HITS."
  (mapcar (lambda (candidate)
            (cons (substring-no-properties candidate)
                  (memex-completion-record-of candidate)))
          (memex-search-tests--published mode query hits)))

(defun memex-search-tests--rows (hits)
  "Return the session rows a grouped lexical search draws from HITS."
  (memex-search-tests--rows-for 'lexical "alfa" hits))

(defun memex-search-tests--snippet (mode query)
  "Return the snippet a MODE search for QUERY puts on the haystack hit."
  (memex-search-tests--field
   (car (memex-search-tests--rows-for
         mode query (memex-search-tests--haystack-matches)))
   'text))

(defun memex-search-tests--field (row field)
  "Return FIELD of the session record ROW carries."
  (alist-get field (cdr row)))

(defun memex-search-tests--keys (rows)
  "Return the session each of ROWS stands for, id paired with path."
  (mapcar (lambda (row)
            (cons (memex-search-tests--field row 'session_id)
                  (memex-search-tests--field row 'source_path)))
          rows))

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
        (let* ((memex-search-group-by-session nil)
               (records (memex-search-tests--matches))
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
        (let* ((memex-search-group-by-session nil)
               (records (memex-search-tests--matches))
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
            (memex-search-group-by-session nil)
            (supplied 'not-called)
            (messages nil))
        (should-not (featurep 'consult))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "alfa")))
                  ((symbol-function 'memex-completion--read)
                   (lambda (_prompt candidates &rest _)
                     (setq supplied (mapcar #'memex-completion-record-of candidates))
                     (car candidates)))
                  ((symbol-function 'memex-herdr-open-session)
                   (memex-search-tests--opener 'herdr))
                  ((symbol-function 'memex-view-session)
                   (memex-search-tests--opener 'view))
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
            (memex-search-group-by-session nil)
            (supplied 'not-called))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "")))
                  ((symbol-function 'memex-completion--read)
                   (lambda (_prompt candidates &rest _)
                     (setq supplied (mapcar #'memex-completion-record-of candidates))
                     (car candidates)))
                  ((symbol-function 'memex-herdr-open-session)
                   (memex-search-tests--opener 'herdr))
                  ((symbol-function 'memex-view-session)
                   (memex-search-tests--opener 'view))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (memex-search 'hybrid "alfa")
          (let ((request (car (memex-search-tests--requests))))
            (should (equal (plist-get request :query) "alfa"))
            (should (eq (memex-search-tests--mode request) 'hybrid)))
          (should (equal supplied records))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-answers-with-one-candidate-to-a-session ()
  "A search groups its hits by session, the way memex's own UIs do.
Grouping is what the command does unasked, so the default is read off
the option rather than bound here.  The corpus is four fifths tool
traffic and a record-level answer is that many rows of it; a session
carries its hit count and the newest `ts' of the hits it stands for.

A session is a `session_id' under a `source_path', both of them, which
is the key memex's server filters a session's records by
\(src/machine.rs:1517), the key the recent-window selectors already
deduplicate on and the key the viewer registers a buffer under.  One id
under two paths is two conversations, and merging them would open
whichever path the better-scoring hit happened to carry."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (should (boundp 'memex-search-group-by-session))
        (should (eq (default-value 'memex-search-group-by-session) t))
        (let ((rows (memex-search-tests--rows
                     (memex-search-tests--session-matches))))
          (should (equal (length rows) 3))
          (should (equal (memex-search-tests--keys rows)
                         '(("s-alpha" . "/tmp/alpha-3.jsonl")
                           ("s-alpha" . "/tmp/alpha-4.jsonl")
                           ("s-beta" . "/tmp/beta.jsonl"))))
          (should (equal (mapcar (lambda (row)
                                   (memex-search-tests--field row 'hit_count))
                                 rows)
                         '(4 1 1)))
          (should (equal (mapcar (lambda (row)
                                   (memex-search-tests--field row 'ts))
                                 rows)
                         '(1787140243500 1787140243300 1787140243400)))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-takes-a-session-from-its-best-scoring-hit ()
  "The snippet, the path and the record a session opens at are the top hit's.
Ties go to the later hit, which is what memex's `>=' does and a `>'
does not, and the label is the project and that snippet with nothing of
the record's own between them.  The alpha group's tie is between its
leading hit and one further down, and neither of them is its newest, so
a summary taken from the head of the list, from the newest hit, or from
the first of two tied scores each answers with a different record."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((memex-search-session-hit 'best)
               (rows (memex-search-tests--rows
                      (memex-search-tests--session-matches)))
               (alpha (car rows))
               (label (car alpha)))
          (should (equal (memex-search-tests--field alpha 'doc_id) 105))
          (should (equal (memex-search-tests--field alpha 'source_path)
                         "/tmp/alpha-3.jsonl"))
          (should (equal (memex-search-tests--field alpha 'text)
                         "later tie alpha"))
          (should (string-match-p "memex\\.el" label))
          (should (string-match-p "later tie alpha" label))
          (should-not (string-match-p "top alpha" label))
          (should-not (string-match-p "newest alpha" label))
          (should-not (string-match-p "assistant" label))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-summarizes-a-snippet-to-the-configured-width ()
  "A snippet is memex's `summarize' at `memex-search-snippet-width'.
Whitespace runs collapse to one space and a leading run is dropped; a
text longer than the width keeps its first width-less-three characters
and ends in an ellipsis of three periods, so the snippet is exactly the
width and never the 64 columns a completion label is cut to."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (should (boundp 'memex-search-snippet-width))
        (should (equal (default-value 'memex-search-snippet-width) 160))
        (let ((wide (memex-search-tests--field
                     (car (memex-search-tests--rows
                           (memex-search-tests--sprawling-matches)))
                     'text)))
          (should (equal wide (concat "lead in " (make-string 149 ?x) "...")))
          (should (equal (length wide) 160)))
        (memex-search-tests--cleanup)
        (let* ((memex-search-snippet-width 12)
               (narrow (memex-search-tests--field
                        (car (memex-search-tests--rows
                              (memex-search-tests--sprawling-matches)))
                        'text)))
          (should (equal narrow "lead in x..."))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-centres-the-snippet-on-the-match ()
  "A snippet is the window around the match, not the head of the record.
Every `codex exec' record opens with the same line of shell boilerplate,
so a snippet taken from character zero reads alike for unrelated hits
and truncates the match away.  memex's own CLI reports the match with
context on either side of it \(src/cli.rs:4558\), and that is the
context a row carries here.

The query has two terms and the later of them matches first and in
another case, so an excerpt built around the query's first term, or one
that matches case-sensitively, takes the wrong window or none at all.
Context on both sides is what tells a centred window from one that
begins at the match, and the leading elision is what says the excerpt
did not start where the record did.

A hit no term literally appears in has no window to centre on, which is
what a semantic or a hybrid search answers with, and it keeps the
head-of-text snippet whole and unelided at its front."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let ((excerpt (memex-search-tests--snippet 'lexical "keymap embark")))
          (should (string-match-p "EMBARK-general-map" excerpt))
          (should (string-match-p "leading-context" excerpt))
          (should (string-match-p "trailing-context" excerpt))
          (should-not (string-match-p "keymap" excerpt))
          (should-not (string-match-p "Script completed" excerpt))
          (should (string-prefix-p "..." excerpt))
          (should (equal (length excerpt) 160)))
        (memex-search-tests--cleanup)
        (let* ((memex-search-snippet-width 80)
               (narrow (memex-search-tests--snippet 'lexical "keymap embark")))
          (should (string-match-p "EMBARK-general-map" narrow))
          (should (equal (length narrow) 80)))
        (memex-search-tests--cleanup)
        (let ((unmatched (memex-search-tests--snippet 'semantic "semantic drift")))
          (should (equal unmatched
                         (concat (substring memex-search-tests--haystack 0 157)
                                 "...")))
          (should-not (string-prefix-p "..." unmatched))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-over-fetches-the-hits-it-is-going-to-group ()
  "Grouping asks memex for more matches than a page of sessions needs.
A limit spent on one session's tool traffic is a page of one session,
so a grouped search over-fetches the way memex's own CLI does before it
post-filters."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((flat (let ((memex-search-group-by-session nil))
                       (memex-search-tests--drive 'lexical 'setup "alfa")
                       (memex-search-tests--limit
                        (car memex-search-tests--requests))))
               (grouped (let ((memex-search-group-by-session t))
                          (memex-search-tests--drive 'lexical 'setup "alfa")
                          (memex-search-tests--limit
                           (car memex-search-tests--requests)))))
          (should (> flat 0))
          (should (equal grouped (max (* flat 5) (+ flat 10))))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-opens-the-session-of-what-was-selected ()
  "A selection opens its session, and still answers with the record.
The herdr bridge takes it when that is loaded and the viewer takes it
when it is not, which is the same fork `memex-org-follow' takes."
  (unwind-protect
      (let ((records (memex-search-tests--matches))
            (memex-search--consult-noted t)
            (memex-search-group-by-session nil))
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "alfa")))
                  ((symbol-function 'memex-completion--read)
                   (lambda (_prompt candidates &rest _) (car candidates)))
                  ((symbol-function 'message) (lambda (&rest _) nil))
                  ((symbol-function 'memex-herdr-open-session)
                   (memex-search-tests--opener 'herdr))
                  ((symbol-function 'memex-view-session)
                   (memex-search-tests--opener 'view)))
          (let ((value (memex-search)))
            (should (equal (memex-search-tests--opened)
                           (list (list :opener 'herdr
                                       :session-id "s-search"
                                       :source-path "/tmp/search.jsonl"
                                       :doc-id 9003))))
            (should (equal value (car records)))))
        (setq memex-search-tests--opened nil)
        (cl-letf (((symbol-function 'memex-api-search)
                   (memex-search-tests--answering-stub records))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "alfa")))
                  ((symbol-function 'memex-completion--read)
                   (lambda (_prompt candidates &rest _) (car candidates)))
                  ((symbol-function 'message) (lambda (&rest _) nil))
                  ((symbol-function 'memex-herdr-open-session) nil)
                  ((symbol-function 'memex-view-session)
                   (memex-search-tests--opener 'view)))
          (memex-search)
          (should (equal (memex-search-tests--opened)
                         (list (list :opener 'view
                                     :session-id "s-search"
                                     :source-path "/tmp/search.jsonl"
                                     :doc-id 9003))))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-annotates-a-row-without-reprinting-what-it-snippeted ()
  "Search annotations contain hit counts, never another output excerpt."
  (unwind-protect
      (cl-letf (((symbol-function 'memex-api-search)
                 (memex-search-tests--stub-search)))
        (let* ((candidates (memex-search-tests--published
                            'lexical "embark"
                            (memex-search-tests--echoing-matches)))
               (echoed (nth 0 candidates))
               (reported (nth 1 candidates))
               (label (substring-no-properties echoed))
               (annotation (memex-completion-annotate echoed)))
          (should (equal (length candidates) 2))
          (should (string-match-p "\\.\\.\\." label))
          (should-not (string-match-p "Script completed" label))
          (should-not (string-match-p "Script completed" annotation))
          (should-not (string-match-p "RESULT 1" annotation))
          (should (string-match-p "\\bexec\\b" label))
          (should (string-match-p "codex" label))
          (should (equal annotation "  1 hit"))
          (should-not (string-match-p "exit status"
                                      (memex-completion-annotate reported)))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-opens-the-newest-returned-hit ()
  (let* ((memex-search-session-hit 'newest)
         (candidates (memex-search--candidates
                      (memex-search-tests--session-matches) "alpha"))
         (record (memex-completion-record-of (car candidates)))
         opened)
    (cl-letf (((symbol-function 'memex-herdr-open-session) nil)
              ((symbol-function 'memex-view-session)
               (lambda (_session _path doc-id) (setq opened doc-id))))
      (memex-search--open record))
    (should (equal opened 102))
    (should (equal (alist-get 'text record) "newest alpha"))))

(ert-deftest memex-search-rows-keep-the-match-and-drop-escaped-lines ()
  (let* ((text (concat "\e[31m" (make-string 200 ?x)
                       "\\nLMDB\\t" (make-string 200 ?y) "\e[0m"))
         (record (memex-search-tests--record 55 "long-project-name" text)))
    (dolist (grouped '(nil t))
      (let* ((memex-search-group-by-session grouped)
             (row (car (memex-search--candidates (list (list 1 record)) "lmdb"))))
        (should (string-match-p "LMDB" row))
        (should-not (string-match-p "[\n\t\e]\\|\\\\[nrt]" row))
        (should (eq 'match (get-text-property (string-match "LMDB" row) 'face row)))
        (should (equal (alist-get 'doc_id (memex-completion-record-of row)) 55))))
    (should (equal (alist-get 'text record) text))))

(ert-deftest memex-search-excerpts-use-display-columns ()
  (let* ((memex-search-snippet-width 30)
         (text (concat (make-string 100 ?界) " LMDB " (make-string 100 ?界)))
         (snippet (memex-search--summarize text "lmdb")))
    (should (string-match-p "LMDB" snippet))
    (should (<= (string-width snippet) 30))))

(ert-deftest memex-search-explicit-commands-choose-their-view ()
  (let ((memex-search-group-by-session t)
        calls)
    (cl-letf (((symbol-function 'memex-search--run)
               (lambda (&rest args) (push args calls))))
      (memex-search-messages 'hybrid "lmdb")
      (memex-search-sessions 'semantic "redis")
      (memex-search 'lexical "sqlite"))
    (should (equal (reverse calls)
                   '((hybrid "lmdb" nil nil)
                     (semantic "redis" t nil)
                     (lexical "sqlite" t nil))))
    (should memex-search-group-by-session)))

(ert-deftest memex-search-toggle-preserves-query-mode-and-scope ()
  (let ((memex-search--mode 'hybrid)
        (memex-search-group-by-session t)
        (memex-search--scope '((:source "codex" :session-id "s" :source-path "/s")))
        scheduled)
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t))
              ((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "#lmdb"))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function &rest args)
                 (setq scheduled (cons function args))))
              ((symbol-function 'abort-recursive-edit) #'ignore))
      (memex-search-toggle-grouping))
    (should (equal scheduled
                   `(memex-search--run hybrid "#lmdb" nil ,memex-search--scope)))))

(ert-deftest memex-search-drilldown-uses-the-highlighted-session ()
  (let* ((record (memex-search-tests--record 55 "memex" "lmdb"))
         (candidate (propertize "hit" 'memex-record record))
         (memex-search--mode 'semantic)
         (memex-search--static-query nil)
         (minibuffer-completion-table (lambda (&rest _) (list candidate)))
         (consult--completion-candidate-hook (list (lambda () "hit")))
         called)
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t))
              ((symbol-function 'memex-search--restart)
               (lambda (&rest args) (setq called args))))
      (memex-search-in-selected-session))
    (should (equal called
                   '(semantic nil ((:source "codex" :session-id "s-search"
                                    :source-path "/tmp/search.jsonl")))))))

(ert-deftest memex-search-scope-is-sent-only-when-present ()
  (unwind-protect
      (dolist (scope '(nil ((:source "codex" :session-id "s" :source-path "/s"))))
        (let ((memex-search--scope scope)
              arguments)
          (cl-letf (((symbol-function 'memex-api-search)
                     (lambda (_query callback &rest args)
                       (setq arguments args)
                       (funcall callback nil))))
            (memex-search-tests--drive 'lexical "lmdb")
            (should (equal (plist-get arguments :session-scope) scope))
            (should (eq (not (null (plist-member arguments :session-scope)))
                        (not (null scope))))
            (setq arguments nil)
            (memex-search--fetch "lmdb" 'lexical)
            (should (equal (plist-get arguments :session-scope) scope))
            (should (eq (not (null (plist-member arguments :session-scope)))
                        (not (null scope)))))))
    (memex-search-tests--cleanup)))

(ert-deftest memex-search-in-sessions-narrows-to-the-scope-it-was-given ()
  (let ((scope '((:source "claude" :session-id "s1" :source-path "/tmp/s1.jsonl")
                 (:source "codex" :session-id "s2" :source-path "/tmp/s2.jsonl")))
        called)
    (cl-letf (((symbol-function 'memex-search--run)
               (lambda (&rest args) (setq called args))))
      (memex-search-in-sessions scope)
      (should (equal called (list 'lexical nil nil scope)))
      (memex-search-in-sessions nil 'semantic "kv")
      (should (equal called (list 'semantic "kv" nil nil))))))

(provide 'memex-search-tests)
;;; memex-search-tests.el ends here
