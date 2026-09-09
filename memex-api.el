;;; memex-api.el --- Wrappers for memex's RPC operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; One function per operation of memex's RPC surface.  Each takes its
;; required arguments positionally, then a CALLBACK, then a keyword per
;; optional field; `:errback' receives `memex-rpc''s error object
;; unchanged.  CALLBACK receives the data the response payload carries:
;; the one data field the operation answers with, unwrapped, or the
;; payload without its `kind' tag when the operation answers with
;; several.  Each returns the process running the request, which
;; `memex-cancel-rpc' accepts.
;;
;; This layer is independent of the UI: it never prompts, never displays
;; a buffer and assumes no interactive context.

;;; Code:

(require 'cl-lib)
(require 'memex-core)

(defconst memex-api-max-session-page-size 500
  "Largest session page memex serves in one request.")

(defconst memex-api-max-session-batch-size 32
  "Largest number of session pages memex serves in one batch.")

(define-error 'memex-api-limit-error "memex request exceeds a cap"
  '(memex-error user-error))

(defun memex-api--wire (value)
  "Return VALUE as memex spells it on the wire.
A symbol travels as its name; t and the keywords :false and :null pass
through so they serialize as JSON true, false and null."
  (if (and value (symbolp value) (not (eq value t)) (not (keywordp value)))
      (symbol-name value)
    value))

(defun memex-api--fields (&rest pairs)
  "Return PAIRS as request fields, dropping every pair whose value is nil.
A field the caller omitted is absent from the JSON, never null."
  (delq nil (mapcar (lambda (pair)
                      (when (cdr pair)
                        (cons (car pair) (memex-api--wire (cdr pair)))))
                    pairs)))

(defun memex-api--tuples (keys properties)
  "Return the plists in KEYS as a vector of tuples, empty when KEYS is.
Each tuple carries the values of PROPERTIES positionally, in the order
given, which is how memex serializes its session keys."
  (vconcat (mapcar (lambda (key)
                     (vconcat
                      (mapcar (lambda (property)
                                (memex-api--wire (plist-get key property)))
                              properties)))
                   keys)))

(defun memex-api--session-key-tuples (keys)
  "Return the plists in KEYS as memex's session key tuples.
Each plist supplies :source and :session-id, in that order on the wire."
  (memex-api--tuples keys '(:source :session-id)))

(defun memex-api--machine-session-key-tuples (keys)
  "Return the plists in KEYS as memex's machine session key tuples.
Each plist supplies :machine, :source and :session-id, in that order on
the wire."
  (memex-api--tuples keys '(:machine :source :session-id)))

(defun memex-api--session-scope (keys)
  "Return the plists in KEYS as a vector of session scope objects.
Each plist supplies :source, :session-id and :source-path.  An empty
KEYS is an empty vector, restricting the search to no session at all."
  (vconcat (mapcar (lambda (key)
                     (list (cons 'source
                                 (memex-api--wire (plist-get key :source)))
                           (cons 'session_id (plist-get key :session-id))
                           (cons 'source_path (plist-get key :source-path))))
                   keys)))

(defun memex-api--page-request (session-id source-path offset limit)
  "Return the page request for SESSION-ID at SOURCE-PATH.
OFFSET defaults to the start of the session and LIMIT to
`memex-api-max-session-page-size'.  Signals `memex-api-limit-error'
when LIMIT is outside what memex accepts, before any request is
started."
  (let ((limit (or limit memex-api-max-session-page-size)))
    (unless (and (integerp limit)
                 (> limit 0)
                 (<= limit memex-api-max-session-page-size))
      (signal 'memex-api-limit-error
              (list (format "Session page limit must be 1 to %d, not %s"
                            memex-api-max-session-page-size limit))))
    (list (cons 'session_id session-id)
          (cons 'source_path source-path)
          (cons 'offset (or offset 0))
          (cons 'limit limit))))

(defun memex-api--data (payload field)
  "Return the data PAYLOAD carries beside its `kind' tag.
FIELD is the payload field the operation answers with, unwrapped to its
value; nil returns the payload whole, its `kind' tag dropped."
  (if field
      (alist-get field payload)
    (assq-delete-all 'kind payload)))

(defun memex-api--call (op field fields callback errback)
  "Run OP with FIELDS and hand the data of its payload FIELD to CALLBACK.
ERRBACK receives `memex-rpc''s error object unchanged.  Returns the
process running the request, which `memex-cancel-rpc' accepts."
  (memex-rpc op fields
             (lambda (payload)
               (funcall callback (memex-api--data payload field)))
             errback))

(cl-defun memex-api-ping (callback &key errback)
  "Ask memex for its version and hand the version string to CALLBACK.
ERRBACK receives the error object instead when the request fails."
  (memex-api--call "ping" 'version nil callback errback))

(cl-defun memex-api-search (query callback
                                  &key errback limit mode project role tool
                                  session-id
                                  (session-scope nil session-scope-supplied)
                                  cwd source since until
                                  min-score project-grouping include-reasoning
                                  recency-weight recency-half-life-days)
  "Search the index for QUERY and hand the matches to CALLBACK.
CALLBACK receives a list of (SCORE RECORD) pairs, best match first.
ERRBACK receives the error object instead when the request fails.

LIMIT caps the number of matches (20).  MODE is `lexical', `semantic'
or `hybrid' (`lexical').  RECENCY-WEIGHT (1.0) and
RECENCY-HALF-LIFE-DAYS (30.0) shape the recency boost.

PROJECT, ROLE, TOOL, SESSION-ID, CWD, SOURCE, SINCE, UNTIL, MIN-SCORE
and PROJECT-GROUPING narrow the result set, SESSION-SCOPE to a list of
plists of :source, :session-id and :source-path.  SINCE and UNTIL are
epoch milliseconds.  Non-nil INCLUDE-REASONING keeps reasoning records.
An omitted filter is left out of the request; an empty SESSION-SCOPE
matches no session rather than every one."
  (memex-api--call
   "search" 'records
   (list (cons 'spec
               (memex-api--fields
                (cons 'query query)
                (cons 'limit (or limit 20))
                (cons 'mode (or mode 'lexical))
                (cons 'recency_weight (or recency-weight 1.0))
                (cons 'recency_half_life_days (or recency-half-life-days 30.0))
                (cons 'project project)
                (cons 'role role)
                (cons 'tool tool)
                (cons 'session_id session-id)
                (cons 'session_scope
                      (when session-scope-supplied
                        (memex-api--session-scope session-scope)))
                (cons 'cwd cwd)
                (cons 'source source)
                (cons 'since since)
                (cons 'until until)
                (cons 'min_score min-score)
                (cons 'project_grouping project-grouping)
                (cons 'include_reasoning include-reasoning))))
   callback errback))

(cl-defun memex-api-recent (callback &key errback limit project-grouping)
  "Hand the most recent records to CALLBACK.
CALLBACK receives a list of (SCORE RECORD) pairs, newest first.
ERRBACK receives the error object instead when the request fails.
LIMIT caps the number of records (20).  PROJECT-GROUPING is `flat' or
`repository'; omitted, it is left out of the request."
  (memex-api--call "recent" 'records
                   (memex-api--fields
                    (cons 'limit (or limit 20))
                    (cons 'project_grouping project-grouping))
                   callback errback))

(defconst memex-api-max-sessions 500
  "Most sessions memex lists in one request.")

(cl-defun memex-api-sessions (callback
                              &key errback session-id source-path
                              cwd project source since origin limit)
  "Hand the indexed sessions to CALLBACK, newest first.
CALLBACK receives a list of session alists, each carrying `session_id',
`source_path', `source', `project', `repo_project', `cwd', `git_root',
`started_at', `last_at', `message_count', `label', `conversation_kind'
and `resume_cmd'.  ERRBACK receives the error object instead when the
request fails.

SESSION-ID and SOURCE-PATH match one session exactly.  CWD, PROJECT,
SOURCE and SINCE narrow the window; ORIGIN is `regular', `interactive',
`subagent' or `all' and LIMIT caps the answer at `memex-api-max-sessions'.

Memex answers in its own order, newest activity first, and offers no sort
key, so a caller wanting another order sorts what it gets."
  (let ((limit (or limit 20)))
    (when (> limit memex-api-max-sessions)
      (signal 'memex-api-limit-error
              (list (format "memex lists at most %d sessions, not %d"
                            memex-api-max-sessions limit))))
    (memex-api--call
     "sessions" 'sessions
     (list (cons 'request
                 (append
                  (memex-api--fields (cons 'session_id session-id)
                                     (cons 'source_path source-path)
                                     (cons 'origin origin)
                                     (cons 'limit limit))
                  (list (cons 'cwd (or cwd :null))
                        (cons 'project (or project :null))
                        (cons 'source (or source :null))
                        (cons 'since (or since :null))))))
     callback errback)))

(cl-defun memex-api-session (session-id source-path callback &key errback)
  "Hand the whole session SESSION-ID at SOURCE-PATH to CALLBACK.
CALLBACK receives the session context alist, whose `records' holds
every record of the session and whose `cwd' is nil when memex has none.
ERRBACK receives the error object instead when the request fails."
  (memex-api--call "session" 'context
                   (list (cons 'session_id session-id)
                         (cons 'source_path source-path))
                   callback errback))

(cl-defun memex-api-show (doc-id callback &key errback)
  "Hand the record DOC-ID to CALLBACK.
CALLBACK receives the record alist, its link fields top-level.
ERRBACK receives the error object instead when the request fails."
  (memex-api--call "show" 'record (list (cons 'doc_id doc-id))
                   callback errback))

(cl-defun memex-api-session-page (session-id source-path callback
                                             &key errback offset limit)
  "Hand one page of the session SESSION-ID at SOURCE-PATH to CALLBACK.
CALLBACK receives the page context alist, whose `next_offset' is nil on
the last page.  ERRBACK receives the error object instead when the
request fails.  OFFSET is the first record to return (0) and LIMIT the
number of records (`memex-api-max-session-page-size').  Signals
`memex-api-limit-error' when LIMIT is outside what memex accepts."
  (memex-api--call
   "session_page" 'context
   (list (cons 'request
               (memex-api--page-request session-id source-path offset limit)))
   callback errback))

(cl-defun memex-api-session-batch (requests callback &key errback)
  "Hand one page of each of REQUESTS to CALLBACK.
REQUESTS is a list of at most `memex-api-max-session-batch-size' plists
of :session-id, :source-path, :offset and :limit, the last two
defaulting as in `memex-api-session-page'.  CALLBACK receives a list of
page context alists in request order.  ERRBACK receives the error
object instead when the request fails.  Signals `memex-api-limit-error'
when REQUESTS is too long or one of its limits is outside what memex
accepts."
  (unless (<= (length requests) memex-api-max-session-batch-size)
    (signal 'memex-api-limit-error
            (list (format "Session batch takes at most %d requests, not %d"
                          memex-api-max-session-batch-size (length requests)))))
  (memex-api--call
   "session_batch" 'contexts
   (list (cons 'requests
               (vconcat (mapcar (lambda (request)
                                  (memex-api--page-request
                                   (plist-get request :session-id)
                                   (plist-get request :source-path)
                                   (plist-get request :offset)
                                   (plist-get request :limit)))
                                requests))))
   callback errback))

(cl-defun memex-api-index (callback &key errback)
  "Reindex the configured sources and hand the result to CALLBACK.
CALLBACK receives an alist of `records_added', `records_embedded',
`files_scanned' and `files_skipped'.  ERRBACK receives the error object
instead when the request fails."
  (memex-api--call "index" nil nil callback errback))

(cl-defun memex-api--usage-spec (&key source project project-grouping session-keys
                                      machine-session-keys since-ms until-ms
                                      cost-mode include-events memo-ttl-ms)
  "Return the usage spec selecting the events to report on.
SOURCE, PROJECT, PROJECT-GROUPING, SESSION-KEYS, MACHINE-SESSION-KEYS,
SINCE-MS, UNTIL-MS, COST-MODE, INCLUDE-EVENTS and MEMO-TTL-MS are as
`memex-api-usage' documents them, the two key collections already on
the wire and nil when their keyword was not supplied."
  (memex-api--fields
   (cons 'source source)
   (cons 'project project)
   (cons 'project_grouping (or project-grouping 'flat))
   (cons 'session_keys session-keys)
   (cons 'machine_session_keys machine-session-keys)
   (cons 'since_ms since-ms)
   (cons 'until_ms until-ms)
   (cons 'cost_mode (or cost-mode 'auto))
   (cons 'include_events (if include-events t :false))
   (cons 'memo_ttl_ms (or memo-ttl-ms 0))))

(cl-defun memex-api-usage (callback
                           &key errback source project project-grouping
                           (session-keys nil session-keys-supplied)
                           (machine-session-keys nil machine-session-keys-supplied)
                           since-ms until-ms
                           cost-mode include-events memo-ttl-ms)
  "Hand a token usage report to CALLBACK.
CALLBACK receives the report alist.  ERRBACK receives the error object
instead when the request fails.

SOURCE, PROJECT, SINCE-MS and UNTIL-MS narrow the selection,
SESSION-KEYS to a list of plists of :source and :session-id and
MACHINE-SESSION-KEYS to a list of plists of :machine, :source and
:session-id; supplied empty, either reports on no session at all.
PROJECT-GROUPING is `flat' or `repository' (`flat') and COST-MODE is
`source', `auto' or `reprice' (`auto').  MEMO-TTL-MS bounds how long
memex may reuse a memoized report (0) and non-nil INCLUDE-EVENTS adds
the individual events to the report."
  (memex-api--call
   "usage" 'report
   (list (cons 'spec (memex-api--usage-spec
                      :source source :project project
                      :project-grouping project-grouping
                      :session-keys
                      (when session-keys-supplied
                        (memex-api--session-key-tuples session-keys))
                      :machine-session-keys
                      (when machine-session-keys-supplied
                        (memex-api--machine-session-key-tuples
                         machine-session-keys))
                      :since-ms since-ms :until-ms until-ms
                      :cost-mode cost-mode :include-events include-events
                      :memo-ttl-ms memo-ttl-ms)))
   callback errback))

(cl-defun memex-api-usage-activity (callback
                                    &key errback source project project-grouping
                                    (session-keys nil session-keys-supplied)
                                    (machine-session-keys
                                     nil machine-session-keys-supplied)
                                    since-ms
                                    until-ms cost-mode include-events memo-ttl-ms)
  "Hand the token usage timeline to CALLBACK.
CALLBACK receives an alist of `points', one per machine, source and
timestamp, and `partial', non-nil when a machine did not answer.
ERRBACK receives the error object instead when the request fails.

SOURCE, PROJECT, SINCE-MS and UNTIL-MS narrow the selection,
SESSION-KEYS to a list of plists of :source and :session-id and
MACHINE-SESSION-KEYS to a list of plists of :machine, :source and
:session-id; supplied empty, either reports on no session at all.
PROJECT-GROUPING is `flat' or `repository' (`flat') and COST-MODE is
`source', `auto' or `reprice' (`auto').  MEMO-TTL-MS bounds how long
memex may reuse a memoized timeline (0) and non-nil INCLUDE-EVENTS adds
the individual events to it."
  (memex-api--call
   "usage_activity" nil
   (list (cons 'spec (memex-api--usage-spec
                      :source source :project project
                      :project-grouping project-grouping
                      :session-keys
                      (when session-keys-supplied
                        (memex-api--session-key-tuples session-keys))
                      :machine-session-keys
                      (when machine-session-keys-supplied
                        (memex-api--machine-session-key-tuples
                         machine-session-keys))
                      :since-ms since-ms :until-ms until-ms
                      :cost-mode cost-mode :include-events include-events
                      :memo-ttl-ms memo-ttl-ms)))
   callback errback))

(cl-defun memex-api-session-activity (callback
                                      &key errback source project
                                      project-grouping since-ms until-ms)
  "Hand the session activity timeline to CALLBACK.
CALLBACK receives a list of points, one per machine, source and
timestamp.  ERRBACK receives the error object instead when the request
fails.  SOURCE, PROJECT, SINCE-MS and UNTIL-MS narrow the selection and
PROJECT-GROUPING is `flat' or `repository' (`flat')."
  (memex-api--call
   "session_activity" 'points
   (list (cons 'spec
               (memex-api--fields
                (cons 'source source)
                (cons 'project project)
                (cons 'project_grouping (or project-grouping 'flat))
                (cons 'since_ms since-ms)
                (cons 'until_ms until-ms))))
   callback errback))

(provide 'memex-api)
;;; memex-api.el ends here
