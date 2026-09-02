;;; memex.el --- Search indexed agent conversation history -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Memex indexes the conversation history of coding agents and answers
;; queries over it through its `rpc' command, whose wire is a protocol-1
;; envelope - {"protocol":1,"request":{"op":...}} - and not JSON-RPC.
;; This package speaks that command and puts the index up in the
;; minibuffer and in Emacs buffers.
;;
;;   M-x memex-search        search the index and open what was chosen
;;   M-x memex-view-session  read a session's transcript in a buffer
;;   M-x memex-usage         report memex's token usage
;;
;; `memex-search' answers with one candidate per session, the way memex's
;; own UIs do, and opens the session it was pointed at while still
;; returning the record for a caller that wants one;
;; `memex-search-group-by-session' nil answers with one per record.  With
;; consult installed it searches as you type: each input change
;; supersedes the request in flight and the matches of the one that is
;; still current are published into consult's sink.  Without consult it
;; reads one query, fetches once and puts the matches up through the
;; matching selector.
;;
;; Consult is a soft requirement.  It is required at call time, never at
;; load time, so the package loads and works without it.
;;
;; memex-core.el runs the RPC transport and memex-api.el wraps every
;; operation; memex-completion.el reads a record, a session or a project
;; through `completing-read'; memex-view.el renders a transcript and
;; memex-usage.el the usage report.

;;; Code:

(require 'subr-x)
(require 'memex-core)
(require 'memex-api)
(require 'memex-completion)
(require 'memex-view)
(require 'memex-usage)

(declare-function consult--read "consult" (table &rest options))
(declare-function consult--async-pipeline "consult" (&rest async))
(declare-function consult--async-min-input "consult" (&optional min-input))
(declare-function consult--async-throttle "consult" (&optional throttle debounce))
(declare-function consult--lookup-member "consult" (selected candidates &rest _))
(declare-function memex-herdr-open-session "memex-herdr"
                  (session-id source-path &optional doc-id))

(defconst memex-search-modes '(lexical semantic hybrid)
  "The search modes memex offers, in the order they are cycled through.")

(defcustom memex-search-default-mode 'hybrid
  "The mode a search names none of its own queries memex under.
Lexical mode matches whole terms, so a query typed one character at a
time drops to nothing on every prefix that is not itself a term in the
index; hybrid scores the embedded input beside the term match.  Hybrid
and semantic both need the embeddings `memex embed' generates, and fall
back to the term match alone when the index carries no vectors."
  :type (cons 'choice (mapcar (lambda (mode) (list 'const mode))
                              memex-search-modes))
  :group 'memex)

(defcustom memex-search-debounce 0.4
  "Seconds of quiet before an embedding query is sent.
Layers on consult's own `consult-async-input-debounce', which lexical
mode keeps: semantic and hybrid embed every debounced input, so it is
worth waiting longer for the typing to settle than a lexical round trip
is."
  :type 'number
  :group 'memex)

(defcustom memex-search-group-by-session t
  "Whether a search answers with one candidate per session.
Four fifths of the index is tool traffic, so a record-level answer is
four fifths rows of it and a page of matches is a handful of sessions;
this is what memex's own UIs list, which never put a record up.  Nil
answers with one candidate per record."
  :type 'boolean
  :group 'memex)

(defcustom memex-search-snippet-width 160
  "Characters of the best-scoring hit a session candidate is read under.
memex summarizes a session's snippet to the same width."
  :type 'natnum
  :group 'memex)

(defconst memex-search--limit 20
  "Matches a record-level search asks memex for.
`memex-api-search's own default, named here because a grouped search
over-fetches against it.")

(defvar memex-search--consult-noted nil
  "Non-nil once the notice that consult unlocks live search was shown.")

(defvar memex-search--mode nil
  "The mode the running `memex-search' session queries memex under.")

(defvar-keymap memex-search-map
  :doc "Keymap of the `memex-search' minibuffer."
  "M-s m" #'memex-search-cycle-mode)

(defun memex-search--prompt (mode)
  "Return the minibuffer prompt naming MODE."
  (format "memex %s search: " mode))

(defun memex-search--debounce (mode)
  "Return the input debounce MODE queries under, nil for consult's own."
  (and (memq mode '(semantic hybrid)) memex-search-debounce))

(defun memex-search--candidate-limit ()
  "Return the number of matches to ask memex for.
Grouping over-fetches the way memex's own CLI does before it
post-filters: a limit spent on one session's tool traffic comes back as
a page of one session."
  (if memex-search-group-by-session
      (max (* memex-search--limit 5) (+ memex-search--limit 10))
    memex-search--limit))

(defun memex-search--match (line query)
  "Return the earliest of QUERY's terms in LINE as a (START . LENGTH) pair.
A term is matched literally and without regard to case, and the earliest
match of any term wins: the passage worth reading a hit under is not the
one the term the query leads with happens to name.  Nil when no term
occurs in LINE at all, which is what a semantic or a hybrid hit answers
with."
  (let ((case-fold-search t)
        (earliest nil))
    (dolist (term (split-string (or query "")) earliest)
      (when-let* ((at (string-match (regexp-quote term) line)))
        (when (or (null earliest) (< at (car earliest)))
          (setq earliest (cons at (length term))))))))

(defun memex-search--window (line start length width)
  "Return WIDTH characters of LINE around the match at START of LENGTH.
The match sits in the middle of the window, or as near the middle as an
end of LINE leaves room for, and an edge the window cuts carries the
same ellipsis the head of the text ends under.  The ellipses are part of
the width, so the window is WIDTH characters however it was cut."
  (let* ((span (- width 6))
         (from (max 0 (- start (max 0 (/ (- span length) 2))))))
    (cond ((zerop from) (concat (substring line 0 (- width 3)) "..."))
          ((>= (+ from span) (length line))
           (concat "..." (substring line (- (length line) (- width 3)))))
          (t (concat "..." (substring line from (+ from span)) "...")))))

(defun memex-search--summarize (text query)
  "Return TEXT as the snippet of at most `memex-search-snippet-width'.
memex's own summary of the whitespace: runs of it collapse to a single
space and a leading run is dropped.  A text longer than the width is cut
to a window around where QUERY first matches it, the way memex's own
`matches' reports a hit with context on either side of it, since every
`codex exec' record opens with the same line of shell boilerplate and a
snippet taken from character zero reads alike for unrelated hits.  A
text no term matches keeps its first width-less-three characters under
an ellipsis."
  (let* ((line (string-trim
                (replace-regexp-in-string "[[:cntrl:][:blank:]]+" " "
                                          (or text ""))))
         (width memex-search-snippet-width)
         (match (and (> width 6) (memex-search--match line query))))
    (cond ((<= (length line) width) line)
          ((< width 3) (string-trim (substring line 0 width)))
          (match (memex-search--window line (car match) (cdr match) width))
          (t (string-trim (concat (substring line 0 (- width 3)) "..."))))))

(defun memex-search--group (hits)
  "Return the (SCORE RECORD) pairs of HITS bucketed by session.
A session is a `session_id' at a `source_path' - the same id under two
paths is two conversations, which is the pair memex's own server filters
a session's records by - and the buckets keep the order their first hit
arrived in, so memex's ranking survives the grouping."
  (let ((buckets (make-hash-table :test #'equal))
        (order nil))
    (dolist (hit hits)
      (let* ((record (cadr hit))
             (key (cons (alist-get 'session_id record)
                        (alist-get 'source_path record))))
        (unless (gethash key buckets) (push key order))
        (puthash key (cons hit (gethash key buckets)) buckets)))
    (mapcar (lambda (key) (nreverse (gethash key buckets))) (nreverse order))))

(defun memex-search--summary (hits query)
  "Return the HITS of one session as the record standing for it.
What memex's own `SessionSummary' keeps: every hit is counted, the
newest `ts' among them is the session's, and the snippet, the path and
the record the session opens at come from the best-scoring hit, a tie
going to the later of the two.  QUERY is what that hit's snippet is
read around.

The text the snippet was cut from stays on the summary under
`memex-label-source', because the snippet replaces it: a tool record's
`text' is the output it reports, and an annotation that no longer knows
which field its label came from prints that output back beside it."
  (let ((top (car hits))
        (ts 0))
    (dolist (hit hits)
      (setq ts (max ts (or (alist-get 'ts (cadr hit)) 0)))
      (when (>= (car hit) (car top)) (setq top hit)))
    (let ((record (copy-alist (cadr top)))
          (text (alist-get 'text (cadr top))))
      (setf (alist-get 'ts record) ts
            (alist-get 'hit_count record) (length hits)
            (alist-get 'memex-label-source record)
            (memex-completion--one-line text)
            (alist-get 'text record) (memex-search--summarize text query))
      record)))

(defun memex-search--summaries (hits query)
  "Return the HITS of a QUERY as one summary record per session."
  (mapcar (lambda (group) (memex-search--summary group query))
          (memex-search--group hits)))

(defun memex-search--candidates (hits query)
  "Return the (SCORE RECORD) pairs of HITS as completion candidates.
Memex's score order is kept and each candidate carries its record, the
way the recent-window selectors build theirs.  Grouped, one candidate
stands for a session and carries the summary of its hits, read around
QUERY."
  (if memex-search-group-by-session
      (memex-completion-session-candidates
       (memex-search--summaries hits query))
    (memex-completion-record-candidates (mapcar #'cadr hits))))

(defun memex-search--open (record)
  "Open the session RECORD belongs to at RECORD, and return RECORD.
The herdr bridge takes it when that is loaded and the viewer takes it
when it is not, which is the fork `memex-org-follow' takes."
  (when record
    (funcall (if (fboundp 'memex-herdr-open-session)
                 #'memex-herdr-open-session
               #'memex-view-session)
             (alist-get 'session_id record)
             (alist-get 'source_path record)
             (alist-get 'doc_id record)))
  record)

(defun memex-search--async (mode)
  "Return the consult async function searching memex in MODE.
The answer is curried the way `consult--async-pipeline' composes its
functions: it takes the downstream sink and returns the function taking
one action.  A string action supersedes the request in flight and starts
a new one, `cancel' and `destroy' abandon it, and every action is passed
on to the sink.  Matches are published as `flush', the candidates, then
`refresh', and only by the request that is still the current one, so a
slow older query cannot replace a newer result set.  A query nothing
matched publishes no candidates at all, since an empty list is nil and
the sink reads a nil action as the request for its own candidate list.
A failed query publishes `flush' and `refresh' without candidates, so
the matches of the last query that worked cannot be selected under a
prompt showing this one.

Every action leaving the string state counts a generation up, abandoning
one included: Emacs runs a sentinel from the event loop, so a process
that `memex-cancel-rpc' finds already exited can still have a callback
queued behind it, which the generation is what keeps out of a sink that
was cancelled or torn down.

A superseded request is abandoned through `memex-cancel-rpc' rather than
deleted, since only the marker it sets keeps memex-core's sentinel from
reporting the kill as a transport failure on every keystroke."
  (lambda (sink)
    (let ((request nil)
          (generation 0))
      (lambda (action)
        (prog1 (funcall sink action)
          (pcase action
            ((or 'cancel 'destroy)
             (memex-cancel-rpc request)
             (setq request nil
                   generation (1+ generation)))
            ((pred stringp)
             (memex-cancel-rpc request)
             (setq generation (1+ generation))
             (let ((current generation))
               (setq request
                     (memex-api-search
                      action
                      (lambda (hits)
                        (when (= current generation)
                          (funcall sink 'flush)
                          (when-let* ((candidates
                                       (memex-search--candidates hits
                                                                 action)))
                            (funcall sink candidates))
                          (funcall sink 'refresh)))
                      :errback
                      (lambda (failure)
                        (when (= current generation)
                          (funcall sink 'flush)
                          (funcall sink 'refresh)
                          (message "memex search: %s"
                                   (or (plist-get (cdr failure) :message)
                                       (error-message-string failure)))))
                      :mode mode :limit (memex-search--candidate-limit)))))))))))

(defun memex-search--next-mode (mode)
  "Return the mode following MODE in `memex-search-modes'."
  (or (cadr (memq mode memex-search-modes)) (car memex-search-modes)))

(defun memex-search-cycle-mode ()
  "Search again in the next mode, the query typed so far kept.
The session exits and starts anew because consult has no in-session
restart, and both its throttle and its minimum-input layer short-circuit
an unchanged input string: a mode switched in place would never re-query."
  (interactive)
  (unless memex-search--mode
    (user-error "No memex search session to cycle the mode of"))
  (let ((mode (memex-search--next-mode memex-search--mode))
        (initial (minibuffer-contents-no-properties)))
    (run-at-time 0 nil #'memex-search mode initial)
    (abort-recursive-edit)))

(defun memex-search--consult (mode initial)
  "Search memex in MODE from INITIAL as it is typed, and return the record."
  (let ((memex-search--mode mode))
    (memex-search--open
     (memex-completion-record-of
      (consult--read (consult--async-pipeline
                      (consult--async-min-input)
                      (consult--async-throttle nil (memex-search--debounce mode))
                      (memex-search--async mode))
                     :prompt (memex-search--prompt mode)
                     :initial initial
                     :category 'memex-record
                     :annotate #'memex-completion-annotate
                     :lookup #'consult--lookup-member
                     :keymap memex-search-map
                     :require-match t
                     :sort nil)))))

(defun memex-search--fetch (query mode)
  "Return the records memex answers QUERY with in MODE.
Grouped, the answer is one summary record per session."
  (let ((hits (memex-completion--fetch
               (lambda (callback errback)
                 (memex-api-search query callback :errback errback :mode mode
                                   :limit (memex-search--candidate-limit))))))
    (if memex-search-group-by-session
        (memex-search--summaries hits query)
      (mapcar #'cadr hits))))

(defun memex-search--static (mode initial)
  "Read a query in MODE starting from INITIAL and return the record chosen.
One query, one fetch: without consult there is nothing that could
refresh the candidates while the query is typed.  The matches are handed
to the selector rather than left to it, and a query nothing matched is
refused, since an empty record list is nil and would fall through to the
recent window."
  (unless memex-search--consult-noted
    (setq memex-search--consult-noted t)
    (message "memex: install consult to search as you type"))
  (let* ((query (read-string (memex-search--prompt mode) initial))
         (records (memex-search--fetch query mode)))
    (unless records
      (user-error "No memex records match %s" query))
    (memex-search--open
     (if memex-search-group-by-session
         (memex-read-session nil records)
       (memex-read-record nil records)))))

;;;###autoload
(defun memex-search (&optional mode initial)
  "Search memex, open what was chosen and return its record.
MODE is `lexical', `semantic' or `hybrid', read from the minibuffer
with a prefix argument and off `memex-search-default-mode' without one.
INITIAL is the query
the session starts from, which is what \\[memex-search-cycle-mode]
carries across a mode switch.

The matches go up one candidate per session unless
`memex-search-group-by-session' is nil, and the session chosen opens at
the record it was matched on.  The search runs as the query is typed
when consult is installed, and falls back to one query read into a
static picker when it is not."
  (interactive
   (list (and current-prefix-arg
              (intern (completing-read "memex search mode: "
                                       (mapcar #'symbol-name
                                               memex-search-modes)
                                       nil t)))))
  (let ((mode (or mode memex-search-default-mode)))
    (if (require 'consult nil t)
        (memex-search--consult mode initial)
      (memex-search--static mode initial))))

(provide 'memex)
;;; memex.el ends here
