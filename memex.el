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
;; queries over it through a JSON-RPC command.  This package speaks that
;; command and puts the index up in the minibuffer and in Emacs buffers.
;;
;;   M-x memex-search        search the index and return the record chosen
;;   M-x memex-view-session  read a session's transcript in a buffer
;;   M-x memex-usage         report memex's token usage
;;
;; `memex-search' reads a record out of memex's index.  With consult
;; installed it searches as you type: each input change supersedes the
;; request in flight and the matches of the one that is still current are
;; published into consult's sink.  Without consult it reads one query,
;; fetches once and puts the matches up through `memex-read-record'.
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

(defconst memex-search-modes '(lexical semantic hybrid)
  "The search modes memex offers, in the order they are cycled through.")

(defcustom memex-search-debounce 0.4
  "Seconds of quiet before a semantic query is sent.
Layers on consult's own `consult-async-input-debounce', which the other
modes keep: semantic mode embeds every debounced input, so it is worth
waiting longer for the typing to settle than a lexical round trip is."
  :type 'number
  :group 'memex)

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
  (and (eq mode 'semantic) memex-search-debounce))

(defun memex-search--candidates (hits)
  "Return the (SCORE RECORD) pairs of HITS as completion candidates.
Memex's score order is kept and each candidate carries its record, the
way the recent-window selectors build theirs."
  (memex-completion-record-candidates (mapcar #'cadr hits)))

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
                                       (memex-search--candidates hits)))
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
                      :mode mode))))))))))

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
                    :sort nil))))

(defun memex-search--fetch (query mode)
  "Return the records memex answers QUERY with in MODE."
  (mapcar #'cadr
          (memex-completion--fetch
           (lambda (callback errback)
             (memex-api-search query callback :errback errback :mode mode)))))

(defun memex-search--static (mode initial)
  "Read a query in MODE starting from INITIAL and return the record chosen.
One query, one fetch: without consult there is nothing that could
refresh the candidates while the query is typed.  The matches are handed
to `memex-read-record' rather than left to it, and a query nothing
matched is refused, since an empty record list is nil and would fall
through to the recent window."
  (unless memex-search--consult-noted
    (setq memex-search--consult-noted t)
    (message "memex: install consult to search as you type"))
  (let* ((query (read-string (memex-search--prompt mode) initial))
         (records (memex-search--fetch query mode)))
    (unless records
      (user-error "No memex records match %s" query))
    (memex-read-record nil records)))

;;;###autoload
(defun memex-search (&optional mode initial)
  "Search memex for a record and return the one chosen.
MODE is `lexical', `semantic' or `hybrid', defaulting to `lexical' and
read from the minibuffer with a prefix argument.  INITIAL is the query
the session starts from, which is what \\[memex-search-cycle-mode]
carries across a mode switch.

The search runs as the query is typed when consult is installed, and
falls back to one query read into a static picker when it is not."
  (interactive
   (list (and current-prefix-arg
              (intern (completing-read "memex search mode: "
                                       (mapcar #'symbol-name
                                               memex-search-modes)
                                       nil t)))))
  (let ((mode (or mode 'lexical)))
    (if (require 'consult nil t)
        (memex-search--consult mode initial)
      (memex-search--static mode initial))))

(provide 'memex)
;;; memex.el ends here
