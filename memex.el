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
(require 'transient)
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

(defcustom memex-search-debounce 0.4
  "Seconds of quiet before a semantic query is sent.
Layers on consult's own `consult-async-input-debounce', which the other
modes keep: semantic mode embeds every debounced input, so it is worth
waiting longer for the typing to settle than a lexical round trip is."
  :type 'number
  :group 'memex)

(defcustom memex-search-preview-key "C-SPC"
  "Key the search draws the highlighted candidate at, or `any' for every one.
Each preview renders a whole record, so a preview that follows the
selection pays that for every candidate the point passes over on its way
to the one that was wanted."
  :type '(choice (const :tag "Every selection" any) key)
  :group 'memex)

(defcustom memex-search-group-by-session t
  "Whether a search answers with one candidate per session.
Four fifths of the index is tool traffic, so a record-level answer is
four fifths rows of it and a page of matches is a handful of sessions;
this is what memex's own UIs list, which never put a record up.  Nil
answers with one candidate per record."
  :type 'boolean
  :group 'memex)

(defcustom memex-search-session-hit 'newest
  "Which returned hit a grouped session opens at and previews.
`newest' chooses the latest matching record; `best' chooses the
highest-scoring record.  This does not change the order of sessions."
  :type '(choice (const :tag "Newest match" newest)
                 (const :tag "Best match" best))
  :group 'memex)

(defcustom memex-search-snippet-width 160
  "Maximum display columns for a search match excerpt."
  :type 'natnum
  :group 'memex)

(defconst memex-search--limit 20
  "Matches a record-level search asks memex for.
`memex-api-search's own default, named here because a grouped search
over-fetches against it.")

(defcustom memex-search-project-width 22
  "Maximum display columns for a search candidate's project."
  :type 'natnum
  :group 'memex)

(defcustom memex-search-identity-width 22
  "Maximum display columns for a search candidate's source and role."
  :type 'natnum
  :group 'memex)

(defvar memex-search--scope nil
  "Session scope of the current search, or nil for the whole index.")

(defvar memex-search--static-query nil
  "Query retained while selecting static search results.")

(defvar memex-search--consult-noted nil
  "Non-nil once the notice that consult unlocks live search was shown.")

(defvar memex-search--mode nil
  "The mode the running `memex-search' session queries memex under.")

(defconst memex-search-record-roles
  '("user" "assistant" "tool_use" "tool_result")
  "The roles memex indexes a record under, in the order they are offered.")

(defcustom memex-search-roles '("user" "assistant")
  "The roles a search asks for, or nil for every role memex indexed.
Four fifths of the index is tool traffic, so a page of matches is four
fifths the calls that carried a conversation out and a query is nearly
always put to the conversation itself.  Memex takes the set and applies it
in the index, so a page asked for comes back a page of these roles;
\\<memex-search-map>\\[memex-search-select-roles] is how the running \
search asks for others."
  :type `(set ,@(mapcar (lambda (role) `(const ,role))
                        memex-search-record-roles))
  :group 'memex)

(defvar memex-search--asked-roles 'unset
  "The roles the running search asks for, `unset' outside a search.
A search asking for every role carries nil, which is what
`memex-search-roles' means by every role, so the two are told apart by
the sentinel rather than by nil.")

(defun memex-search--roles ()
  "Return the roles the search asks for, or nil for every role."
  (if (eq memex-search--asked-roles 'unset)
      memex-search-roles
    memex-search--asked-roles))

(defvar-keymap memex-search-map
  :doc "Bindings for the `memex-search' minibuffer.
The keys reach for the meta prefix rather than a self-inserting one: the
minibuffer is where the query is typed, and a search narrows while it is
being read or not at all."
  "M-m" #'memex-search-cycle-mode
  "M-g" #'memex-search-toggle-grouping
  "M-r" #'memex-search-select-roles
  "M-t" #'memex-search-toggle-roles
  "M-." #'memex-search-in-selected-session)

(defun memex-search--prompt (mode)
  "Return the minibuffer prompt naming MODE.
Every narrowing the search is under is named in it: what the keys change
is worth nothing where the reader cannot see what it changed to."
  (format "memex %s %s%s%s: " mode
          (if memex-search-group-by-session "sessions" "messages")
          (if memex-search--scope " [session]" "")
          (if-let* ((roles (memex-search--roles)))
              (format " [%s]" (string-join roles "+"))
            "")))

(defun memex-search--debounce (mode)
  "Return the input debounce MODE queries under, nil for consult's own."
  (and (eq mode 'semantic) memex-search-debounce))

(defun memex-search--text-limit ()
  "Return the characters of text a search hit needs to carry.
A row shows an excerpt of `memex-search-snippet-width' columns around
the query, so whole tool transcripts only cost transfer and parsing."
  (* 4 memex-search-snippet-width))

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
  "Return up to WIDTH columns of LINE around START and LENGTH.
START and LENGTH are character offsets.  Omitted text is marked with
ellipses included in the display width."
  (let* ((span (- width 6))
         (column (string-width (substring line 0 start)))
         (match-width (string-width (substring line start (+ start length))))
         (from (max 0 (- column (max 0 (/ (- span match-width) 2))))))
    (cond ((zerop from)
           (concat (truncate-string-to-width line (- width 3)) "..."))
          ((>= (+ from span) (string-width line))
           (concat "..." (truncate-string-to-width
                          line (string-width line)
                          (- (string-width line) (- width 3)))))
          (t (concat "..." (truncate-string-to-width line (+ from span) from)
                     "...")))))

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
  (let* ((line (or (memex-completion--clean text) ""))
         (width memex-search-snippet-width)
         (match (and (> width 6) (memex-search--match line query))))
    (cond ((<= (string-width line) width) line)
          ((< width 3) (string-trim (truncate-string-to-width line width)))
          (match (memex-search--window line (car match) (cdr match) width))
          (t (string-trim (concat (truncate-string-to-width line (- width 3))
                                 "..."))))))

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
Every hit is counted and the newest `ts' is the session's timestamp.
`memex-search-session-hit' selects the record to open and preview.
QUERY is what that hit's snippet is read around.

The text the snippet was cut from stays on the summary under
`memex-label-source', because the snippet replaces it: a tool record's
`text' is the output it reports, and an annotation that no longer knows
which field its label came from prints that output back beside it."
  (let ((top (car hits))
        (ts 0))
    (dolist (hit hits)
      (setq ts (max ts (or (alist-get 'ts (cadr hit)) 0)))
      (when (if (eq memex-search-session-hit 'newest)
                (>= (or (alist-get 'ts (cadr hit)) 0)
                    (or (alist-get 'ts (cadr top)) 0))
              (>= (car hit) (car top)))
        (setq top hit)))
    (let ((record (copy-alist (cadr top)))
          (text (alist-get 'text (cadr top))))
      (setf (alist-get 'ts record) ts
            (alist-get 'hit_count record) (length hits)
            (alist-get 'memex-label-source record)
            (memex-completion--one-line text)
            (alist-get 'memex-search-text record) text
            (alist-get 'text record) (memex-search--summarize text query))
      record)))

(defun memex-search--summaries (hits query)
  "Return the HITS of a QUERY as one summary record per session."
  (mapcar (lambda (group) (memex-search--summary group query))
          (memex-search--group hits)))

(defun memex-search--column (text width face)
  "Return sanitized TEXT padded to WIDTH columns in FACE."
  (propertize (truncate-string-to-width
               (or (memex-completion--clean text) "") width nil ?\s "…")
              'face face))

(defun memex-search--width ()
  "Return the columns a search row is drawn for.
The narrowest window the minibuffer is shown in, which is what
marginalia measures its own fields against: `vertico-buffer' puts the
minibuffer in a window of its own, and the miniwindow that
`minibuffer-window' names spans the frame whatever that window is
doing, so a row laid out against it wraps in the window it lands in."
  (let* ((mini (minibuffer-window))
         (windows (and (window-live-p mini)
                       (get-buffer-window-list (window-buffer mini) t 0))))
    (if windows
        (apply #'min (mapcar #'window-width windows))
      (frame-width))))

(defun memex-search--row (record query)
  "Return RECORD's search row with an excerpt around QUERY.
When the record was written is left to the annotation, which carries it
as an age rather than as a date and is the one thing beside the row."
  (let* ((width (memex-search--width))
         (project-width (min memex-search-project-width (max 8 (/ width 7))))
         (identity-width (min memex-search-identity-width (max 10 (/ width 7))))
         (memex-search-snippet-width
          (min memex-search-snippet-width
               (max 12 (- width project-width identity-width 18))))
         (text (or (alist-get 'memex-search-text record)
                   (alist-get 'text record) (alist-get 'tool_output record)))
         (snippet (memex-search--summarize text query))
         (case-fold-search t))
    (setq snippet (truncate-string-to-width
                   snippet memex-search-snippet-width nil nil "…"))
    (dolist (term (split-string (or query "")))
      (let ((start 0))
        (while (string-match (regexp-quote term) snippet start)
          (add-face-text-property (match-beginning 0) (match-end 0)
                                  'match t snippet)
          (setq start (match-end 0)))))
    (concat
     (memex-search--column (alist-get 'project record) project-width
                          'font-lock-function-name-face)
     "  "
     (memex-search--column
      (memex-completion--join (alist-get 'source record)
                              (or (alist-get 'tool_name record)
                                  (alist-get 'role record)))
      identity-width 'font-lock-keyword-face)
     "  " snippet)))

(defun memex-search--annotate (candidate)
  "Return the metadata drawn in the margin beside search CANDIDATE.
How many hits a session row stands for and how long ago the record was
written: what the row itself has no column for."
  (let* ((record (memex-completion-record-of candidate))
         (fields (memex-completion--join
                  (memex-completion--hits (alist-get 'hit_count record))
                  (memex-completion--age (alist-get 'ts record)))))
    (unless (string-empty-p fields)
      (concat memex-completion-align " "
              (propertize fields 'face 'completions-annotations)))))

(defun memex-search--record-candidates (records query)
  "Return search candidates for RECORDS around QUERY."
  (mapcar
   (lambda (candidate)
     (propertize candidate 'memex-annotation
                 (or (memex-search--annotate candidate) "")))
   (memex-completion--candidates
    records (lambda (record) (memex-search--row record query))
    (if memex-search-group-by-session 'session_id 'doc_id))))

(defun memex-search--candidates (hits query)
  "Return scored HITS as search candidates around QUERY."
  (memex-search--record-candidates
   (if memex-search-group-by-session
       (memex-search--summaries hits query)
     (mapcar #'cadr hits))
   query))

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

(defconst memex-search-preview-buffer-name "*memex preview*"
  "Name of the buffer the highlighted search candidate is drawn in.")

(defun memex-search--previewed (record)
  "Return RECORD with the whole text its snippet was cut from.
A grouped summary carries its hit's text under `memex-search-text'
because `text' is the snippet drawn in the row, and a preview showing
the snippet back would say nothing the row does not."
  (if-let* ((text (alist-get 'memex-search-text record)))
      (let ((whole (copy-alist record)))
        (setf (alist-get 'text whole) text)
        whole)
    record))

(defun memex-search--preview (candidate)
  "Draw the record CANDIDATE carries in the preview buffer.
A record that cannot be drawn leaves the search running and says why:
the preview is beside the work, not the work."
  (when-let* ((record (and (stringp candidate)
                           (memex-completion-record-of candidate))))
    (condition-case failure
        (display-buffer
         (memex-view-record-buffer (memex-search--previewed record)
                                   memex-search-preview-buffer-name))
      (error (message "memex preview: %s" (error-message-string failure))))))

(defun memex-search--reset-preview ()
  "Take the preview down and give its window back what it was showing.
Killing the buffer alone would leave the window consult's preview opened
standing over whatever the search was called from."
  (when-let* ((buffer (get-buffer memex-search-preview-buffer-name)))
    (if-let* ((window (get-buffer-window buffer 0)))
        (quit-restore-window window 'kill)
      (kill-buffer buffer))))

(defun memex-search--state ()
  "Return the consult state function previewing the highlighted candidate.
Consult asks for the preview of nothing when the selection is gone and
once more before the search exits, and both mean the same here: the
preview belongs to the search and no command opens it again."
  (lambda (action candidate)
    (pcase action
      ('preview (if candidate
                    (memex-search--preview candidate)
                  (memex-search--reset-preview)))
      ('exit (memex-search--reset-preview)))))

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
                     (apply #'memex-api-search
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
                      :mode mode :limit (memex-search--candidate-limit)
                      :text-limit (memex-search--text-limit)
                      :roles (memex-search--roles)
                      (and memex-search--scope
                           (list :session-scope memex-search--scope))))))))))))

(defun memex-search--next-mode (mode)
  "Return the mode following MODE in `memex-search-modes'."
  (or (cadr (memq mode memex-search-modes)) (car memex-search-modes)))

(defun memex-search--restart (mode grouped scope &optional roles)
  "Restart search in MODE, GROUPED by session, restricted to SCOPE.
ROLES are the roles the new search asks for, `every' for all of them and
nil for the running search's own."
  (unless (and (minibufferp) memex-search--mode)
    (user-error "No memex search is active"))
  (let ((initial (or memex-search--static-query
                     (minibuffer-contents-no-properties)))
        (roles (or roles (memex-search--roles) 'every)))
    (run-at-time 0 nil #'memex-search--run mode initial grouped scope roles)
    (abort-recursive-edit)))

(defun memex-search-cycle-mode ()
  "Cycle the search mode, keeping query, grouping, scope and roles."
  (interactive)
  (memex-search--restart (memex-search--next-mode memex-search--mode)
                         memex-search-group-by-session memex-search--scope))

(defun memex-search-toggle-grouping ()
  "Switch between matching messages and sessions."
  (interactive)
  (memex-search--restart memex-search--mode
                         (not memex-search-group-by-session) memex-search--scope))

(defvar memex-search--pending-roles nil
  "The roles `memex-search-select-roles' has put together so far.")

(defun memex-search--toggle-pending-role (role)
  "Put ROLE in or out of the pending set, in the order roles are offered."
  (let ((pending (if (member role memex-search--pending-roles)
                     (remove role memex-search--pending-roles)
                   (cons role memex-search--pending-roles))))
    (setq memex-search--pending-roles
          (seq-filter (lambda (known) (member known pending))
                      memex-search-record-roles))))

(defun memex-search--pending-role-description (role)
  "Return ROLE as the roles menu shows it, lit while the pending set holds it."
  (propertize role 'face (if (member role memex-search--pending-roles)
                             'transient-value
                           'transient-inactive-value)))

(defmacro memex-search--define-role-toggle (role)
  "Define the roles menu command putting ROLE in or out of the pending set."
  (let ((name (intern (format "memex-search-toggle-role-%s"
                              (string-replace "_" "-" role)))))
    `(transient-define-suffix ,name ()
       ,(format "Put %s in or out of the roles the search will ask for." role)
       :transient t
       :description (lambda () (memex-search--pending-role-description ,role))
       (interactive)
       (memex-search--toggle-pending-role ,role))))

(memex-search--define-role-toggle "user")
(memex-search--define-role-toggle "assistant")
(memex-search--define-role-toggle "tool_use")
(memex-search--define-role-toggle "tool_result")

(transient-define-suffix memex-search-select-every-role ()
  "Put every role in the pending set."
  :transient t
  :description "every role"
  (interactive)
  (setq memex-search--pending-roles (copy-sequence memex-search-record-roles)))

(transient-define-suffix memex-search-select-default-roles ()
  "Put `memex-search-roles' back as the pending set."
  :transient t
  :description "defaults"
  (interactive)
  (setq memex-search--pending-roles
        (copy-sequence (or memex-search-roles memex-search-record-roles))))

(transient-define-suffix memex-search-apply-selected-roles ()
  "Restart the running search asking for the pending set of roles.
A set holding every role, or none, asks for every role."
  :description "apply"
  (interactive)
  (memex-search--restart
   memex-search--mode memex-search-group-by-session memex-search--scope
   (if (or (null memex-search--pending-roles)
           (equal memex-search--pending-roles memex-search-record-roles))
       'every
     memex-search--pending-roles)))

(transient-define-prefix memex-search-select-roles ()
  "Choose the roles the running search asks for, and restart it once.
Each role goes in or out at one key while the menu stays up, so a set of
any size costs one new query.  Memex takes the set and applies it in the
index, so the page that comes back is a page of these roles rather than a
page of the corpus with the rest of it dropped."
  [:description "Roles the search asks for"
   ("u" memex-search-toggle-role-user)
   ("a" memex-search-toggle-role-assistant)
   ("c" memex-search-toggle-role-tool-use)
   ("r" memex-search-toggle-role-tool-result)]
  [("SPC" memex-search-select-every-role)
   ("DEL" memex-search-select-default-roles)
   ("RET" memex-search-apply-selected-roles)
   ("q" "cancel" transient-quit-one)]
  (interactive)
  (unless (and (minibufferp) memex-search--mode)
    (user-error "No memex search is active"))
  (setq memex-search--pending-roles
        (copy-sequence (or (memex-search--roles) memex-search-record-roles)))
  (transient-setup 'memex-search-select-roles))

(defun memex-search-toggle-roles ()
  "Ask for every role, or go back to `memex-search-roles'.
The set is chosen with \\<memex-search-map>\\[memex-search-select-roles]; \
this is the one move worth a key of
its own, since a query the conversation does not answer is a query for
the calls it was carried out by."
  (interactive)
  (memex-search--restart memex-search--mode memex-search-group-by-session
                         memex-search--scope
                         (if (memex-search--roles) 'every memex-search-roles)))

(defun memex-search--selected-record ()
  "Return the highlighted search candidate's record."
  (let* ((selected
          (or (and (boundp 'consult--completion-candidate-hook)
                   (run-hook-with-args-until-success
                    'consult--completion-candidate-hook))
              (and (bound-and-true-p vertico-mode)
                   (fboundp 'vertico--candidate) (vertico--candidate))
              (minibuffer-contents-no-properties)))
         (candidates (all-completions "" minibuffer-completion-table))
         (candidate (car (member selected candidates))))
    (or (memex-completion-record-of candidate)
        (user-error "No memex candidate selected"))))

(defun memex-search-in-selected-session ()
  "Search matching messages in the selected candidate's session."
  (interactive)
  (unless (and (minibufferp) memex-search--mode)
    (user-error "No memex search is active"))
  (let* ((record (memex-search--selected-record))
         (source (alist-get 'source record))
         (session (alist-get 'session_id record))
         (path (alist-get 'source_path record)))
    (unless (and source session path)
      (user-error "Selected match has no complete session identity"))
    (memex-search--restart
     memex-search--mode nil
     (list (list :source source :session-id session :source-path path)))))

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
                     :annotate #'memex-search--annotate
                     :state (memex-search--state)
                     :preview-key memex-search-preview-key
                     :lookup #'consult--lookup-member
                     :keymap memex-search-map
                     :require-match t
                     :sort nil)))))

(defun memex-search--fetch (query mode)
  "Return the records memex answers QUERY with in MODE.
Grouped, the answer is one summary record per session."
  (let ((hits (memex-completion--fetch
               (lambda (callback errback)
                 (apply #'memex-api-search query callback :errback errback
                        :mode mode :limit (memex-search--candidate-limit)
                      :text-limit (memex-search--text-limit)
                        :roles (memex-search--roles)
                        (and memex-search--scope
                             (list :session-scope memex-search--scope)))))))
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
         (records (memex-search--fetch query mode))
         (memex-search--mode mode)
         (memex-search--static-query query))
    (unless records
      (user-error "No memex records match %s" query))
    (memex-search--open
     (memex-completion-record-of
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap memex-search-map (current-local-map))))
        (memex-completion--read
         (memex-search--prompt mode)
         (memex-search--record-candidates records query)
         'memex-record #'memex-search--annotate "matches"))))))

(defun memex-search--read-mode ()
  "Read an optional search mode for a prefix argument."
  (and current-prefix-arg
       (intern (completing-read "memex search mode: "
                                (mapcar #'symbol-name memex-search-modes)
                                nil t))))

(defun memex-search--run (mode initial grouped scope &optional roles)
  "Search in MODE from INITIAL, GROUPED by session and restricted to SCOPE.
ROLES are the roles to ask for: `every' for all of them, nil for
`memex-search-roles', a list for itself."
  (let ((mode (or mode 'lexical))
        (memex-search-group-by-session grouped)
        (memex-search--scope scope)
        (memex-search--asked-roles (cond ((null roles) 'unset)
                                         ((eq roles 'every) nil)
                                         (t roles)))
        (memex-search--static-query nil))
    (if (require 'consult nil t)
        (memex-search--consult mode initial)
      (memex-search--static mode initial))))

;;;###autoload
(defun memex-search-messages (&optional mode initial)
  "Search individual matching messages in MODE, starting with INITIAL."
  (interactive (list (memex-search--read-mode)))
  (memex-search--run mode initial nil nil))

;;;###autoload
(defun memex-search-sessions (&optional mode initial)
  "Search matching sessions in MODE, starting with INITIAL."
  (interactive (list (memex-search--read-mode)))
  (memex-search--run mode initial t nil))

;;;###autoload
(defun memex-search (&optional mode initial)
  "Search memex, open what was chosen and return its record.
MODE is `lexical', `semantic' or `hybrid', defaulting to `lexical' and
read from the minibuffer with a prefix argument.  INITIAL is the query
the session starts from, which is what \\[memex-search-cycle-mode]
carries across a mode switch.

The matches go up one candidate per session unless
`memex-search-group-by-session' is nil, and the session chosen opens at
the record it was matched on.  The search runs as the query is typed
when consult is installed, and falls back to one query read into a
static picker when it is not."
  (interactive (list (memex-search--read-mode)))
  (memex-search--run mode initial memex-search-group-by-session nil))

;;;###autoload
(defun memex-search-in-sessions (scope &optional mode initial)
  "Search the sessions SCOPE names in MODE, starting from INITIAL.
SCOPE is a list of plists, each carrying `:source', `:session-id' and
`:source-path' - a `session_id' names a session only along with the
transcript it was read from.  A nil SCOPE searches everything.

The matches go up one per message rather than one per session: a search
already narrowed to sessions the caller named has no grouping left to do."
  (memex-search--run (or mode 'lexical) initial nil scope))

(provide 'memex)
;;; memex.el ends here
