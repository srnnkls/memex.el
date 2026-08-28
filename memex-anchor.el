;;; memex-anchor.el --- Read an indexed record where the live agent drew it -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-anchor-show' takes a record memex indexed and opens the live
;; agent's own scrollback at it, so a search hit can be read in the
;; rendering the agent produced - collapsed tool output, the surrounding
;; turn - rather than as an indexed record.
;;
;; Two things stand between a record and that position.  A terminal is
;; not a document: it strips markdown, prefixes glyphs, draws diff
;; gutters and breaks words across columns, so a record's text is never
;; literally on screen.  Reducing both sides to their letters alone
;; removes every one of those, and a 45-letter window of what is left
;; carries the record's identity - measured against 15 live panes, a
;; window that long matched a different session in the same project in
;; under 2% of cases and a different project in none.
;;
;; And herdr's pane list carries no session identity, so which pane runs
;; a session has to be recovered rather than looked up.  It is recovered
;; from the same test: the tail of the session is anchored against every
;; live pane and the pane holding enough of it wins.  On those same 15
;; panes that resolved 11, misresolved none, and abstained on 3 - two
;; panes with almost no scrollback and one freshly resumed, whose screen
;; held file references rather than any indexed text.  Abstaining is the
;; answer in that case; the caller falls back to the viewer.
;;
;; herdr cannot scroll a pane to an offset, so nothing here disturbs the
;; running agent: the scrollback is copied into a buffer of its own.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)
(require 'memex-completion)
(require 'memex-entry)
(require 'memex-view)

(declare-function memex-herdr-resume "memex-herdr" (record))
(declare-function memex-herdr--row "memex-herdr" (session-id source-path source))
(declare-function memex-herdr--directory "memex-herdr" (row))
(declare-function memex-herdr--start "memex-herdr" (row command &optional name))
(declare-function herdr-start-server-if-needed "herdr-core" ())

(declare-function herdr-agents "herdr" ())
(declare-function herdr-attach-entry "herdr" (entry))
(declare-function herdr-terminal-buffer "herdr" (terminal-id))
(declare-function herdr-pane-text "herdr" (pane-id &optional source lines))
(declare-function herdr-session-for "herdr" (&optional directory))
(defvar herdr-attach-takeover)

(defgroup memex-anchor nil
  "Reading an indexed record in the live agent's scrollback."
  :group 'memex)

(defcustom memex-anchor-window 45
  "Letters of a record that must appear on screen together for a match.
Shorter windows match a different session in the same project often
enough to land the reader in the wrong conversation."
  :type 'natnum
  :group 'memex-anchor)

(defcustom memex-anchor-minimum-hits 1
  "Records of a session that have to be on a pane to call it the runner.
A fraction of the tail was the test before, and it cannot be met by a
screen too small to hold the tail: a background agent draws a condensed
view - \"Ran 5 shell commands\" where the calls were - and keeps a couple
of thousand characters of it.  The pane running this very session scored
0.14 against a floor of 0.25 while every other pane scored zero, so the
margin is what tells the panes apart and the floor only refused the
right answer."
  :type 'natnum
  :group 'memex-anchor)

(defcustom memex-anchor-minimum-margin 2.0
  "How far ahead of the runner-up the winning pane has to score.
A pane that resumed a sibling session shows much of the same text as the
real one, and choosing between them is how a reader lands in the wrong
conversation; a tie is answered with no pane at all."
  :type 'number
  :group 'memex-anchor)

(defcustom memex-anchor-tail 120
  "Records from the end of a session read to identify its pane.
Only the messages among them are matched, and a tool call and its result
are most of what a session is made of, so this has to reach well past
the handful of messages it is after."
  :type 'natnum
  :group 'memex-anchor)

(defcustom memex-anchor-scrollback 20000
  "Lines of pane scrollback read when looking for which pane runs a session.
This is read to tell the panes apart and nothing else.  What an attached
buffer ends up holding is `herdr-attach-history', which the attach
replays for itself."
  :type 'natnum
  :group 'memex-anchor)

(defun memex-anchor--skeleton (text)
  "Return TEXT's letters as (SKELETON . POSITIONS).
POSITIONS maps each index of SKELETON to the index in TEXT it came
from.  Digits go along with the punctuation because diff gutters draw
line numbers into the middle of a span."
  (let* ((n (length text))
         (skeleton (make-string n ?\s))
         (positions (make-vector (max n 1) 0))
         (j 0))
    (dotimes (i n)
      (let ((c (downcase (aref text i))))
        (when (and (>= c ?a) (<= c ?z))
          (aset skeleton j c)
          (aset positions j i)
          (setq j (1+ j)))))
    (cons (substring skeleton 0 j) (substring positions 0 j))))

(defun memex-anchor--windows (skeleton width)
  "Return the windows of SKELETON WIDTH letters long, each with its offset.
Windows overlap by half so a record is probed along its whole length:
the terminal truncates long output, and what survives may be anywhere
but the head."
  (if (< (length skeleton) width)
      (and (>= (length skeleton) 24) (list (cons 0 skeleton)))
    (let ((stride (max 1 (/ width 2)))
          (i 0)
          (out nil))
      (while (<= (+ i width) (length skeleton))
        (push (cons i (substring skeleton i (+ i width))) out)
        (setq i (+ i stride)))
      (nreverse out))))

(defun memex-anchor--locate-reduced (text haystack positions)
  "Return where TEXT was drawn in an already reduced screen, or nil.
HAYSTACK is that screen's letters and POSITIONS maps them back to it.
Taking the reduction as an argument is what keeps a lookup over many
records from re-reducing the same screen once per record."
  (let ((needle (car (memex-anchor--skeleton text)))
        (found nil))
    (unless (string-empty-p haystack)
      (dolist (window (memex-anchor--windows needle memex-anchor-window))
        (unless found
          (let ((at (string-search (cdr window) haystack)))
            (when at (setq found (aref positions at)))))))
    found))

(defun memex-anchor--locate (text screen)
  "Return where in SCREEN the record TEXT was drawn, or nil.
Both sides are reduced to their letters, which is what survives a
terminal: the markdown it strips, the glyphs it prefixes and the
columns it breaks words across all fall out of the comparison."
  (pcase-let ((`(,haystack . ,positions) (memex-anchor--skeleton screen)))
    (memex-anchor--locate-reduced text haystack positions)))

(defun memex-anchor-on-screen-p (text screen)
  "Return non-nil when the record TEXT is drawn anywhere in SCREEN."
  (and (memex-anchor--locate text screen) t))

(defun memex-anchor--drawn (records)
  "Return the RECORDS a terminal draws as text.
A tool call is drawn as a summary of itself and its result as a fold, so
neither is ever on screen the way memex recorded it; counting them only
makes every pane look like it is running nothing."
  (seq-filter (lambda (record)
                (memq (memex-entry-kind (cons record nil))
                      '(human assistant)))
              records))

(defun memex-anchor--hits (records screen)
  "Return how many of RECORDS are drawn in SCREEN."
  (pcase-let* ((`(,haystack . ,positions) (memex-anchor--skeleton screen))
               (texts (delq nil
                            (mapcar (lambda (record)
                                      (let ((text (alist-get 'text record)))
                                        (and text
                                             (>= (length text) 24)
                                             text)))
                                    (memex-anchor--drawn records)))))
    (seq-count (lambda (text)
                 (and (memex-anchor--locate-reduced text haystack positions) t))
               texts)))

(defun memex-anchor--best (records panes)
  "Return the (PANE-ID . HITS) of the pane running the session RECORDS end.
PANES is an alist of pane id to that pane's scrollback.  Answers nil
when no pane shows enough of the tail, and equally when the runner-up is
close: a pane that merely resumed a sibling session scores like the real
one, and picking between them is how a reader lands in the wrong
conversation."
  (let ((scored (sort (mapcar (lambda (pane)
                                (cons (car pane)
                                      (memex-anchor--hits records (cdr pane))))
                              panes)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (when scored
      (let ((winner (car scored))
            (runner (or (cdr (cadr scored)) 0)))
        (and (>= (cdr winner) memex-anchor-minimum-hits)
             (>= (cdr winner) (* memex-anchor-minimum-margin runner))
             winner)))))

(defun memex-anchor--agent-session (agent)
  "Return the session identifier AGENT reports running, or nil.
herdr answers with `agent_session', an object naming what kind of thing
it holds; only an `id' is the session identifier memex indexes under."
  (when-let* ((reported (alist-get 'agent_session agent))
              ((equal (alist-get 'kind reported) "id")))
    (alist-get 'value reported)))

(defun memex-anchor--by-session (record)
  "Return the herdr pane whose agent reports RECORD's session, or nil.
herdr learns which session an agent is running from the agent itself,
through `pane.report_agent_session', and hands it back on the listing.
That is the whole join: an identifier on both sides, no guessing from
what happens to be on a screen.

An agent reports it on starting up, so it is there for every session
begun since `herdr integration install <agent>' wrote the hook, and for
none begun before.  The screens are what tell the rest apart."
  (when-let* ((session-id (alist-get 'session_id record)))
    (seq-some (lambda (agent)
                (and (equal (memex-anchor--agent-session agent) session-id)
                     (alist-get 'pane_id agent)))
              (memex-anchor--agents))))

;;;###autoload
(defun memex-anchor-pane-for (record)
  "Return the herdr pane running RECORD's session, or nil for none.
Answers from what the agents report about themselves, so it costs one
listing and no guessing: a caller deciding whether to start a session
can ask before it starts one, which is what keeps an agent from being
told to resume a session it is already running."
  (and (memex-anchor--herdr-p) (memex-anchor--by-session record)))

(defun memex-anchor--kind (source)
  "Return the herdr agent kind memex names SOURCE, or SOURCE itself."
  (pcase source ("omp" "omp") ("openclaw" "openclaw") (_ source)))

(defun memex-anchor--by-directory (record row)
  "Return the one herdr agent of RECORD's kind working in ROW's directory.
Nil where there is no such agent or more than one, which is the same
answer as an ambiguous match: adopting the wrong one puts the reader in
another conversation.

A pane is the fallback when nothing on screen matched.  A background
agent draws a condensed view and keeps a couple of thousand characters
of it, so the little it holds has usually scrolled past whatever memex
has got round to indexing - the two windows simply do not meet, however
good the matching is."
  (when-let* ((directory (and row (alist-get 'cwd row)))
              (kind (memex-anchor--kind (alist-get 'source record)))
              (matching (seq-filter
                         (lambda (agent)
                           (and (equal (alist-get 'cwd agent) directory)
                                (equal (alist-get 'agent agent) kind)))
                         (memex-anchor--agents))))
    (and (equal (length matching) 1)
         (alist-get 'pane_id (car matching)))))

(defvar-local memex-anchor-pane-id nil
  "The herdr pane whose terminal this buffer is attached to.")

(defvar-local memex-anchor-session-id nil
  "The `session_id' the agent in this buffer was running.")

(defun memex-anchor--session-buffer (session-id)
  "Return the live buffer already attached to SESSION-ID's agent, or nil.
Derived from `buffer-list' rather than stored, so a terminal the user
detached leaves no entry behind to go stale.  Finding one is what lets a
second jump into the same conversation skip both the request and the
sweep across every pane."
  (when session-id
    (seq-find (lambda (buffer)
                (and (buffer-live-p buffer)
                     (equal (buffer-local-value 'memex-anchor-session-id buffer)
                            session-id)))
              (buffer-list))))

(defun memex-anchor--herdr-p ()
  "Return non-nil once herdr's API is loaded, loading it if it is installed.
herdr autoloads its commands but not `herdr-api', so a bridge that only
probed `fboundp' would report herdr missing on a machine running it."
  (or (fboundp 'herdr-agents)
      (and (require 'herdr nil t) (fboundp 'herdr-agents))))

(defun memex-anchor--agents ()
  "Return herdr's live agent entries, or nil where herdr is not to be had."
  (when (memex-anchor--herdr-p)
    (condition-case nil
        (herdr-agents)
      (error nil))))

(defun memex-anchor--screen (pane-id)
  "Return the scrollback of PANE-ID, or nil when herdr cannot read it."
  (condition-case nil
      (herdr-pane-text pane-id 'recent memex-anchor-scrollback)
    (error nil)))

(defcustom memex-anchor-takeover t
  "Whether the attached terminal accepts input while its window has focus.
With Herdr's focus-following session stream, control and geometry return
to its foreground client as soon as focus leaves Emacs; the buffer keeps
observing and its searchable scrollback stays intact.  Nil keeps the
Emacs terminal read-only throughout."
  :type 'boolean
  :group 'memex-anchor)

(defun memex-anchor--attach (entry)
  "Attach the terminal of the agent ENTRY and return its buffer, or nil.
The live terminal is the session, so point lands in the conversation
rather than in a copy of it.  A terminal already attached is switched to
rather than attached twice."
  (when (and entry (require 'herdr nil t) (fboundp 'herdr-attach-entry))
    (let ((terminal (alist-get 'terminal_id entry))
          (herdr-attach-takeover memex-anchor-takeover))
      (or (and terminal (fboundp 'herdr-terminal-buffer)
               (herdr-terminal-buffer terminal))
          (condition-case nil
              (herdr-attach-entry
               (if (alist-get 'session entry)
                   entry
                 (cons (cons 'session (and (fboundp 'herdr-session-for)
                                           (herdr-session-for)))
                       entry)))
            (error nil))))))

(defun memex-anchor--goto (buffer text)
  "Put point in BUFFER where TEXT was drawn, returning non-nil when found."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((at (memex-anchor--locate text (buffer-string))))
        (goto-char (+ (point-min) at))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))
        t))))

(defun memex-anchor--panes ()
  "Return every live agent pane as (PANE-ID . SCROLLBACK).
Answers nil where herdr is not to be had, which the caller reports as
the viewer fallback rather than as an error: this runs inside a
response callback, where a signal reaches the user as a failure in a
process sentinel instead of as a message.

A pane herdr cannot read is left out rather than counted as empty,
which would let a pane with no scrollback win by everyone else failing."
  (delq nil
        (mapcar (lambda (agent)
                  (let* ((pane-id (alist-get 'pane_id agent))
                         (screen (and pane-id (memex-anchor--screen pane-id))))
                    (and pane-id screen (cons pane-id screen))))
                (memex-anchor--agents))))

(defun memex-anchor--resume-command (record)
  "Return the command that resumes RECORD's session, or nil without one.
The command is not on the RPC surface, so the herdr bridge's own lookup
is what finds it; that module is optional and its absence is a nil."
  (when (fboundp 'memex-herdr--row)
    (let ((row (memex-herdr--row (alist-get 'session_id record)
                                 (alist-get 'source_path record)
                                 (alist-get 'source record))))
      (when-let* ((command (alist-get 'resume_cmd row)))
        (and (not (string-empty-p command)) (cons command row))))))

(defun memex-anchor--start (record command row)
  "Resume RECORD's session with COMMAND in a herdr pane for ROW.
`memex-herdr--start' is what opens the tab and gets the agent up; this
attaches the pane it answers with, so the resumed session comes up where
the reader is rather than only in the terminal."
  (herdr-start-server-if-needed)
  (memex-anchor--enter record (memex-herdr--start
                               row command (alist-get 'session_id record))))

(defun memex-anchor--resume (record)
  "Start the session RECORD belongs to, no pane being on it already.
A conversation nobody is running is worth starting rather than
answering with a copy of what it used to say.  Anything the start
refuses is reported and answered with the indexed transcript: this runs
inside a response callback, where a signal is swallowed as a failure in
a process sentinel and the user sees nothing happen."
  (condition-case failure
      (pcase (memex-anchor--resume-command record)
        (`(,command . ,row)
         (message "memex anchor: no pane is running this session, resuming it")
         (memex-anchor--start record command row))
        (_ (memex-anchor--fallback
            record "no pane is running this session and memex knows no way to")))
    (error (memex-anchor--fallback record (error-message-string failure)))))

(defun memex-anchor--fallback (record why)
  "Show RECORD in the viewer, saying WHY the live agent was not used."
  (message "memex anchor: %s, showing the indexed transcript" why)
  (memex-view-session (alist-get 'session_id record)
                      (alist-get 'source_path record)
                      (alist-get 'doc_id record)))

(defun memex-anchor--land (buffer record session-id pane-id)
  "Show BUFFER with point where RECORD was drawn, remembering what it is.
SESSION-ID and PANE-ID are stamped on the buffer so a later jump into
the same conversation finds it without resolving anything."
  (with-current-buffer buffer
    (setq-local memex-anchor-session-id session-id)
    (setq-local memex-anchor-pane-id pane-id))
  (pop-to-buffer buffer)
  (unless (memex-anchor--goto buffer (alist-get 'text record))
    (message "memex anchor: %s is running this session but has scrolled past this record"
             pane-id))
  buffer)

(defcustom memex-anchor-target 'terminal
  "Where a record is read once the pane running its session is known.

`transcript' reads it in the viewer, saying which pane is running it.
`terminal' attaches that pane and lands in it.

Attaching costs a herdr new enough to stream a terminal session per
client - `herdr-attach-bridge' - since a direct attach resizes the pane
to the attaching client and locks the herdr UI out of sizing it back.
It replays the current screen and nothing above it either way, so a
record older than that screen is not there to land on and the viewer is
where a session is read further back than its terminal goes."
  :type '(choice (const :tag "The indexed transcript" transcript)
                 (const :tag "The agent's own terminal" terminal))
  :group 'memex-anchor)

(defun memex-anchor--enter (record pane-id)
  "Land on RECORD for the session PANE-ID is running.
`memex-anchor-target' says whether that is the viewer or the pane's own
terminal."
  (if (eq memex-anchor-target 'terminal)
      (let* ((entry (seq-find (lambda (agent)
                                (equal (alist-get 'pane_id agent) pane-id))
                              (memex-anchor--agents)))
             (buffer (memex-anchor--attach (or entry `((pane_id . ,pane-id))))))
        (if (buffer-live-p buffer)
            (memex-anchor--land buffer record (alist-get 'session_id record)
                                pane-id)
          (memex-anchor--fallback
           record (format "herdr would not attach %s" pane-id))))
    (message "memex anchor: %s is running this session" pane-id)
    (memex-view-session (alist-get 'session_id record)
                        (alist-get 'source_path record)
                        (alist-get 'doc_id record))))

(defun memex-anchor--resolve (record tail)
  "Attach the agent running RECORD's session and land on RECORD in it.
TAIL is the end of that session, which is what tells the panes apart
whenever herdr has not been told which session each agent is running."
  (let* ((panes (memex-anchor--panes))
         (pane-id (or (memex-anchor--by-session record)
                      (car (and panes (memex-anchor--best tail panes)))
                      (memex-anchor--by-directory
                       record (cdr (memex-anchor--resume-command record))))))
    (cond
     ((not (memex-anchor--herdr-p))
      (memex-anchor--fallback record "herdr is not available here"))
     ((null pane-id) (memex-anchor--resume record))
     (t (memex-anchor--enter record pane-id)))))

;;;###autoload
(defun memex-anchor-show (record)
  "Open the agent still running RECORD's session, at RECORD.
RECORD is a record alist, which is what the selectors, the viewer and
memex's search all carry.  The agent's own terminal is attached and
point put where it drew RECORD, so the session is entered rather than
copied and can be typed at from where it is read.

A session already attached is switched to without asking memex or herdr
anything.  Otherwise herdr is asked which of its agents reports this
session, and failing that the session's own tail says which pane is
running it.  A session nobody is running is started; one this machine
cannot reach is read in the viewer instead.

`herdr terminal attach' replays the current screen and nothing above it,
so a record the terminal has scrolled past is not in the buffer to land
on.  The viewer is where a session is read further back than its
terminal goes."
  (interactive
   (list (if (derived-mode-p 'memex-session-mode)
             (or (memex-view-record-at-point)
                 (user-error "No record at point"))
           (memex-read-record))))
  (let* ((session-id (alist-get 'session_id record))
         (source-path (alist-get 'source_path record))
         (open (memex-anchor--session-buffer session-id)))
    (if open
        (memex-anchor--land open record session-id
                            (buffer-local-value 'memex-anchor-pane-id open))
      (memex-api-session-page
       session-id source-path
       (lambda (context)
         (let ((total (alist-get 'total context)))
           (memex-api-session-page
            session-id source-path
            (lambda (page)
              (memex-anchor--resolve record
                                     (append (alist-get 'records page) nil)))
            :offset (max 0 (- total memex-anchor-tail))
            :limit memex-anchor-tail)))
       :offset 0 :limit 1))))

;;;###autoload
(defun memex-anchor-setup ()
  "Bind the anchor onto the viewer's map."
  (keymap-set memex-session-mode-map "a" #'memex-anchor-show))

(with-eval-after-load 'memex-view (memex-anchor-setup))
;;;###autoload (with-eval-after-load 'memex-view (memex-anchor-setup))

(provide 'memex-anchor)
;;; memex-anchor.el ends here
