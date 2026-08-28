;;; memex-anchor-tests.el --- Tests for memex-anchor.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-anchor-tests.el -f ert-run-tests-batch-and-exit
;;
;; `memex-anchor-tests--record' and `memex-anchor-tests--rendered' are one
;; message as memex indexed it and as Claude Code drew it, so what the
;; matching assertions survive is what a terminal does rather than what a
;; fixture was written to allow.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'memex-tests-support)
(require 'memex-anchor nil t)

(declare-function memex-anchor--skeleton "memex-anchor")
(declare-function memex-anchor--locate "memex-anchor")
(declare-function memex-anchor-on-screen-p "memex-anchor")
(declare-function memex-anchor--hits "memex-anchor")
(declare-function memex-anchor--drawn "memex-anchor")
(declare-function memex-anchor--by-directory "memex-anchor")
(declare-function memex-anchor--by-session "memex-anchor")
(declare-function memex-anchor--agents "memex-anchor")
(declare-function memex-anchor--best "memex-anchor")
(declare-function memex-anchor--session-buffer "memex-anchor")
(declare-function memex-anchor--goto "memex-anchor")
(declare-function memex-anchor--land "memex-anchor")
(declare-function memex-anchor--resolve "memex-anchor")
(declare-function memex-herdr-resume "memex-herdr")
(declare-function memex-anchor--resume "memex-anchor")
(declare-function memex-anchor--resume-command "memex-anchor")
(declare-function memex-anchor--start "memex-anchor")
(declare-function memex-anchor-setup "memex-anchor")
(declare-function memex-anchor--panes "memex-anchor")
(declare-function memex-anchor--enter "memex-anchor")
(declare-function memex-anchor--attach "memex-anchor")
(defvar memex-anchor-takeover)
(defvar memex-anchor-target)
(defvar herdr-attach-takeover)
(declare-function memex-anchor--herdr-p "memex-anchor")
(declare-function memex-anchor--locate-reduced "memex-anchor")

(defvar memex-anchor-window)
(defvar memex-anchor-minimum-hits)
(defvar memex-anchor-minimum-margin)

(defconst memex-anchor-tests--record
  "The `herdr-status` scope is ready: all three round-13 reviewers passed \
with zero findings, and YAML/task-graph validation passed."
  "A message as memex indexed it, markdown intact.")

(defconst memex-anchor-tests--rendered
  "⏺ The herdr-status scope is ready: all three round-13 reviewers passed \
with zero findings, and YAML/task-graph validation passed.\n\n  - Review \
gate: scopes/draft/h"
  "The same message as the terminal drew it, backticks gone.")


(ert-deftest memex-anchor-skeleton-keeps-only-letters-and-maps-them-back ()
  "Every skeleton character indexes the position it came from."
  (pcase-let* ((`(,skel . ,map) (memex-anchor--skeleton "a-B 3c!")))
    (should (equal skel "abc"))
    (should (equal (aref map 0) 0))
    (should (equal (aref map 1) 2))
    (should (equal (aref map 2) 5))))

(ert-deftest memex-anchor-skeleton-drops-digits ()
  "Digits go, because diff gutters inject line numbers mid-span."
  (should (equal (car (memex-anchor--skeleton "53 + review_gate")) "reviewgate")))

(ert-deftest memex-anchor-skeleton-of-empty-text-is-empty ()
  (pcase-let ((`(,skel . ,map) (memex-anchor--skeleton "")))
    (should (equal skel ""))
    (should (equal (length map) 0))))


(ert-deftest memex-anchor-locate-finds-a-record-the-terminal-restyled ()
  "Markdown punctuation the TUI stripped does not prevent a match."
  (should (equal (memex-anchor--locate memex-anchor-tests--record
                                       memex-anchor-tests--rendered)
                 (string-search "The herdr-status"
                                memex-anchor-tests--rendered))))

(ert-deftest memex-anchor-locate-survives-a-word-split-across-a-column ()
  "A narrow column splits words mid-token; letters-only matching rejoins them."
  (let ((pane "  described wrong behavio\n     +r-directed cap on the review"))
    (should (memex-anchor--locate "described wrong behavior-directed cap on the review"
                                  pane))))

(ert-deftest memex-anchor-locate-refuses-unrelated-text ()
  (should-not (memex-anchor--locate memex-anchor-tests--record
                                    "totally different scrollback about cabbages")))

(ert-deftest memex-anchor-locate-refuses-text-shorter-than-it-can-judge ()
  "Too short to be distinctive is a refusal, not a match on a common word."
  (should-not (memex-anchor--locate "the scope" "the scope is ready")))

(ert-deftest memex-anchor-locate-probes-past-a-truncated-head ()
  "A record whose opening was cut still matches on a later window."
  (let* ((tail (concat "and then the deployment pipeline rewrote every overlay "
                       "entry before the lock was released"))
         (record (concat "PREAMBLE THE TERMINAL NEVER DREW: " tail)))
    (should (memex-anchor--locate record (concat "  " tail "\n")))))

(ert-deftest memex-anchor-on-screen-p-answers-without-a-position ()
  (should (memex-anchor-on-screen-p memex-anchor-tests--record
                                    memex-anchor-tests--rendered))
  (should-not (memex-anchor-on-screen-p memex-anchor-tests--record
                                        "unrelated scrollback")))


(ert-deftest memex-anchor-hits-counts-the-messages-drawn-on-screen ()
  "Records too short to judge are not counted either way."
  (let ((records `(((role . "assistant") (text . ,memex-anchor-tests--record))
                   ((role . "assistant")
                    (text . "nothing here matches the pane at all, not one bit"))
                   ((role . "assistant") (text . "short")))))
    (should (equal (memex-anchor--hits records memex-anchor-tests--rendered)
                   1))))

(ert-deftest memex-anchor-counts-only-what-a-terminal-draws ()
  "A tool call is drawn as a summary of itself and its result as a fold,
so neither is ever on screen the way memex recorded it.  Counting them
made every pane look like it was running nothing: the pane on this very
session scored 0.03 with them in and 0.14 with them out."
  (let ((call `((role . "tool_use") (tool_name . "Bash")
                (text . ,memex-anchor-tests--record)))
        (message `((role . "assistant") (text . ,memex-anchor-tests--record)))
        (injected `((role . "user")
                    (text . ,(concat "<system-reminder> "
                                     memex-anchor-tests--record)))))
    (should (equal (mapcar (lambda (r) (alist-get 'role r))
                           (memex-anchor--drawn (list call message injected)))
                   '("assistant")))
    (should (equal (memex-anchor--hits (list call)
                                       memex-anchor-tests--rendered)
                   0))))

(ert-deftest memex-anchor-hits-of-nothing-judgeable-is-zero ()
  (should (equal (memex-anchor--hits '(((role . "assistant") (text . "tiny")))
                                     "whatever")
                 0)))

(ert-deftest memex-anchor-adopts-the-one-agent-working-where-the-session-did ()
  "A background agent keeps a couple of thousand characters of a
condensed view, so what it holds has usually scrolled past whatever
memex has indexed and no text matches.  One agent of the right kind in
the session's own directory is the answer then; two is not."
  (let ((record '((source . "claude")))
        (row '((cwd . "/tmp/one"))))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda () '(((pane_id . "w1:p1") (agent . "claude")
                             (cwd . "/tmp/one"))
                            ((pane_id . "w2:p1") (agent . "claude")
                             (cwd . "/tmp/other"))))))
      (should (equal (memex-anchor--by-directory record row) "w1:p1")))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda () '(((pane_id . "w1:p1") (agent . "claude")
                             (cwd . "/tmp/one"))
                            ((pane_id . "w2:p1") (agent . "claude")
                             (cwd . "/tmp/one"))))))
      (should-not (memex-anchor--by-directory record row)))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda () '(((pane_id . "w1:p1") (agent . "codex")
                             (cwd . "/tmp/one"))))))
      (should-not (memex-anchor--by-directory record row)))))

(ert-deftest memex-anchor-best-picks-the-pane-the-session-is-running-in ()
  (let* ((records `(((role . "assistant") (text . ,memex-anchor-tests--record))))
         (panes `(("w1:p1" . "unrelated scrollback about cabbages and rain")
                  ("w2:p1" . ,memex-anchor-tests--rendered))))
    (should (equal (car (memex-anchor--best records panes)) "w2:p1"))))

(ert-deftest memex-anchor-best-abstains-when-no-pane-clears-the-rate ()
  "Nothing on screen is answered with nothing, never with a best guess."
  (let ((records `(((role . "assistant") (text . ,memex-anchor-tests--record))))
        (panes '(("w1:p1" . "unrelated scrollback about cabbages and rain"))))
    (should-not (memex-anchor--best records panes))))

(ert-deftest memex-anchor-best-abstains-when-two-panes-tie ()
  "A tie is the shape a wrong answer takes, so it is refused outright.
Measured live, a resumed pane scored 24% for a session another pane
scored 24% for; the rate alone would have picked one of them."
  (let* ((records `(((role . "assistant") (text . ,memex-anchor-tests--record))))
         (panes `(("w1:p1" . ,memex-anchor-tests--rendered)
                  ("w2:p1" . ,memex-anchor-tests--rendered))))
    (should-not (memex-anchor--best records panes))))

(ert-deftest memex-anchor-best-requires-the-margin-not-only-the-rate ()
  "A runner-up above half the winner's rate is not a decision."
  (let* ((hit `((role . "assistant") (text . ,memex-anchor-tests--record)))
         (miss '((role . "assistant")
                 (text . "nothing on either pane matches this line of text")))
         (records (list hit hit hit miss))
         (near (concat memex-anchor-tests--rendered "\n"))
         (panes `(("w1:p1" . ,near) ("w2:p1" . ,near))))
    (should-not (memex-anchor--best records panes))))


(ert-deftest memex-anchor-binds-itself-onto-the-viewer ()
  "Reading a record and jumping to the agent still running it is one
motion, so the verb belongs on the viewer's own map.  Not on `j': evil
normal state spends that on `next-line'."
  (require 'memex-view)
  (memex-anchor-setup)
  (should (eq (keymap-lookup memex-session-mode-map "a") #'memex-anchor-show)))

(ert-deftest memex-anchor-without-herdr-answers-no-panes ()
  "The lookup runs inside a response callback, where a signal reaches the
user as a failure in a process sentinel rather than as a message."
  (cl-letf (((symbol-function 'memex-anchor--herdr-p) (lambda () nil)))
    (should-not (memex-anchor--panes))))

(ert-deftest memex-anchor-reduces-a-pane-once-not-once-per-record ()
  "Reducing a pane costs the length of the pane, so doing it per record
multiplies a 60k screen by the length of the tail.  Measured before this
was fixed, one lookup over 15 panes took tens of seconds."
  (let ((calls 0)
        (records (mapcar (lambda (n)
                           `((role . "assistant")
                             (text . ,(format "record %d with enough letters to judge it" n))))
                         (number-sequence 1 20))))
    (cl-letf* ((original (symbol-function 'memex-anchor--skeleton))
               ((symbol-function 'memex-anchor--skeleton)
                (lambda (text) (setq calls (1+ calls)) (funcall original text))))
      (memex-anchor--hits records "a screen showing nothing in particular"))
    (should (<= calls (1+ (* 2 (length records)))))))

(ert-deftest memex-anchor-goto-puts-point-where-the-terminal-drew-the-record ()
  "The live terminal's scrollback is its buffer's own text, so a record
is reached by moving point in the session rather than by copying it."
  (with-temp-buffer
    (insert "earlier output\n" memex-anchor-tests--rendered "\nlater output\n")
    (should (memex-anchor--goto (current-buffer) memex-anchor-tests--record))
    (should (looking-at-p "The herdr-status scope is ready"))))

(ert-deftest memex-anchor-goto-answers-nil-for-a-record-not-on-screen ()
  (with-temp-buffer
    (insert "nothing to do with it")
    (should-not (memex-anchor--goto (current-buffer)
                                    memex-anchor-tests--record))))

(ert-deftest memex-anchor-finds-a-session-already-attached ()
  "A second jump into a conversation already on screen resolves nothing."
  (let ((buffer (generate-new-buffer "*fake terminal*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local memex-anchor-session-id "sess-1")
            (setq-local memex-anchor-pane-id "w9:p9"))
          (should (eq (memex-anchor--session-buffer "sess-1") buffer))
          (should-not (memex-anchor--session-buffer "sess-2")))
      (kill-buffer buffer))))

(ert-deftest memex-anchor-forgets-a-session-whose-terminal-was-detached ()
  "The memory is derived from live buffers, so closing one clears it."
  (let ((buffer (generate-new-buffer "*fake terminal*")))
    (with-current-buffer buffer (setq-local memex-anchor-session-id "sess-1"))
    (kill-buffer buffer)
    (should-not (memex-anchor--session-buffer "sess-1"))))

(ert-deftest memex-anchor-land-stamps-the-buffer-it-landed-in ()
  "What was resolved once is remembered on the buffer, which is what the
fast path reads on the next jump."
  (let ((buffer (generate-new-buffer "*fake terminal*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert memex-anchor-tests--rendered))
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (memex-anchor--land buffer
                                `((text . ,memex-anchor-tests--record))
                                "sess-1" "w9:p9"))
          (should (equal (buffer-local-value 'memex-anchor-session-id buffer)
                         "sess-1"))
          (should (equal (buffer-local-value 'memex-anchor-pane-id buffer)
                         "w9:p9")))
      (kill-buffer buffer))))

(ert-deftest memex-anchor-starts-a-session-no-pane-is-running ()
  "A conversation nobody is running is worth starting, not worth
answering with a read-only copy of what it used to say."
  (let ((started nil))
    (cl-letf (((symbol-function 'memex-anchor--resume-command)
               (lambda (_record) (cons "claude --resume x" '((cwd . "/tmp")))))
              ((symbol-function 'memex-anchor--start)
               (lambda (record command _row) (setq started (cons record command))))
              ((symbol-function 'memex-anchor--agents) (lambda () nil))
              ((symbol-function 'memex-anchor--herdr-p) (lambda () t)))
      (let ((record '((session_id . "sess-1") (text . "whatever"))))
        (memex-anchor--resolve record nil)
        (should (equal (car started) record))
        (should (equal (cdr started) "claude --resume x"))))))

(ert-deftest memex-anchor-shows-the-transcript-when-nothing-can-resume ()
  "Resuming needs a command memex recorded, and old sessions have none."
  (let ((shown nil))
    (cl-letf (((symbol-function 'memex-anchor--resume-command) (lambda (_r) nil))
              ((symbol-function 'memex-view-session)
               (lambda (&rest args) (setq shown args))))
      (memex-anchor--resume '((session_id . "sess-1") (text . "x"))))
    (should shown)))

(ert-deftest memex-anchor-reports-a-start-that-refuses ()
  "This runs inside a response callback, where a signal is swallowed as a
process-sentinel failure and the user sees nothing happen at all."
  (let ((shown nil) (said nil))
    (cl-letf (((symbol-function 'memex-anchor--resume-command)
               (lambda (_r) (cons "claude --resume x" nil)))
              ((symbol-function 'memex-anchor--start)
               (lambda (&rest _) (user-error "Herdr is not installed")))
              ((symbol-function 'memex-view-session)
               (lambda (&rest args) (setq shown args)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (memex-anchor--resume '((session_id . "sess-1") (text . "x"))))
    (should shown)
    (should (string-search "Herdr is not installed" said))))

(ert-deftest memex-anchor-enters-the-agents-own-terminal ()
  "The attached terminal is the session; a copy of what herdr remembers
of it is a third thing that looks like the session and is not one.  A
record the terminal has scrolled past is said to be gone rather than
conjured back."
  (let ((terminal (generate-new-buffer "*herdr: test*"))
        (memex-anchor-target 'terminal)
        (attached nil)
        (said nil))
    (unwind-protect
        (cl-letf (((symbol-function 'memex-anchor--agents)
                   (lambda () '(((pane_id . "w1:p1") (terminal_id . "t1")))))
                  ((symbol-function 'memex-anchor--attach)
                   (lambda (entry) (setq attached entry) terminal))
                  ((symbol-function 'message)
                   (lambda (format &rest arguments)
                     (push (apply #'format format arguments) said)))
                  ((symbol-function 'pop-to-buffer) (lambda (b &rest _) b)))
          (with-current-buffer terminal
            (insert "a line the terminal is still showing, long enough to judge"))
          (memex-anchor--enter
           '((session_id . "s1")
             (text . "a line the terminal is still showing, long enough to judge"))
           "w1:p1")
          (should (equal (alist-get 'terminal_id attached) "t1"))
          (should (equal (buffer-local-value 'memex-anchor-session-id terminal)
                         "s1"))
          (should (equal (buffer-local-value 'memex-anchor-pane-id terminal)
                         "w1:p1"))
          (should-not said)
          (memex-anchor--enter
           '((session_id . "s1")
             (text . "a line the terminal scrolled past a long time ago now"))
           "w1:p1")
          (should (seq-some (lambda (line) (string-search "scrolled past" line))
                            said)))
      (kill-buffer terminal))))

(ert-deftest memex-anchor-land-moves-a-window-nobody-selected ()
  "A window carries a point of its own, so landing has to move the
window and not only the buffer: every jump after the first went to a
scrollback already on screen and stayed at the head of it."
  (let ((terminal (generate-new-buffer "*herdr: test land*"))
        (other (get-buffer-create "*memex anchor tests elsewhere*"))
        (line "the reviewer found nothing worth blocking on here"))
    (unwind-protect
        (progn
          (with-current-buffer terminal
            (insert (make-string 400 ?x) "\n" line "\n" (make-string 400 ?y)))
          (set-window-buffer (selected-window) other)
          (set-window-buffer (split-window) terminal)
          (memex-anchor--land terminal `((text . ,line)) "s1" "w1:p1")
          (with-current-buffer terminal
            (should (equal (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           line))
            (dolist (window (get-buffer-window-list terminal nil t))
              (should (equal (window-point window) (point))))))
      (kill-buffer terminal)
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest memex-anchor-joins-a-pane-on-the-session-it-reports ()
  "An identifier on both sides is the whole join; the screens are the
fallback for a herdr nobody has told which session an agent is running.
herdr answers with an object naming what kind of thing it holds, and a
pane running an agent that never reported one carries no object at all,
so both have to read as no match rather than as a match against nil."
  (let ((record '((session_id . "abc-123"))))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda ()
                 '(((pane_id . "w1:p1")
                    (agent_session . ((kind . "id") (value . "other"))))
                   ((pane_id . "w2:p1")
                    (agent_session . ((kind . "id") (value . "abc-123"))))))))
      (should (equal (memex-anchor--by-session record) "w2:p1")))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda () '(((pane_id . "w1:p1")) ((pane_id . "w2:p1"))))))
      (should-not (memex-anchor--by-session record)))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda ()
                 '(((pane_id . "w1:p1")
                    (agent_session . ((kind . "title") (value . "abc-123"))))))))
      (should-not (memex-anchor--by-session record)))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda () '(((pane_id . "w1:p1"))))))
      (should-not (memex-anchor--by-session '((session_id . nil)))))))

(ert-deftest memex-anchor-attaches-with-focus-following-control ()
  "The terminal accepts input while selected, then releases control and
geometry when focus leaves; an explicit nil still asks for observation."
  (let ((seen 'unset))
    (cl-letf (((symbol-function 'herdr-attach-entry)
               (lambda (_entry) (setq seen herdr-attach-takeover) nil))
              ((symbol-function 'herdr-terminal-buffer) (lambda (_id) nil))
              ((symbol-function 'herdr-session-for) (lambda (&rest _) "s"))
              ((symbol-function 'require) (lambda (&rest _) t)))
      (let ((herdr-attach-takeover nil))
        (memex-anchor--attach '((pane_id . "w1:p1") (terminal_id . "t1")))
        (should seen)
        (should-not herdr-attach-takeover))
      (let ((herdr-attach-takeover t)
            (memex-anchor-takeover nil))
        (memex-anchor--attach '((pane_id . "w1:p1") (terminal_id . "t1")))
        (should-not seen)))))

(ert-deftest memex-anchor-reads-a-record-without-attaching-by-default ()
  "A pty carries one size, so attaching from Emacs resizes the pane
under whoever is at the real terminal, and the replay it starts from
holds only the current screen.  Reading is worth neither, so it goes to
the viewer - which says which pane is running the session all the same."
  (let ((shown nil)
        (said nil)
        (memex-anchor-target 'transcript))
    (cl-letf (((symbol-function 'memex-view-session)
               (lambda (id path &optional doc &rest _)
                 (setq shown (list id path doc))))
              ((symbol-function 'memex-anchor--attach)
               (lambda (&rest _) (error "attached when it should not have")))
              ((symbol-function 'message)
               (lambda (format &rest arguments)
                 (push (apply #'format format arguments) said))))
      (memex-anchor--enter '((session_id . "s1") (source_path . "/tmp/s.jsonl")
                             (doc_id . 42) (text . "whatever"))
                           "w1:p1")
      (should (equal shown '("s1" "/tmp/s.jsonl" 42)))
      (should (seq-some (lambda (line) (string-search "w1:p1" line)) said)))))

(provide 'memex-anchor-tests)
;;; memex-anchor-tests.el ends here
