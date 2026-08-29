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
(declare-function memex-anchor--by-directory "memex-anchor")
(declare-function memex-anchor--by-session "memex-anchor")
(declare-function memex-anchor--agents "memex-anchor")
(declare-function memex-anchor--session-buffer "memex-anchor")
(declare-function memex-anchor--goto "memex-anchor")
(declare-function memex-anchor--land "memex-anchor")
(declare-function memex-anchor--resolve "memex-anchor")
(declare-function memex-anchor-resume "memex-anchor")
(declare-function memex-herdr-resume "memex-herdr")
(declare-function memex-anchor--resume "memex-anchor")
(declare-function memex-anchor--resume-command "memex-anchor")
(declare-function memex-anchor--start "memex-anchor")
(declare-function memex-anchor-setup "memex-anchor")
(declare-function memex-anchor--enter "memex-anchor")
(declare-function memex-anchor--attach "memex-anchor")
(defvar memex-anchor-takeover)
(defvar memex-anchor-target)
(defvar herdr-attach-takeover)
(declare-function memex-anchor--herdr-p "memex-anchor")
(declare-function memex-anchor--locate-reduced "memex-anchor")

(defvar memex-anchor-window)

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

(ert-deftest memex-anchor-binds-itself-onto-the-viewer ()
  "Reading a record and jumping to the agent still running it is one
motion, so the verb belongs on the viewer's own map.  Not on `j': evil
normal state spends that on `next-line'."
  (require 'memex-view)
  (memex-anchor-setup)
  (should (eq (keymap-lookup memex-session-mode-map "a") #'memex-anchor-show)))

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

(ert-deftest memex-anchor-resume-always-targets-the-live-terminal ()
  (let ((memex-anchor-target 'transcript)
        seen)
    (cl-letf (((symbol-function 'memex-anchor--resolve-with-resume)
               (lambda (_record _resume)
                 (setq seen memex-anchor-target))))
      (memex-anchor-resume '((session_id . "sess-1")) '("resume")))
    (should (eq seen 'terminal))
    (should (eq memex-anchor-target 'transcript))))

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
        (memex-anchor--resolve record)
        (should (equal (car started) record))
        (should (equal (cdr started) "claude --resume x"))))))

(ert-deftest memex-anchor-shows-the-transcript-when-nothing-can-resume ()
  "Resuming needs a command memex recorded, and old sessions have none."
  (let ((shown nil))
    (cl-letf (((symbol-function 'memex-view-session)
               (lambda (&rest args) (setq shown args))))
      (memex-anchor--resume '((session_id . "sess-1") (text . "x")) nil))
    (should shown)))

(ert-deftest memex-anchor-reports-a-start-that-refuses ()
  "This runs inside a response callback, where a signal is swallowed as a
process-sentinel failure and the user sees nothing happen at all."
  (let ((shown nil) (said nil))
    (cl-letf (((symbol-function 'memex-anchor--start)
               (lambda (&rest _) (user-error "Herdr is not installed")))
              ((symbol-function 'memex-view-session)
               (lambda (&rest args) (setq shown args)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (memex-anchor--resume '((session_id . "sess-1") (text . "x"))
                            (cons "claude --resume x" nil)))
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
  "An exact ID or transcript path on both sides is the whole join."
  (let ((record '((session_id . "abc-123")
                  (source_path . "/tmp/abc-123.jsonl"))))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda ()
                 '(((pane_id . "w1:p1")
                    (agent_session . ((kind . "id") (value . "other"))))
                   ((pane_id . "w2:p1")
                    (agent_session . ((kind . "id") (value . "abc-123"))))))))
      (should (equal (memex-anchor--by-session record) "w2:p1")))
    (cl-letf (((symbol-function 'memex-anchor--agents)
               (lambda ()
                 '(((pane_id . "w3:p1")
                    (agent_session . ((kind . "path")
                                      (value . "/tmp/abc-123.jsonl"))))))))
      (should (equal (memex-anchor--by-session record) "w3:p1")))
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

(ert-deftest memex-anchor-resolves-an-exact-session-without-reading-pane-screens ()
  (let ((entered nil)
        (record '((session_id . "abc-123") (source_path . "/tmp/a.jsonl"))))
    (cl-letf (((symbol-function 'memex-anchor--herdr-p) (lambda () t))
              ((symbol-function 'memex-anchor--by-session) (lambda (_record) "w2:p1"))
              ((symbol-function 'herdr-pane-text)
               (lambda (&rest _) (error "read pane screen")))
              ((symbol-function 'memex-anchor--enter)
               (lambda (seen pane) (setq entered (cons seen pane)))))
      (memex-anchor--resolve record)
      (should (equal entered (cons record "w2:p1"))))))

(ert-deftest memex-anchor-show-resolves-without-fetching-a-transcript-tail ()
  (let ((record '((session_id . "abc-123") (source_path . "/tmp/a.jsonl")))
        (resolved nil))
    (cl-letf (((symbol-function 'memex-anchor--session-buffer) (lambda (_id) nil))
              ((symbol-function 'memex-api-session-page)
               (lambda (&rest _) (error "fetched transcript tail")))
              ((symbol-function 'memex-anchor--resolve)
               (lambda (seen &optional _tail) (setq resolved seen))))
      (memex-anchor-show record)
      (should (equal resolved record)))))

(ert-deftest memex-anchor-computes-resume-data-once ()
  (let ((lookups 0)
        (record '((session_id . "abc-123") (source_path . "/tmp/a.jsonl"))))
    (cl-letf (((symbol-function 'memex-anchor--herdr-p) (lambda () t))
              ((symbol-function 'memex-anchor--by-session) (lambda (_record) nil))
              ((symbol-function 'memex-anchor--agents) (lambda () nil))
              ((symbol-function 'memex-anchor--resume-command)
               (lambda (_record)
                 (cl-incf lookups)
                 (cons "claude --resume abc-123" '((cwd . "/tmp")))))
              ((symbol-function 'memex-anchor--start) (lambda (&rest _) nil)))
      (memex-anchor--resolve record)
      (should (= lookups 1)))))

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
