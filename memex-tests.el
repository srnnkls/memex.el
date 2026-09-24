;;; memex-tests.el --- Aggregate suite and live contract for memex -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-tests.el -f ert-run-tests-batch-and-exit
;;
;; Loading this file loads all thirteen per-module suites, so one command runs
;; every test; each suite still runs on its own.  The fixtures record the
;; wire and pin what the client makes of it; the live-contract test is the
;; only one that can see memex change underneath them, and it skips itself
;; when memex is not installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'memex-core)
(require 'memex-api)
(require 'memex-completion)

(require 'memex-core-tests)
(require 'memex-api-tests)
(require 'memex-completion-tests)
(require 'memex-search-tests)
(require 'memex-view-tests)
(require 'memex-status-tests)
(require 'memex-usage-tests)
(require 'memex-evil-tests)
(require 'memex-herdr-tests)
(require 'memex-entry-tests)
(require 'memex-anchor-tests)
(require 'memex-markdown-tests)
(require 'memex-embark-tests)
(require 'memex-org-tests)

(defconst memex-tests-record-keys '(doc_id project session_id source_path)
  "Record keys memex 0.11.6 puts on every record it sends.
Measured across 5000 `recent' records spanning the claude, codex and pi
sources: all four present on every one, `doc_id' an integer on every
one.  `Record' itself marks more than the tool fields optional, so the
client still tolerates their absence; this is what memex sends today,
and drift away from it is what the live-contract test looks for.")

(defconst memex-tests-open-claw-record
  '((source . "open-claw")
    (doc_id . 388401)
    (ts . 1787140243301)
    (project . "memex.el")
    (session_id . "caea32e0-f5ad-4906-9f1f-b9b7047bd7a1")
    (role . "assistant")
    (text . "the open-claw source is kebab-case on the wire")
    (event_id . "evt-9f31")
    (parent_event_id . "evt-9f30")
    (conversation_kind . "chat")
    (source_path . "/tmp/memex-tests/caea32e0.jsonl"))
  "A record from the source whose wire name and CLI name differ.
`open-claw' is what serde emits; `openclaw' is what the CLI takes, and
sending it back as a `source' filter is rejected.  The link fields are
flattened onto the record rather than nested under `links'.")

(defconst memex-tests-colliding-records
  '(((source . "codex")
     (doc_id . 388402)
     (ts . 1787140243302)
     (project . "memex.el")
     (session_id . "3b71c8de-2a04-4d59-8f7c-1c0b6d2e5a11")
     (role . "user")
     (text . "run the suite")
     (source_path . "/tmp/memex-tests/3b71c8de.jsonl"))
    ((source . "codex")
     (doc_id . 388403)
     (ts . 1787140243302)
     (project . "memex.el")
     (session_id . "3b71c8de-2a04-4d59-8f7c-1c0b6d2e5a11")
     (role . "user")
     (text . "run the suite")
     (source_path . "/tmp/memex-tests/3b71c8de-two.jsonl")))
  "Two records a picker would label identically.
Every field a candidate shows agrees; only `doc_id' and `source_path'
differ, so a label built from the visible fields alone collapses one
onto the other and the selection returns the wrong record.")

(defconst memex-tests-records-response
  (json-serialize
   `((protocol . 1)
     (response
      . ((kind . "records")
         (records
          . ,(vconcat
              (cl-loop for record in (cons memex-tests-open-claw-record
                                           memex-tests-colliding-records)
                       for score downfrom 99.0 by 1.0
                       collect (vector score record))))))))
  "A recorded `records' payload, the answer `search' and `recent' share.
Each entry is the two-element array memex sends, score first, and not
an object with a score field.")

(defvar memex-tests--dir nil)

(defun memex-tests--stub (body)
  "Write BODY as a stub memex executable and return its path."
  (unless memex-tests--dir
    (setq memex-tests--dir (make-temp-file "memex-tests" t)))
  (let ((path (expand-file-name "memex-stub" memex-tests--dir)))
    (with-temp-file path (insert "#!/bin/sh\n" body))
    (set-file-modes path #o755)
    path))

(defun memex-tests--cleanup ()
  "Remove the temporary directory and any stub process left behind."
  (dolist (process (process-list))
    (when (string-match-p "memex" (process-name process))
      (delete-process process)))
  (when (and memex-tests--dir (file-directory-p memex-tests--dir))
    (delete-directory memex-tests--dir t))
  (setq memex-tests--dir nil))

(defun memex-tests--answering (response)
  "Return a stub printing RESPONSE and reading its request away."
  (memex-tests--stub
   (format "cat > /dev/null\nprintf '%%s' %s\n"
           (shell-quote-argument response))))

(defun memex-tests--await (invoke)
  "Start one request with INVOKE and return what its callback was handed.
INVOKE is called with a callback and an errback."
  (let ((payload 'pending)
        (failure nil))
    (funcall invoke
             (lambda (value) (setq payload value))
             (lambda (err) (setq failure err)))
    (should (memex-core-tests--wait
             (lambda () (or failure (not (eq payload 'pending))))))
    (should-not failure)
    payload))

(defun memex-tests--assert-records (pairs)
  "Assert PAIRS carries the records shape the client is written against.
PAIRS is what a `records' callback is handed: score and record, in that
order, in a two-element sequence."
  (should pairs)
  (dolist (pair pairs)
    (should (numberp (nth 0 pair)))
    (let ((record (nth 1 pair)))
      (should (consp record))
      (dolist (key memex-tests-record-keys)
        (should (assq key record)))
      (should (integerp (alist-get 'doc_id record)))
      (should (stringp (alist-get 'source_path record)))
      (should (stringp (alist-get 'session_id record))))))

(ert-deftest memex-tests-the-recorded-payload-reaches-the-client-intact ()
  "The recording decodes into the shape the client is written against.
The kebab-case source survives, the flattened link fields stay on the
record, and the two records a picker would label identically still
reach it as two candidates that hand back the record they stand for."
  (unwind-protect
      (let* ((memex-executable
              (memex-tests--answering memex-tests-records-response))
             (pairs (memex-tests--await
                     (lambda (cb eb) (memex-api-recent cb :errback eb))))
             (records (mapcar (lambda (pair) (nth 1 pair)) pairs)))
        (memex-tests--assert-records pairs)
        (should (equal (alist-get 'source (car records)) "open-claw"))
        (should (equal (alist-get 'event_id (car records)) "evt-9f31"))
        (let* ((candidates (memex-completion-record-candidates
                            memex-tests-colliding-records))
               (labels (mapcar #'substring-no-properties candidates)))
          (should (equal (length candidates) 2))
          (should (equal (length (delete-dups (copy-sequence labels))) 2))
          (should (equal (mapcar (lambda (candidate)
                                   (alist-get 'doc_id
                                              (memex-completion-record-of
                                               candidate)))
                                 candidates)
                         '(388402 388403)))))
    (memex-tests--cleanup)))

(ert-deftest memex-tests-live-memex-still-answers-the-recorded-shape ()
  "The installed memex answers `ping' and `recent' as the fixtures record.
This is the half a recording cannot cover: the fixtures pin what the
client does with a payload, and only the binary can say whether memex
still sends that payload.  The shape can only be read off a record, and
memex answers `recent' against an empty index with an empty array and
exit 0, so an unindexed machine retires this test rather than failing
it."
  (unless (executable-find memex-executable)
    (ert-skip "memex is not installed"))
  (let ((version (memex-tests--await
                  (lambda (cb eb) (memex-api-ping cb :errback eb)))))
    (should (stringp version))
    (should (string-match-p "\\`[0-9]+\\.[0-9]+" version)))
  (let ((pairs (memex-tests--await
                (lambda (cb eb) (memex-api-recent cb :limit 25 :errback eb)))))
    (when (zerop (length pairs))
      (ert-skip "memex has nothing indexed"))
    (memex-tests--assert-records pairs)))

(ert-deftest memex-tests-resume-without-the-binary-names-it-not-the-window ()
  "A missing memex is reported as missing, not as a lookup window too short.
The session lookup shells memex out, so with nothing to run it comes
back empty and reaches the same outcome an out-of-window session
reaches: raise `memex-resume-lookup-limit', which would not help.  The
readiness check is where the two part, and it must part before anything
is run."
  (let* ((path (make-temp-file "memex-tests-" nil ".jsonl"))
         (record (memex-herdr-tests--record path))
         (memex-herdr-tests--installed nil))
    (unwind-protect
        (memex-herdr-tests--run "[]"
          (memex-herdr-tests--reporting (memex-herdr-resume record))
          (should (null (memex-herdr-tests--of 'shell)))
          (should (null (memex-herdr-tests--of 'tab-create)))
          (should (null (memex-herdr-tests--of 'send-text)))
          (should (memex-herdr-tests--reported-p memex-executable))
          (should-not (memex-herdr-tests--reported-p
                       "memex-resume-lookup-limit")))
      (delete-file path))))

(provide 'memex-tests)
;;; memex-tests.el ends here
