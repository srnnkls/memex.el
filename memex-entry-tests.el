;;; memex-entry-tests.el --- Tests for the entry model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-entry-tests.el -f ert-run-tests-batch-and-exit
;;
;; An entry is a call and what it returned, which memex sends as two
;; records joined by `parent_tool_use_id'.  The fixtures carry the field
;; names and shapes memex actually sends, taken from a transcript.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(require 'memex-tests-support)
(require 'memex-entry nil t)

(declare-function memex-entry-pair "memex-entry")
(declare-function memex-entry-call "memex-entry")
(declare-function memex-entry-result "memex-entry")
(declare-function memex-entry-duration "memex-entry")
(declare-function memex-entry-description "memex-entry")
(declare-function memex-entry-status "memex-entry")
(declare-function memex-entry-kind "memex-entry")
(declare-function memex-entry-metadata "memex-entry")
(declare-function memex-entry-payload "memex-entry")
(declare-function memex-entry-fields "memex-entry")

(defconst memex-entry-tests--call
  '((doc_id . 172234) (ts . 1785392873761) (role . "tool_use")
    (tool_name . "Bash") (event_id . "toolu_01MX")
    (tool_input . "{\"command\": String(\"ls -la\"), \"description\": String(\"List repo structure\")}")
    (text . "{\"command\": String(\"ls -la\"), \"description\": String(\"List repo structure\")}"))
  "A tool call as memex sends it, its text a copy of its input.")

(defconst memex-entry-tests--result
  '((doc_id . 172270) (ts . 1785392875150) (role . "tool_result")
    (tool_name . "Bash") (parent_tool_use_id . "toolu_01MX")
    (tool_output . "one\ntwo\nthree") (text . "one\ntwo\nthree"))
  "The result of that call, joined to it by `parent_tool_use_id'.")

(defconst memex-entry-tests--message
  '((doc_id . 172215) (ts . 1785392866000) (role . "assistant")
    (text . "a plain message"))
  "A record that is nobody's call and nobody's result.")


(ert-deftest memex-entry-pairs-a-result-with-the-call-it-answered ()
  "A call and its result are one entry: memex sends them as two records
joined by `parent_tool_use_id', and shown apart they read as two."
  (let ((entries (memex-entry-pair (list memex-entry-tests--call
                                         memex-entry-tests--result))))
    (should (equal (length entries) 1))
    (should (equal (memex-entry-call (car entries)) memex-entry-tests--call))
    (should (equal (memex-entry-result (car entries)) memex-entry-tests--result))))

(ert-deftest memex-entry-keeps-a-message-as-an-entry-of-its-own ()
  (let ((entries (memex-entry-pair (list memex-entry-tests--message
                                         memex-entry-tests--call
                                         memex-entry-tests--result))))
    (should (equal (length entries) 2))
    (should (equal (memex-entry-call (car entries)) memex-entry-tests--message))
    (should-not (memex-entry-result (car entries)))))

(ert-deftest memex-entry-keeps-an-orphan-result-rather-than-dropping-it ()
  "A transcript can begin mid-conversation, its first result answering a
call that is not in the page."
  (let ((entries (memex-entry-pair (list memex-entry-tests--result))))
    (should (equal (length entries) 1))
    (should (equal (memex-entry-call (car entries)) memex-entry-tests--result))))

(ert-deftest memex-entry-keeps-entries-in-the-order-they-were-recorded ()
  (let ((entries (memex-entry-pair (list memex-entry-tests--message
                                         memex-entry-tests--call
                                         memex-entry-tests--result))))
    (should (equal (mapcar (lambda (e) (alist-get 'doc_id (memex-entry-call e)))
                           entries)
                   '(172215 172234)))))

(ert-deftest memex-entry-duration-is-the-gap-between-call-and-result ()
  (let ((entry (car (memex-entry-pair (list memex-entry-tests--call
                                            memex-entry-tests--result)))))
    (should (equal (memex-entry-duration entry) 1389)))
  (should-not (memex-entry-duration
               (car (memex-entry-pair (list memex-entry-tests--message))))))

(ert-deftest memex-entry-description-is-what-the-agent-wrote-for-a-human ()
  "The `description' argument is already prose; the role name is not."
  (let ((entry (car (memex-entry-pair (list memex-entry-tests--call)))))
    (should (equal (memex-entry-description entry) "List repo structure"))))

(ert-deftest memex-entry-description-falls-back-to-what-the-tool-touched ()
  "Most tools write no description, and the path they touched says more
than their own name does."
  (let ((entry (car (memex-entry-pair
                     '(((role . "tool_use") (tool_name . "Read")
                        (tool_input . "{\"file_path\": String(\"/tmp/a/b.py\")}")))))))
    (should (equal (memex-entry-description entry) "b.py"))))

(ert-deftest memex-entry-description-of-a-message-is-its-first-line ()
  (let ((entry (car (memex-entry-pair
                     '(((role . "assistant")
                        (text . "the opening line\nand a second one")))))))
    (should (equal (memex-entry-description entry) "the opening line"))))

(ert-deftest memex-entry-status-is-good-until-the-output-says-otherwise ()
  (should (equal (car (memex-entry-status
                       (car (memex-entry-pair (list memex-entry-tests--call
                                                    memex-entry-tests--result)))))
                 'ok)))

(ert-deftest memex-entry-status-warns-on-output-that-reports-a-failure ()
  "memex records no exit status, so a job that failed under a zero exit
is only visible in what it printed: `ert' says `1 unexpected' and exits
0.  The line that said so is the reason shown beside the fold."
  (let* ((result `((role . "tool_result") (parent_tool_use_id . "toolu_01MX")
                   (tool_output . "Ran 12 tests\n1 unexpected\nbye")))
         (entry (car (memex-entry-pair (list memex-entry-tests--call result))))
         (status (memex-entry-status entry)))
    (should (equal (car status) 'warn))
    (should (equal (cdr status) "1 unexpected"))))

(ert-deftest memex-entry-status-reads-a-passing-suite-as-passing ()
  "`Ran 7 tests, 7 as expected, 0 unexpected' is the success line of the
tool this transcript runs most, and `failed' in prose is prose.  Both
flagged most of a session before the count and the case were pinned."
  (cl-flet ((state (output)
              (car (memex-entry-status
                    (car (memex-entry-pair
                          (list memex-entry-tests--call
                                `((role . "tool_result")
                                  (parent_tool_use_id . "toolu_01MX")
                                  (tool_output . ,output)))))))))
    (should (equal (state "Ran 7 tests, 7 results as expected, 0 unexpected")
                   'ok))
    (should (equal (state "a failed fetch must not open a pane") 'ok))
    (should (equal (state "Ran 7 tests, 5 as expected, 2 unexpected") 'warn))
    (should (equal (state "FAILED  1/1  some-test") 'warn))
    (should (equal (state "error: unrecognized subcommand") 'warn))))

(ert-deftest memex-entry-status-does-not-blame-a-tool-for-what-it-quoted ()
  "A file that says `error:' says nothing about the call that read it,
and a transcript that flags every such read flags most of itself."
  (let* ((output "config.yaml\n  on_error: retry\nError: bad\n")
         (read `((role . "tool_use") (tool_name . "Read") (event_id . "r1")
                 (tool_input . "{\"file_path\": String(\"/tmp/a.yaml\")}")))
         (answer `((role . "tool_result") (tool_name . "Read")
                   (parent_tool_use_id . "r1") (tool_output . ,output))))
    (should (equal (car (memex-entry-status
                         (car (memex-entry-pair (list read answer)))))
                   'ok))
    (should (equal (car (memex-entry-status
                         (car (memex-entry-pair
                               (list memex-entry-tests--call
                                     `((role . "tool_result")
                                       (parent_tool_use_id . "toolu_01MX")
                                       (tool_output . ,output)))))))
                   'warn))))

(ert-deftest memex-entry-status-of-a-call-still-running-is-unknown ()
  (should (equal (car (memex-entry-status
                       (car (memex-entry-pair (list memex-entry-tests--call)))))
                 'pending)))

(ert-deftest memex-entry-metadata-promotes-the-structural-arguments ()
  "The counts the reader wants belong on the block; the map the
arguments came in does not, and neither does what the heading and the
principal section already show."
  (let* ((entry (car (memex-entry-pair (list memex-entry-tests--call
                                             memex-entry-tests--result))))
         (meta (memex-entry-metadata entry)))
    (should (equal (alist-get "lines" meta nil nil #'equal) "3"))
    (should (alist-get "took" meta nil nil #'equal))
    (should-not (alist-get "description" meta nil nil #'equal))
    (should-not (alist-get "command" meta nil nil #'equal))))

(ert-deftest memex-entry-payload-is-the-long-argument-not-the-short-one ()
  "A file's contents belong in a section of their own; a one-line
command belongs on the metadata block."
  (let* ((long (make-string 400 ?x))
         (entry (car (memex-entry-pair
                      `(((role . "tool_use") (tool_name . "Write")
                         (tool_input . ,(format "{\"file_path\": String(\"/tmp/a.py\"), \"content\": String(\"%s\")}"
                                                long)))))))
         (payload (memex-entry-payload entry)))
    (should payload)
    (should (equal (car payload) "content"))
    (should (equal (cadr payload) 'python-mode))
    (should (equal (cddr payload) long))))

(ert-deftest memex-entry-payload-is-the-principal-argument-however-short ()
  "A one-line command and a hundred-line patch are the same thing to a
reader looking for what happened, so both get a section; showing one on
the metadata block and folding the other makes a transcript read two
ways.  A tool with no principal argument and nothing long still has no
payload."
  (let ((payload (memex-entry-payload
                  (car (memex-entry-pair (list memex-entry-tests--call))))))
    (should (equal (car payload) "command"))
    (should (equal (cddr payload) "ls -la")))
  (should-not (memex-entry-payload
               (car (memex-entry-pair
                     '(((role . "tool_use") (tool_name . "Glob")
                        (tool_input . "{\"pattern\": String(\"*.el\")}"))))))))

(ert-deftest memex-entry-fields-reads-every-argument-not-a-chosen-few ()
  "Tool arguments arrive as a Rust `Debug' map.  Every tool has its own
keys, so they are read as they come rather than looked for by name."
  (let ((fields (memex-entry-fields
                 (concat "{\"file_path\": String(\"/tmp/a.py\"), "
                         "\"replace_all\": Static(Bool(false)), "
                         "\"limit\": Number(120)}"))))
    (should (equal (mapcar #'car fields) '("file_path" "replace_all" "limit")))
    (should (equal (alist-get "file_path" fields nil nil #'equal) "/tmp/a.py"))
    (should (equal (alist-get "replace_all" fields nil nil #'equal) "false"))
    (should (equal (alist-get "limit" fields nil nil #'equal) "120"))))

(ert-deftest memex-entry-fields-keeps-a-nested-argument-whole ()
  "An argument holding a list or a map is kept as it came: reading it is
the payload section's job, not this one's."
  (let ((fields (memex-entry-fields
                 "{\"edits\": Array([Object({\"a\": String(\"1\")})]), \"n\": Number(2)}")))
    (should (equal (mapcar #'car fields) '("edits" "n")))
    (should (string-search "Object" (alist-get "edits" fields nil nil #'equal)))))

(ert-deftest memex-entry-fields-is-not-fooled-by-punctuation-inside-a-string ()
  "A command carries its own braces, quotes and commas."
  (let ((fields (memex-entry-fields
                 (concat "{\"command\": String(\"awk '{print $1}' x, y\"), "
                         "\"description\": String(\"count\")}"))))
    (should (equal (alist-get "command" fields nil nil #'equal)
                   "awk '{print $1}' x, y"))
    (should (equal (alist-get "description" fields nil nil #'equal) "count"))))

(ert-deftest memex-entry-tells-a-person-apart-from-the-harness-and-the-tools ()
  "A transcript carries far more injected `user' records - slash
commands, skill bodies, task notifications - than anything anyone typed,
and the two arrive under the same role."
  (cl-flet ((kind (record) (car (mapcar #'memex-entry-kind
                                        (memex-entry-pair (list record))))))
    (should (equal (kind '((role . "user") (text . "go on"))) 'human))
    (should (equal (kind '((role . "assistant") (text . "on it"))) 'assistant))
    (should (equal (kind memex-entry-tests--call) 'tool))
    (should (equal (kind '((role . "developer") (text . "you are"))) 'system))
    (dolist (opening '("<recommended_plugins>"
                       "<command-name>/clear</command-name>"
                       "<system-reminder>\nnote\n</system-reminder>"
                       "<task-notification> <task-id>ab8</task-id>"
                       "<file name=\"/tmp/a.el\">x</file>"
                       "Base directory for this skill: /tmp/s"
                       "This session is being continued from a prior one."))
      (should (equal (kind `((role . "user") (text . ,opening))) 'system)))))

(ert-deftest memex-entry-fields-of-nothing-is-nothing ()
  (should-not (memex-entry-fields nil))
  (should-not (memex-entry-fields "")))

(provide 'memex-entry-tests)
;;; memex-entry-tests.el ends here
