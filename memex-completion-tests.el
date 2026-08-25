;;; memex-completion-tests.el --- Tests for memex-completion.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-completion-tests.el -f ert-run-tests-batch-and-exit
;;
;; The selector tests run against a stub executable answering one records
;; payload, so neither a memex install nor an indexed corpus is needed.
;; `completing-read' is replaced for the duration of a read, which is how
;; the completion table, its metadata and its candidates are read without
;; a completion UI.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)

(require 'memex-completion nil t)

(declare-function memex-read-record "memex-completion")
(declare-function memex-read-session "memex-completion")
(declare-function memex-read-project "memex-completion")

(defvar memex-completion-tests--dir nil)

(defun memex-completion-tests--tempdir ()
  "Return this test's temporary directory, creating it once."
  (or memex-completion-tests--dir
      (setq memex-completion-tests--dir (make-temp-file "memex-completion-test" t))))

(defun memex-completion-tests--cleanup ()
  "Remove the temporary directory and any stub process left behind."
  (dolist (process (process-list))
    (when (string-match-p "memex" (process-name process))
      (delete-process process)))
  (when (and memex-completion-tests--dir
             (file-directory-p memex-completion-tests--dir))
    (delete-directory memex-completion-tests--dir t))
  (setq memex-completion-tests--dir nil))

(defun memex-completion-tests--stub (body)
  "Write BODY as a stub memex executable and return its path."
  (let ((path (expand-file-name "memex-stub" (memex-completion-tests--tempdir))))
    (with-temp-file path (insert "#!/bin/sh\n" body))
    (set-file-modes path #o755)
    path))

(defun memex-completion-tests--request-file ()
  "Return the path the stub records the request it received in."
  (expand-file-name "request.json" (memex-completion-tests--tempdir)))

(defun memex-completion-tests--recording-stub (response)
  "Return a stub saving the request it is handed and printing RESPONSE."
  (memex-completion-tests--stub
   (format "cat > %s\nprintf '%%s' %s\n"
           (shell-quote-argument (memex-completion-tests--request-file))
           (shell-quote-argument response))))

(defun memex-completion-tests--received ()
  "Return the request envelope memex was handed, decoded as an alist."
  (let ((path (memex-completion-tests--request-file)))
    (when (file-exists-p path)
      (json-parse-string (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         :object-type 'alist :array-type 'list))))

(defun memex-completion-tests--sent (exchange)
  "Return the request object memex received in EXCHANGE."
  (alist-get 'request (plist-get exchange :request)))

(cl-defun memex-completion-tests--record (&key doc-id ts (source "codex") project
                                               session-id (role "assistant") text
                                               tool-name tool-output source-path)
  "Return a record alist carrying the fields it was given.
DOC-ID, TS, SOURCE, PROJECT, SESSION-ID, ROLE, TEXT, TOOL-NAME,
TOOL-OUTPUT and SOURCE-PATH are the record's wire fields.  Every one
left nil is absent from the alist, the way memex omits a `Record'
optional rather than sending null."
  (delq nil
        (list (cons 'source source)
              (cons 'doc_id doc-id)
              (cons 'ts ts)
              (and project (cons 'project project))
              (and session-id (cons 'session_id session-id))
              (and role (cons 'role role))
              (and text (cons 'text text))
              (and tool-name (cons 'tool_name tool-name))
              (and tool-output (cons 'tool_output tool-output))
              (and source-path (cons 'source_path source-path)))))

(defun memex-completion-tests--records-response (records)
  "Return a records payload pairing RECORDS with descending scores."
  (json-serialize
   `((protocol . 1)
     (response
      . ((kind . "records")
         (records . ,(vconcat (cl-loop for record in records
                                       for score downfrom 99.0 by 1.0
                                       collect (vector score record)))))))))

(defun memex-completion-tests--ranked-records ()
  "Return three records, every field of them against alphabetical order."
  (list (memex-completion-tests--record
         :doc-id 9003 :ts 1787140243303 :project "zebra" :session-id "sess-c"
         :role "user" :text "zulu record" :source-path "/tmp/zulu.jsonl")
        (memex-completion-tests--record
         :doc-id 9001 :ts 1787140243301 :project "alpha" :session-id "sess-a"
         :role "user" :text "alfa record" :source-path "/tmp/alfa.jsonl")
        (memex-completion-tests--record
         :doc-id 9002 :ts 1787140243302 :project "mango" :session-id "sess-b"
         :role "user" :text "mike record" :source-path "/tmp/mike.jsonl")))

(defun memex-completion-tests--colliding-records ()
  "Return records forcing every rung of the label collision ladder.
The first two share every label field and differ only in the middle of
their `doc_id', so a short fragment taken from either end of it
collides while the full ids differ.  The next two share a `doc_id'
under different `source_path's, the two after that are identical in
every field, and the last collides with nothing."
  (list (memex-completion-tests--record
         :doc-id 100000001000000042 :ts 1787140243301 :project "memex.el"
         :session-id "s-collide" :text "duplicate label"
         :source-path "/tmp/collide.jsonl")
        (memex-completion-tests--record
         :doc-id 100000002000000042 :ts 1787140243301 :project "memex.el"
         :session-id "s-collide" :text "duplicate label"
         :source-path "/tmp/collide.jsonl")
        (memex-completion-tests--record
         :doc-id 5150 :ts 1787140243301 :project "memex.el"
         :session-id "s-split" :text "duplicate label"
         :source-path "/tmp/split-a1b2.jsonl")
        (memex-completion-tests--record
         :doc-id 5150 :ts 1787140243301 :project "memex.el"
         :session-id "s-split" :text "duplicate label"
         :source-path "/tmp/split-c3d4.jsonl")
        (memex-completion-tests--record
         :doc-id 7777 :ts 1787140243301 :project "memex.el"
         :session-id "s-twin" :text "duplicate label"
         :source-path "/tmp/twin.jsonl")
        (memex-completion-tests--record
         :doc-id 7777 :ts 1787140243301 :project "memex.el"
         :session-id "s-twin" :text "duplicate label"
         :source-path "/tmp/twin.jsonl")
        (memex-completion-tests--record
         :doc-id 4242 :ts 1787140243301 :source "open-claw" :project "memex.el"
         :session-id "s-solo" :text "unique label"
         :source-path "/tmp/solo.jsonl")))

(defconst memex-completion-tests--alpha-session
  "01a0380e-ec8f-7b63-bce1-10e5a205e000"
  "The `session_id' two of the session fixture's `source_path's share.")

(defun memex-completion-tests--session-records ()
  "Return records of three sessions, two of which share a `session_id'.
The alpha group's newest record is neither its first nor its last and
does not carry the group's extreme `doc_id', so keeping the newest `ts'
is distinguishable from keeping the first, the last, or the largest id.
The beta record repeats alpha's `session_id' under another
`source_path' and is a session of its own."
  (list (memex-completion-tests--record
         :doc-id 14 :ts 1787140243050 :source "open-claw" :project "memex.el"
         :session-id "caea32e0" :text "other session"
         :source-path "/tmp/caea32e0.jsonl")
        (memex-completion-tests--record
         :doc-id 13 :ts 1787140243100 :project "memex.el"
         :session-id memex-completion-tests--alpha-session
         :text "oldest alpha" :source-path "/tmp/alpha.jsonl")
        (memex-completion-tests--record
         :doc-id 11 :ts 1787140243300 :project "memex.el"
         :session-id memex-completion-tests--alpha-session
         :text "newest alpha" :source-path "/tmp/alpha.jsonl")
        (memex-completion-tests--record
         :doc-id 15 :ts 1787140243200 :project "memex.el"
         :session-id memex-completion-tests--alpha-session
         :text "middle alpha" :source-path "/tmp/alpha.jsonl")
        (memex-completion-tests--record
         :doc-id 12 :ts 1787140243200 :project "memex.el"
         :session-id memex-completion-tests--alpha-session
         :text "only beta" :source-path "/tmp/beta.jsonl")))

(defun memex-completion-tests--project-records ()
  "Return records whose projects repeat, run unsorted and once go missing."
  (list (memex-completion-tests--record
         :doc-id 21 :ts 1787140243304 :project "zebra" :session-id "sess-c"
         :text "zulu record" :source-path "/tmp/zulu.jsonl")
        (memex-completion-tests--record
         :doc-id 22 :ts 1787140243303 :project "alpha" :session-id "sess-a"
         :text "alfa record" :source-path "/tmp/alfa.jsonl")
        (memex-completion-tests--record
         :doc-id 23 :ts 1787140243302 :project "zebra" :session-id "sess-d"
         :text "zulu again" :source-path "/tmp/zulu-two.jsonl")
        (memex-completion-tests--record
         :doc-id 24 :ts 1787140243301 :source "open-claw" :session-id "sess-e"
         :text "no project at all" :source-path "/tmp/loose.jsonl")
        (memex-completion-tests--record
         :doc-id 25 :ts 1787140243300 :project "mango" :session-id "sess-b"
         :text "mike record" :source-path "/tmp/mike.jsonl")))

(defun memex-completion-tests--noisy-records ()
  "Return three records, one unruly, one quiet and one carrying no `text'.
The third is the ordinary shape of a tool call: with no `text' its label
is its `tool_output', so an annotation repeating the tool fields would
show the same string twice."
  (list (memex-completion-tests--record
         :doc-id 8801 :ts 1787140243301 :source "open-claw"
         :project "mem\nex.el" :session-id "s-noise" :role "tool_result"
         :tool-name "sh\nell" :tool-output "out\nput\ttabbed"
         :text (concat "first line\nsecond\tcolumn\n\n   padded   "
                       (make-string 4000 ?x))
         :source-path "/tmp/noisy.jsonl")
        (memex-completion-tests--record
         :doc-id 8802 :ts 1787140243301 :source "open-claw"
         :project "mem\nex.el" :session-id "s-noise" :role "tool_result"
         :text "quiet" :source-path "/tmp/noisy.jsonl")
        (memex-completion-tests--record
         :doc-id 8803 :ts 1787140243301 :source "open-claw"
         :project "mem\nex.el" :session-id "s-noise" :role "tool_result"
         :tool-name "grep" :tool-output "matched\t3 files"
         :source-path "/tmp/noisy.jsonl")))

(defun memex-completion-tests--read (records reader choose)
  "Run READER against a memex answering with RECORDS and return the exchange.
CHOOSE is called with the candidates the completion table enumerates and
returns the one to select.  The exchange is a plist of `:value', what
READER returned, `:table', the collection `completing-read' was handed,
`:candidates', what that collection enumerates, and `:request', the
request memex received.

The chosen candidate reaches READER stripped of its text properties,
because `minibuffer-allow-text-properties' is nil by default and
`read-from-minibuffer' therefore discards them.  A selector must
recover the record by looking the string up, which is what candidate
uniqueness is for."
  (let ((memex-executable
         (memex-completion-tests--recording-stub
          (memex-completion-tests--records-response records)))
        (table nil)
        (candidates nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (error "A selector must not prompt for input")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq table collection
                       candidates (all-completions "" collection nil))
                 (substring-no-properties (funcall choose candidates)))))
      (let ((value (funcall reader)))
        (list :value value :table table :candidates candidates
              :request (memex-completion-tests--received))))))

(defun memex-completion-tests--metadata (table)
  "Return the completion metadata TABLE answers the metadata action with."
  (let ((metadata (funcall table "" nil 'metadata)))
    (should (eq (car metadata) 'metadata))
    (cdr metadata)))

(defun memex-completion-tests--record-of (candidate)
  "Return the record CANDIDATE carries in its `memex-record' property, or nil.
Position 0 is where `embark' and `marginalia' look, so MXE-008 reads
the record the same way this does."
  (get-text-property 0 'memex-record candidate))

(defun memex-completion-tests--doc-ids (candidates)
  "Return the `doc_id' of the record each of CANDIDATES carries."
  (mapcar (lambda (candidate)
            (alist-get 'doc_id (memex-completion-tests--record-of candidate)))
          candidates))

(defun memex-completion-tests--session-keys (candidates)
  "Return the session each of CANDIDATES stands for, id paired with path."
  (mapcar (lambda (candidate)
            (let ((record (memex-completion-tests--record-of candidate)))
              (cons (alist-get 'session_id record)
                    (alist-get 'source_path record))))
          candidates))

(defun memex-completion-tests--plain (candidates)
  "Return CANDIDATES as the bare strings a completion UI displays."
  (mapcar #'substring-no-properties candidates))

(defun memex-completion-tests--labels (records reader)
  "Return the candidate strings READER puts up for RECORDS."
  (memex-completion-tests--plain
   (plist-get (memex-completion-tests--read records reader #'car) :candidates)))

(defun memex-completion-tests--annotation (metadata candidate)
  "Return the annotation METADATA renders beside CANDIDATE."
  (let ((annotate (alist-get 'annotation-function metadata))
        (affix (alist-get 'affixation-function metadata)))
    (cond (affix (let ((row (car (funcall affix (list candidate)))))
                   (if (consp row) (concat (nth 1 row) (nth 2 row)) "")))
          (annotate (or (funcall annotate candidate) "")))))

(ert-deftest memex-completion-candidates-carry-their-record-and-hand-it-back ()
  (unwind-protect
      (let* ((exchange (memex-completion-tests--read
                        (memex-completion-tests--ranked-records)
                        (lambda () (memex-read-record))
                        (lambda (candidates) (nth 1 candidates))))
             (candidates (plist-get exchange :candidates))
             (value (plist-get exchange :value)))
        (should (equal (length candidates) 3))
        (dolist (candidate candidates)
          (should (stringp candidate))
          (should-not (string-empty-p (substring-no-properties candidate)))
          (should (memex-completion-tests--record-of candidate)))
        (should (equal (memex-completion-tests--doc-ids candidates)
                       '(9003 9001 9002)))
        (should-not (stringp value))
        (should (consp value))
        (should (consp (car value)))
        (should (equal (alist-get 'doc_id value) 9001))
        (should (equal (alist-get 'session_id value) "sess-a"))
        (should (equal (alist-get 'source_path value) "/tmp/alfa.jsonl"))
        (should (equal (alist-get 'project value) "alpha"))
        (should (equal (alist-get 'source value) "codex"))
        (should (equal (alist-get 'ts value) 1787140243301))
        (should (equal (sort (mapcar #'car value) #'string<)
                       '(doc_id project role session_id source source_path text ts)))
        (should (equal value
                       (memex-completion-tests--record-of (nth 1 candidates)))))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-metadata-pins-the-category-order-and-annotation ()
  (unwind-protect
      (let* ((exchange (memex-completion-tests--read
                        (memex-completion-tests--ranked-records)
                        (lambda () (memex-read-record))
                        #'car))
             (table (plist-get exchange :table)))
        (should (functionp table))
        (let* ((metadata (memex-completion-tests--metadata table))
               (category (alist-get 'category metadata))
               (display (alist-get 'display-sort-function metadata))
               (cycle (alist-get 'cycle-sort-function metadata)))
          (should category)
          (should (symbolp category))
          (should (string-prefix-p "memex" (symbol-name category)))
          (should (functionp display))
          (should (functionp cycle))
          (should (equal (funcall display (list "zulu" "alfa" "mike"))
                         '("zulu" "alfa" "mike")))
          (should (equal (funcall cycle (list "zulu" "alfa" "mike"))
                         '("zulu" "alfa" "mike")))
          (should (or (functionp (alist-get 'annotation-function metadata))
                      (functionp (alist-get 'affixation-function metadata))))))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-read-project-returns-a-project-string ()
  (unwind-protect
      (let* ((exchange (memex-completion-tests--read
                        (memex-completion-tests--project-records)
                        (lambda () (memex-read-project))
                        (lambda (candidates) (nth 1 candidates))))
             (request (memex-completion-tests--sent exchange))
             (candidates (memex-completion-tests--plain
                          (plist-get exchange :candidates)))
             (value (plist-get exchange :value))
             (table (plist-get exchange :table)))
        (should (equal (alist-get 'op request) "recent"))
        (should (equal (alist-get 'limit request) 500))
        (should (equal candidates '("zebra" "alpha" "mango")))
        (should (stringp value))
        (should (equal value "alpha"))
        (should (functionp table))
        (let ((category (alist-get 'category
                                   (memex-completion-tests--metadata table))))
          (should category)
          (should (symbolp category))
          (should (string-prefix-p "memex" (symbol-name category)))))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-disambiguates-every-candidate ()
  (unwind-protect
      (let* ((records (memex-completion-tests--colliding-records))
             (reader (lambda () (memex-read-record)))
             (together (memex-completion-tests--labels records reader))
             (again (memex-completion-tests--labels records reader))
             (alone (memex-completion-tests--labels (list (nth 6 records)) reader))
             (collider (memex-completion-tests--labels
                        (list (nth 0 records)) reader))
             (exchange (memex-completion-tests--read records reader #'car)))
        (should (equal (length together) 7))
        (dolist (label together)
          (should (stringp label))
          (should-not (string-empty-p label)))
        (should (equal (length (delete-dups (copy-sequence together))) 7))
        (should (equal together again))
        (should (string-match-p "a1b2" (nth 2 together)))
        (should (string-match-p "c3d4" (nth 3 together)))
        (should (equal (memex-completion-tests--doc-ids
                        (plist-get exchange :candidates))
                       '(100000001000000042 100000002000000042
                                            5150 5150 7777 7777 4242)))
        (should (equal (car alone) (nth 6 together)))
        (should-not (equal (car collider) (nth 0 together))))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-sessions-deduplicate-by-id-and-source-path ()
  (unwind-protect
      (let* ((exchange (memex-completion-tests--read
                        (memex-completion-tests--session-records)
                        (lambda () (memex-read-session))
                        (lambda (candidates)
                          (seq-find
                           (lambda (candidate)
                             (equal (alist-get
                                     'source_path
                                     (memex-completion-tests--record-of candidate))
                                    "/tmp/alpha.jsonl"))
                           candidates))))
             (candidates (plist-get exchange :candidates))
             (value (plist-get exchange :value)))
        (should (equal (memex-completion-tests--session-keys candidates)
                       `(("caea32e0" . "/tmp/caea32e0.jsonl")
                         (,memex-completion-tests--alpha-session . "/tmp/alpha.jsonl")
                         (,memex-completion-tests--alpha-session . "/tmp/beta.jsonl"))))
        (should-not (stringp value))
        (should (consp (car value)))
        (should (equal (alist-get 'doc_id value) 11))
        (should (equal (alist-get 'ts value) 1787140243300))
        (should (equal (alist-get 'text value) "newest alpha"))
        (should (equal (alist-get 'source_path value) "/tmp/alpha.jsonl")))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-annotations-collapse-backend-whitespace ()
  (unwind-protect
      (let* ((exchange (memex-completion-tests--read
                        (memex-completion-tests--noisy-records)
                        (lambda () (memex-read-record))
                        #'car))
             (candidates (plist-get exchange :candidates))
             (metadata (memex-completion-tests--metadata
                        (plist-get exchange :table)))
             (noisy (memex-completion-tests--annotation metadata (nth 0 candidates)))
             (quiet (memex-completion-tests--annotation metadata (nth 1 candidates)))
             (tooled (memex-completion-tests--annotation metadata (nth 2 candidates)))
             (labels (memex-completion-tests--plain candidates)))
        (should (equal (length candidates) 3))
        (should (stringp noisy))
        (should (stringp quiet))
        (should (stringp tooled))
        (should-not (string-empty-p noisy))
        (should-not (string-match-p "[\n\r\t]" noisy))
        (should-not (string-match-p "[\n\r\t]" quiet))
        (should-not (string-match-p "[\n\r\t]" tooled))
        (should (<= (length noisy) 300))
        (should-not (equal noisy quiet))
        (should (string-match-p "matched 3 files" (nth 2 labels)))
        (should-not (string-match-p "matched 3 files" tooled))
        (should-not (string-match-p "grep" tooled))
        (should (string-match-p "/tmp/noisy\\.jsonl" tooled))
        (should (string-match-p "sh ell" noisy))
        (dolist (label labels)
          (should-not (string-match-p "[\n\r\t]" label))))
    (memex-completion-tests--cleanup)))

(ert-deftest memex-completion-propagates-the-fetch-failure ()
  (unwind-protect
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (error "A failed fetch must not open a picker"))))
        (let* ((memex-executable
                (memex-completion-tests--stub
                 (format "cat > /dev/null\nprintf '%%s' %s\n"
                         (shell-quote-argument
                          (concat "{\"protocol\":1,\"response\":"
                                  "{\"kind\":\"error\","
                                  "\"message\":\"no session caea32e0\"}}")))))
               (failure (should-error (memex-read-record) :type 'memex-rpc-error)))
          (should (equal (plist-get (cdr failure) :message) "no session caea32e0")))
        (let* ((memex-executable
                (memex-completion-tests--stub
                 (concat "cat > /dev/null\n"
                         "printf '%s\\n' 'Error: missing field `query`' >&2\n"
                         "exit 1\n")))
               (failure (should-error (memex-read-session)
                                      :type 'memex-transport-error)))
          (should (equal (plist-get (cdr failure) :exit-status) 1))
          (should (equal (plist-get (cdr failure) :stderr)
                         "Error: missing field `query`\n"))))
    (memex-completion-tests--cleanup)))

(provide 'memex-completion-tests)
;;; memex-completion-tests.el ends here
