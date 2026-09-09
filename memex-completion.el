;;; memex-completion.el --- Selectors for memex records -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-read-record', `memex-read-session' and `memex-read-project'
;; read one of memex's recent records, sessions or projects through
;; `completing-read'.  Each does one bounded synchronous fetch and then
;; puts the answer up, so any caller can use them the way it would use
;; `read-file-name'; the callback-style layer in memex-api.el serves
;; everything that renders asynchronously.

;;; Code:

(require 'ansi-color)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)

(defcustom memex-completion-timeout 10.0
  "Seconds a selector waits for memex before giving up."
  :type 'number
  :group 'memex)

(defcustom memex-completion-width 64
  "Width a backend string is cut to before it enters a candidate line."
  :type '(integer :match (lambda (_widget value)
                           (and (integerp value) (> value 0))))
  :group 'memex)

(defcustom memex-completion-recent-limit 500
  "Number of recent records the selectors draw their candidates from."
  :type 'natnum
  :group 'memex)

(defun memex-completion--clean (string)
  "Return STRING as one display line, without terminal or escaped whitespace."
  (when (stringp string)
    (string-trim
     (replace-regexp-in-string
      "[[:cntrl:][:blank:]]+" " "
      (replace-regexp-in-string
       "\\\\[nrt]" " " (ansi-color-filter-apply string) t t)))))

(defun memex-completion--one-line (string)
  "Return STRING as one line of at most `memex-completion-width'.
Nil when STRING is absent or blank.  Runs of whitespace and control
characters collapse to a single space: record text and tool output
arrive with the newlines and tabs of the transcript they were read
from, which break a single-line candidate or annotation."
  (when (stringp string)
    (let ((line (memex-completion--clean string)))
      (unless (string-empty-p line)
        (truncate-string-to-width line memex-completion-width nil nil t)))))

(defun memex-completion--join (&rest fields)
  "Return the FIELDS carrying something as one space-separated line."
  (string-join (seq-remove #'string-empty-p (delq nil fields)) " "))

(defun memex-completion--time (milliseconds)
  "Return the epoch MILLISECONDS as a local timestamp, or nil without one."
  (when (numberp milliseconds)
    (format-time-string "%F %R" (/ milliseconds 1000))))

(defun memex-completion--age (milliseconds)
  "Return how long ago the epoch MILLISECONDS was, or nil without one.
The coarsest unit that still counts is the one shown, since a candidate
line is read for how stale a record is and not for when it was written."
  (when (numberp milliseconds)
    (let ((seconds (max 0 (floor (- (float-time) (/ milliseconds 1000.0))))))
      (cond ((< seconds 60) (format "%ds" seconds))
            ((< seconds 3600) (format "%dm" (/ seconds 60)))
            ((< seconds 86400) (format "%dh" (/ seconds 3600)))
            (t (format "%dd" (/ seconds 86400)))))))

(defun memex-completion--hits (count)
  "Return COUNT as the hits a session summary stands for, or nil without one."
  (when (numberp count)
    (format (ngettext "%d hit" "%d hits" count) count)))

(defun memex-completion--content (record)
  "Return the text, tool output or tool name of RECORD as one line."
  (or (memex-completion--one-line (alist-get 'text record))
      (memex-completion--one-line (alist-get 'tool_output record))
      (memex-completion--one-line (alist-get 'tool_name record))))

(defun memex-completion--record-base (record)
  "Return the label RECORD is read under, before it is told apart."
  (memex-completion--join
   (memex-completion--one-line (alist-get 'project record))
   (memex-completion--one-line (alist-get 'role record))
   (memex-completion--content record)))

(defun memex-completion--session-base (record)
  "Return the label the session RECORD stands for is read under."
  (memex-completion--join
   (memex-completion--one-line (alist-get 'project record))
   (memex-completion--content record)))

(defun memex-completion--label-source (record)
  "Return the one line RECORD's label was cut from.
A search summary overwrites `text' with a window into the text the
label was cut from, so by the time the record is annotated that text is
no longer among its fields; `memex-label-source' is what carries it
across.  A record without one was labelled as it stands."
  (or (memex-completion--one-line (alist-get 'memex-label-source record))
      (memex-completion--content record)))

(defun memex-completion--beside (record field)
  "Return FIELD of RECORD as one line, or nil when the label came from it.
A tool record's `text' is the output it is reporting, so the field the
label was built from and the field beside it are the same string on most
of the index.  Sameness is decided against the field the label was cut
from and not against the label, which on a session row is a window into
that field and shares none of its content.  The tool fields are left out
altogether when the record carries no `text', because
`memex-completion--content' has then already spent one of them on the
label."
  (and (alist-get 'text record)
       (let ((line (memex-completion--one-line (alist-get field record))))
         (unless (equal line (memex-completion--label-source record)) line))))

(defun memex-completion-annotate (candidate)
  "Return the metadata rendered beside CANDIDATE.
What the label does not already carry: how many hits a session summary
stands for, how long ago the record was written, where it came from and
the tool fields the label was not built from."
  (or (and candidate (get-text-property 0 'memex-annotation candidate))
      (let ((record (memex-completion-record-of candidate)))
        (concat "  "
                (memex-completion--join
                 (memex-completion--hits (alist-get 'hit_count record))
                 (memex-completion--age (alist-get 'ts record))
                 (memex-completion--one-line (alist-get 'source record))
                 (memex-completion--beside record 'tool_name)
                 (memex-completion--beside record 'tool_output))))))

(defun memex-completion--short-id (id)
  "Return the tail of ID standing for it in a candidate label."
  (when id
    (let ((text (if (stringp id) id (format "%s" id))))
      (if (> (length text) 8) (substring text -8) text))))

(defun memex-completion--suffixes (id path index)
  "Return the suffixes telling apart the candidates ID names.
They run from a short tail of ID to the whole of it, then on to the file
name of PATH and finally INDEX, which no two candidates share."
  (let ((whole (concat "#" (if (stringp id) id (format "%s" id))))
        (file (and path (file-name-nondirectory path))))
    (list (concat "#" (memex-completion--short-id id))
          whole
          (memex-completion--join whole file)
          (memex-completion--join whole file (format "(%d)" index)))))

(defun memex-completion--tally (strings)
  "Return a table counting how often each of STRINGS occurs."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (string strings counts)
      (puthash string (1+ (gethash string counts 0)) counts))))

(defun memex-completion--labels (bases suffixes)
  "Return one distinct label per candidate.
BASES and SUFFIXES run in parallel, each entry of SUFFIXES what its
candidate appends in turn.  A label follows from the whole candidate
set rather than from the order the set was built in: it is its bare
base until another candidate wants the same one, and only then climbs
the suffixes, whose last rung no two candidates share."
  (let* ((ladders (seq-mapn (lambda (base entry)
                              (cons base
                                    (mapcar (lambda (suffix)
                                              (memex-completion--join base
                                                                      suffix))
                                            entry)))
                            bases suffixes))
         (labels (mapcar #'car ladders))
         (depth (if ladders (seq-max (mapcar #'length ladders)) 0))
         (rung 0))
    (while (< (setq rung (1+ rung)) depth)
      (let ((counts (memex-completion--tally labels)))
        (setq labels (seq-mapn (lambda (label ladder)
                                 (if (> (gethash label counts) 1)
                                     (or (nth rung ladder) label)
                                   label))
                               labels ladders))))
    labels))

(defun memex-completion--candidates (records base-function id)
  "Return RECORDS as completion candidates, memex's ranking kept.
BASE-FUNCTION labels one record and ID names the record field the
candidates are told apart by.  Each candidate carries its record in the
`memex-record' text property at position 0, which is where embark and
marginalia read a candidate's data from."
  (seq-mapn (lambda (label record) (propertize label 'memex-record record))
            (memex-completion--labels
             (mapcar base-function records)
             (seq-map-indexed
              (lambda (record index)
                (memex-completion--suffixes (alist-get id record)
                                            (alist-get 'source_path record)
                                            index))
              records))
            records))

(defun memex-completion-record-candidates (records)
  "Return RECORDS as `memex-record' candidates, memex's ranking kept.
The one place the convention behind a record candidate lives: the label
a record is read under, and `doc_id' as the field the candidates are
told apart by."
  (memex-completion--candidates records #'memex-completion--record-base 'doc_id))

(defun memex-completion-session-candidates (records)
  "Return RECORDS as `memex-session' candidates, memex's ranking kept.
Each of RECORDS stands for the session it belongs to: the label leaves
out the role a lone record is told apart by, and `session_id' is the
field the candidates are told apart by."
  (memex-completion--candidates records #'memex-completion--session-base
                                'session_id))

(defun memex-completion--table (candidates category &optional annotate)
  "Return a completion table offering CANDIDATES under CATEGORY.
ANNOTATE renders the line shown beside a candidate.  Both sort
functions are `identity' because memex has already ranked the
candidates by recency or score, and a completion UI sorts by its own
lights unless told not to: vertico runs its candidates through
`vertico-sort-history-length-alpha' by default."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata . ,(delq nil
                            (list (cons 'category category)
                                  (and annotate
                                       (cons 'annotation-function annotate))
                                  '(display-sort-function . identity)
                                  '(cycle-sort-function . identity))))
      (complete-with-action action candidates string predicate))))

(defun memex-completion--read (prompt candidates category annotate what)
  "Read one of CANDIDATES with PROMPT and return the candidate chosen.
CATEGORY and ANNOTATE go into the completion metadata.  WHAT names what
was searched in the `user-error' raised when nothing was found, which is
signalled before a picker opens: `completing-read' requiring a match
over no candidate at all cannot be left except by \\[keyboard-quit].

The answer is looked up rather than used as it comes back, because
`minibuffer-allow-text-properties' is nil and `read-from-minibuffer'
therefore strips what the chosen candidate carried.  Candidate
uniqueness is what makes the lookup sound."
  (unless candidates
    (user-error "No memex %s to read" what))
  (let ((choice (completing-read
                 prompt
                 (memex-completion--table candidates category annotate)
                 nil t)))
    (or (car (member choice candidates))
        (user-error "No memex %s chosen" what))))

;;;###autoload
(defun memex-completion-record-of (candidate)
  "Return the record CANDIDATE carries, or nil when it carries none."
  (and candidate (get-text-property 0 'memex-record candidate)))

(defun memex-completion--fetch (start)
  "Run START and return what memex answered with.
START is called with a callback and an errback and answers with the
process running the request.  The wait is bounded by
`memex-completion-timeout' and can be left with \\[keyboard-quit];
either way the request is abandoned.  A failure re-signals memex-core's
error object unchanged."
  (let ((outcome nil)
        (process nil))
    (unwind-protect
        (let ((deadline (+ (float-time) memex-completion-timeout)))
          (setq process
                (funcall start
                         (lambda (data) (setq outcome (cons 'ok data)))
                         (lambda (failure) (setq outcome (cons 'failed failure)))))
          (while (and (null outcome) (< (float-time) deadline))
            (accept-process-output process 0.05)))
      (memex-cancel-rpc process))
    (cond ((eq (car outcome) 'ok) (cdr outcome))
          ((eq (car outcome) 'failed) (signal (cadr outcome) (cddr outcome)))
          (t (signal 'memex-error
                     (list :message
                           (format "memex did not answer within %s seconds"
                                   memex-completion-timeout)))))))

(defun memex-completion--recent ()
  "Return the records of memex's recent window, newest first."
  (mapcar #'cadr
          (memex-completion--fetch
           (lambda (callback errback)
             (memex-api-recent callback :errback errback
                               :limit memex-completion-recent-limit)))))

(defun memex-completion--sessions (records)
  "Return one record per session of RECORDS, memex's order kept.
A session is a `session_id' at a `source_path' - the same id under two
paths is two sessions, since one transcript file is one session - and
the newest record of a session stands for it."
  (let ((newest (make-hash-table :test #'equal))
        (order nil))
    (dolist (record records)
      (let ((id (alist-get 'session_id record))
            (path (alist-get 'source_path record)))
        (when (and id path)
          (let* ((key (cons id path))
                 (kept (gethash key newest)))
            (unless kept (push key order))
            (when (or (null kept)
                      (> (or (alist-get 'ts record) 0)
                         (or (alist-get 'ts kept) 0)))
              (puthash key record newest))))))
    (mapcar (lambda (key) (gethash key newest)) (nreverse order))))

(defun memex-completion--projects (records)
  "Return the projects of RECORDS, each first appearance kept in order.
An entry is a project name consed onto how many of RECORDS carry it and
the newest `ts' among them."
  (let ((tallies nil))
    (dolist (record records)
      (let ((project (alist-get 'project record))
            (ts (alist-get 'ts record)))
        (when project
          (let ((entry (assoc project tallies)))
            (if entry
                (setcdr entry (cons (1+ (cadr entry))
                                    (if (> (or ts 0) (or (cddr entry) 0))
                                        ts
                                      (cddr entry))))
              (push (cons project (cons 1 ts)) tallies))))))
    (nreverse tallies)))

(defun memex-completion--annotate-project (projects)
  "Return the function annotating a project with its entry in PROJECTS."
  (lambda (candidate)
    (let ((entry (cdr (assoc candidate projects))))
      (concat "  "
              (memex-completion--join
               (and entry (format (ngettext "%d record" "%d records" (car entry))
                                  (car entry)))
               (and entry (memex-completion--time (cdr entry))))))))

;;;###autoload
(defun memex-read-record (&optional prompt records)
  "Read one of memex's recent records and return it.
PROMPT replaces the minibuffer prompt.  RECORDS replaces the recent
window the candidates are otherwise fetched from.  The answer is the
record alist memex sent, the same one the chosen candidate carried in
its `memex-record' text property."
  (let ((candidates (memex-completion-record-candidates
                     (or records (memex-completion--recent)))))
    (memex-completion-record-of
     (memex-completion--read (or prompt "memex record: ") candidates
                             'memex-record #'memex-completion-annotate
                             "records"))))

;;;###autoload
(defun memex-read-session (&optional prompt records)
  "Read one of the sessions of memex's recent records and return its record.
PROMPT replaces the minibuffer prompt.  RECORDS replaces the recent
window the sessions are otherwise gathered from, and is grouped the same
way.  The answer is the newest record of the session, whose `session_id'
and `source_path' name the session to `memex-api-session'."
  (let ((candidates (memex-completion-session-candidates
                     (memex-completion--sessions
                      (or records (memex-completion--recent))))))
    (memex-completion-record-of
     (memex-completion--read (or prompt "memex session: ") candidates
                             'memex-session #'memex-completion-annotate
                             "sessions"))))

;;;###autoload
(defun memex-read-project (&optional prompt records)
  "Read one of the projects of memex's recent records and return its name.
PROMPT replaces the minibuffer prompt.  RECORDS replaces the recent
window the projects are otherwise drawn from.  This is the one selector
answering with a string: no operation enumerates projects, so the
candidates are the distinct `project' fields of the records and a
project is its name."
  (let ((projects (memex-completion--projects
                   (or records (memex-completion--recent)))))
    (memex-completion--read (or prompt "memex project: ")
                            (mapcar #'car projects) 'memex-project
                            (memex-completion--annotate-project projects)
                            "projects")))

(provide 'memex-completion)
;;; memex-completion.el ends here
