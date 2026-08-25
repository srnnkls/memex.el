;;; memex-view-tests.el --- Tests for memex-view.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-view-tests.el -f ert-run-tests-batch-and-exit
;;
;; The viewer tests replace `memex-api-session' for the duration of a
;; fetch, so a session renders without a memex install or an indexed
;; corpus.  Every assertion reads the rendered buffer through the
;; `memex-record' text property rather than through its text: that
;; property is the seam navigation, embark and the herdr bridge all go
;; through, and the rendering around it is free to change.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)

(require 'memex-view nil t)

(declare-function memex-view-session "memex-view")
(declare-function memex-view-next-record "memex-view")
(declare-function memex-view-previous-record "memex-view")
(declare-function memex-view-record-at-point "memex-view")
(declare-function memex-view-toggle-tool-content "memex-view")
(declare-function memex-view-jump-to-hit "memex-view")
(declare-function memex-view-search-in-session "memex-view")

(defvar memex-view-session-id)
(defvar memex-view-source-path)
(defvar memex-view-source)
(defvar so-long-predicate)

(declare-function memex-session-mode "memex-view")

(defconst memex-view-tests--session-id "caea32e0-f5ad-4906-9f1f-b9b7047bd7a1"
  "The `session_id' the fixture session is served under.")

(defconst memex-view-tests--source-path
  "/tmp/memex-view-tests/caea32e0-f5ad-4906-9f1f-b9b7047bd7a1.jsonl"
  "The `source_path' the fixture session is served under.")

(defconst memex-view-tests--other-source-path
  "/tmp/memex-view-tests/other/caea32e0-f5ad-4906-9f1f-b9b7047bd7a1.jsonl"
  "A second transcript repeating the fixture's `session_id'.")

(defconst memex-view-tests--adopted-session-id
  "3b71c8de-2a04-4d59-8f7c-1c0b6d2e5a11"
  "The `session_id' of a viewer buffer the tests set up themselves.")

(defconst memex-view-tests--adopted-source-path
  "/tmp/memex-view-tests/3b71c8de-2a04-4d59-8f7c-1c0b6d2e5a11.jsonl"
  "The `source_path' of a viewer buffer the tests set up themselves.")

(defvar memex-view-tests--requests nil
  "The session fetches memex was handed, as (SESSION-ID . SOURCE-PATH).")

(cl-defun memex-view-tests--record (&key doc-id ts source project
                                        session-id turn-id role text tool-name
                                        tool-input tool-output parent-tool-use-id
                                        source-path)
  "Return a record alist carrying the fields it was given.
DOC-ID, TS, SOURCE, PROJECT, SESSION-ID, TURN-ID, ROLE, TEXT, TOOL-NAME,
TOOL-INPUT, TOOL-OUTPUT, PARENT-TOOL-USE-ID and SOURCE-PATH are the
record's wire fields, the link fields top-level the way memex flattens
them.  Every one left nil is absent from the alist, the way memex omits
a `Record' optional rather than sending null."
  (delq nil
        (list (and source (cons 'source source))
              (cons 'doc_id doc-id)
              (cons 'ts ts)
              (and project (cons 'project project))
              (and session-id (cons 'session_id session-id))
              (and turn-id (cons 'turn_id turn-id))
              (and role (cons 'role role))
              (and text (cons 'text text))
              (and tool-name (cons 'tool_name tool-name))
              (and tool-input (cons 'tool_input tool-input))
              (and tool-output (cons 'tool_output tool-output))
              (and parent-tool-use-id
                   (cons 'parent_tool_use_id parent-tool-use-id))
              (and source-path (cons 'source_path source-path)))))

(defun memex-view-tests--records (&optional source)
  "Return the fixture session's four records, oldest first.
Every record spans several lines and every one opens with a token of
its own, so a record boundary is nowhere near a line boundary and each
record is told apart from the first one.  The third is a `tool_result'
carrying both tool fields.  SOURCE is the agent the records came from,
`codex' by default: two sessions rendered at different sources are what
tells a viewer reading the field from one hardcoding a constant."
  (let ((source (or source "codex")))
    (list (memex-view-tests--record
           :doc-id 8801 :ts 1787671116043 :source source :project "memex.el"
           :session-id memex-view-tests--session-id :turn-id 7400 :role "user"
           :text "alpha question\nsecond line of alpha\nthird line of alpha"
           :source-path memex-view-tests--source-path)
          (memex-view-tests--record
           :doc-id 8802 :ts 1787671116244 :source source :project "memex.el"
           :session-id memex-view-tests--session-id :turn-id 7401
           :role "assistant"
           :text "beta answer\nsecond line of beta\nthird line of beta"
           :source-path memex-view-tests--source-path)
          (memex-view-tests--record
           :doc-id 8803 :ts 1787671116802 :source source :project "memex.el"
           :session-id memex-view-tests--session-id :turn-id 7402
           :role "tool_result" :text "gamma result summary"
           :tool-name "Bash"
           :tool-input "rg --files-with-matches gamma-input-token ."
           :tool-output "gamma-output-token line one\ngamma-output-token line two"
           :parent-tool-use-id "toolu_01gamma"
           :source-path memex-view-tests--source-path)
          (memex-view-tests--record
           :doc-id 8804 :ts 1787671117011 :source source :project "memex.el"
           :session-id memex-view-tests--session-id :turn-id 7403 :role "user"
           :text "delta follow-up\nsecond line of delta"
           :source-path memex-view-tests--source-path))))

(defun memex-view-tests--long-line-records ()
  "Return one record whose text is a single 20000-character line.
Minified output and base64 blobs reach this length in real transcripts,
which is what would hand the buffer to `so-long'."
  (list (memex-view-tests--record
         :doc-id 8901 :ts 1787671118000 :source "codex" :project "memex.el"
         :session-id memex-view-tests--session-id :turn-id 7404
         :role "assistant"
         :text (concat "epsilon blob " (make-string 20000 ?x))
         :source-path memex-view-tests--source-path)))

(defun memex-view-tests--context (records)
  "Return the session context memex answers with for RECORDS.
Its `cwd' is nil, which is what memex-core makes of the JSON null memex
sends for a session it knows no working directory for."
  (list (cons 'records records) (cons 'cwd nil)))

(defun memex-view-tests--viewer-buffers ()
  "Return every live buffer carrying a buffer-local `memex-view-session-id'.
This is the registry AD-6 derives from `buffer-list' on each open, so
duplicate suppression is measured the way the herdr bridge measures it."
  (seq-filter (lambda (buffer) (local-variable-p 'memex-view-session-id buffer))
              (buffer-list)))

(defun memex-view-tests--session-buffer (session-id source-path)
  "Return the viewer buffer of SESSION-ID at SOURCE-PATH, or nil."
  (seq-find (lambda (buffer)
              (and (equal (buffer-local-value 'memex-view-session-id buffer)
                          session-id)
                   (equal (buffer-local-value 'memex-view-source-path buffer)
                          source-path)))
            (memex-view-tests--viewer-buffers)))

(defun memex-view-tests--open (records session-id source-path &optional doc-id)
  "Show RECORDS as SESSION-ID at SOURCE-PATH and return the viewer buffer.
`memex-api-session' answers the viewer's one fetch with those RECORDS
and records it in `memex-view-tests--requests', so no memex install is
needed.  DOC-ID is passed on to `memex-view-session'."
  (let ((context (memex-view-tests--context records)))
    (cl-letf (((symbol-function 'memex-api-session)
               (lambda (id path callback &rest _)
                 (push (cons id path) memex-view-tests--requests)
                 (funcall callback context)
                 nil)))
      (memex-view-session session-id source-path doc-id)))
  (memex-view-tests--session-buffer session-id source-path))

(defun memex-view-tests--adopted-buffer ()
  "Return a viewer buffer for the adopted session, made without the viewer.
It carries the major mode and the three buffer locals an open leaves
behind and nothing else, so only a registry derived from `buffer-list'
finds it."
  (let ((buffer (generate-new-buffer "*memex adopted session*")))
    (with-current-buffer buffer
      (memex-session-mode)
      (setq-local memex-view-session-id memex-view-tests--adopted-session-id)
      (setq-local memex-view-source-path memex-view-tests--adopted-source-path)
      (setq-local memex-view-source "codex"))
    buffer))

(defun memex-view-tests--cleanup ()
  "Kill every viewer buffer left behind."
  (setq memex-view-tests--requests nil)
  (dolist (buffer (buffer-list))
    (when (local-variable-p 'memex-view-session-id buffer)
      (kill-buffer buffer))))

(defun memex-view-tests--position-of (text)
  "Return the position of TEXT in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t) (match-beginning 0))))

(defun memex-view-tests--rendered-records ()
  "Return the records the buffer's `memex-record' property carries, in order."
  (let ((position (point-min))
        (records nil))
    (while (< position (point-max))
      (let ((record (get-text-property position 'memex-record)))
        (when (and record (not (equal record (car records))))
          (push record records)))
      (setq position (next-single-property-change
                      position 'memex-record nil (point-max))))
    (nreverse records)))

(defun memex-view-tests--starts-record-p (record)
  "Return non-nil when point sits on the first character RECORD covers."
  (and (equal (get-text-property (point) 'memex-record) record)
       (not (equal (get-text-property (max (point-min) (1- (point)))
                                      'memex-record)
                   record))))

(ert-deftest memex-view-renders-every-record-under-the-record-property ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (memex-view-tests--requests nil)
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (should (buffer-live-p buffer))
        (should (equal memex-view-tests--requests
                       (list (cons memex-view-tests--session-id
                                   memex-view-tests--source-path))))
        (with-current-buffer buffer
          (should (equal (memex-view-tests--rendered-records) records))
          (seq-mapn
           (lambda (token record)
             (let ((position (memex-view-tests--position-of token)))
               (should position)
               (should (equal (get-text-property position 'memex-record)
                              record))))
           '("alpha question" "beta answer" "gamma result summary"
             "delta follow-up")
           records)))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-keys-the-buffer-on-the-session-id-and-source-path ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (local-variable-p 'memex-view-session-id))
          (should (local-variable-p 'memex-view-source-path))
          (should (local-variable-p 'memex-view-source))
          (should (equal memex-view-session-id memex-view-tests--session-id))
          (should (equal memex-view-source-path
                         memex-view-tests--source-path))
          (should (equal memex-view-source "codex")))
        (memex-view-tests--open records
                                memex-view-tests--session-id
                                memex-view-tests--source-path)
        (should (equal (length (memex-view-tests--viewer-buffers)) 1))
        (should (eq (memex-view-tests--session-buffer
                     memex-view-tests--session-id
                     memex-view-tests--source-path)
                    buffer))
        (let ((other (memex-view-tests--open
                      (memex-view-tests--records "claude")
                      memex-view-tests--session-id
                      memex-view-tests--other-source-path)))
          (should (buffer-live-p other))
          (should-not (eq other buffer))
          (should (equal (length (memex-view-tests--viewer-buffers)) 2))
          (should (equal (buffer-local-value 'memex-view-source other) "claude"))
          (should (equal (buffer-local-value 'memex-view-source buffer)
                         "codex")))
        (let ((adopted (memex-view-tests--adopted-buffer)))
          (memex-view-tests--open records
                                  memex-view-tests--adopted-session-id
                                  memex-view-tests--adopted-source-path)
          (should (equal (length (memex-view-tests--viewer-buffers)) 3))
          (should (buffer-live-p adopted))
          (should (eq (memex-view-tests--session-buffer
                       memex-view-tests--adopted-session-id
                       memex-view-tests--adopted-source-path)
                      adopted))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-record-at-point-returns-the-record-point-is-in ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (tool (nth 2 records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (memex-view-tests--position-of "gamma result summary"))
          (forward-char 6)
          (let ((record (memex-view-record-at-point)))
            (should (equal (alist-get 'doc_id record) 8803))
            (should (equal record tool)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-navigation-moves-by-record-not-by-line ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (assistant (nth 1 records))
             (tool (nth 2 records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (memex-view-tests--position-of "second line of beta"))
          (let ((line (line-number-at-pos)))
            (memex-view-next-record)
            (should (memex-view-tests--starts-record-p tool))
            (should (> (line-number-at-pos) (1+ line))))
          (memex-view-previous-record)
          (should (memex-view-tests--starts-record-p assistant))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-folds-tool-content-and-toggles-it-back ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((input (memex-view-tests--position-of "gamma-input-token"))
                (output (memex-view-tests--position-of "gamma-output-token"))
                (summary (memex-view-tests--position-of "gamma result summary"))
                (answer (memex-view-tests--position-of "beta answer")))
            (should input)
            (should output)
            (should summary)
            (should (get-text-property input 'invisible))
            (should (get-text-property output 'invisible))
            (should (invisible-p input))
            (should (invisible-p output))
            (should-not (invisible-p summary))
            (should-not (invisible-p answer))
            (memex-view-toggle-tool-content)
            (should-not (invisible-p input))
            (should-not (invisible-p output))
            (should-not (invisible-p summary))
            (should (memex-view-tests--position-of "gamma-output-token line two"))
            (memex-view-toggle-tool-content)
            (should (invisible-p output))
            (should-not (invisible-p summary))
            (should (memex-view-tests--position-of
                     "gamma-output-token line two")))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-keeps-its-mode-on-a-transcript-of-very-long-lines ()
  (unwind-protect
      (let* ((records (memex-view-tests--long-line-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (eq major-mode 'memex-session-mode))
          (should (local-variable-p 'so-long-predicate))
          (should (eq so-long-predicate 'ignore))
          (let ((position (memex-view-tests--position-of "epsilon blob")))
            (should position)
            (should (equal (get-text-property position 'memex-record)
                           (car records))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-session-puts-point-on-the-record-it-was-given ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (tool (nth 2 records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path
                                             8803)))
        (with-current-buffer buffer
          (should (equal (get-text-property (point) 'memex-record) tool))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-jump-to-hit-moves-point-to-the-record-it-names ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (follow-up (nth 3 records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (memex-view-jump-to-hit follow-up)
          (should (memex-view-tests--starts-record-p follow-up))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-search-in-session-narrows-to-this-session-alone ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path))
             (call nil))
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _)
                       (error "The query was given, not read")))
                    ((symbol-function 'memex-api-search)
                     (lambda (query _callback &rest keys)
                       (setq call (cons query keys))
                       nil)))
            (memex-view-search-in-session "gamma-input-token")))
        (should (equal (car call) "gamma-input-token"))
        (let ((scope (plist-get (cdr call) :session-scope)))
          (should (equal (length scope) 1))
          (should (equal (plist-get (car scope) :source) "codex"))
          (should (equal (plist-get (car scope) :session-id)
                         memex-view-tests--session-id))
          (should (equal (plist-get (car scope) :source-path)
                         memex-view-tests--source-path))))
    (memex-view-tests--cleanup)))

(provide 'memex-view-tests)
;;; memex-view-tests.el ends here
