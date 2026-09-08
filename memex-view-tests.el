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

(require 'memex-tests-support)

(require 'magit-section nil t)
(require 'memex-entry nil t)
(require 'memex-view nil t)

(declare-function memex-view-session "memex-view")
(declare-function memex-view-session-buffer "memex-view")
(declare-function memex-view-next-record "memex-view")
(declare-function memex-view-previous-record "memex-view")
(declare-function memex-view-record-at-point "memex-view")
(declare-function memex-view-toggle-tool-content "memex-view")
(declare-function memex-view-jump-to-hit "memex-view")
(declare-function memex-view--fontify "memex-view")
(declare-function memex-view-entry-at-point "memex-view")
(declare-function memex-view-next-problem "memex-view")
(declare-function memex-view--imenu-index "memex-view")
(declare-function memex-view--cut "memex-view")
(declare-function memex-view--tool-label "memex-view")
(declare-function memex-view-copy-command "memex-view")
(declare-function memex-view--payload-buffer "memex-view")
(declare-function memex-entry-status "memex-entry")
(declare-function memex-entry-call "memex-entry")
(declare-function memex-view--debug-string "memex-view")
(declare-function memex-view--tool-payload "memex-view")
(declare-function memex-view-search-in-session "memex-view")
(declare-function memex-entry-kind "memex-entry")
(declare-function memex-entry-tool "memex-entry")
(declare-function memex-view-cycle-assistant "memex-view")
(declare-function memex-view--header-line "memex-view")
(declare-function memex-view--apply-states "memex-view")
(declare-function memex-view--record-position "memex-view")
(declare-function memex-view--reveal "memex-view")
(declare-function memex-view-toggle-details "memex-view")
(declare-function memex-view-tool-section-p "memex-view")

(defvar memex-view-session-id)
(defvar memex-view-source-path)
(defvar memex-view-source)
(defvar so-long-predicate)
(defvar memex-view-output-lines)
(defvar memex-view-states)
(defvar memex-view-indent)

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

(defun memex-view-tests--entry-records (&optional output)
  "Return a message and a Bash call answered by OUTPUT, as memex sends them.
The call's text is a copy of its arguments and the result's a copy of
its output, which is what memex records and what must not be echoed."
  (let ((arguments (concat "{\"command\": String(\"ls -la\"), "
                           "\"description\": String(\"List repo structure\")}"))
        (output (or output "one\ntwo\nthree")))
    (list `((doc_id . 8801) (ts . 1787671116043) (source . "codex")
            (project . "memex.el") (session_id . ,memex-view-tests--session-id)
            (turn_id . 1) (role . "assistant") (text . "an opening remark")
            (source_path . ,memex-view-tests--source-path))
          `((doc_id . 8802) (ts . 1787671116244) (source . "codex")
            (project . "memex.el") (session_id . ,memex-view-tests--session-id)
            (turn_id . 2) (role . "tool_use") (tool_name . "Bash")
            (event_id . "toolu_01gamma") (tool_input . ,arguments)
            (text . ,arguments)
            (source_path . ,memex-view-tests--source-path))
          `((doc_id . 8803) (ts . 1787671117633) (source . "codex")
            (project . "memex.el") (session_id . ,memex-view-tests--session-id)
            (turn_id . 3) (role . "tool_result") (tool_name . "Bash")
            (parent_tool_use_id . "toolu_01gamma") (tool_output . ,output)
            (text . ,output)
            (source_path . ,memex-view-tests--source-path)))))

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

(defun memex-view-tests--show-tool-record ()
  "Show and return the first tool record in the current viewer."
  (let ((section (seq-find
                  (lambda (candidate)
                    (memex-entry-tool (oref candidate value)))
                  (oref magit-root-section children))))
    (magit-section-show section)
    section))

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
          (memex-view-toggle-tool-content)
          (should (equal (memex-view-tests--rendered-records) records))
          (seq-mapn
           (lambda (token record)
             (let ((position (memex-view-tests--position-of token)))
               (should position)
               (should (equal (get-text-property position 'memex-record)
                              record))))
           '("alpha question" "beta answer" "gamma-output-token"
             "delta follow-up")
           records)))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-draws-the-tail-of-a-session-before-the-rest ()
  (unwind-protect
      (let* ((memex-view-chunk-size 2)
             (records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (= (length (oref magit-root-section children)) 2))
          (should (equal (memex-view-tests--rendered-records) (nthcdr 2 records)))
          (should (car memex-view--pending))
          (memex-view--fill buffer)
          (should (= (length (oref magit-root-section children)) 4))
          (should-not (car memex-view--pending))
          (should (= (marker-position (oref magit-root-section end))
                     (point-max)))
          (memex-view-toggle-tool-content)
          (should (equal (memex-view-tests--rendered-records) records))
          (dolist (token '("alpha question" "beta answer" "gamma-output-token"
                           "delta follow-up"))
            (should (memex-view-tests--position-of token)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-quit-keeps-the-window-it-was-read-in ()
  (let ((other (generate-new-buffer "*memex view tests other*")))
    (unwind-protect
        (let ((buffer (memex-view-tests--open
                       (memex-view-tests--records)
                       memex-view-tests--session-id
                       memex-view-tests--source-path)))
          (set-window-buffer (selected-window) other)
          (display-buffer buffer '(display-buffer-same-window))
          (set-window-dedicated-p (selected-window) t)
          (with-current-buffer buffer (memex-view-quit))
          (should (window-live-p (selected-window)))
          (should (eq (window-buffer (selected-window)) other)))
      (kill-buffer other)
      (memex-view-tests--cleanup))))

(ert-deftest memex-view-prepends-history-without-moving-the-reader ()
  (save-window-excursion
    (unwind-protect
        (let* ((memex-view-chunk-size 1)
               (records (memex-view-tests--records))
               (buffer (memex-view-tests--open records
                                               memex-view-tests--session-id
                                               memex-view-tests--source-path)))
          (switch-to-buffer buffer)
          (let ((window (selected-window)))
            (set-window-start window (point-min))
            (should (equal (memex-view-record-at-point) (car (last records))))
            (memex-view--fill buffer)
            (should (equal (memex-view-record-at-point) (car (last records))))
            (should (= (window-start window) (point)))
            (should (eq (magit-current-section)
                        (car (last (oref magit-root-section children)))))
            (goto-char (point-min))
            (let ((record (memex-view-record-at-point)))
              (set-window-start window (point))
              (memex-view--fill buffer)
              (should (equal (memex-view-record-at-point) record))
              (should (= (window-start window) (point))))
            (memex-view--fill-completely)
            (should (equal (memex-view-tests--rendered-records) records))
            (should (= (marker-position (oref magit-root-section start)) 1))
            (should (= (marker-position (oref magit-root-section end))
                       (point-max)))))
      (memex-view-tests--cleanup))))

(ert-deftest memex-view-prepending-preserves-existing-folds ()
  (unwind-protect
      (let* ((memex-view-chunk-size 2)
             (buffer (memex-view-tests--open
                      (memex-view-tests--records)
                      memex-view-tests--session-id
                      memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((section (car (last (oref magit-root-section children)))))
            (magit-section-hide section)
            (memex-view--fill-completely)
            (should (oref section hidden))
            (should (invisible-p (oref section content)))
            (should (eq section (car (last (oref magit-root-section children)))))
            (magit-section-show section)
            (should-not (invisible-p (oref section content))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-reopening-and-killing-cancel-pending-fill ()
  (unwind-protect
      (let* ((memex-view-chunk-size 1)
             (records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((timer memex-view--fill-timer))
            (should (memq timer timer-idle-list))
            (memex-view--render buffer (memex-view-tests--context records)
                                memex-view-tests--session-id
                                memex-view-tests--source-path)
            (should-not (memq timer timer-idle-list)))
          (let ((timer memex-view--fill-timer))
            (kill-buffer buffer)
            (should-not (memq timer timer-idle-list)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-heads-a-turn-with-its-source-and-its-clock ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path))
             (clock (format-time-string "%T" (/ 1787671116244 1000))))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "⌬ codex" nil t))
          (should (memq 'memex-view-source-codex
                        (ensure-list
                         (get-text-property (match-beginning 0) 'face))))
          (should (string-search clock (thing-at-point 'line t)))
          (goto-char (point-min))
          (should (search-forward "● you" nil t))
          (should-not (string-search "⌬" (thing-at-point 'line t)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-sets-every-entry-off-the-left-edge ()
  (unwind-protect
      (let ((buffer (memex-view-tests--open
                     (memex-view-tests--records)
                     memex-view-tests--session-id
                     memex-view-tests--source-path)))
        (with-current-buffer buffer
          (dolist (token '("alpha question" "beta answer" "delta follow-up"))
            (let* ((position (memex-view-tests--position-of token))
                   (prefix (get-text-property position 'line-prefix)))
              (should (equal (get-text-property 0 'display prefix)
                             `(space :width (,memex-view-message-padding))))
              (should (equal (get-text-property position 'wrap-prefix)
                             prefix))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-draws-flush-when-the-padding-is-off ()
  (let ((memex-view-message-padding 0))
    (unwind-protect
        (let ((buffer (memex-view-tests--open
                       (memex-view-tests--records)
                       memex-view-tests--session-id
                       memex-view-tests--source-path)))
          (with-current-buffer buffer
            (should-not (get-text-property
                         (memex-view-tests--position-of "alpha question")
                         'line-prefix))))
      (memex-view-tests--cleanup))))

(ert-deftest memex-view-leaves-the-clock-out-when-it-is-turned-off ()
  (let ((memex-view-heading-clock nil))
    (unwind-protect
        (let ((buffer (memex-view-tests--open
                       (memex-view-tests--records)
                       memex-view-tests--session-id
                       memex-view-tests--source-path))
              (clock (format-time-string "%T" (/ 1787671116244 1000))))
          (with-current-buffer buffer
            (should-not (memex-view-tests--position-of clock))))
      (memex-view-tests--cleanup))))

(ert-deftest memex-view-counts-the-failures-as-it-draws-them ()
  (unwind-protect
      (let* ((memex-view-chunk-size 1)
             (records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (memex-view--fill-completely)
          (should (= memex-view--problems
                     (seq-count (lambda (section)
                                  (eq (car (memex-entry-status
                                            (oref section value)))
                                      'warn))
                                (oref magit-root-section children))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-draws-a-record-a-jump-asks-for-before-finding-it ()
  (unwind-protect
      (let* ((memex-view-chunk-size 1)
             (records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (= (length (oref magit-root-section children)) 1))
          (should (memex-view--record-position 8804))
          (should-not (car memex-view--pending))))
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
          (memex-view-toggle-tool-content)
          (goto-char (memex-view-tests--position-of "gamma-output-token"))
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
          (should (equal (get-text-property (point) 'memex-record) tool))
          (let ((position (point))
                (windows (get-buffer-window-list buffer nil t)))
            (should windows)
            (dolist (window windows)
              (should (equal (window-point window) position))))))
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

(ert-deftest memex-view-session-buffer-answers-the-registry-on-the-compound-key ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path))
             (other (memex-view-tests--open (memex-view-tests--records "claude")
                                            memex-view-tests--session-id
                                            memex-view-tests--other-source-path))
             (impostor (generate-new-buffer "*memex view tests impostor*")))
        (with-current-buffer impostor
          (setq-local memex-view-session-id memex-view-tests--adopted-session-id)
          (setq-local memex-view-source-path
                      memex-view-tests--adopted-source-path))
        (unwind-protect
            (progn
              (should (eq (memex-view-session-buffer
                           memex-view-tests--session-id
                           memex-view-tests--source-path)
                          buffer))
              (should (eq (memex-view-session-buffer
                           memex-view-tests--session-id
                           memex-view-tests--other-source-path)
                          other))
              (should-not (memex-view-session-buffer
                           memex-view-tests--session-id
                           "/tmp/memex-view-tests/never-opened.jsonl"))
              (should-not (memex-view-session-buffer nil nil))
              (should-not (memex-view-session-buffer
                           memex-view-tests--adopted-session-id
                           memex-view-tests--adopted-source-path)))
          (kill-buffer impostor)))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-session-displays-through-the-function-it-was-given ()
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (context (memex-view-tests--context records))
             (given nil)
             (displayed nil))
        (cl-letf (((symbol-function 'memex-api-session)
                   (lambda (_id _path callback &rest _)
                     (funcall callback context)
                     nil))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (push buffer displayed) nil)))
          (memex-view-session memex-view-tests--session-id
                              memex-view-tests--source-path nil
                              (lambda (buffer) (push buffer given)))
          (let ((buffer (memex-view-tests--session-buffer
                         memex-view-tests--session-id
                         memex-view-tests--source-path)))
            (should (buffer-live-p buffer))
            (should (equal given (list buffer)))
            (should (null displayed))
            (memex-view-session memex-view-tests--session-id
                                memex-view-tests--source-path)
            (should (equal given (list buffer)))
            (should (equal displayed (list buffer))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-clean-strips-the-escapes-a-terminal-left-behind ()
  "Tool output is captured from a terminal and carries its SGR sequences.
Recorded from a memex record: the version banner arrives wrapped in
colour codes and renders as literal escapes in the transcript."
  (should (equal (memex-view--clean
                  "\033[33mA new version of memex is available\033[0m\nindexed 3")
                 "A new version of memex is available\nindexed 3")))

(ert-deftest memex-view-clean-keeps-newline-and-tab-and-drops-other-controls ()
  "A carriage return is a terminal instruction, not a character to show."
  (should (equal (memex-view--clean "a\tb\r\nc\rd\ae")
                 "a\tb\nc\nde")))

(ert-deftest memex-view-clean-passes-a-missing-field-through ()
  "Most records carry no tool fields at all."
  (should (equal (memex-view--clean nil) nil)))

(ert-deftest memex-view-renders-cleaned-text ()
  (unwind-protect
      (let* ((records (list (memex-view-tests--record
                             :doc-id 9001 :ts 1787671116043 :source "codex"
                             :project "memex.el"
                             :session-id memex-view-tests--session-id
                             :turn-id 1 :role "tool_result"
                             :text "\033[33mbanner\033[0m done"
                             :source-path memex-view-tests--source-path)))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (memex-view-tests--position-of "banner done"))
          (should-not (string-search "\033" (buffer-string)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-opens-across-the-whole-frame-by-default ()
  "A transcript read in a window a few lines tall is not read."
  (unwind-protect
      (let ((action nil))
        (cl-letf (((symbol-function 'memex-api-session)
                   (lambda (_id _path callback &rest _)
                     (funcall callback (memex-view-tests--context
                                        (memex-view-tests--records)))
                     nil))
                  ((symbol-function 'display-buffer)
                   (lambda (_buffer &optional a &rest _) (setq action a) nil)))
          (memex-view-session memex-view-tests--session-id
                              memex-view-tests--source-path))
        (should (equal action memex-view-display-action)))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-renders-the-markdown-an-agent-wrote ()
  "Prose arrives as markdown; a transcript showing its asterisks shows
the source of the message rather than the message."
  (unwind-protect
      (let* ((records (list (memex-view-tests--record
                             :doc-id 9101 :ts 1787671116043 :source "codex"
                             :project "memex.el"
                             :session-id memex-view-tests--session-id
                             :turn-id 1 :role "assistant"
                             :text "a **bold** claim"
                             :source-path memex-view-tests--source-path)))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (memex-view-tests--position-of "bold claim"))
          (should-not (string-search "**" (buffer-string)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-is-a-magit-section-buffer ()
  "The transcript is a section tree, so magit-section's own collapsing,
cycling and level commands are what drive it."
  (skip-unless (featurep 'magit-section))
  (should (provided-mode-derived-p 'memex-session-mode 'magit-section-mode)))

(ert-deftest memex-view-makes-every-record-a-section-carrying-it ()
  "The section's value is the record, so magit's commands and the
`memex-record' property name the same thing."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (memex-view-tests--position-of "alpha question"))
          (let ((section (magit-section-at (point))))
            (should section)
            (should (equal (alist-get 'doc_id
                                      (memex-entry-call (oref section value)))
                           8801))
            (should (equal (memex-entry-call (oref section value))
                           (memex-view-record-at-point))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-magit-toggle-hides-the-record-and-keeps-its-text ()
  "Folding may not cost a search its reach, which is why it is not deletion."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let* ((records (memex-view-tests--records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((body (memex-view-tests--position-of "second line of alpha")))
            (magit-section-toggle (magit-section-at body))
            (should (invisible-p body))
            (goto-char (point-min))
            (should (search-forward "second line of alpha" nil t)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-fontifies-tool-output-in-the-mode-that-fits-it ()
  "Tool output is code and a diff, and reads as one only with its faces."
  (let ((drawn (memex-view--fontify "if [ -f x ]; then echo hi; fi" 'sh-mode)))
    (should (stringp drawn))
    (should (text-property-not-all 0 (length drawn) 'face nil drawn))))

(ert-deftest memex-view-fontify-without-a-mode-leaves-the-text-alone ()
  (should (equal (memex-view--fontify "plain" nil) "plain")))

(ert-deftest memex-view-fontify-survives-a-mode-that-will-not-load ()
  "A mode that errors may not cost the reader the record."
  (should (equal (memex-view--fontify "plain" 'no-such-mode) "plain")))

(ert-deftest memex-view-sets-up-a-mode-once-not-once-per-record ()
  "Entering a major mode is the expensive part of fontifying, and a
session holds hundreds of records in the same handful of modes.
Measured on one transcript: 586 records wanting `sh-mode'."
  (let ((entries 0))
    (cl-letf* ((original (symbol-function 'sh-mode))
               ((symbol-function 'sh-mode)
                (lambda (&rest args) (setq entries (1+ entries))
                  (apply original args))))
      (dotimes (i 25)
        (memex-view--fontify (format "echo %d; if [ -f x ]; then :; fi" i)
                             'sh-mode)))
    (should (<= entries 2))))

(ert-deftest memex-view-defers-a-tool-record-body-until-it-is-opened ()
  (let ((parses 0))
    (unwind-protect
        (cl-letf* ((original (symbol-function 'memex-entry-fields))
                   ((symbol-function 'memex-entry-fields)
                    (lambda (input)
                      (when input (setq parses (1+ parses)))
                      (funcall original input))))
          (let ((buffer (memex-view-tests--open
                         (memex-view-tests--entry-records)
                         memex-view-tests--session-id
                         memex-view-tests--source-path)))
            (with-current-buffer buffer
              (let* ((section (seq-find
                               (lambda (candidate)
                                 (memex-entry-tool (oref candidate value)))
                               (oref magit-root-section children)))
                     (entry (oref section value)))
                (should (equal parses 1))
                (should (oref section hidden))
                (should-not (oref section children))
                (should-not (string-search "command (1 line)" (buffer-string)))
                (should-not (string-match-p "lines +3" (buffer-string)))
                (magit-section-show section)
                (should (equal parses 2))
                (should (equal (length (oref section children)) 2))
                (should (string-search "command (1 line)" (buffer-string)))
                (should (string-match-p "lines +3" (buffer-string)))
                (let* ((position (memex-view-tests--position-of "command (1 line)"))
                       (children (oref section children))
                       (rendered (buffer-string)))
                  (should (equal (get-text-property position 'memex-entry) entry))
                  (magit-section-hide section)
                  (magit-section-show section)
                  (should (equal parses 2))
                  (should (equal (oref section children) children))
                  (should (equal (buffer-string) rendered)))))))
      (memex-view-tests--cleanup))))

(ert-deftest memex-view-heads-an-entry-with-what-it-was-for ()
  "The role and the raw type say nothing a reader wants; the
description the agent wrote for one says all of it."
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (re-search-forward "Bash.*List repo structure" nil t))
          (goto-char (point-min))
          (should-not (re-search-forward "^tool_use" nil t))
          (goto-char (point-min))
          (should-not (re-search-forward "^tool_result" nil t))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-shows-a-call-and-its-result-as-one-section ()
  "Two records, one entry: the result is a child of the call it answered."
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (equal (length (oref magit-root-section children)) 2))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-never-echoes-an-argument-back-as-body-text ()
  "A call's text is a copy of its arguments and a result's is a copy of
its output, so showing both prints everything twice."
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (memex-view-tests--show-tool-record)
          (should-not (string-search "String(" (buffer-string)))
          (goto-char (point-min))
          (should (re-search-forward "^ +command (1 line)$" nil t))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-shows-the-metadata-as-an-aligned-block ()
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (memex-view-tests--show-tool-record)
          (goto-char (point-min))
          (should (re-search-forward "^ +lines +3$" nil t))
          (goto-char (line-beginning-position))
          (should (re-search-forward "lines" (line-end-position) t))
          (should (memq 'shadow
                        (ensure-list
                         (get-text-property (match-beginning 0) 'face))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-gives-every-command-a-fold-and-sets-it-in ()
  "A command was a metadata row when it was short and a fold when it was
long, so one transcript read two ways; and a fold nobody indented reads
as a sibling of the entry holding it."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (memex-view-tests--show-tool-record)
          (goto-char (point-min))
          (should (re-search-forward "^\\( +\\)command (1 line)$" nil t))
          (should (equal (length (match-string 1)) memex-view-indent))
          (let ((section (magit-section-at (match-beginning 0))))
            (should (memex-view-tool-section-p section))
            (should (oref section hidden))
            (magit-section-show section))
          (goto-char (point-min))
          (should (re-search-forward "^\\( +\\)ls -la$" nil t))
          (should (equal (length (match-string 1)) (* 2 memex-view-indent)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-marks-an-entry-whose-output-reported-a-failure ()
  "A suite that exits 0 and reports `1 unexpected' is the case worth
seeing folded, so the reason rides on the heading."
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records "Ran 12 tests\n1 unexpected\n"))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (re-search-forward "▲" nil t))
          (goto-char (point-min))
          (should (re-search-forward "1 unexpected" nil t))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-moves-between-the-entries-that-went-wrong ()
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records "boom\nError: bad\n"))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (memex-view-next-problem)
          (should (eq (car (memex-entry-status
                            (memex-view-entry-at-point)))
                      'warn))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-has-no-problem-to-move-to-when-nothing-went-wrong ()
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should-error (memex-view-next-problem) :type 'user-error)))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-builds-imenu-from-the-descriptions ()
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((index (memex-view--imenu-index)))
            (should (assoc "List repo structure" index))
            (should (markerp (cdr (assoc "List repo structure" index)))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-header-line-counts-the-session-it-is-showing ()
  "The pinned line says what the whole transcript is, since which entry
is at the top of the window is what the fold markers already show."
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records "boom\nError: bad\n"))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((line (memex-view--header-line)))
            (should (string-search "memex.el" line))
            (should (string-search "2 entries" line))
            (should (string-search "1 err" line)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-header-line-survives-an-empty-transcript ()
  "The header line is evaluated on every redisplay, so a session memex
answered with no records would otherwise raise once per frame."
  (with-temp-buffer
    (should (stringp (memex-view--header-line)))))

(ert-deftest memex-view-cuts-a-long-output-and-says-how-much-is-left ()
  "A thousand-line build log is not read in a transcript; its first
screenful is, and the rest is one keystroke away."
  (let* ((memex-view-output-lines 3)
         (long (mapconcat #'number-to-string (number-sequence 1 40) "\n")))
    (pcase-let ((`(,shown . ,rest) (memex-view--cut long)))
      (should (equal (length (split-string shown "\n")) 3))
      (should (equal rest 37)))))

(ert-deftest memex-view-cuts-nothing-that-already-fits ()
  (let ((memex-view-output-lines 10))
    (should-not (cdr (memex-view--cut "one\ntwo")))))

(ert-deftest memex-view-names-the-tool-the-way-a-reader-writes-it ()
  (should (equal (memex-view--tool-label "Bash") "bash"))
  (should (equal (memex-view--tool-label "NotebookEdit") "notebookedit"))
  (should-not (memex-view--tool-label nil)))

(ert-deftest memex-view-copies-the-command-an-entry-ran ()
  (unwind-protect
      (let* ((records (memex-view-tests--entry-records))
             (buffer (memex-view-tests--open records
                                             memex-view-tests--session-id
                                             memex-view-tests--source-path))
             (kill-ring nil))
        (with-current-buffer buffer
          (goto-char (memex-view-tests--position-of "List repo structure"))
          (memex-view-copy-command)
          (should (equal (current-kill 0) "ls -la"))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-pops-a-payload-into-a-buffer-of-its-own ()
  "A file's contents are read in the mode they are written in, not in a
transcript."
  (let ((buffer (memex-view--payload-buffer "x = 1\n" 'python-mode "content")))
    (unwind-protect
        (with-current-buffer buffer
          (should (eq major-mode 'python-mode))
          (should (equal (buffer-string) "x = 1\n")))
      (kill-buffer buffer))))

(defun memex-view-tests--mixed-records ()
  "Return the fixture session with an injected record among the four.
A real transcript carries far more of these than of anything a person
typed, and they arrive under the same `user' role."
  (append (memex-view-tests--records)
          (list (memex-view-tests--record
                 :doc-id 8805 :ts 1787671117500 :source "codex"
                 :project "memex.el"
                 :session-id memex-view-tests--session-id :turn-id 7404
                 :role "user"
                 :text "<system-reminder> harness noise </system-reminder>"
                 :source-path memex-view-tests--source-path))))

(defun memex-view-tests--folds ()
  "Return each record section as its kind consed onto whether it is folded."
  (mapcar (lambda (section)
            (cons (memex-entry-kind (oref section value))
                  (and (oref section hidden) t)))
          (oref magit-root-section children)))

(ert-deftest memex-view-opens-with-the-talking-shown-and-the-calls-folded ()
  "A session is read for the conversation in it: the calls that carried
the conversation out are worth a line each until one is asked for, and
what the harness injected is worth none."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--mixed-records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (equal (memex-view-tests--folds)
                         '((human . nil) (assistant . nil) (tool . t)
                           (human . nil) (system . t))))
          (should (memq 'system buffer-invisibility-spec))
          (should (memq t buffer-invisibility-spec))
          (should-not (memex-view-tests--position-of "bytes"))
          (should (invisible-p (memex-view-tests--position-of "noise")))
          (should-not
           (invisible-p (memex-view-tests--position-of "second line of alpha")))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-cycles-a-kind-through-whole-heading-and-gone ()
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((heading (memex-view--record-position 8802))
                (body (memex-view-tests--position-of "second line of beta")))
            (should-not (invisible-p body))
            (memex-view-cycle-assistant)
            (should (invisible-p body))
            (should-not (invisible-p heading))
            (should (string-search "assistant" (memex-view--header-line)))
            (memex-view-cycle-assistant)
            (should (invisible-p heading))
            (memex-view-cycle-assistant)
            (should-not (invisible-p body))
            (should-not (memq 'assistant buffer-invisibility-spec))
            (should-not (string-search "assistant"
                                       (memex-view--header-line))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-moves-over-what-is-filtered-without-deleting-it ()
  "Filtering may not cost a search its reach, and may not strand `n' on
an entry the reader cannot see."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--mixed-records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((reached nil))
            (while (ignore-errors (memex-view-next-record) t)
              (push (alist-get 'doc_id (memex-view-record-at-point)) reached))
            (should (equal (nreverse reached) '(8802 8803 8804))))
          (goto-char (point-min))
          (should (search-forward "noise" nil t))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-lays-the-agent-on-different-ground-than-the-person ()
  "A run of turns is unreadable when they all share one background."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (cl-flet ((ground (text)
                      (ensure-list
                       (get-text-property (memex-view-tests--position-of text)
                                          'face))))
            (should (memq 'memex-view-human (ground "second line of alpha")))
            (should (memq 'memex-view-assistant (ground "second line of beta")))
            (should-not (memq 'memex-view-human
                              (ground "second line of beta"))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-heads-an-agent-turn-in-its-own-colour ()
  "The mark and the name beside it say the same thing, so they say it
in the same colour."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open
                     (memex-view-tests--records "claude")
                     memex-view-tests--session-id
                     memex-view-tests--source-path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (re-search-forward "claude" nil t))
          (should (memq 'memex-view-source-claude
                        (ensure-list (get-text-property (match-beginning 0)
                                                        'face))))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-brings-a-hit-out-of-whatever-is-folded-over-it ()
  "Most of a transcript is folded or filtered by the time a hit is
searched for, and a hit the reader cannot see is worth nothing."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--mixed-records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (should (invisible-p (memex-view-tests--position-of "noise")))
          (memex-view-jump-to-hit '((doc_id . 8805)))
          (should-not (invisible-p (point)))
          (should (equal (alist-get 'doc_id (memex-view-record-at-point)) 8805))
          (goto-char (point-min))
          (memex-view-jump-to-hit '((doc_id . 8803)))
          (should-not (invisible-p (point)))))
    (memex-view-tests--cleanup)))

(ert-deftest memex-view-keeps-the-numbers-behind-a-key-rather-than-on-every-line ()
  "How long a call took and what it is called are worth having on
request and worth nobody's eye on every line of a session."
  (skip-unless (featurep 'magit-section))
  (unwind-protect
      (let ((buffer (memex-view-tests--open (memex-view-tests--entry-records)
                                            memex-view-tests--session-id
                                            memex-view-tests--source-path)))
        (with-current-buffer buffer
          (let ((identifier (memex-view-tests--position-of "#8802")))
            (should identifier)
            (should (invisible-p identifier))
            (should (memq 'detail buffer-invisibility-spec))
            (memex-view-toggle-details)
            (should-not (invisible-p identifier))
            (should-not (memq 'detail buffer-invisibility-spec))
            (memex-view-toggle-details)
            (should (invisible-p identifier)))))
    (memex-view-tests--cleanup)))

(provide 'memex-view-tests)
;;; memex-view-tests.el ends here
