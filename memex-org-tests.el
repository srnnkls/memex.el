;;; memex-org-tests.el --- Tests for memex-org.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-org-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ol)
(require 'org)
(require 'org-capture)
(require 'memex-core)
(require 'memex-api)
(require 'memex-view)

(require 'memex-org nil t)

(declare-function memex-org-link "memex-org")
(declare-function memex-org-link-path "memex-org")
(declare-function memex-org-link-parse "memex-org")
(declare-function memex-org-capture "memex-org")
(declare-function memex-herdr-open-session "memex-herdr")

(defvar memex-org-capture-template)

(defconst memex-org-tests--session-id "caea32e0-f5ad-4906-9f1f-b9b7047bd7a1"
  "The `session_id' the fixture records carry.")

(defconst memex-org-tests--source-path
  "/tmp/memex-org-tests/caea32e0-f5ad-4906-9f1f-b9b7047bd7a1.jsonl"
  "The `source_path' the fixture records carry.")

(defconst memex-org-tests--other-source-path
  "/tmp/memex-org-tests/other/caea32e0-f5ad-4906-9f1f-b9b7047bd7a1.jsonl"
  "A second transcript repeating the fixture's `session_id'.")

(defconst memex-org-tests--awkward-source-path
  "/tmp/memex org tests/odd [1]::2/session.jsonl"
  "A `source_path' carrying the characters Org link syntax reserves.
Spaces, brackets and the `::' search separator all appear in it.")

(defun memex-org-tests--record (doc-id session-id source-path &optional text)
  "Return a record alist for DOC-ID in SESSION-ID at SOURCE-PATH.
TEXT is the record's text, a default one without it.  A nil DOC-ID is
absent from the alist, the way memex omits a `Record' optional rather
than sending null."
  (delq nil
        (list (cons 'source "codex")
              (and doc-id (cons 'doc_id doc-id))
              (cons 'ts 1787671116043)
              (cons 'project "memex.el")
              (cons 'session_id session-id)
              (cons 'role "assistant")
              (cons 'text (or text "alpha answer\nsecond line of alpha"))
              (cons 'source_path source-path))))

(defun memex-org-tests--path-in (string)
  "Return the memex link path STRING carries, or nil when it carries none."
  (and (string-match "\\[\\[memex:\\([^]]*\\)\\]" string)
       (match-string 1 string)))

(ert-deftest memex-org-link-round-trips-the-compound-key ()
  "A link built from a record parses back to the record's triple.
The awkward path and the record without a `doc_id' go through the same
pair of functions."
  (let* ((record (memex-org-tests--record
                  8801 memex-org-tests--session-id
                  memex-org-tests--source-path))
         (parsed (memex-org-link-parse (memex-org-link-path record))))
    (should (equal (plist-get parsed :session-id) memex-org-tests--session-id))
    (should (equal (plist-get parsed :source-path)
                   memex-org-tests--source-path))
    (should (equal (plist-get parsed :doc-id) 8801)))
  (let* ((record (memex-org-tests--record
                  8802 memex-org-tests--session-id
                  memex-org-tests--awkward-source-path))
         (parsed (memex-org-link-parse (memex-org-link-path record))))
    (should (equal (plist-get parsed :source-path)
                   memex-org-tests--awkward-source-path))
    (should (equal (plist-get parsed :doc-id) 8802)))
  (let* ((record (memex-org-tests--record
                  nil memex-org-tests--session-id
                  memex-org-tests--source-path))
         (parsed (memex-org-link-parse (memex-org-link-path record))))
    (should (equal (plist-get parsed :session-id) memex-org-tests--session-id))
    (should (equal (plist-get parsed :source-path)
                   memex-org-tests--source-path))
    (should-not (plist-get parsed :doc-id))))

(ert-deftest memex-org-link-distinguishes-sources-under-one-session-id ()
  "Two transcripts sharing a `session_id' get two distinct links.
Keying on the id alone would collapse one onto the other."
  (let* ((one (memex-org-tests--record
               8801 memex-org-tests--session-id
               memex-org-tests--source-path))
         (other (memex-org-tests--record
                 8801 memex-org-tests--session-id
                 memex-org-tests--other-source-path))
         (one-path (memex-org-link-path one))
         (other-path (memex-org-link-path other)))
    (should-not (equal one-path other-path))
    (should (equal (plist-get (memex-org-link-parse one-path) :source-path)
                   memex-org-tests--source-path))
    (should (equal (plist-get (memex-org-link-parse other-path) :source-path)
                   memex-org-tests--other-source-path))))

(ert-deftest memex-org-registers-a-followable-link-type ()
  "Loading memex-org registers the memex link type with a follow function."
  (should (assoc "memex" org-link-parameters))
  (should (functionp (org-link-get-parameter "memex" :follow))))

(ert-deftest memex-org-following-a-link-reaches-the-open-path ()
  "Opening a link through Org hands the open path the record's triple.
The herdr bridge takes the links when it is loaded and the viewer takes
them when it is not — memex-org never requires it.  The awkward path
arrives intact and the record without a `doc_id' opens its session with
none."
  (let ((links (list (memex-org-link (memex-org-tests--record
                                      8801 memex-org-tests--session-id
                                      memex-org-tests--source-path))
                     (memex-org-link (memex-org-tests--record
                                      8802 memex-org-tests--session-id
                                      memex-org-tests--awkward-source-path))
                     (memex-org-link (memex-org-tests--record
                                      nil memex-org-tests--session-id
                                      memex-org-tests--source-path))))
        (triples (list (list memex-org-tests--session-id
                             memex-org-tests--source-path 8801)
                       (list memex-org-tests--session-id
                             memex-org-tests--awkward-source-path 8802)
                       (list memex-org-tests--session-id
                             memex-org-tests--source-path nil)))
        (bridged nil)
        (viewed nil))
    (cl-letf (((symbol-function 'memex-herdr-open-session)
               (lambda (session-id source-path &optional doc-id &rest _)
                 (push (list session-id source-path doc-id) bridged)
                 nil))
              ((symbol-function 'memex-view-session)
               (lambda (session-id source-path &optional doc-id &rest _)
                 (push (list session-id source-path doc-id) viewed)
                 nil)))
      (mapc #'org-link-open-from-string links))
    (should (equal (nreverse bridged) triples))
    (should-not viewed)
    (setq viewed nil)
    (let ((bridge (and (fboundp 'memex-herdr-open-session)
                       (symbol-function 'memex-herdr-open-session))))
      (unwind-protect
          (progn
            (fmakunbound 'memex-herdr-open-session)
            (cl-letf (((symbol-function 'memex-view-session)
                       (lambda (session-id source-path &optional doc-id &rest _)
                         (push (list session-id source-path doc-id) viewed)
                         nil)))
              (mapc #'org-link-open-from-string links)))
        (if bridge
            (fset 'memex-herdr-open-session bridge)
          (fmakunbound 'memex-herdr-open-session))))
    (should (equal (nreverse viewed) triples))))

(ert-deftest memex-org-capture-without-a-template-yields-an-excerpt ()
  "Without a template key the excerpt lands in a buffer and the kill ring.
Nothing reaches `org-capture', so an unconfigured Org setup is never
asked for a target, and the excerpt carries a link back to the record."
  (let ((memex-org-capture-template nil)
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (captured nil)
        (before (buffer-list))
        (record (memex-org-tests--record
                 8801 memex-org-tests--session-id
                 memex-org-tests--source-path "alpha answer under capture")))
    (unwind-protect
        (let ((excerpt
               (cl-letf (((symbol-function 'org-capture-string)
                          (lambda (&rest args) (push args captured) nil)))
                 (memex-org-capture record))))
          (should-not captured)
          (should (string-match-p (regexp-quote "alpha answer under capture")
                                  excerpt))
          (should (equal (current-kill 0 t) excerpt))
          (let ((parsed (memex-org-link-parse
                         (memex-org-tests--path-in excerpt))))
            (should (equal (plist-get parsed :session-id)
                           memex-org-tests--session-id))
            (should (equal (plist-get parsed :source-path)
                           memex-org-tests--source-path))
            (should (equal (plist-get parsed :doc-id) 8801)))
          (should (seq-find (lambda (buffer)
                              (string-match-p
                               (regexp-quote excerpt)
                               (with-current-buffer buffer (buffer-string))))
                            (seq-difference (buffer-list) before))))
      (mapc #'kill-buffer (seq-difference (buffer-list) before)))))

(ert-deftest memex-org-capture-with-a-template-reaches-org-capture ()
  "A configured template key sends the same excerpt to `org-capture'."
  (let ((memex-org-capture-template "m")
        (kill-ring nil)
        (kill-ring-yank-pointer nil)
        (captured nil)
        (record (memex-org-tests--record
                 8801 memex-org-tests--session-id
                 memex-org-tests--source-path "alpha answer under capture")))
    (let ((excerpt
           (cl-letf (((symbol-function 'org-capture-string)
                      (lambda (&rest args) (push args captured) nil)))
             (memex-org-capture record))))
      (should (equal (length captured) 1))
      (should (equal (car captured) (list excerpt "m")))
      (should (string-match-p (regexp-quote "alpha answer under capture")
                              excerpt)))))

(provide 'memex-org-tests)
;;; memex-org-tests.el ends here
