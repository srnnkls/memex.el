;;; memex-core.el --- Transport for the memex RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; Request/response plumbing for memex's stdio RPC.  `memex rpc' reads one
;; JSON request from stdin, writes one response to stdout and exits, so
;; every call gets a process of its own and there is no server to manage.
;; The wrappers in memex-api.el and the commands in memex.el sit on top of
;; this.

;;; Code:

(require 'subr-x)

(defgroup memex nil
  "Search indexed agent conversation history with memex."
  :group 'external
  :prefix "memex-")

(defcustom memex-executable "memex"
  "Name of, or path to, the memex executable."
  :type 'string)

(defconst memex-protocol-version 1
  "Version of memex's RPC protocol this client speaks.")

(define-error 'memex-error "memex error")
(define-error 'memex-rpc-error "memex answered with an error" 'memex-error)
(define-error 'memex-transport-error "cannot talk to memex" 'memex-error)
(define-error 'memex-protocol-error "memex speaks another protocol" 'memex-error)

(defun memex--encode-request (op &optional fields)
  "Return the JSON envelope requesting OP with FIELDS.
FIELDS is an alist encoded as siblings of the operation tag, which is
what memex's internally tagged `RpcOperation' expects."
  (json-serialize `((protocol . ,memex-protocol-version)
                    (request . ,(cons (cons 'op op) fields)))))

(defun memex--decode (string)
  "Return the memex JSON in STRING as an alist.
Null and false both decode to nil: memex emits null for
`SessionContext.cwd' and `next_offset', and false for
`usage_activity.partial'."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun memex--binary-version (executable)
  "Return the version EXECUTABLE reports, or nil when the probe fails."
  (condition-case nil
      (with-temp-buffer
        (let ((default-directory temporary-file-directory))
          (when (eq 0 (call-process executable nil t nil "--version"))
            (let ((output (string-trim (buffer-string))))
              (unless (string-empty-p output) output)))))
    (error nil)))

(defun memex--fail (errback symbol data)
  "Report the error SYMBOL carrying DATA to ERRBACK.
Reports through `message' instead when ERRBACK is nil, since a sentinel
cannot signal to the caller."
  (if errback
      (funcall errback (cons symbol data))
    (message "memex: %S" (cons symbol data))))

(defun memex--stderr-text (buffer)
  "Return what memex wrote to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (buffer-string))))

(defun memex--response-payload (decoded)
  "Return the response object in DECODED, or nil when it is malformed."
  (let ((payload (alist-get 'response decoded)))
    (and (consp payload)
         (consp (car payload))
         (stringp (alist-get 'kind payload))
         payload)))

(defun memex--dispatch-result (status output stderr executable callback errback)
  "Dispatch a finished request to CALLBACK or ERRBACK.
STATUS is the exit status, OUTPUT what memex wrote to stdout and
STDERR what it wrote to stderr.  EXECUTABLE is the binary that
answered.  CALLBACK receives the response payload; ERRBACK receives
the error object."
  (let* ((decoded (and (zerop status)
                       (condition-case nil (memex--decode output) (error nil))))
         (payload (and decoded (memex--response-payload decoded))))
    (cond
     ((null decoded)
      (memex--fail errback 'memex-transport-error
                   (list :exit-status status :stderr stderr)))
     ((not (equal (alist-get 'protocol decoded) memex-protocol-version))
      (memex--fail errback 'memex-protocol-error
                   (list :expected memex-protocol-version
                         :received (alist-get 'protocol decoded)
                         :version (memex--binary-version executable))))
     ((null payload)
      (memex--fail errback 'memex-transport-error
                   (list :exit-status status :stderr stderr)))
     ((equal (alist-get 'kind payload) "error")
      (memex--fail errback 'memex-rpc-error
                   (list :message (alist-get 'message payload))))
     (t (funcall callback payload)))))

(defun memex-rpc (op fields callback &optional errback)
  "Run OP with FIELDS through `memex rpc' and return the process.
FIELDS is an alist of request fields.  CALLBACK is called with the
response payload alist once memex exits, the envelope stripped and its
`kind' tag kept.  ERRBACK is called with the error object
\(SYMBOL . PLIST) instead when the request fails; without one the
failure is reported through `message'.  Signals
`memex-error' before starting anything when the executable is missing
or the request does not encode."
  (unless (executable-find memex-executable)
    (signal 'memex-error
            (list :message (format "memex executable not found: %s"
                                   memex-executable))))
  (let* ((executable memex-executable)
         (request (condition-case err (memex--encode-request op fields)
                    (error (signal 'memex-error
                                   (list :message
                                         (format "cannot encode the %s request: %s"
                                                 op (error-message-string err)))))))
         (stdout (generate-new-buffer " *memex-rpc*" t))
         (stderr (generate-new-buffer " *memex-rpc-stderr*" t))
         (default-directory temporary-file-directory)
         (process
          (make-process
           :name "memex-rpc"
           :command (list executable "rpc")
           :buffer stdout
           :stderr stderr
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((status (process-exit-status process))
                     (output (with-current-buffer stdout (buffer-string)))
                     (stderr-text (memex--stderr-text stderr)))
                 (kill-buffer stdout)
                 (kill-buffer stderr)
                 (memex--dispatch-result status output stderr-text executable
                                         callback errback)))))))
    (condition-case nil
        (progn (process-send-string process request)
               (process-send-eof process))
      (error nil))
    process))

(provide 'memex-core)
;;; memex-core.el ends here
