;;; memex-usage.el --- Token usage report buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; `memex-usage' fetches memex's token usage report and puts it up in a
;; read-only buffer: the totals and the pricing context, then every
;; source with its own tokens, cost and cache waste, then whatever the
;; machines warned about or failed with.  `memex-usage--render' turns a
;; report alist into that text and touches nothing else, so the layout
;; can be read without a memex install.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'memex-api)
(require 'memex-completion)

(defconst memex-usage-buffer-name "*memex usage*"
  "Name of the buffer `memex-usage' reports every project into.")

(defun memex-usage--count (value)
  "Return VALUE as a thousands-grouped count, or \"-\" without a number."
  (if (not (numberp value))
      "-"
    (let ((digits (number-to-string (truncate value)))
          (grouped ""))
      (while (> (length digits) 3)
        (setq grouped (concat "," (substring digits -3) grouped)
              digits (substring digits 0 -3)))
      (concat digits grouped))))

(defun memex-usage--cost (value)
  "Return VALUE as a USD amount, or \"-\" without a number."
  (if (numberp value) (format "$%.2f" value) "-"))

(defun memex-usage--row (label value)
  "Return one indented report row naming LABEL and carrying VALUE."
  (concat "  " (string-pad label 22) value))

(defun memex-usage--waste (waste)
  "Return the cache waste alist WASTE as one line."
  (format "%s tokens (%s) over %s misses (%s idle, %s model switch)"
          (memex-usage--count (alist-get 'missed_tokens waste))
          (memex-usage--cost (alist-get 'missed_cost_usd waste))
          (memex-usage--count (alist-get 'miss_count waste))
          (memex-usage--count (alist-get 'idle_misses waste))
          (memex-usage--count (alist-get 'model_switch_misses waste))))

(defun memex-usage--totals (report)
  "Return the totals and the pricing context of REPORT as lines."
  (list (format "Token usage reported by %s" (or (alist-get 'authority report) "-"))
        ""
        (memex-usage--row "events" (memex-usage--count (alist-get 'events report)))
        (memex-usage--row "total tokens"
                          (memex-usage--count (alist-get 'total_tokens report)))
        (memex-usage--row "known cost"
                          (memex-usage--cost (alist-get 'known_cost_usd report)))
        (memex-usage--row "priced events"
                          (memex-usage--count (alist-get 'priced_events report)))
        (memex-usage--row "unpriced events"
                          (memex-usage--count (alist-get 'unpriced_events report)))
        (memex-usage--row "unknown model events"
                          (memex-usage--count (alist-get 'unknown_model_events report)))
        (memex-usage--row "conservative events"
                          (memex-usage--count (alist-get 'conservative_events report)))
        (memex-usage--row "cost mode" (or (alist-get 'cost_mode report) "-"))
        (memex-usage--row "price catalog" (or (alist-get 'price_catalog report) "-"))
        (memex-usage--row "cache waste"
                          (memex-usage--waste (alist-get 'cache_waste report)))))

(defun memex-usage--source (source)
  "Return the per-source alist SOURCE as lines, its own figures under its name."
  (append
   (list ""
         (format "  %s: %s events, %s tokens, %s"
                 (or (alist-get 'source source) "-")
                 (memex-usage--count (alist-get 'events source))
                 (memex-usage--count (alist-get 'total_tokens source))
                 (memex-usage--cost (alist-get 'known_cost_usd source))))
   (mapcar (lambda (field)
             (memex-usage--row (concat "  " (cdr field))
                               (memex-usage--count (alist-get (car field) source))))
           '((uncached_input . "uncached input")
             (cache_read . "cache read")
             (cache_write . "cache write")
             (output . "output")
             (reasoning . "reasoning")
             (priced_events . "priced events")
             (unpriced_events . "unpriced events")))
   (list (memex-usage--row "  cache waste"
                           (memex-usage--waste (alist-get 'cache_waste source))))))

(defun memex-usage--sources (sources)
  "Return the per-source alists SOURCES as lines, or nothing without any."
  (when sources
    (cons "" (cons "By source" (seq-mapcat #'memex-usage--source sources)))))

(defun memex-usage--section (heading lines)
  "Return LINES under HEADING, or nothing when there are no LINES."
  (when lines
    (append (list "" heading "") (mapcar (lambda (line) (concat "  " line)) lines))))

(defun memex-usage--failure-line (failure)
  "Return the two-element FAILURE of machine and message as one line."
  (format "%s: %s" (car failure) (cadr failure)))

(defun memex-usage--render (report)
  "Return REPORT, a memex usage report alist, as report buffer text."
  (concat (string-join
           (append (memex-usage--totals report)
                   (memex-usage--sources (alist-get 'by_source report))
                   (memex-usage--section "Warnings" (alist-get 'warnings report))
                   (memex-usage--section
                    "Failures"
                    (mapcar #'memex-usage--failure-line (alist-get 'failures report))))
           "\n")
          "\n"))

(define-derived-mode memex-usage-mode special-mode "Memex Usage"
  "Major mode for the memex token usage report.")

(defun memex-usage--buffer-name (project)
  "Return the name the report buffer of PROJECT is made under.
Each project reports into a buffer of its own, so a report never
overwrites another project's."
  (if project
      (format "*memex usage: %s*" project)
    memex-usage-buffer-name))

(defun memex-usage--display (report &optional project)
  "Put REPORT up in the report buffer of PROJECT and select it."
  (let ((buffer (get-buffer-create (memex-usage--buffer-name project))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'memex-usage-mode) (memex-usage-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (memex-usage--render report))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun memex-usage--report-failure (failure)
  "Report FAILURE, memex-core's error object, through `message'.
An errback runs inside a process sentinel, where a signal would reach
the user as an error in a sentinel rather than as the report the
failure path owes them."
  (message "memex usage: %s"
           (or (plist-get (cdr failure) :message)
               (error-message-string failure))))

;;;###autoload
(defun memex-usage (&optional project)
  "Report memex's token usage in a buffer and return the request process.
PROJECT narrows the report to one project and is read from the
minibuffer with a prefix argument; without one the report covers every
project.  The buffer, one per project, is put up when the asynchronous
response arrives rather than by the time this returns, which answers
with the request process."
  (interactive (list (and current-prefix-arg (memex-read-project))))
  (memex-api-usage (lambda (report) (memex-usage--display report project))
                   :project project
                   :errback #'memex-usage--report-failure))

(provide 'memex-usage)
;;; memex-usage.el ends here
