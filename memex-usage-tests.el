;;; memex-usage-tests.el --- Tests for memex-usage.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-usage-tests.el -f ert-run-tests-batch-and-exit
;;
;; The report tests drive `memex-usage--render' with a report alist of the
;; shape memex 0.11.6 puts on the wire, so no memex install and no process
;; are needed.  The two command tests replace `memex-api-usage' to reach
;; the callback and the errback without one either.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'memex-core)
(require 'memex-api)

(require 'memex-usage nil t)

(declare-function memex-usage "memex-usage")
(declare-function memex-usage--render "memex-usage" (report))

(defun memex-usage-tests--report ()
  "Return a usage report alist covering two sources.
Every number is its own, so a renderer showing the totals in place of a
source's row, or one source's row in place of the other's, is
distinguishable from one attributing each figure to its source."
  '((authority . "aurora")
    (events . 1487)
    (total_tokens . 9384617)
    (unknown_model_events . 26)
    (conservative_events . 58)
    (cost_mode . "auto")
    (price_catalog . "builtin-2026-04")
    (known_cost_usd . 41.73)
    (priced_events . 1391)
    (unpriced_events . 96)
    (cache_waste . ((missed_tokens . 284391)
                    (missed_cost_usd . 12.58)
                    (miss_count . 429)
                    (idle_misses . 271)
                    (model_switch_misses . 158)))
    (by_source
     . (((source . "codex")
         (events . 613)
         (uncached_input . 118427)
         (cache_read . 3921144)
         (cache_write . 204773)
         (output . 71852)
         (reasoning . 39418)
         (total_tokens . 4355614)
         (known_cost_usd . 17.42)
         (priced_events . 601)
         (unpriced_events . 12)
         (cache_waste . ((missed_tokens . 96214)
                         (missed_cost_usd . 4.11)
                         (miss_count . 183)
                         (idle_misses . 121)
                         (model_switch_misses . 62))))
        ((source . "open-claw")
         (events . 874)
         (uncached_input . 236918)
         (cache_read . 4419237)
         (cache_write . 187644)
         (output . 96331)
         (reasoning . 88873)
         (total_tokens . 5029003)
         (known_cost_usd . 24.31)
         (priced_events . 790)
         (unpriced_events . 84)
         (cache_waste . ((missed_tokens . 188177)
                         (missed_cost_usd . 8.47)
                         (miss_count . 246)
                         (idle_misses . 150)
                         (model_switch_misses . 96))))))
    (details . nil)
    (warnings . ("machine dune did not answer in time"))
    (failures . (("tundra" "connection refused")))))

(defun memex-usage-tests--mentions-p (text number)
  "Return non-nil when TEXT carries NUMBER, its digit grouping aside.
The digits may be separated the way a formatted report groups
thousands, so 4355614 is found whether it was written plain or as
4,355,614."
  (string-match-p (mapconcat (lambda (char) (regexp-quote (string char)))
                             (number-to-string number)
                             "[,._' ]?")
                  text))

(defun memex-usage-tests--line (text needle)
  "Return the first line of TEXT carrying NEEDLE, or nil when none does."
  (seq-find (lambda (line) (string-match-p (regexp-quote needle) line))
            (split-string text "\n" t)))

(defun memex-usage-tests--labelling (text number sources)
  "Return which of SOURCES labels the line of TEXT carrying NUMBER.
The label is the last of SOURCES named at or above that line, so a
figure is attributed to its source whether the renderer keeps it on the
source's own row or in a table of its own further down.  Returns nil
when no line carries NUMBER, or when none of SOURCES is named above the
one that does."
  (let ((label nil))
    (catch 'found
      (dolist (line (split-string text "\n" t))
        (dolist (source sources)
          (when (string-match-p (regexp-quote source) line)
            (setq label source)))
        (when (memex-usage-tests--mentions-p line number)
          (throw 'found label))))))

(defun memex-usage-tests--report-buffers ()
  "Return the live report buffers, found by their major mode.
The mode is what the report buffer is pinned on and a buffer name is
not: ERT visits this file to print the source location of a failing
test, and a buffer named after it answers to any pattern loose enough
to also match the report's own name."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'memex-usage-mode))
              (buffer-list)))

(defun memex-usage-tests--cleanup ()
  "Kill the report buffers a test left behind."
  (mapc #'kill-buffer (memex-usage-tests--report-buffers)))

(defun memex-usage-tests--messaged (thunk)
  "Return what THUNK hands `message', as one string.
An error THUNK signals is left to propagate: the errback runs inside a
process sentinel, where a signal reaches the user as an error in a
sentinel rather than as the report the failure path owes them."
  (let ((messaged ""))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (when format
                   (setq messaged (concat messaged
                                          (apply #'format-message format args)
                                          "\n")))
                 nil)))
      (funcall thunk))
    messaged))

(ert-deftest memex-usage-render-reports-the-totals-and-the-pricing-context ()
  (let ((text (memex-usage--render (memex-usage-tests--report))))
    (should (stringp text))
    (should (string-match-p "aurora" text))
    (should (string-match-p "auto" text))
    (should (string-match-p "builtin-2026-04" text))
    (should (memex-usage-tests--mentions-p text 1487))
    (should (memex-usage-tests--mentions-p text 9384617))
    (should (memex-usage-tests--mentions-p text 41.73))))

(ert-deftest memex-usage-render-attributes-each-source-its-own-numbers ()
  (let* ((text (memex-usage--render (memex-usage-tests--report)))
         (codex (memex-usage-tests--line text "codex"))
         (claw (memex-usage-tests--line text "open-claw"))
         (sources '("codex" "open-claw")))
    (should codex)
    (should claw)
    (should-not (equal codex claw))
    (should (memex-usage-tests--mentions-p codex 4355614))
    (should (memex-usage-tests--mentions-p codex 17.42))
    (should-not (memex-usage-tests--mentions-p codex 5029003))
    (should-not (memex-usage-tests--mentions-p codex 9384617))
    (should (memex-usage-tests--mentions-p claw 5029003))
    (should (memex-usage-tests--mentions-p claw 24.31))
    (should-not (memex-usage-tests--mentions-p claw 4355614))
    (should-not (memex-usage-tests--mentions-p claw 9384617))
    (should (equal (memex-usage-tests--labelling text 96214 sources) "codex"))
    (should (equal (memex-usage-tests--labelling text 4.11 sources) "codex"))
    (should (equal (memex-usage-tests--labelling text 183 sources) "codex"))
    (should (equal (memex-usage-tests--labelling text 188177 sources) "open-claw"))
    (should (equal (memex-usage-tests--labelling text 8.47 sources) "open-claw"))
    (should (equal (memex-usage-tests--labelling text 246 sources) "open-claw"))))

(ert-deftest memex-usage-render-reports-the-cache-waste ()
  (let ((text (memex-usage--render (memex-usage-tests--report))))
    (should (memex-usage-tests--mentions-p text 284391))
    (should (memex-usage-tests--mentions-p text 12.58))
    (should (memex-usage-tests--mentions-p text 271))
    (should (memex-usage-tests--mentions-p text 158))))

(ert-deftest memex-usage-render-keeps-the-warnings-and-the-failures ()
  (let ((text (memex-usage--render (memex-usage-tests--report))))
    (should (string-match-p "machine dune did not answer in time" text))
    (should (string-match-p "tundra" text))
    (should (string-match-p "connection refused" text))))

(ert-deftest memex-usage-opens-a-read-only-report-buffer ()
  (unwind-protect
      (let ((callback nil)
            (shown nil))
        (cl-letf (((symbol-function 'memex-api-usage)
                   (lambda (cb &rest _) (setq callback cb) nil))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq shown buffer) nil))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer &rest _) (setq shown buffer) nil))
                  ((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq shown buffer) nil)))
          (memex-usage)
          (should (functionp callback))
          (funcall callback (memex-usage-tests--report)))
        (let ((buffers (memex-usage-tests--report-buffers)))
          (should (equal (length buffers) 1))
          (with-current-buffer (car buffers)
            (should (eq major-mode 'memex-usage-mode))
            (should buffer-read-only)
            (should (string-match-p "aurora" (buffer-string)))
            (should (memex-usage-tests--mentions-p (buffer-string) 9384617))
            (should (memex-usage-tests--mentions-p (buffer-string) 4355614)))
          (should (or (eq shown (car buffers))
                      (equal shown (buffer-name (car buffers)))))))
    (memex-usage-tests--cleanup)))

(ert-deftest memex-usage-reports-a-memex-failure-and-opens-no-buffer ()
  (unwind-protect
      (let ((errback nil))
        (cl-letf (((symbol-function 'memex-api-usage)
                   (lambda (_callback &rest args)
                     (setq errback (plist-get args :errback))
                     nil)))
          (memex-usage))
        (should (functionp errback))
        (let ((messaged
               (memex-usage-tests--messaged
                (lambda ()
                  (funcall errback
                           '(memex-rpc-error
                             :message
                             "token usage tracking is disabled on this machine"))))))
          (should (string-match-p "token usage tracking is disabled on this machine"
                                  messaged)))
        (should-not (memex-usage-tests--report-buffers)))
    (memex-usage-tests--cleanup)))

(provide 'memex-usage-tests)
;;; memex-usage-tests.el ends here
