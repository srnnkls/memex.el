;;; memex-markdown.el --- Render an agent's markdown for reading -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, matching
;; URL: https://github.com/srnnkls/memex.el

;;; Commentary:

;; Agents write markdown, and a transcript that shows its asterisks and
;; backticks is showing the source of the message rather than the
;; message.  `memex-markdown-render' answers with the text shr draws
;; from it, ready to insert.
;;
;; The markdown reaches shr as HTML, and pandoc is what turns it into
;; HTML wherever it is installed: tables, footnotes, task lists and the
;; rest of GFM are its job rather than this file's.  Without it
;; `memex-markdown--to-html' covers what an agent writes most of -
;; fences, headings, lists, quotes and the inline spans - so a machine
;; without pandoc still reads its transcripts, less well.

;;; Code:

(require 'rx)
(require 'seq)
(require 'shr)
(require 'subr-x)

(defgroup memex-markdown nil
  "Rendering the markdown an agent wrote."
  :group 'memex)

(defcustom memex-markdown-pandoc-executable "pandoc"
  "Name of, or path to, the pandoc executable.
Markdown is rendered by the parser in this file wherever it is missing."
  :type 'string
  :group 'memex-markdown)

(defconst memex-markdown--fence
  (rx bol (group (>= 3 (any "`~"))) (group (zero-or-more nonl)) "\n"
      (group (minimal-match (zero-or-more anychar)))
      bol (backref 1) (zero-or-more (any " \t")) (or "\n" eos))
  "A fenced code block: its opening run, its info string and its body.
The run is captured so the closing fence has to match the opening one,
and the info string because it names the language the body is in.")

(defface memex-markdown-code
  '((((background light)) :background "#eceef1" :foreground "#8a3d52")
    (((background dark)) :background "#2f333b" :foreground "#e6a1b0"))
  "Face for a code span written between backticks."
  :group 'memex-markdown)

(defconst memex-markdown--heading
  (rx bol (group (repeat 1 6 "#")) (one-or-more " ") (group (one-or-more nonl)))
  "An ATX heading and its level.")

(defun memex-markdown--escape (text)
  "Return TEXT with the four HTML metacharacters spelled out."
  (thread-last text
               (replace-regexp-in-string "&" "&amp;")
               (replace-regexp-in-string "<" "&lt;")
               (replace-regexp-in-string ">" "&gt;")
               (replace-regexp-in-string "\"" "&quot;")))

(defun memex-markdown--spans (text)
  "Return TEXT with its inline markdown turned into HTML.
Code spans are taken first and their contents left alone, since a
backtick is how an agent writes the paths and flags that would
otherwise read as emphasis."
  (let ((parts nil)
        (position 0))
    (while (string-match "`\\([^`\n]+\\)`" text position)
      (push (memex-markdown--emphasis
             (substring text position (match-beginning 0)))
            parts)
      (push (format "<code>%s</code>"
                    (memex-markdown--escape (match-string 1 text)))
            parts)
      (setq position (match-end 0)))
    (push (memex-markdown--emphasis (substring text position)) parts)
    (apply #'concat (nreverse parts))))

(defun memex-markdown--emphasis (text)
  "Return TEXT escaped, with its links and emphasis turned into HTML."
  (thread-last (memex-markdown--escape text)
               (replace-regexp-in-string
                (rx "[" (group (zero-or-more (not (any "]" "\n")))) "]"
                    "(" (group (zero-or-more (not (any ")" "\n")))) ")")
                "<a href=\"\\2\">\\1</a>")
               (replace-regexp-in-string
                (rx (or "**" "__") (group (minimal-match (one-or-more nonl)))
                    (or "**" "__"))
                "<strong>\\1</strong>")
               (replace-regexp-in-string
                (rx (any "*_") (group (minimal-match (one-or-more (not (any "*_")))))
                    (any "*_"))
                "<em>\\1</em>")))

(defun memex-markdown--list-html (lines ordered)
  "Return LINES, each an item's text, as an ORDERED or bulleted list."
  (format "<%s>\n%s\n</%s>"
          (if ordered "ol" "ul")
          (mapconcat (lambda (line)
                       (format "<li>%s</li>" (memex-markdown--spans line)))
                     lines "\n")
          (if ordered "ol" "ul")))

(defun memex-markdown--block-html (block)
  "Return the paragraph, list, heading or quote BLOCK as HTML."
  (let ((lines (split-string block "\n" t "[ \t]+")))
    (cond
     ((null lines) "")
     ((string-match memex-markdown--heading block)
      (format "<h%d>%s</h%d>"
              (length (match-string 1 block))
              (memex-markdown--spans (match-string 2 block))
              (length (match-string 1 block))))
     ((seq-every-p (lambda (line) (string-match-p (rx bos (any "-*+") " ") line))
                   lines)
      (memex-markdown--list-html
       (mapcar (lambda (line) (substring line 2)) lines) nil))
     ((seq-every-p (lambda (line)
                     (string-match-p (rx bos (one-or-more digit) "." " ") line))
                   lines)
      (memex-markdown--list-html
       (mapcar (lambda (line)
                 (replace-regexp-in-string (rx bos (one-or-more digit) ". ") ""
                                           line))
               lines)
       t))
     ((seq-every-p (lambda (line) (string-prefix-p ">" line)) lines)
      (format "<blockquote>\n<p>%s</p>\n</blockquote>"
              (memex-markdown--spans
               (string-join (mapcar (lambda (line)
                                      (string-trim (substring line 1)))
                                    lines)
                            " "))))
     (t (format "<p>%s</p>" (memex-markdown--spans (string-join lines " ")))))))

(defun memex-markdown--to-html (markdown)
  "Return MARKDOWN as HTML, without pandoc.
Fences come out first so nothing inside one is read as markup, and what
is left between them is split into blocks on blank lines."
  (let ((out nil)
        (position 0))
    (while (string-match memex-markdown--fence markdown position)
      (let ((before (substring markdown position (match-beginning 0)))
            (code (match-string 3 markdown))
            (after (match-end 0)))
        (dolist (block (split-string before "\n[ \t]*\n" t))
          (push (memex-markdown--block-html block) out))
        (push (format "<pre><code>%s</code></pre>"
                      (memex-markdown--escape code))
              out)
        (setq position after)))
    (dolist (block (split-string (substring markdown position) "\n[ \t]*\n" t))
      (push (memex-markdown--block-html block) out))
    (string-join (nreverse out) "\n")))

(defalias 'memex-markdown-html #'memex-markdown--to-html
  "Return MARKDOWN as HTML.")

(defun memex-markdown--tag-code (dom)
  "Draw the code span DOM as shr would, marked as the code it is.
shr draws `code' as running text, which loses the one thing the span
was written to say."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'memex-markdown-code t)))

(defun memex-markdown--draw (html)
  "Return the text shr draws from HTML."
  (with-temp-buffer
    (insert html)
    (let ((shr-use-fonts nil)
          (shr-width nil)
          (shr-indentation 0)
          (shr-external-rendering-functions
           (cons '(code . memex-markdown--tag-code)
                 shr-external-rendering-functions)))
      (shr-render-region (point-min) (point-max)))
    (string-trim (buffer-string))))

(defun memex-markdown--plain-code (_language code)
  "Return CODE as it was written, whatever LANGUAGE it is in."
  (string-trim-right code))

(defun memex-markdown-render (markdown &optional code)
  "Return the text shr draws from MARKDOWN, or MARKDOWN when it is empty.
A fenced block never reaches shr, which would flatten it to running
text: CODE is called with the fence's language and body and answers
with what to draw in its place, `memex-markdown--plain-code' by default."
  (if (or (null markdown) (string-empty-p (string-trim markdown)))
      markdown
    (let ((code (or code #'memex-markdown--plain-code))
          (position 0)
          (out nil))
      (while (string-match memex-markdown--fence markdown position)
        (let ((prose (substring markdown position (match-beginning 0)))
              (language (string-trim (match-string 2 markdown)))
              (body (match-string 3 markdown))
              (after (match-end 0)))
          (push (memex-markdown--draw (memex-markdown--to-html prose)) out)
          (push (funcall code language body) out)
          (setq position after)))
      (push (memex-markdown--draw
             (memex-markdown--to-html (substring markdown position)))
            out)
      (string-join (seq-remove #'string-empty-p (nreverse out)) "\n\n"))))

(defun memex-markdown-render-all (markdowns &optional code)
  "Return the text shr draws from each of MARKDOWNS, in order.
Every entry keeps its place, empty ones included, so a caller can zip
the answer back against the records it asked about.  CODE draws the
fenced blocks, as in `memex-markdown-render'."
  (mapcar (lambda (markdown) (memex-markdown-render markdown code)) markdowns))

(provide 'memex-markdown)
;;; memex-markdown.el ends here
