;;; memex-markdown-tests.el --- Tests for the markdown renderer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sören Nikolaus

;; Author: Sören Nikolaus <soeren@code17.io>

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l memex-markdown-tests.el -f ert-run-tests-batch-and-exit
;;
;; The fallback parser is asserted on its HTML rather than on what shr
;; draws from it: shr's line breaking and indentation are its own and
;; change between Emacs releases, while the markup the parser owes it
;; does not.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(require 'memex-markdown nil t)

(declare-function memex-markdown--escape "memex-markdown")
(declare-function memex-markdown--to-html "memex-markdown")
(declare-function memex-markdown-html "memex-markdown")
(declare-function memex-markdown-render "memex-markdown")
(declare-function memex-markdown-render-all "memex-markdown")


(defun memex-markdown-tests--html (markdown)
  "Return the fallback parser's HTML for MARKDOWN, whitespace squeezed."
  (string-join (split-string (memex-markdown--to-html markdown)) " "))

(ert-deftest memex-markdown-escapes-html-metacharacters ()
  (should (equal (memex-markdown--escape "a < b & c > d \"e\"")
                 "a &lt; b &amp; c &gt; d &quot;e&quot;")))

(ert-deftest memex-markdown-fenced-code-is-verbatim-and-unparsed ()
  "What is inside a fence is code, including anything that looks like markup."
  (let ((html (memex-markdown-tests--html
               "before\n\n```sh\nrm -rf * # **not bold** <tag>\n```\n\nafter")))
    (should (string-search "<pre><code>rm -rf * # **not bold** &lt;tag&gt;" html))
    (should (string-search "before" html))
    (should (string-search "after" html))))

(ert-deftest memex-markdown-renders-headings-at-their-level ()
  (let ((html (memex-markdown-tests--html "# One\n\n### Three\n")))
    (should (string-search "<h1>One</h1>" html))
    (should (string-search "<h3>Three</h3>" html))))

(ert-deftest memex-markdown-renders-both-kinds-of-list ()
  (let ((bullets (memex-markdown-tests--html "- alpha\n- beta\n"))
        (numbers (memex-markdown-tests--html "1. first\n2. second\n")))
    (should (string-search "<ul> <li>alpha</li> <li>beta</li> </ul>" bullets))
    (should (string-search "<ol> <li>first</li> <li>second</li> </ol>" numbers))))

(ert-deftest memex-markdown-renders-inline-spans ()
  (let ((html (memex-markdown-tests--html
               "a **bold** and *thin* and `code` and [text](https://e.org)")))
    (should (string-search "<strong>bold</strong>" html))
    (should (string-search "<em>thin</em>" html))
    (should (string-search "<code>code</code>" html))
    (should (string-search "<a href=\"https://e.org\">text</a>" html))))

(ert-deftest memex-markdown-does-not-parse-spans-inside-inline-code ()
  "Backticks are what an agent writes a path or a flag in."
  (let ((html (memex-markdown-tests--html "run `ls *.el` and `a_b_c`")))
    (should (string-search "<code>ls *.el</code>" html))
    (should (string-search "<code>a_b_c</code>" html))
    (should-not (string-search "<em>" html))))

(ert-deftest memex-markdown-separates-paragraphs ()
  (let ((html (memex-markdown-tests--html "one\ntwo\n\nthree")))
    (should (string-search "<p>one two</p>" html))
    (should (string-search "<p>three</p>" html))))

(ert-deftest memex-markdown-renders-a-blockquote ()
  (should (string-search "<blockquote> <p>quoted</p> </blockquote>"
                         (memex-markdown-tests--html "> quoted"))))

(ert-deftest memex-markdown-render-returns-text-shr-drew ()
  "The renderer answers with a string, since a record is inserted into a
buffer the viewer already owns."
  (let ((drawn (memex-markdown-render "# Heading\n\nsome **bold** prose")))
    (should (stringp drawn))
    (should (string-search "Heading" drawn))
    (should (string-search "bold" drawn))
    (should-not (string-search "**" drawn))))

(ert-deftest memex-markdown-render-of-nothing-is-nothing ()
  (should (equal (memex-markdown-render "") ""))
  (should (equal (memex-markdown-render nil) nil)))

(ert-deftest memex-markdown-batch-answers-one-rendering-per-input ()
  "Records are rendered as a batch so a caller can zip the answer back
against the records it asked about."
  (let ((out (memex-markdown-render-all
              (list "**bold**" "# Heading" "plain prose"))))
    (should (equal (length out) 3))
    (should (string-search "bold" (nth 0 out)))
    (should-not (string-search "**" (nth 0 out)))
    (should (string-search "Heading" (nth 1 out)))
    (should (string-search "plain prose" (nth 2 out)))))

(ert-deftest memex-markdown-batch-keeps-empty-inputs-in-place ()
  "Records without text still hold their position in the answer."
  (let ((out (memex-markdown-render-all (list "**a**" "" "**b**"))))
    (should (equal (length out) 3))
    (should (equal (nth 1 out) ""))
    (should (string-search "a" (nth 0 out)))
    (should (string-search "b" (nth 2 out)))))

(ert-deftest memex-markdown-batch-of-nothing-is-nothing ()
  (should (equal (memex-markdown-render-all nil) nil)))

(ert-deftest memex-markdown-fontifies-a-fence-in-the-language-it-names ()
  "A fence is code and reads as code only in colour, so the caller is
handed its language and its body rather than the flattening shr makes
of a `pre'.  Prose has to come before it: rendering that prose runs a
regexp of its own, which is what loses the fence's groups."
  (let ((seen nil))
    (memex-markdown-render
     "Here is how:\n\n```sh\nls -la\n```\n"
     (lambda (language code) (push (cons language code) seen) "DRAWN"))
    (should (equal seen '(("sh" . "ls -la\n"))))))

(ert-deftest memex-markdown-marks-a-code-span-as-code ()
  "shr draws `code' as running text, which loses the one thing the span
was written to say."
  (let ((drawn (memex-markdown-render "use `git rebase` first")))
    (should (string-search "git rebase" drawn))
    (should (memq 'memex-markdown-code
                  (ensure-list
                   (get-text-property (string-search "git" drawn)
                                      'face drawn))))))

(provide 'memex-markdown-tests)
;;; memex-markdown-tests.el ends here
