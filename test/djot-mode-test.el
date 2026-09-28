;;; djot-mode-test.el --- Public foundation behavior tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with make test; the real Djot grammar is required.
;;; Code:
(require 'ert)
(require 'djot-mode)

(defmacro djot-test-with (source &rest body)
  "Create a real Djot buffer with SOURCE and evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,source)
     (djot-mode)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun djot-test-face-p (position face)
  "Check whether POSITION has FACE in its actual fontification."
  (let ((value (get-text-property position 'face)))
    (or (eq value face) (memq face value))))

(ert-deftest djot-mode-native-language-trust ()
  ;; This suite runs in a fresh -Q process; never change the user's session.
  (require 'autorevert)
  (require 'python)
  (let* ((called nil)
         (python-mode-hook (list (lambda () (setq called t)))))
    (unwind-protect
        (progn
          (should-not global-auto-revert-mode)
          (djot-test-with "```global-auto-revert\nx\n```\n"
            (should-not global-auto-revert-mode))
          (cl-letf (((symbol-function 'djot-test-unapproved-mode)
                     (lambda () (setq called t))))
            (djot-test-with "```djot-test-unapproved\nx\n```\n"
              (should-not called)))
          (djot-test-with "```python\n@cache\ndef f():\n    pass\n```\n"
            (search-forward "@cache")
            (should (djot-test-face-p (- (point) 6) 'font-lock-type-face))
            (should-not called))
          (let ((djot-language-modes '(("trusted" . emacs-lisp-mode))))
            (djot-test-with "```trusted\n(defun hello ())\n```\n"
              (search-forward "defun")
              (should (djot-test-face-p (1- (point)) 'font-lock-keyword-face)))))
      (global-auto-revert-mode -1))))

(ert-deftest djot-mode-utf8-fragment-public-command ()
  (djot-test-with "# café\n\n[go](#caf%C3%A9)\n"
    (search-forward "go") (backward-char)
    (djot-open-at-point)
    (should (= (point) (point-min))))
  (let* ((directory (make-temp-file "djot-unicode-" t))
         (file (expand-file-name "café.djot" directory))
         target)
    (unwind-protect
        (progn
          (with-temp-file file (insert "# café\n\nExisting target\n"))
          (djot-test-with "[go](caf%C3%A9.djot#caf%C3%A9)\n"
            (setq default-directory (file-name-as-directory directory))
            (search-forward "go") (backward-char)
            (djot-open-at-point)
            (setq target (current-buffer))
            (should (file-equal-p buffer-file-name file))
            (should (equal (buffer-string) "# café\n\nExisting target\n"))
            (should (= (point) (point-min)))))
      (when (buffer-live-p target) (kill-buffer target))
      (delete-directory directory t))))

(ert-deftest djot-mode-effective-heading-id ()
  (dolist (attributes '("{#first #last}" "{#first #last #last}"
                        "{#first}\n{#last}"))
    (djot-test-with (concat attributes "\n# Heading\n\n[Heading][] [last](#last) [first](#first)\n")
      (search-forward "[Heading]") (backward-char 2)
      (should (equal (djot-link-destination (djot--link-at-point)) "#last"))
      (djot-open-at-point)
      (should (looking-at "# Heading"))
      (search-forward "[last]") (backward-char 2)
      (djot-open-at-point)
      (should (looking-at "# Heading"))
      (search-forward "[first]") (backward-char 2)
      (should-error (djot-open-at-point) :type 'user-error))))

(ert-deftest djot-mode-association-and-single-parser ()
  (should (eq (assoc-default "note.djot" auto-mode-alist #'string-match) 'djot-mode))
  (djot-test-with "# One\n\n## Two\n\n### Three\n\n#### Four\n\n##### Five\n\n###### Six\n"
    (should (derived-mode-p 'text-mode))
    (should (= 1 (length (treesit-parser-list))))
    (cl-loop for name in '("One" "Two" "Three" "Four" "Five" "Six")
             for level from 1 do
             (search-forward name)
             (should (djot-test-face-p (1- (point)) (intern (format "djot-heading-%d" level))))))
  (should-not (equal (face-attribute 'djot-heading-1 :inherit)
                    (face-attribute 'djot-heading-2 :inherit))))

(ert-deftest djot-mode-descriptive-links-independent-default ()
  (djot-test-with "_emphasis_ [site](https://example.org)\n"
    (should-not (invisible-p (point)))
    (search-forward "[")
    (should (invisible-p (1- (point))))
    (should (keymapp (get-char-property (point) 'keymap)))
    (should (equal (get-char-property (point) 'help-echo) "https://example.org"))
    (should (get-char-property (point) 'follow-link))
    (let ((position (point)) (text (buffer-string)) (modified (buffer-modified-p)))
      (djot-show-source 1)
      (should-not (invisible-p (1- position)))
      (should (= position (point)))
      (should (equal text (buffer-string)))
      (should (eq modified (buffer-modified-p)))
      (djot-show-source 0)
      (should (invisible-p (1- position))))))

(ert-deftest djot-mode-concealment-literals-and-malformed ()
  (djot-test-with "_yes_ *bold* _unclosed\n\n\\_escape_ `literal _code_`\n\n```elisp\n_unclosed *code*\n```\n\n`raw *text*`{=html}\n"
    (djot-toggle-markup)
    (should (invisible-p (point-min)))
    (search-forward "_unclosed")
    (should-not (invisible-p (- (point) 9)))
    (search-forward "\\_")
    (should-not (invisible-p (1- (point))))
    (search-forward "`literal")
    (should (invisible-p (- (point) 8)))
    (search-forward "_code_")
    (should-not (invisible-p (- (point) 6)))
    (search-forward "```elisp")
    (should-not (invisible-p (- (point) 8)))
    (search-forward "*code*")
    (should-not (invisible-p (- (point) 6)))
    (search-forward "*text*")
    (should-not (invisible-p (- (point) 6))))
  (djot-test-with "_incomplete\n"
    (djot-toggle-markup)
    (should-not (invisible-p (point-min)))))

(ert-deftest djot-mode-point-does-not-reveal ()
  (djot-test-with "_yes_\n"
    (djot-toggle-markup)
    (goto-char 2)
    (run-hooks 'post-command-hook)
    (should (invisible-p 1))))

(ert-deftest djot-mode-edit-and-undo ()
  (djot-test-with "_yes_\n"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (djot-toggle-markup)
    (should-not buffer-undo-list)
    (goto-char 5)
    (delete-char 1)
    (font-lock-ensure)
    (should-not (invisible-p 1))
    (undo-boundary)
    (let ((inhibit-message t)) (undo 1))
    (font-lock-ensure)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "_yes_\n"))
    (should (invisible-p 1))))

(ert-deftest djot-mode-concealment-boundaries-are-nonsticky ()
  (djot-test-with "_yes_\n"
    (djot-toggle-markup)
    (goto-char 1)
    (insert "x ")
    (should-not (invisible-p 1))
    (should-not (invisible-p 2))
    (font-lock-ensure)
    (should (invisible-p 3))))

(ert-deftest djot-mode-teardown-preserves-foreign-state ()
  (djot-test-with "_yes_ [link](https://example.org)\n"
    (djot-toggle-markup)
    (let ((foreign (make-overlay 2 4)))
      (overlay-put foreign 'display "OTHER")
      (put-text-property 2 3 'djot-test 'retained)
      (djot-refresh)
      (should (overlay-buffer foreign))
      (text-mode)
      (should-not (cl-find-if (lambda (o) (overlay-get o 'djot-mode)) (overlays-in 1 (point-max))))
      (should (overlay-buffer foreign))
      (should (eq (get-text-property 2 'djot-test) 'retained))
      (should-not (treesit-parser-list)))))

(ert-deftest djot-mode-links-browse-and-reference ()
  (djot-test-with "[inline](https://example.org/a) [ref][case] [bad][Case] <a@example.org>\n\n[case]: https://example.org/reference\n"
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (search-forward "inline") (djot-open-at-point)
        (should (equal opened "https://example.org/a"))
        (search-forward "ref") (djot-open-at-point)
        (should (equal opened "https://example.org/reference"))
        (search-forward "bad") (should-error (djot-open-at-point) :type 'user-error)
        (search-forward "a@example") (djot-open-at-point)
        (should (equal opened "mailto:a@example.org"))))))

(ert-deftest djot-mode-link-local-file-real-visit ()
  (let ((file (make-temp-file "djot-link-" nil ".txt" "Local fixture")))
    (unwind-protect
        (djot-test-with (format "[local](%s)\n" file)
          (search-forward "local")
          (save-window-excursion
            (djot-open-at-point)
            (should (equal buffer-file-name file))
            (should (equal (buffer-string) "Local fixture"))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest djot-mode-links-anchors-and-footnotes ()
  (djot-test-with "[Intro][] [anchor](#Intro) note[^one]\n\n# Intro\n\nProse.\n\n[^one]: A footnote.\n"
    (search-forward "Intro") (djot-open-at-point)
    (should (looking-at "# Intro"))
    (goto-char 1) (search-forward "anchor") (djot-open-at-point)
    (should (looking-at "# Intro"))
    (goto-char 1) (search-forward "[^one]") (backward-char 2)
    (let ((reference (point)))
      (djot-open-at-point)
      (should (looking-at "\\[\\^one\\]:"))
      (djot-footnote-return)
      (should (= reference (point))))))

(ert-deftest djot-mode-refuses-executable-schemes ()
  (djot-test-with "[bad](javascript:alert) [data](data:text/html,bad)\n"
    (search-forward "bad") (should-error (djot-open-at-point) :type 'user-error)
    (search-forward "data") (should-error (djot-open-at-point) :type 'user-error)))

(ert-deftest djot-mode-native-source-real-token-faces ()
  (djot-test-with "```elisp\n(defun hello (name)\n  \"Documentation.\"\n  (message \"Hello %s\" name))\n```\n\n```=html\n(defun not-code ())\n```\n"
    (search-forward "defun")
    (should (djot-test-face-p (1- (point)) 'font-lock-keyword-face))
    (search-forward "hello")
    (should (djot-test-face-p (1- (point)) 'font-lock-function-name-face))
    (search-forward "Hello %s")
    (should (djot-test-face-p (1- (point)) 'font-lock-string-face))
    (should (djot-test-face-p (1- (point)) 'djot-code))
    (search-forward "not-code")
    (should-not (djot-test-face-p (1- (point)) 'font-lock-function-name-face))
    (should (= 1 (length (treesit-parser-list))))))

(ert-deftest djot-mode-fontify-hook-range-and-repeatability ()
  (djot-test-with "_one_\n\n_two_\n"
    (let ((calls nil))
      (add-hook 'djot-after-fontify-hook (lambda (beg end) (push (cons beg end) calls)) nil t)
      (djot-toggle-markup)
      (let ((count (length (overlays-in 1 (point-max)))))
        (djot-refresh)
        (should (= count (length (overlays-in 1 (point-max))))))
      (should calls)
      (should (cl-every (lambda (range) (and (<= 1 (car range)) (<= (cdr range) (point-max)))) calls)))))

(ert-deftest djot-mode-heading-identifiers-match-official-renderer ()
  (djot-test-with "[A][] [A B][] [same][] [!!!][]\n\n{#custom}\n{.important}\n# A\n\n# A *B*[^n]\n\n# same\n\n# same\n\n# !!!\n\n[^n]: note\n"
    (search-forward "A]") (backward-char 1)
    (should (equal (djot-link-destination (djot--link-at-point)) "#custom"))
    (djot-open-at-point)
    (should (looking-at "# A"))
    (goto-char 1) (search-forward "A B")
    (should (equal (djot-link-destination (djot--link-at-point)) "#A-B"))
    (search-forward "same")
    (should (equal (djot-link-destination (djot--link-at-point)) "#same"))
    (search-forward "!!!")
    (should (equal (djot-link-destination (djot--link-at-point)) "#s-1"))))

(ert-deftest djot-mode-reference-last-definition-wins ()
  (djot-test-with "[ref][]\n\n[ref]: /one\n\n[ref]: /two\n"
    (search-forward "ref")
    (should (equal (djot-link-destination (djot--link-at-point)) "/two"))))

(ert-deftest djot-mode-multiline-references-and-old-grammar-safety ()
  (dolist (pair '(("[ref]: https://example.org/first\n  second\n" . "https://example.org/firstsecond")
                  ("[ref]:\n  https://example.org/\n" . "https://example.org/")))
    (djot-test-with (concat "[ref][]\n\n" (car pair))
      (search-forward "ref")
      (if djot--multiline-references-p
          (progn
            (should (invisible-p 1))
            (should (equal (djot-link-destination (djot--link-at-point)) (cdr pair))))
        (should-not (invisible-p 1))
        (should-not (get-char-property (1- (point)) 'keymap))
        (should-error (djot-open-at-point) :type 'user-error)))))

(ert-deftest djot-mode-reference-edits-update-distant-links ()
  (djot-test-with "[ref][]\n\nSome other text.\n\n[ref]: https://example.org/\n"
    (search-forward "ref")
    (should (equal (get-char-property (1- (point)) 'help-echo) "https://example.org/"))
    (goto-char (point-max))
    (insert "  continuation\n")
    (font-lock-ensure)
    (goto-char 3)
    (if djot--multiline-references-p
        (should (equal (get-char-property (point) 'help-echo) "https://example.org/continuation"))
      (should-not (get-char-property (point) 'keymap))
      (should-not (invisible-p 1)))))

(ert-deftest djot-mode-multiline-link-partial-refresh-preserves-ranges ()
  (djot-test-with "[link\n  more](https://example.org) after\n"
    (should (invisible-p 1))
    (search-forward "https")
    (should (invisible-p (1- (point))))
    (let ((point-before (point)))
      (djot-refresh (line-beginning-position) (line-end-position))
      (should (= point-before (point)))
      (should (invisible-p 1))
      (should (invisible-p (1- (point)))))))

(ert-deftest djot-mode-native-fontification-does-not-run-hooks-or-code ()
  (let ((hook-ran nil) (executed nil))
    (let ((emacs-lisp-mode-hook (list (lambda () (setq hook-ran t)))))
      (djot-test-with (concat "```elisp\n(setq executed t)\n;; Local "
                              "Variables:\n;; eval: (setq executed t)\n;; End:\n```\n")
        (should-not hook-ran)
        (should-not executed)))))

(ert-deftest djot-mode-edit-source-command-reveals-destination ()
  (djot-test-with "[link](https://example.org)\n"
    (search-forward "link")
    (djot-edit-at-point)
    (should djot-source-visible)
    (should (looking-at "https://example.org"))
    (should-not (invisible-p 1))))

(ert-deftest djot-mode-plain-angle-text-is-not-an-autolink ()
  (djot-test-with "<not-a-url>\n"
    (should-not (invisible-p 1))
    (forward-char 2)
    (should-not (get-char-property (point) 'keymap))
    (should-error (djot-open-at-point) :type 'user-error)))

(ert-deftest djot-mode-multiline-inline-destinations-with-following-prose ()
  (dolist (source '("[link](https://example.org/\n  more) after\n"
                    "[link](https://example.org/\nmore) after\n"
                    "[link](https://example.org/\n  more)\n"))
    (djot-test-with source
      (when djot--multiline-references-p
        (search-forward "link")
        (should (equal (djot-link-destination (djot--link-at-point)) "https://example.org/more"))
        (should (invisible-p 1))
        (when (search-forward "after" nil t)
          (should-not (invisible-p (1- (point)))))))))

(ert-deftest djot-mode-official-0-3-2-trailing-reference-whitespace ()
  (djot-test-with "[r][]\n\n[r]: /first   \n  second\n"
    (when djot--multiline-references-p
      (search-forward "r")
      (should-not (djot-link-destination (djot--link-at-point)))
      (should-not (invisible-p 1))))
  (djot-test-with "[r][]\n\n[r]: /first\n  second  \n"
    (when djot--multiline-references-p
      (search-forward "r")
      (should (equal (djot-link-destination (djot--link-at-point)) "/first"))
      (search-forward "second")
      (should-not (invisible-p (1- (point)))))))

(ert-deftest djot-inline-code-markers-preserve-literal-source ()
  (dolist (ticks '("`" "``" "```"))
    (djot-test-with (concat "A " ticks "code _literal_ *x*" ticks "\n")
      (setq-local djot-hide-markup t)
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (let ((source (buffer-string)) (position (point))
            (foreign (make-overlay 2 4)))
        (overlay-put foreign 'face 'bold)
        (djot-refresh)
        (dotimes (i (length ticks))
          (should (invisible-p (+ 3 i)))
          (should (invisible-p (- (point-max) 2 i))))
        (search-forward "_literal_")
        (should-not (invisible-p (- (point) 9)))
        (goto-char position)
        (djot-show-source 1)
        (should-not (invisible-p 3))
        (djot-show-source 0)
        (should (invisible-p 3))
        (should (= position (point)))
        (should (equal source (buffer-string)))
        (should-not buffer-undo-list)
        (should-not (buffer-modified-p))
        (text-mode)
        (should (overlay-buffer foreign))
        (should-not (invisible-p 1)))))
  (dolist (source '("`unclosed\n\nAfter\n" "``mismatch`\n\nAfter\n"
                    "\\`escaped\\`\n" "`raw`{=html}\n" "```text\nx\n```\n"))
    (djot-test-with source
      (setq-local djot-hide-markup t)
      (djot-refresh)
      (should-not (invisible-p 1)))))

(ert-deftest djot-inline-code-table-widths-follow-source-display ()
  (djot-test-with "| `code` | z |\n| a | b |\n"
    (setq-local djot-hide-markup t)
    (djot-refresh)
    (djot-table-align)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "| `code` | z   |\n| a    | b   |\n"))
    (let ((source (buffer-string)))
      (djot-table-align)
      (should (equal source (buffer-string)))
      (djot-show-source 1)
      (should (equal source (buffer-string))))
    (djot-table-align)
    (should (string-match-p "| a      | b   |" (buffer-string)))))

(provide 'djot-mode-test)
;;; djot-mode-test.el ends here
