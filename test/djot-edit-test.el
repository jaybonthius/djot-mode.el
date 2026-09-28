;;; djot-edit-test.el --- Public structural editing tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise actual source changes and real grammar, not parser mocks.
;;; Code:
(require 'djot-mode-test)

(ert-deftest djot-heading-navigation-ignores-code-and-continuation ()
  (djot-test-with "# One\n# continuation\n\n```\n# fake\n```\n\n## Child\n\ntext\n\n# Two\n"
    (djot-next-heading)
    (should (looking-at "## Child"))
    (djot-up-heading)
    (should (= 1 (point)))
    (djot-next-heading)
    (djot-next-heading)
    (should (looking-at "# Two"))
    (djot-previous-heading)
    (should (looking-at "## Child"))))

(ert-deftest djot-heading-fold-cycle-and-edit-safety ()
  (djot-test-with "# One\n\nbody\n\n## Child\n\nchild body\n\n# Two\n"
    (let ((text (buffer-string)) (foreign (make-overlay 1 3)))
      (overlay-put foreign 'foreign t)
      (djot-cycle)
      (should (invisible-p 8))
      (goto-char 8)
      (should-error (insert "unsafe") :type 'user-error)
      (should (equal text (buffer-string)))
      (goto-char 1)
      (djot-refresh)
      (should (invisible-p 8))
      (djot-cycle)
      (search-forward "Child")
      (should-not (invisible-p (1- (point))))
      (search-forward "child body")
      (should (invisible-p (1- (point))))
      (goto-char 1)
      (djot-cycle)
      (should-not (invisible-p 8))
      (djot-cycle-global)
      (should (invisible-p 8))
      (djot-cycle-global)
      (should-not (invisible-p 8))
      (djot-cycle)
      (text-mode)
      (should (overlay-buffer foreign))
      (should-not (invisible-p 8)))))

(ert-deftest djot-heading-promote-demote-multiline ()
  (djot-test-with "## One\n## continued\n\n### Child\n\ntext\n\n# Other\n"
    (djot-promote-heading)
    (should (equal (buffer-string) "# One\n# continued\n\n## Child\n\ntext\n\n# Other\n"))
    (goto-char 1)
    (should-error (djot-promote-heading) :type 'user-error)
    (djot-demote-heading)
    (should (equal (buffer-string) "## One\n## continued\n\n### Child\n\ntext\n\n# Other\n"))))

(ert-deftest djot-list-newline-split-exit-and-task-reset ()
  (djot-test-with "- [X] first second"
    (search-forward "first")
    (djot-newline)
    (should (equal (buffer-string) "- [X] first\n- [ ] second"))
    (goto-char (point-max))
    (djot-newline)
    (should (string-suffix-p "\n- [ ] " (buffer-string)))
    (djot-newline)
    (should (string-suffix-p "second\n" (buffer-string)))))

(ert-deftest djot-list-ordered-marker-styles ()
  (dolist (pair '(("9." . "10.") ("(9)" . "(10)") ("b)" . "c)")
                  ("(B)" . "(C)") ("iv." . "v.") ("(XIX)" . "(XX)")))
    (djot-test-with (concat (car pair) " item")
      (goto-char (point-max))
      (djot-insert-item)
      (should (equal (buffer-string) (concat (car pair) " item\n" (cdr pair) " ")))))
  (djot-test-with "more) ordinary prose"
    (goto-char (point-max))
    (should-error (djot-insert-item) :type 'user-error)
    (djot-newline)
    (should (equal (buffer-string) "more) ordinary prose\n"))))

(ert-deftest djot-list-toggle-checkbox-and-normal-undo ()
  (djot-test-with "- text\n"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (djot-toggle-checkbox)
    (should (equal (buffer-string) "- [ ] text\n"))
    (undo-boundary)
    (undo 1)
    (should (equal (buffer-string) "- text\n"))
    (djot-toggle-checkbox)
    (djot-toggle-checkbox)
    (should (equal (buffer-string) "- [X] text\n"))
    (djot-toggle-checkbox t)
    (should (equal (buffer-string) "- text\n"))))

(ert-deftest djot-list-indent-outdent-subtree ()
  (djot-test-with "- first\n- second\n\n  - child\n"
    (search-forward "second")
    (djot-indent-item)
    (should (equal (buffer-string) "- first\n\n  - second\n\n    - child\n"))
    (should (= 3 (length (treesit-query-capture (treesit-parser-root-node djot-parser) '((list_item) @item)))))
    (djot-outdent-item)
    (should (equal (buffer-string) "- first\n\n- second\n\n  - child\n"))))

(ert-deftest djot-list-quoted-and-lazy-continuation ()
  (djot-test-with "> - one\n> - two"
    (goto-char (point-max))
    (djot-newline)
    (should (equal (buffer-string) "> - one\n> - two\n> - ")))
  (djot-test-with "- one\n  - not a child"
    (search-forward "child")
    (should-error (djot-outdent-item) :type 'user-error)))

(ert-deftest djot-list-marker-cycle-siblings-not-children ()
  (djot-test-with "- one\n- two\n\n  - child\n"
    (djot-cycle-list-marker)
    (should (equal (buffer-string) "+ one\n+ two\n\n  - child\n"))))

(ert-deftest djot-definition-term-body-and-new-item ()
  (djot-test-with ": orange"
    (goto-char (point-max))
    (djot-newline)
    (insert "A citrus fruit.")
    (should (equal (buffer-string) ": orange\n\n  A citrus fruit."))
    (djot-insert-item)
    (should (equal (buffer-string) ": orange\n\n  A citrus fruit.\n\n: "))))

(ert-deftest djot-malformed-and-code-editing-do-not-guess ()
  (djot-test-with "```\n- fake\n| unfinished\n`````\n"
    (search-forward "fake")
    (should-error (djot-toggle-checkbox) :type 'user-error)
    (should-error (djot-table-align) :type 'user-error)
    (djot-newline)
    (should (string-match-p "- fake\n\n" (buffer-string))))
  (djot-test-with "| unfinished"
    (should-error (djot-table-align) :type 'user-error)))

(ert-deftest djot-table-align-escaped-verbatim-empty-cells-caption ()
  (djot-test-with "| a\\|b || `c|d` |\n|:---|---:|:---:|\n| long | x | y |\n\n^ Caption\n"
    (let ((origin (point)))
      (djot-table-align)
      (should (= origin (point)))
      (should (equal (mapcar (lambda (row) (nth 3 row)) (djot--table-rows))
                     '(("a\\|b" "" "`c|d`") (":-----" "----:" ":-----:") ("long" "x" "y"))))
      (should (string-suffix-p "\n\n^ Caption\n" (buffer-string)))
      (let ((text (buffer-string)))
        (djot-table-align)
        (should (equal text (buffer-string)))))))

(ert-deftest djot-table-align-one-step-undo ()
  (djot-test-with "|a| b |\n|--|--|\n| cc |d|\n"
    (let ((text (buffer-string)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (djot-table-align)
      (undo-boundary)
      (undo 1)
      (should (equal text (buffer-string))))))

(ert-deftest djot-table-cell-navigation-and-appended-row ()
  (djot-test-with "| a | b |\n|---|---|\n| c | d |\n"
    (search-forward "a")
    (djot-table-next-cell)
    (should (looking-at "b"))
    (djot-table-next-cell)
    (should (looking-at "c"))
    (djot-table-previous-cell)
    (should (looking-at "b"))
    (search-forward "d")
    (djot-table-next-cell)
    (should (= 4 (length (djot--table-rows))))
    (should (equal (nth 3 (car (last (djot--table-rows)))) '("" "")))))

(ert-deftest djot-table-row-column-and-separator-edits ()
  (djot-test-with "| a | b |\n| c | d |\n"
    (djot-table-insert-separator)
    (should (nth 4 (nth 1 (djot--table-rows))))
    (djot-table-insert-column)
    (should (equal (nth 3 (car (djot--table-rows))) '("" "a" "b")))
    (djot-table-delete-column)
    (should (equal (nth 3 (car (djot--table-rows))) '("a" "b")))
    (djot-table-insert-row t)
    (should (= 4 (length (djot--table-rows))))
    (djot-table-delete-row)
    (should (= 3 (length (djot--table-rows))))))

(ert-deftest djot-table-edits-preserve-quote-prefixes ()
  (djot-test-with "> | a | b |\n> | c | d |\n"
    (search-forward "a")
    (djot-table-align)
    (djot-table-insert-row)
    (goto-char (point-min))
    (while (not (eobp))
      (should (looking-at "> |"))
      (forward-line 1))))

(ert-deftest djot-link-insertion-and-edit-source ()
  (djot-test-with ""
    (djot-insert-link "local (file).djot" "a [label]")
    (goto-char 3)
    (should (equal (djot-link-destination (djot--link-at-point)) "local%20(file).djot"))
    (should djot-source-visible)
    (djot-insert-link "#Heading" "Heading")
    (should (equal (buffer-string) "[Heading](#Heading)"))))

(ert-deftest djot-reference-insertion-update-and-visit ()
  (djot-test-with "[Example][ref]\n"
    (djot-insert-reference "ref" "https://example.org/one")
    (goto-char 3)
    (should (equal (djot-link-destination (djot--link-at-point)) "https://example.org/one"))
    (djot-insert-reference "ref" "https://example.org/two")
    (should (equal (djot-link-destination (djot--link-at-point)) "https://example.org/two"))
    (djot-goto-reference)
    (should (looking-at "\\[ref\\]: https://example.org/two"))
    (should-error (djot-insert-reference "bad]label" "x") :type 'user-error)))

(ert-deftest djot-footnote-create-reuse-and-return ()
  (djot-test-with "Text here."
    (search-forward "Text")
    (let ((origin (point)))
      (djot-insert-footnote)
      (insert "A note.")
      (should (equal (buffer-string) "Text[^1] here.\n\n[^1]: A note."))
      (djot-footnote-return)
      (should (= origin (point)))
      (djot-open-at-point)
      (should (looking-at "\\[\\^1\\]:"))
      (djot-footnote-return)
      (goto-char (point-max))
      (insert "\n\nOther")
      (djot-insert-footnote "1")
      (should (looking-at "\\[\\^1\\]:"))
      (should (= 1 (length (treesit-query-capture (treesit-parser-root-node djot-parser) '((footnote) @note))))))))


(ert-deftest djot-table-concealed-width-unicode-and-display-toggle ()
  (djot-test-with "| [link](https://example.org/long) | 界 |\n| plain | é |\n"
    (search-forward "link")
    (let ((offset (- (point) (line-beginning-position))))
      (djot-table-align)
      (should (= offset (- (point) (line-beginning-position)))))
    (let ((source (buffer-string)))
      (dotimes (_ 3) (djot-table-align))
      (should (equal source (buffer-string)))
      (should (string-match-p "| plain |" source))
      ;; Modern/foreign display spacing must not influence alignment.
      (let ((foreign (make-overlay (point-min) (1+ (point-min)))))
        (overlay-put foreign 'display "                         ")
        (djot-table-align)
        (should (equal source (buffer-string)))
        (delete-overlay foreign))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (djot-show-source 1)
      (should (equal source (buffer-string)))
      (should-not buffer-undo-list)
      (djot-table-align)
      (should (> (length (buffer-string)) (length source)))
      (let ((visible-source (buffer-string)))
        (dotimes (_ 3) (djot-table-align))
        (should (equal visible-source (buffer-string))))
      (undo-boundary)
      (undo 1)
      (should (equal source (buffer-string))))))

(ert-deftest djot-table-empty-columns-survive-every-edit ()
  (dolist (source '("|a||c|\n" "||b||\n" "||||\n" "| | | |\n"))
    (djot-test-with source
      (let ((before (nth 3 (car (djot--table-rows)))))
        (should (= 3 (length before)))
        (djot-table-align)
        (should (equal before (nth 3 (car (djot--table-rows)))))
        (djot-table-insert-column)
        (should (= 4 (length (nth 3 (car (djot--table-rows))))))
        (djot-table-delete-column)
        (should (equal before (nth 3 (car (djot--table-rows)))))))))

(ert-deftest djot-definition-body-ret-is-not-a-new-term ()
  (djot-test-with ": term\n\n  definition"
    (goto-char (point-max))
    (djot-newline)
    (insert "continued")
    (should (equal (buffer-string) ": term\n\n  definition\n  continued"))
    (should (= 1 (length (treesit-query-capture (treesit-parser-root-node djot-parser) '((list_item) @item)))))))

(ert-deftest djot-edit-reference-reveals-definition-destination ()
  (djot-test-with "[text][r]\n\n[r]: https://example.org\n"
    (goto-char 3)
    (djot-edit-at-point)
    (should (looking-at "https://example.org"))
    (should djot-source-visible)))

(ert-deftest djot-folded-link-target-is-revealed-explicitly ()
  (djot-test-with "# One\n\n## Child\n\nbody\n\n# Two\n\n[visit](#Child)\n"
    (djot-cycle)
    (goto-char (point-max))
    (search-backward "visit")
    (djot-open-at-point)
    (should (looking-at "## Child"))
    (should-not (invisible-p (point)))))


(ert-deftest djot-table-eof-source-preservation-and-navigation ()
  (dolist (source '("|a||c|" "||||" "|a\\|b|`c|d`||" "|a|b|\n|---|---:|"
                    "> |a||c|" "- table\n\n  ||b||"))
    (djot-test-with source
      (goto-char (point-max))
      (let ((cells (mapcar (lambda (row) (nth 3 row)) (djot--table-rows))))
        (djot-table-align)
        (should-not (string-suffix-p "\n" (buffer-string)))
        (cl-loop for before in cells for after in (djot--table-rows) do
                 (if (nth 4 after)
                     (should (equal (mapcar (lambda (cell) (replace-regexp-in-string "-+" "-" cell)) before)
                                    (mapcar (lambda (cell) (replace-regexp-in-string "-+" "-" cell)) (nth 3 after))))
                   (should (equal before (nth 3 after)))))
        (djot-table-next-cell)
        (should (= (1+ (length cells)) (length (djot--table-rows))))))))

(ert-deftest djot-table-quoted-caption-edit-preserves-caption ()
  (djot-test-with "> |a||c|\n>\n> ^ caption\n"
    (search-forward "a")
    (djot-table-align)
    (djot-table-insert-column)
    (should (string-suffix-p ">\n> ^ caption\n" (buffer-string)))
    (should (= 1 (length (treesit-query-capture (treesit-parser-root-node djot-parser) '((table_caption) @caption)))))))

(ert-deftest djot-heading-fold-keeps-eof-title-visible ()
  (djot-test-with "# Title"
    (djot-cycle)
    (should-not (invisible-p (1- (point-max))))))


(ert-deftest djot-heading-container-folds-stop-at-siblings ()
  (dolist (source '("::: note\n## One\n\nbody\n\n## Two\n\nbody\n:::\n\noutside\n"
                    "::: note\n## One\n\nbody\n\n# Two\n\nbody\n:::\n\noutside\n"
                    "> ## One\n>\n> body\n>\n> ## Two\n>\n> body\n\noutside\n"
                    "::: outer\n:::: inner\n## One\n\nbody\n\n## Two\n\nbody\n::::\n:::\n\noutside\n"))
    (djot-test-with source
      (let ((original (buffer-string)))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (search-forward "One")
        (djot-cycle)
        (search-forward "body")
        (should (invisible-p (1- (point))))
        (should-error (insert "unsafe") :type 'user-error)
        (search-forward "Two")
        (should-not (invisible-p (1- (point))))
        (search-forward "outside")
        (should-not (invisible-p (1- (point))))
        (should (equal original (buffer-string)))
        (should-not buffer-undo-list)))))

(ert-deftest djot-heading-container-subtree-keeps-child-until-next-sibling ()
  (djot-test-with "::: outer\n## One\n\nbody\n\n### Child\n\nchild body\n\n## Two\n\nbody\n:::\n"
    (search-forward "One") (djot-cycle)
    (search-forward "Child")
    (should (invisible-p (1- (point))))
    (goto-char 1) (search-forward "One") (djot-cycle)
    (search-forward "Child")
    (should-not (invisible-p (1- (point))))
    (search-forward "child body")
    (should (invisible-p (1- (point))))
    (search-forward "Two")
    (should-not (invisible-p (1- (point))))))


(defun djot-test-container (body context final-newline)
  "Wrap BODY in CONTEXT, preserving FINAL-NEWLINE when requested."
  (let ((text (pcase context
                ('plain body)
                ('div (concat "::: note\n" body ":::\n"))
                ('list (concat "- container\n\n"
                               (replace-regexp-in-string "^\\(.+\\)$" "  \\1" body)))
                ('quote (concat (replace-regexp-in-string "^" "> " (string-remove-suffix "\n" body)) "\n"))
                ('nested-quote (concat (replace-regexp-in-string "^" "> > " (string-remove-suffix "\n" body)) "\n")))))
    (if final-newline text (string-trim-right text "\n"))))

(ert-deftest djot-editing-container-boundary-matrix ()
  (dolist (context '(plain div list quote nested-quote))
    (dolist (final-newline '(nil t))
      (ert-info ((format "Context %s, final newline %S" context final-newline))
        (djot-test-with (djot-test-container
                        "# One\n\nbody\n\n## Child\n\nchild body\n\n# Two\n\nbody\n" context final-newline)
          (should (= 3 (length (djot--heading-entries))))
          (search-forward "One") (djot-cycle)
          (search-forward "Child")
          (should (invisible-p (1- (point))))
          (search-forward "Two")
          (should-not (invisible-p (1- (point))))
          (djot-show-all))
        (djot-test-with (djot-test-container
                        "|a||`b|c`|\n|---|---|---:|\n|d||e|\n\n^ caption\n  continued\n" context final-newline)
          (should (= 1 (length (treesit-query-capture (treesit-parser-root-node djot-parser) '((table) @table)))))
          (let ((captions (treesit-query-capture (treesit-parser-root-node djot-parser) '((table_caption) @caption))))
            (should (= 1 (length captions)))
            (should (string-match-p "continued" (djot--text (cdar captions)))))
          (search-forward "|a")
          (djot-table-align)
          (should (= 3 (length (djot--table-rows))))
          (should (cl-every (lambda (row) (= 3 (length (nth 3 row)))) (djot--table-rows))))))))

(provide 'djot-edit-test)
;;; djot-edit-test.el ends here
