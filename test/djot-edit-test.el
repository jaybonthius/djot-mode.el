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

(provide 'djot-edit-test)
;;; djot-edit-test.el ends here
