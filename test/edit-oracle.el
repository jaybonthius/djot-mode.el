;;; edit-oracle.el --- Real renderer checks for structural edits -*- lexical-binding: t; -*-

;;; Commentary:
;; Run via make edit-oracle with DJOT_ORACLE_MODULE set to @djot/djot.
;;; Code:
(require 'djot-mode)
(require 'json)

(defun djot-edit-oracle-html (source)
  "Render SOURCE with the real official parser, never a local approximation."
  (with-temp-buffer
    (insert source)
    (unless (eq 0 (call-process-region
                   (point-min) (point-max) "node" t t nil "-e"
                   "const d=require(process.argv[1]);process.stdout.write(d.renderHTML(d.parse(require('fs').readFileSync(0,'utf8'))));"
                   (or (getenv "DJOT_ORACLE_MODULE") (error "Set DJOT_ORACLE_MODULE"))))
      (error "Renderer failed: %s" (buffer-string)))
    (buffer-string)))

(let ((fixtures '("|a||c|\n" "||b||" "||||" "| | | |\n"
                  "|a\\|b||`c|d`|\n|:---|---:|:---:|\n|x|y|z|\n\n^ Caption\n"
                  "|a|b|\n|---|---:|" "> |a||c|" "- table\n\n  ||b||"
                  "> |a||c|\n>\n> ^ caption\n"
                  "| [link](https://example.org/long) | 界 |\n| plain | é |\n")))
  (dolist (source fixtures)
    (let ((expected (djot-edit-oracle-html source)))
      (with-temp-buffer
        (insert source)
        (djot-mode)
        (goto-char (point-min))
        (search-forward "|")
        (djot-table-align)
        (unless (equal expected (djot-edit-oracle-html (buffer-string)))
          (error "Alignment changed official HTML for %S to %S" source (buffer-string)))
        (djot-show-source 1)
        (djot-table-align)
        (unless (equal expected (djot-edit-oracle-html (buffer-string)))
          (error "Source-visible alignment changed official HTML for %S" source))
        (princ (format "PASS table source/visible alignment preserves official HTML: %S\n" source)))))
  (princ (format "%d table fixtures, 20 real before/after HTML comparisons passed\n" (length fixtures))))

(dolist (source '("more) after\n" "item. after\n" "mixing) after\n" "(More) after\n"))
  (unless (string-prefix-p "<p>" (djot-edit-oracle-html source)) (error "Oracle is not prose"))
  (with-temp-buffer
    (insert source) (djot-mode)
    (when (treesit-query-capture (treesit-parser-root-node djot-parser) '((list_item) @item))
      (error "False list item: %S" source)))
  (princ (format "PASS official/parser agree word is prose: %S\n" source)))
;;; edit-oracle.el ends here
