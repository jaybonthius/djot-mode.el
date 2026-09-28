;;; oracle.el --- Compare parsed link targets with official Djot -*- lexical-binding: t; -*-

;;; Commentary:
;; Set DJOT_ORACLE_MODULE to the scoped @djot/djot module directory.
;; The static Node program parses stdin; it never executes document code.
;;; Code:
(require 'djot-mode)
(require 'json)
(unless (getenv "DJOT_ORACLE_MODULE")
  (error "Set DJOT_ORACLE_MODULE to an installed official @djot/djot module"))
(let ((fixtures
       '("[r]:\n  https://example.test/path\n"
         "[r]: https://example.test/path?x=12\n  34\n   56\nparagraph\n"
         "[r]: /first\n\n  paragraph\n\n[s]: /second\n[t]: /third\n"
         "  [r]: /first\n  same-indent\n\n  [s]: /second\n   deeper\n"
         "- [r]: /first\n    continued\n\n- [s]: /second\n  paragraph\n"
         "> [r]: /first\n>   continued\n> paragraph\n"
         "{title=\"source\"}\n[r]:/first\n  continued\n\n[empty]:\n"
         "[r]: /first two\n\n[s]: /second\n  three four\n"
         "[r]: /first   \n  second\n"
         "[r]: /first\n  second  \n  third\n"
         "> [r]: /first\n  paragraph\n"
         "> > [r]:\n> >   /first\n> >    second\n"
         "[r]: /first\r\n  second\r\n"
         "[r]: /first\n\tsecond\n"
         "[r]: /first\n  second")))
  (dolist (source fixtures)
    (let ((oracle
           (with-temp-buffer
             (insert source)
             (let ((status (call-process-region
                            (point-min) (point-max) "node" t t nil "-e"
                            "const d=require(process.argv[1]);const s=require('fs').readFileSync(0,'utf8');process.stdout.write(JSON.stringify(d.parse(s).references));"
                            (getenv "DJOT_ORACLE_MODULE"))))
               (unless (eq status 0) (error "Official parser failed: %s" (buffer-string))))
             (goto-char (point-min))
             (json-parse-buffer))))
      (with-temp-buffer
        (insert source)
        (djot-mode)
        (unless djot--multiline-references-p (error "Corrected grammar not loaded"))
        (unless (djot-node-valid-p (treesit-parser-root-node djot-parser))
          (error "Grammar error for %S" source))
        (djot--ensure-index)
        (unless (= (hash-table-count oracle) (length djot--references))
          (error "Reference count differs for %S: %S" source djot--references))
        (maphash
         (lambda (label reference)
           (let ((expected (gethash "destination" reference))
                 (actual (cdr (assoc label djot--references))))
             (unless (equal expected actual)
               (error "For %S, %s: official %S, mode %S" source label expected actual))))
         oracle)
        (princ (format "PASS %S -> %S\n" source djot--references)))))
  (princ (format "%d real official-parser comparisons passed\n" (length fixtures))))
(let ((fixtures '("[link](https://example.org/\n  more) after\n"
                  "[link](https://example.org/\nmore) after\n"
                  "[link](https://example.org/\n  more)\n"
                  "_before_ [*link*](https://example.org/\n  more) *after*\n"
                  "[link](https://example.org/part\\)\n  more) after\n"
                  "[link](https://example.org/\n  more after\n"
                  "[link](https://example.org/\n\n  /more) after\n")))
  (dolist (source fixtures)
    (let ((oracle
           (with-temp-buffer
             (insert source)
             (let ((status (call-process-region
                            (point-min) (point-max) "node" t t nil "-e"
                            "const d=require(process.argv[1]);const s=require('fs').readFileSync(0,'utf8');let a=[];function walk(n){if(n.tag==='link' && n.destination!==undefined)a.push(n.destination);(n.children||[]).forEach(walk)}walk(d.parse(s));process.stdout.write(JSON.stringify(a));"
                            (getenv "DJOT_ORACLE_MODULE"))))
               (unless (eq status 0) (error "Official parser failed: %s" (buffer-string))))
             (goto-char (point-min))
             (json-parse-buffer :array-type 'list)))
          destinations)
      (with-temp-buffer
        (insert source) (djot-mode)
        (unless (djot-node-valid-p (treesit-parser-root-node djot-parser))
          (error "Grammar error for inline fixture %S" source))
        (djot-map-nodes (lambda (node) (push (djot-link-destination node) destinations))
                        nil nil '("inline_link"))
        (unless (equal oracle (nreverse destinations))
          (error "Inline destinations differ for %S: official %S, mode %S" source oracle destinations))
        (princ (format "PASS inline %S -> %S\n" source oracle)))))
  (princ (format "%d real inline-link comparisons passed\n" (length fixtures))))
;;; oracle.el ends here
