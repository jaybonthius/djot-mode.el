;;; code-oracle.el --- Real fenced-code semantics oracle -*- lexical-binding: t; -*-

;;; Commentary:
;; Compare semantic native input with official Djot, never execute document code.
;;; Code:
(require 'djot-mode)
(require 'json)
(unless (getenv "DJOT_ORACLE_MODULE")
  (error "Set DJOT_ORACLE_MODULE to the official @djot/djot module"))
(let ((count 0))
  (dolist (prefixes '(("" . "") ("  " . "  ") ("> " . "> ")
                      (">  " . ">  ") ("> > " . "> > ")
                      ("- " . "  ") ("- > " . "  > ") ("> - " . ">   ")))
    (dolist (fences '(("```" "```" "@cache\ndef f():\n    return 1 > 0\n")
                      ("```" "`````" "@cache\n")
                      ("````" "````" "```\n~~~\n````` info\n    @cache\n")))
      (dolist (ending '("" "\n"))
        (let* ((body (split-string (string-remove-suffix "\n" (nth 2 fences)) "\n"))
               (source (concat (car prefixes) (car fences) "python\n"
                               (mapconcat (lambda (line) (concat (cdr prefixes) line "\n")) body "")
                               (cdr prefixes) (cadr fences) ending))
               (official
                (with-temp-buffer
                  (insert source)
                  (unless (zerop (call-process-region
                                  (point-min) (point-max) "node" t t nil "-e"
                                  "const d=require(process.argv[1]);const doc=d.parse(require('fs').readFileSync(0,'utf8'));let a=[];function walk(n){if(n.tag==='code_block')a.push(n.text);(n.children||[]).forEach(walk)}walk(doc);process.stdout.write(JSON.stringify({code:a,html:d.renderHTML(doc)}));"
                                  (getenv "DJOT_ORACLE_MODULE")))
                    (error "Official code oracle failed"))
                  (json-parse-string (buffer-string) :array-type 'list)))
               actual)
          (with-temp-buffer
            (insert source) (djot-mode)
            (unless (djot-node-valid-p (treesit-parser-root-node djot-parser))
              (error "Code grammar error for %S: %s" source
                     (treesit-node-string (treesit-parser-root-node djot-parser))))
            (djot-map-nodes
             (lambda (node)
               (push (mapconcat (lambda (range)
                                  (buffer-substring-no-properties (car range) (cdr range)))
                                (djot--code-segments node) "") actual))
             nil nil '("code_block")))
          (unless (equal (nreverse actual) (gethash "code" official))
            (error "Code text differs for %S: mode %S official %S"
                   source actual (gethash "code" official)))
          (cl-incf count)
          (princ (format "PASS code %S -> %S\n%s" source (gethash "code" official)
                         (gethash "html" official)))))))
  (princ (format "%d real code-text and HTML comparisons passed\n" count)))
;;; code-oracle.el ends here
