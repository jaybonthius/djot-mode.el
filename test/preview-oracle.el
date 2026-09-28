;;; preview-oracle.el --- Compare math with official Djot -*- lexical-binding: t; -*-

;;; Commentary:
;; Set DJOT_ORACLE_MODULE to the scoped @djot/djot 0.3.2 installation.
;; Compare real parser output, not mocked rendering or regexp extraction.
;;; Code:
(require 'djot-mode)
(require 'json)
(unless (getenv "DJOT_ORACLE_MODULE")
  (error "Set DJOT_ORACLE_MODULE to the official @djot/djot module"))
(let ((fixtures '("A $`x` and $$`` x ` y `` and $```x `` y```.\n"
                  "$$``x + y``" "$$` x^2 `\n"
                  "A $$`` x\n+ y `` end.\n" "- A $$``x`` formula.\n" "> $$`x`\n"
                  "`$x` and ``$$`x` `` and `$x`{=html}\n\n```tex\n$$`x`\n```\n"
                  "$``x\n\nAfter\n" "$``x`\n\nAfter\n")))
  (dolist (source fixtures)
    (let ((expected
           (with-temp-buffer
             (insert source)
             (unless (eq 0 (call-process-region
                            (point-min) (point-max) "node" t t nil "-e"
                            "const d=require(process.argv[1]);const fs=require('fs');let a=[];function w(n){if(n.tag==='inline_math'||n.tag==='display_math')a.push([n.tag,n.text]);(n.children||[]).forEach(w)}w(d.parse(fs.readFileSync(0,'utf8')));process.stdout.write(JSON.stringify(a))"
                            (getenv "DJOT_ORACLE_MODULE")))
               (error "Official math oracle failed"))
             (goto-char (point-min))
             (json-parse-buffer :array-type 'list)))
          actual)
      (with-temp-buffer
        (insert source) (djot-mode)
        (unless (djot-node-valid-p (treesit-parser-root-node djot-parser))
          (error "Math grammar error for %S" source))
        (djot-map-nodes
         (lambda (node)
           (push (list (if (equal (djot--text (djot--field node "math_marker")) "$$")
                           "display_math" "inline_math")
                       (djot--text (djot--field node "content"))) actual))
         nil nil '("math")))
      (unless (equal expected (nreverse actual))
        (error "Math mismatch %S: expected %S, actual %S" source expected actual))
      (princ (format "PASS math %S -> %S\n" source expected))))
  (princ (format "%d real official math comparisons passed\n" (length fixtures))))
;;; preview-oracle.el ends here
