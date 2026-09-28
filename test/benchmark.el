;;; benchmark.el --- Measure parser and display costs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run under an external timeout; DJOT_BENCH_HEADINGS controls heading count.
;; Report timings, not a hardware-dependent pass/fail performance threshold.
;;; Code:
(require 'djot-mode)
(require 'benchmark)
(let ((count (string-to-number (or (getenv "DJOT_BENCH_HEADINGS") "100"))))
  (with-temp-buffer
    (dotimes (i count)
      (insert (format "# Heading %d\n\nParagraph _emphasis_ [link](https://example.org).\n\n" i)))
    (princ (format "Emacs=%s grammar=%S headings=%d bytes=%d\n"
                   emacs-version treesit-extra-load-path count (buffer-size)))
    (princ (format "mode=%S\n" (benchmark-run 1 (djot-mode))))
    (setq-local djot-hide-markup t)
    (princ (format "parse=%S\n" (benchmark-run 1 (treesit-parser-root-node djot-parser))))
    (princ (format "fontify=%S\n" (benchmark-run 1 (font-lock-ensure))))
    (let ((last-overlay (car (overlays-in (- (point-max) 30) (point-max)))))
      (goto-char 1)
      (search-forward "Paragraph")
      (princ (format "edit-fontify=%S\n"
                     (benchmark-run 1
                       (insert " changed")
                       (font-lock-ensure (line-beginning-position) (line-end-position)))))
      (princ (format "distant-overlay-preserved=%S overlays=%d parsers=%d\n"
                     (and last-overlay (not (null (overlay-buffer last-overlay))))
                     (length (overlays-in (point-min) (point-max)))
                     (length (treesit-parser-list)))))))
;;; benchmark.el ends here
