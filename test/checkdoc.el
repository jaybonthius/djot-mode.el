;;; checkdoc.el --- Strict package documentation gate -*- lexical-binding: t; -*-

;;; Commentary:
;; Exit nonzero on any package checkdoc diagnostic.
;;; Code:
(require 'checkdoc)
(let ((checkdoc-spellcheck-documentation-flag nil)
      (failures 0))
  (advice-add 'checkdoc-error :before
              (lambda (_position message)
                (setq failures (1+ failures))
                (princ (concat message "\n"))))
  (find-file "djot-mode.el")
  (checkdoc-current-buffer t)
  (when (> failures 0) (kill-emacs 1)))
;;; checkdoc.el ends here
