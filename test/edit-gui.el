;;; edit-gui.el --- Isolated interactive editing evidence -*- lexical-binding: t; -*-

;;; Commentary:
;; GUI Emacs -Q -L . -l test/edit-gui.el under Xvfb; set GRAMMAR_DIR and
;; DJOT_GUI_DIR.  No personal init, modern package, or external resources.
;;; Code:
(condition-case error-data
    (progn
      (require 'djot-mode)
      (require 'org)
      (require-theme 'modus-themes)
      (setq treesit-extra-load-path (list (getenv "GRAMMAR_DIR")))
      (set-face-attribute 'default nil :family "Fira Code" :height 130)
      (set-face-attribute 'fixed-pitch nil :family "Fira Code")
      (set-face-attribute 'variable-pitch nil :family "Noto Sans")
      (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
      (set-frame-size (selected-frame) 1500 1100 t)
      (let ((org-buffer (get-buffer-create "Org base editing"))
            (djot-buffer (get-buffer-create "Djot base editing — modern OFF")))
        (with-current-buffer org-buffer
          (insert "* Editing a document\n\nA [[https://djot.net][descriptive link]] and /readable emphasis/.\n\n** Lists\n\n- [X] Review source\n- [ ] Write tests\n- [ ] Check the result\n\n** Tables\n\n| Item | Link | Status |\n|------+------ +--------|\n| Escaped pipe | [[https://djot.net][Djot]] | checked |\n| Literal code | text | ready |\n\n** Native source\n\n#+begin_src emacs-lisp\n(defun greet (name)\n  \"Say hello.\"\n  (format \"Hello %s\" name))\n#+end_src\n\n** Folded notes\n\nA hidden body stays safe from accidental edits.\n")
          (org-mode)
          (setq-local org-hide-emphasis-markers t)
          (goto-char (point-min)) (search-forward "| Item") (org-table-align)
          (goto-char (point-max)) (search-backward "** Folded notes") (org-cycle)
          (goto-char 1) (font-lock-ensure))
        (switch-to-buffer djot-buffer)
        (insert "# Editing a document\n\nA [descriptive link](https://djot.net) and _readable emphasis_.\n\n## Lists\n\n- [ ] Review source\n- [ ] Write tests\n\n## Tables\n\n| Item | Link | Status |\n|---|---|---|\n| Escaped \\| pipe | [Djot](https://djot.net) | checked |\n| `literal|code` | text | ready |\n\n## Native source\n\n```elisp\n(defun greet (name)\n  \"Say hello.\"\n  (format \"Hello %s\" name))\n```\n\n## Folded notes\n\nA hidden body stays safe from accidental edits.\n")
        (djot-mode) (setq-local djot-hide-markup t)
        ;; Exercise the actual user command bindings, then render the result.
        (goto-char 1) (search-forward "Review source")
        (execute-kbd-macro (kbd "C-c C-c"))
        (search-forward "Write tests")
        (execute-kbd-macro (kbd "M-RET")) (insert "Check the result")
        (search-forward "| Item") (execute-kbd-macro (kbd "C-c |"))
        (goto-char (point-max)) (search-backward "## Folded notes")
        (execute-kbd-macro (kbd "TAB"))
        (unless (invisible-p (+ (point) (length "## Folded notes\n\n")))
          (error "TAB did not fold the body"))
        (goto-char 1) (djot-refresh)
        (delete-other-windows)
        (switch-to-buffer org-buffer)
        (let ((right (split-window-right)))
          (set-window-buffer right djot-buffer)
          (dolist (theme '(modus-operandi-tinted modus-vivendi-tinted))
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            (with-current-buffer org-buffer (font-lock-flush) (font-lock-ensure))
            (with-current-buffer djot-buffer (djot-refresh))
            (redisplay t) (sit-for 0.3)
            (let ((coding-system-for-write 'no-conversion))
              (write-region (x-export-frames nil 'png) nil
                            (expand-file-name (format "editing-%s.png" theme) (getenv "DJOT_GUI_DIR")) nil 'silent)))
          (with-current-buffer djot-buffer
            (goto-char 1) (search-forward "Escaped")
            (let* ((rows (djot--table-rows))
                   (data (cl-remove-if (lambda (row) (nth 4 row)) rows))
                   (positions (mapcar (lambda (row) (cdr (cadr (nth 5 row)))) data))
                   (pixels (mapcar (lambda (pos) (car (posn-x-y (posn-at-point pos right)))) positions)))
              (unless (apply #'= pixels) (error "Visible table bars do not align: %S" pixels))
              (with-temp-file (expand-file-name "editing-validation.txt" (getenv "DJOT_GUI_DIR"))
                (insert (format "PASS real command keys: checkbox, item, alignment, folding\nPASS displayed table column bar x coordinates: %S\nEmacs %s; actual font %S\nModern OFF, isolated -Q, original source retained.\n"
                                pixels emacs-version (font-info (face-font 'default)))))))))
      (kill-emacs 0))
  (error
   (with-temp-file (expand-file-name "editing-error.txt" (getenv "DJOT_GUI_DIR"))
     (insert (format "%S\n" error-data)))
   (kill-emacs 1)))
;;; edit-gui.el ends here
