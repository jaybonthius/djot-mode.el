;;; gui.el --- Isolated Djot foundation visual evidence -*- lexical-binding: t; -*-

;;; Commentary:
;; Run a GUI Emacs -Q -L . -l test/gui.el under Xvfb, with DJOT_GUI_DIR
;; and GRAMMAR_DIR set.  Errors are written to the artifact directory.
;;; Code:
(condition-case error-data
    (progn
      (require 'djot-mode)
      (require 'org)
      (require-theme 'modus-themes)
      (setq treesit-extra-load-path (list (getenv "GRAMMAR_DIR"))
            inhibit-startup-screen t)
      (set-face-attribute 'default nil :family "Fira Code" :height 130)
      (set-face-attribute 'fixed-pitch nil :family "Fira Code")
      (set-face-attribute 'variable-pitch nil :family "Noto Sans")
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (set-frame-size (selected-frame) 1500 1100 t)
      (let ((org-buffer (get-buffer-create "Org base"))
            (djot-buffer (get-buffer-create "Djot base — no modern")))
        (with-current-buffer org-buffer
          (insert "* A readable document\n\nProse with /emphasis/ and *strong text*.\nA [[https://djot.net][descriptive link]] stays interactive.\n\n** Structure and references\n\n*** Third level\n\n**** Fourth level\n\n***** Fifth level\n\n****** Sixth level\n\n** Native code\n\n#+begin_src emacs-lisp\n(defun greet (name)\n  \"Return a friendly greeting.\"\n  (format \"Hello, %s!\" name))\n#+end_src\n\n#+begin_quote\nQuoted prose follows the body font.\n#+end_quote\n")
          (org-mode)
          (setq-local org-hide-emphasis-markers t)
          (font-lock-ensure)
          (goto-char 1))
        (with-current-buffer djot-buffer
          (insert "# A readable document\n\nProse with _emphasis_ and *strong text*.\nA [descriptive link](https://djot.net) stays interactive.\n\n## Structure and references\n\n### Third level\n\n#### Fourth level\n\n##### Fifth level\n\n###### Sixth level\n\n## Native code\n\n```elisp\n(defun greet (name)\n  \"Return a friendly greeting.\"\n  (format \"Hello, %s!\" name))\n```\n\n> Quoted prose follows the body font.\n")
          (djot-mode)
          (setq-local djot-hide-markup t)
          (font-lock-ensure)
          (goto-char 1))
        (delete-other-windows)
        (switch-to-buffer org-buffer)
        (set-window-buffer (split-window-right) djot-buffer)
        (dolist (theme '(modus-operandi-tinted modus-vivendi-tinted))
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t)
          (with-current-buffer org-buffer (font-lock-flush) (font-lock-ensure))
          (with-current-buffer djot-buffer (djot-refresh))
          (redisplay t)
          (sit-for 0.3)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (x-export-frames nil 'png) nil
                          (expand-file-name (format "%s.png" theme) (getenv "DJOT_GUI_DIR")) nil 'silent)))
        (with-current-buffer djot-buffer
          (let ((link (cl-find-if (lambda (overlay) (overlay-get overlay 'djot-link))
                                  (overlays-in (point-min) (point-max)))))
            (goto-char (point-min))
            (search-forward "Return a friendly")
            (insert " test")
            (redisplay t)
            (sit-for 0.3)
            (unless (and link (overlay-buffer link))
              (error "GUI JIT fontification replaced an unrelated link overlay"))
            (with-temp-file (expand-file-name "incremental.txt" (getenv "DJOT_GUI_DIR"))
              (insert "PASS: real GUI redisplay after code edit preserved distant link overlay identity\n"))))
        (with-temp-file (expand-file-name "environment.txt" (getenv "DJOT_GUI_DIR"))
          (insert (format "Emacs %s\nFeatures %s\nFrame font %S\nDefault actual %S\nVariable pitch actual %S\nBase packages only; modern OFF. No personal init loaded.\n"
                          emacs-version system-configuration-features
                          (frame-parameter nil 'font)
                          (font-info (face-font 'default))
                          (font-info (face-font 'variable-pitch))))))
      (kill-emacs 0))
  (error
   (with-temp-file (expand-file-name "error.txt" (getenv "DJOT_GUI_DIR"))
     (insert (format "%S\n" error-data)))
   (kill-emacs 1)))
;;; gui.el ends here
