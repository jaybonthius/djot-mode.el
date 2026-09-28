;;; preview-gui.el --- Isolated real preview gate and screenshots -*- lexical-binding: t; -*-

;;; Commentary:
;; Run GTK Emacs -Q -L . -L test -l test/preview-gui.el under Xvfb.
;; Set GRAMMAR_DIR, DJOT_COMMAND and DJOT_GUI_DIR.  Optionally put modern
;; on load-path for on/off captures.  No personal init or network resources.
;;; Code:
(require 'cl-lib)
(defun djot-preview-gui--write (name text)
  "Write TEXT into artifact NAME."
  (with-temp-file (expand-file-name name (getenv "DJOT_GUI_DIR")) (insert text)))
(defun djot-preview-gui--capture (name)
  "Capture the real frame into artifact NAME."
  (redisplay t) (sit-for 0.2)
  (let ((coding-system-for-write 'no-conversion))
    (write-region (x-export-frames nil 'png) nil
                  (expand-file-name name (getenv "DJOT_GUI_DIR")) nil 'silent)))

(condition-case err
    (progn
      (setq treesit-extra-load-path (list (getenv "GRAMMAR_DIR"))
            use-dialog-box nil debug-on-error nil)
      (require 'djot-edit-test)
      (require 'djot-preview-test)
      (require-theme 'modus-themes)
      (set-face-attribute 'default nil :family "Fira Code" :height 130)
      (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 130)
      (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 130)
      (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
      (set-frame-size (selected-frame) 1200 1050 t)
      (let ((stats (ert-run-tests-batch t)))
        (djot-preview-gui--write
         "ert.txt"
         (mapconcat (lambda (test)
                      (format "%s %S" (ert-test-name test) (type-of (ert-test-most-recent-result test))))
                    (ert-select-tests t t) "\n"))
        (unless (zerop (ert-stats-completed-unexpected stats)) (error "Preview GUI ERT failed")))
      (let* ((default-directory (file-name-as-directory (getenv "DJOT_GUI_DIR")))
             (djot-command (getenv "DJOT_COMMAND"))
             (source "# Explicit local previews\n\nA [local link](#Explicit-local-previews) and `code _literal_` with hidden outer markers.\n\n![Local image](local.svg)\n\nInline $`x^2+y^2=z^2` and display $$``\\frac{a}{b}=\\sqrt{c}``.\n\n> Quoted math $$``x\n> + y``.\n\n## Native code remains native\n\n```elisp\n(defun square (x)\n  \"Square x.\"\n  (* x x))\n```\n\n| Code | Link |\n|---|---|\n| `literal|pipe` | [heading](#Explicit-local-previews) |\n\n`<b>Trusted local browser fixture</b>`{=html}\n")
             (buffer (get-buffer-create "Djot explicit previews")))
        (with-temp-file "local.svg" (insert djot-test-image))
        (switch-to-buffer buffer)
        (insert source) (djot-mode)
        (setq-local djot-hide-markup t)
        (setq buffer-file-name (expand-file-name "trusted.djot"))
        (write-region (point-min) (point-max) buffer-file-name nil 'silent)
        (set-buffer-modified-p nil)
        (buffer-enable-undo) (setq buffer-undo-list nil)
        (goto-char (point-min))
        (dolist (theme '(modus-operandi-tinted modus-vivendi-tinted))
          (mapc #'disable-theme custom-enabled-themes) (load-theme theme t)
          (djot-refresh)
          (djot-display-inline-images) (djot-preview-math t)
          (djot-preview-gui--capture (format "base-%s.png" theme))
          (when (require 'djot-modern nil t)
            (djot-modern-mode 1)
            (djot-preview-gui--capture (format "modern-%s.png" theme))
            (djot-modern-mode -1)
            (djot-preview-gui--capture (format "modern-off-%s.png" theme))))
        (djot-show-source 1)
        (djot-preview-gui--capture "explicit-source.png")
        (djot-show-source 0)
        (unless (and (equal source (buffer-substring-no-properties (point-min) (point-max)))
                     (= (point) (point-min)) (null buffer-undo-list) (not (buffer-modified-p)))
          (error "Preview commands changed source, point, undo or modified status"))
        (djot-export-to-html)
        ;; Exercise the real interactive trust prompt using keyboard events.
        ;; The separate browser smoke opens the identical retained wrapper.
        (let ((unread-command-events (list ?y ?e ?s ?\r)) preview)
          (cl-letf (((symbol-function 'browse-url-of-file) (lambda (file) (setq preview file))))
            (call-interactively #'djot-preview-html))
          (unless preview (error "Confirmed browser command did not open a file"))
          (copy-file preview "trusted-preview.html" t))
        (djot-preview-gui--write
         "environment.txt"
         (format "Emacs %s\nFeatures %s\nDefault %s\nVariable %s\nFixed %s\nParser count %s\nSource/point/undo/modified preserved.\nReal interactive trusted-preview confirmation accepted; browser target copied to trusted-preview.html.\n"
                 emacs-version system-configuration-features
                 (face-font 'default) (face-font 'variable-pitch) (face-font 'fixed-pitch)
                 (length (treesit-parser-list))))
        (with-current-buffer "*Djot diagnostics*"
          (write-region (point-min) (point-max) (expand-file-name "tools.log" (getenv "DJOT_GUI_DIR")) nil 'silent))
        (kill-buffer buffer))
      (kill-emacs 0))
  (error (djot-preview-gui--write "error.txt" (format "%S\n" err)) (kill-emacs 1)))
;;; preview-gui.el ends here
