;;; djot-preview.el --- Explicit Djot previews and official export -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jay Bonthius
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of djot-mode, distributed under the GNU GPL version 3
;; or (at your option) any later version.  See COPYING.

;;; Commentary:

;; Optional commands, loaded on demand.  The overlay/cache design follows
;; Org's LaTeX preview and markdown-mode's local images; this is an independent
;; implementation using public APIs, not their parsers or private functions.
;; No tools run during fontification or redisplay.  TeX is NOT sandboxed.
;; HTML is official renderer output, NOT sanitized.  Only the explicitly
;; confirmed trusted-document browser preview adds a defensive CSP wrapper.

;;; Code:

(require 'djot-mode)
(require 'color)
(require 'image)
(require 'face-remap)
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-flush "image.c" (spec &optional frame))

(defgroup djot-preview nil "Explicit Djot previews and export." :group 'djot)

(defcustom djot-command "djot"
  "Official Djot executable, used only by explicit HTML commands.
Install the official @djot/djot package or specify its executable path.
No shell command or document-supplied filters are accepted."
  :type 'file :group 'djot-preview)

(defcustom djot-latex-command "latex"
  "LaTeX executable for explicit math previews, with shell escape disabled."
  :type 'file :group 'djot-preview)

(defcustom djot-dvisvgm-command "dvisvgm"
  "DVI-to-SVG executable for explicit math previews."
  :type 'file :group 'djot-preview)

(defcustom djot-image-max-width 600
  "Maximum local image preview width in pixels."
  :type 'natnum :group 'djot-preview)

(defcustom djot-image-max-height 400
  "Maximum local image preview height in pixels."
  :type 'natnum :group 'djot-preview)

(defcustom djot-math-scale 1.5
  "Math preview scale relative to the current default font height.
After changing scale or font/theme, explicitly preview again."
  :type 'number :group 'djot-preview)

(defvar-local djot--preview-overlays nil)
(defvar-local djot--math-cache-directory nil)
(defvar-local djot--math-metrics nil)
(defvar-local djot--preview-html-files nil)

(defun djot--preview-setup ()
  "Install local cleanup hooks without starting any external work."
  (unless (derived-mode-p 'djot-mode) (user-error "This is not a Djot buffer"))
  (add-hook 'before-change-functions #'djot--preview-before-change nil t)
  (add-hook 'djot-after-fontify-hook #'djot--preview-source-display -90 t)
  (add-hook 'pre-redisplay-functions #'djot--preview-redisplay nil t)
  (add-hook 'change-major-mode-hook #'djot--preview-teardown nil t)
  (add-hook 'kill-buffer-hook #'djot--preview-teardown nil t))

(defun djot--preview-invalidate (beg end)
  "Invalidate display-dependent layout around a preview in BEG END.
Table layout needs the whole table, even when source text is unchanged."
  (when (< beg end)
    (let ((table (djot-node-at beg "table")))
      (font-lock-flush (if table (max (point-min) (treesit-node-start table)) beg)
                       (if table (min (point-max) (treesit-node-end table)) end)))))

(defun djot--clear-previews (&optional kind)
  "Delete only our preview overlays, optionally restricted to KIND."
  (setq djot--preview-overlays
        (cl-delete-if
         (lambda (overlay)
           (when (or (null kind) (eq kind (overlay-get overlay 'djot-preview)))
             (when (overlay-buffer overlay)
               (let ((beg (max (point-min) (overlay-start overlay)))
                     (end (min (point-max) (overlay-end overlay))))
                 (djot--preview-invalidate beg end)))
             (delete-overlay overlay)
             t))
         djot--preview-overlays)))

(defun djot--preview-before-change (&rest _arguments)
  "Invalidate previews before source edits, including reference edits.
_ARGUMENTS are the standard change-hook arguments.  Cached math survives."
  (djot--clear-previews))

(defun djot--preview-source-display (&rest _arguments)
  "Suspend or restore preview display following explicit source visibility.
_ARGUMENTS are fontification bounds; no renderer runs here."
  (dolist (overlay djot--preview-overlays)
    (overlay-put overlay 'display
                 (unless djot-source-visible
                   (overlay-get overlay 'djot-preview-display)))
    (overlay-put overlay 'face
                 (unless djot-source-visible
                   '(:underline nil :strike-through nil :box nil)))
    (when (eq (overlay-get overlay 'djot-preview) 'image)
      (overlay-put overlay 'keymap (unless djot-source-visible djot-link-map))
      (overlay-put overlay 'follow-link (not djot-source-visible)))))

(defun djot--preview-metrics ()
  "Return the current font, theme and math scale signature."
  (list (face-foreground 'default nil t) (face-background 'default nil t)
        (frame-char-height)
        (and (boundp 'text-scale-mode-amount) text-scale-mode-amount)
        djot-math-scale))

(defun djot--preview-redisplay (_windows)
  "Invalidate old math when font/theme settings change; never rerender it.
_WINDOWS is the standard redisplay-hook argument."
  (unless (equal djot--math-metrics (djot--preview-metrics))
    (djot--clear-previews 'math)))

(defun djot--preview-overlay (node kind image)
  "Display IMAGE over NODE in an owned overlay of KIND."
  (let ((beg (treesit-node-start node)) (end (treesit-node-end node)))
    (dolist (old (copy-sequence djot--preview-overlays))
      (when (and (overlay-buffer old) (= beg (overlay-start old))
                 (eq kind (overlay-get old 'djot-preview)))
        (delete-overlay old)
        (setq djot--preview-overlays (delq old djot--preview-overlays))))
    (let ((overlay (make-overlay beg end nil t nil)))
      (overlay-put overlay 'djot-preview kind)
      (overlay-put overlay (if (eq kind 'image) 'djot-image-preview 'djot-math-preview) t)
      (overlay-put overlay 'priority 1000)
      ;; Link interaction remains on base overlays, but do not draw its
      ;; underline (or a surrounding emphasis decoration) through the image.
      (overlay-put overlay 'face
                   (unless djot-source-visible '(:underline nil :strike-through nil :box nil)))
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'djot-preview-display image)
      (when (eq kind 'image)
        (overlay-put overlay 'keymap (unless djot-source-visible djot-link-map))
        (overlay-put overlay 'follow-link (not djot-source-visible))
        (overlay-put overlay 'help-echo (djot-link-destination node)))
      (overlay-put overlay 'display (unless djot-source-visible image))
      (push overlay djot--preview-overlays)
      ;; Presentation widths may change without a source modification tick.
      (djot--preview-invalidate beg end)
      overlay)))

(defun djot--source-directory ()
  "Return the local source directory without visiting remote files."
  (let ((directory (if buffer-file-name (file-name-directory buffer-file-name)
                     default-directory)))
    (when (file-remote-p directory) (user-error "Djot previews require a local source directory"))
    directory))

(defun djot--local-image-path (destination)
  "Return a local file path for DESTINATION, or nil for nonlocal URLs.
Reject schemes, authorities and remote file handlers before accessing files."
  (when (and destination
             (not (string-prefix-p "//" destination))
             (not (string-prefix-p "#" destination))
             (not (and (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" destination)
                       (not (string-prefix-p "file:" destination)))))
    (let ((path (string-remove-prefix "file:" destination)))
      (when (string-prefix-p "///" path) (setq path (substring path 2)))
      (unless (string-prefix-p "//" path)
        (setq path (decode-coding-string
                    (url-unhex-string (car (split-string path "[?#]"))) 'utf-8))
        (unless (or (string-empty-p path) (string-match-p "[\0\n\r]" path)
                    (string-prefix-p "//" path) (file-remote-p path))
          (setq path (expand-file-name path (djot--source-directory)))
          (unless (file-remote-p path) path))))))

(defun djot-display-inline-images ()
  "Preview parser-confirmed local images in the accessible buffer.
Remote resources are never fetched.  Source edits clear all previews;
explicit source display suspends them.  Use `djot-remove-inline-images'
to remove them, without affecting links or modern presentation."
  (interactive)
  (djot--preview-setup)
  (unless (display-images-p) (user-error "Local previews need a graphical Emacs with image support"))
  (let ((count 0) (skipped 0) failures)
    (djot--clear-previews 'image)
    (djot-map-nodes
     (lambda (node)
       (when (and (djot-node-valid-p node)
                  (<= (point-min) (treesit-node-start node))
                  (<= (treesit-node-end node) (point-max)))
         (condition-case err
             (let ((file (djot--local-image-path (djot-link-destination node))))
               (if (and file (file-regular-p file) (file-readable-p file))
                   (let ((image (create-image file nil nil :max-width djot-image-max-width
                                              :max-height djot-image-max-height :ascent 'center)))
                     (unless image (error "Unsupported image: %s" file))
                     (image-flush image)
                     (let ((size (image-size image t)))
                       (unless (and size (> (car size) 0) (> (cdr size) 0))
                         (error "Cannot decode image: %s" file)))
                     (djot--preview-overlay node 'image image)
                     (cl-incf count))
                 (cl-incf skipped)))
           (error (push (error-message-string err) failures)))))
     nil nil '("inline_image" "full_reference_image" "collapsed_reference_image"))
    (when failures
      (with-current-buffer (get-buffer-create "*Djot diagnostics*")
        (goto-char (point-max)) (insert (string-join (nreverse failures) "\n") "\n")))
    (message "Djot: %d local images, %d skipped%s" count skipped
             (if failures "; image errors in *Djot diagnostics*" ""))
    count))

(defun djot-remove-inline-images ()
  "Remove only Djot local-image preview overlays."
  (interactive)
  (djot--clear-previews 'image))

(defun djot--executable (program)
  "Resolve local executable PROGRAM or report the optional dependency."
  (when (file-remote-p program) (user-error "Djot tools must be local executables"))
  (or (executable-find program)
      (user-error "Djot needs %s for this command; install it or customize its executable" program)))

(defun djot--run-tool (program arguments &optional input directory)
  "Run local PROGRAM with ARGUMENTS, optional INPUT and working DIRECTORY.
Return stdout on success.  Preserve diagnostics on failure.  Quitting cancels
synchronous subprocess work; unwind cleanup also runs on cancellation."
  (let* ((default-directory (or directory temporary-file-directory))
         (executable (djot--executable program))
         (stderr (progn
                   (when (file-remote-p default-directory)
                     (user-error "Djot tools require a local directory"))
                   (make-temp-file "djot-stderr-")))
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (with-temp-buffer
          (let ((status (if input
                            (progn (insert input)
                                   (apply #'call-process-region (point-min) (point-max)
                                          executable t (list t stderr) nil arguments))
                          (apply #'call-process executable nil (list t stderr) nil arguments))))
            (let ((output (buffer-string)))
              (with-current-buffer (get-buffer-create "*Djot diagnostics*")
                (goto-char (point-max))
                (insert (format "\n%s %S: %s\n" executable arguments status))
                (unless (eq status 0) (insert output))
                (insert-file-contents stderr))
              (unless (eq status 0)
                (user-error "Djot tool %s failed (%s); see *Djot diagnostics*" program status))
              output)))
      (delete-file stderr))))

(defconst djot--math-preamble
  "\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath,amssymb,xcolor}\n\\pagestyle{empty}\n"
  "Fixed math preamble; never derived from document code or attributes.")

(defun djot--math-image (node)
  "Render math NODE with real LaTeX/dvisvgm, using a private session cache."
  (let* ((latex (djot--executable djot-latex-command))
         (converter (djot--executable djot-dvisvgm-command))
         (metrics (djot--preview-metrics))
         (rgb (color-name-to-rgb (car metrics)))
         (content (djot--text (djot--field node "content")))
         (display (equal "$$" (djot--text (djot--field node "math_marker"))))
         (tex (concat djot--math-preamble "\\begin{document}\n"
                      (format "\\color[rgb]{%s}\n" (mapconcat (lambda (c) (format "%.6f" c)) rgb ","))
                      (if display "\\[" "\\(") content (if display "\\]" "\\)")
                      "\n\\end{document}\n"))
         (key (secure-hash 'sha256
                           (prin1-to-string (list tex metrics latex converter
                                                  (file-attribute-modification-time (file-attributes latex))
                                                  (file-attribute-modification-time (file-attributes converter))))))
         file)
    (unless (and rgb (> djot-math-scale 0)) (user-error "Math needs a resolved color and positive scale"))
    (unless djot--math-cache-directory
      (setq djot--math-cache-directory (make-temp-file "djot-math-cache-" t)))
    (setq file (expand-file-name (concat key ".svg") djot--math-cache-directory))
    (unless (file-exists-p file)
      (let ((directory (make-temp-file "djot-math-work-" t)))
        (unwind-protect
            (let ((source (expand-file-name "formula.tex" directory))
                  (svg (expand-file-name "formula.svg" directory)))
              (with-temp-file source (insert tex))
              (djot--run-tool latex
                              (list "-interaction=nonstopmode" "-halt-on-error" "-no-shell-escape"
                                    (concat "-output-directory=" directory) source) nil directory)
              (djot--run-tool converter
                              (list (expand-file-name "formula.dvi" directory) "--no-fonts"
                                    "--exact-bbox" (concat "--output=" svg)) nil directory)
              (unless (and (file-regular-p svg) (> (file-attribute-size (file-attributes svg)) 0))
                (user-error "Djot math conversion produced no SVG; see *Djot diagnostics*"))
              (rename-file svg file))
          (delete-directory directory t))))
    (setq djot--math-metrics metrics)
    (create-image file 'svg nil :ascent 'center :foreground (car metrics)
                  :scale (* djot-math-scale (/ (float (frame-char-height)) 16)
                            (if (and (boundp 'text-scale-mode-amount) text-scale-mode-amount)
                                (expt text-scale-mode-step text-scale-mode-amount) 1)))))

(defun djot-preview-math (&optional all)
  "Preview math at point, in the active region, or with ALL in the buffer.
Only matched parser-confirmed math runs LaTeX, with no shell escape.
This is NOT a sandbox: untrusted TeX may read local files or consume resources.
Quitting cancels processing.  Edits clear previews but retain the cache."
  (interactive "P")
  (djot--preview-setup)
  (unless (and (display-images-p) (image-type-available-p 'svg))
    (user-error "Math previews need graphical Emacs with SVG support"))
  (let* ((region (use-region-p))
         (beg (cond (all (point-min)) (region (region-beginning)) (t (point))))
         (end (cond (all (point-max)) (region (region-end)) (t (min (point-max) (1+ (point))))))
         nodes (count 0))
    (djot-map-nodes
     (lambda (node)
       (when (and (djot--matched-verbatim-p node)
                  (<= (point-min) (treesit-node-start node))
                  (<= (treesit-node-end node) (point-max)))
         (push node nodes))) beg end '("math"))
    (unless nodes (user-error "No complete Djot math in the requested range"))
    (dolist (node (nreverse nodes))
      (djot--preview-overlay node 'math (djot--math-image node))
      (cl-incf count))
    (message "Djot: previewed %d math fragments" count)
    count))

(defun djot-clear-math-preview ()
  "Remove only Djot math preview overlays, retaining cached SVGs."
  (interactive)
  (djot--clear-previews 'math))

(defun djot-clear-math-cache ()
  "Clear math previews and their private per-buffer session cache."
  (interactive)
  (djot-clear-math-preview)
  (when djot--math-cache-directory
    (when (file-directory-p djot--math-cache-directory)
      (delete-directory djot--math-cache-directory t))
    (setq djot--math-cache-directory nil)))

(defun djot--preview-teardown ()
  "Remove owned preview overlays and temporary files on buffer teardown."
  (djot--clear-previews)
  (djot-clear-math-cache)
  (dolist (file djot--preview-html-files)
    (when (file-exists-p file) (delete-file file)))
  (setq djot--preview-html-files nil)
  (remove-hook 'before-change-functions #'djot--preview-before-change t)
  (remove-hook 'djot-after-fontify-hook #'djot--preview-source-display t)
  (remove-hook 'pre-redisplay-functions #'djot--preview-redisplay t))

(defun djot--render-html ()
  "Return the official renderer's unchanged HTML for the accessible source."
  (djot--run-tool djot-command '("--to" "html")
                  (buffer-substring-no-properties (point-min) (point-max))
                  (djot--source-directory)))

(defun djot-export-to-html (&optional file)
  "Export accessible source to FILE through the official Djot renderer.
Default to the visited filename with .html suffix, or prompt if unvisited.
HTML is not sanitized: no browser is launched.  Relative resources resolve
from FILE's directory; the default keeps it beside the source document.
Only replace an existing output after successful rendering and writing."
  (interactive)
  (let ((target (expand-file-name
                 (or file (and buffer-file-name
                               (concat (file-name-sans-extension buffer-file-name) ".html"))
                     (if noninteractive (user-error "Specify an export file for an unvisited buffer")
                       (read-file-name "Export Djot HTML: " nil nil nil "document.html"))))))
    (when (file-remote-p target) (user-error "Choose a local HTML output file"))
    (when (and buffer-file-name
               (or (equal target (expand-file-name buffer-file-name))
                   (file-equal-p target buffer-file-name)))
      (user-error "HTML output must not replace the Djot source"))
    (let* ((html (djot--render-html))
           (temporary (make-temp-file (expand-file-name ".djot-export-" (file-name-directory target)))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file temporary (insert html)))
            (rename-file temporary target t)
            target)
        (when (file-exists-p temporary) (delete-file temporary))))))

(defconst djot--preview-csp
  "default-src 'none'; img-src file:; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'; object-src 'none'"
  "Defense-in-depth for trusted previews, not a sanitizer or network guarantee.")

(defun djot-preview-html ()
  "Confirm trust EACH time, then open an official-renderer HTML preview.
The external browser can execute active content or access the network;
CSP is defense-in-depth, not sanitization, and does not stop all navigation.
Refuse noninteractive invocation.  The wrapper lives beside the source so
relative resources resolve correctly; it is deleted on buffer teardown."
  (interactive)
  (unless (and (not noninteractive)
               (yes-or-no-p "Active content/network access may occur despite CSP.  Trust this document in an external browser? "))
    (user-error "Djot browser preview requires explicit trusted-document confirmation"))
  (djot--preview-setup)
  (let* ((html (djot--render-html))
         (file (make-temp-file (expand-file-name ".djot-preview-" (djot--source-directory)) nil ".html"))
         success)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file file
              (insert "<!doctype html>\n<html><head><meta charset=\"utf-8\">\n"
                      "<meta http-equiv=\"Content-Security-Policy\" content=\"" djot--preview-csp "\">\n"
                      "<title>Djot trusted preview</title></head><body>\n" html "</body></html>\n")))
          (browse-url-of-file file)
          (push file djot--preview-html-files)
          (setq success t)
          file)
      (unless success (delete-file file)))))

(provide 'djot-preview)
;;; djot-preview.el ends here
