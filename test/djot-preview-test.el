;;; djot-preview-test.el --- Explicit preview/export behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch tests need only the grammar.  Set DJOT_COMMAND to the official CLI
;; for export integration.  GTK tests additionally use real latex/dvisvgm.
;;; Code:
(require 'djot-mode-test)
(require 'djot-preview)

(defconst djot-test-image
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"140\" height=\"64\"><rect width=\"140\" height=\"64\" rx=\"8\" fill=\"#397e92\"/><circle cx=\"35\" cy=\"32\" r=\"18\" fill=\"#ebcf80\"/></svg>")

(defmacro djot-test-directory (&rest body)
  "Run BODY in a private local source directory."
  (declare (indent 0) (debug t))
  `(let ((default-directory (file-name-as-directory (make-temp-file "djot-test-" t))))
     (unwind-protect (progn ,@body) (delete-directory default-directory t))))

(defun djot-test-official-command ()
  "Return the explicitly configured official CLI or skip this integration."
  (or (getenv "DJOT_COMMAND") (executable-find "djot")
      (ert-skip "Set DJOT_COMMAND for the real official-renderer gate")))

(defun djot-test-preview-count (kind)
  "Count live owned overlays of KIND."
  (cl-count-if (lambda (o) (and (overlay-buffer o) (eq kind (overlay-get o 'djot-preview))))
               djot--preview-overlays))

(ert-deftest djot-preview-local-path-policy ()
  (djot-test-directory
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "note.djot"))
      (set-buffer-modified-p nil)
      (should (equal (djot--local-image-path "image%20one.svg?x=1#part")
                     (expand-file-name "image one.svg")))
      (should (equal (djot--local-image-path "file:///tmp/image.svg") "/tmp/image.svg"))
      (dolist (path '("https://example.invalid/image.svg" "http://x/a" "data:image/png;base64,abc"
                      "javascript:alert(1)" "ftp://x/a" "//host/a" "file://host/a"
                      "/ssh:host:/tmp/a" "%2fssh:host:/tmp/a" "#anchor" ""))
        (should-not (djot--local-image-path path))))))

(ert-deftest djot-preview-never-runs-during-editing ()
  (cl-letf (((symbol-function 'djot--run-tool) (lambda (&rest _) (ert-fail "Automatic tool execution")))
            ((symbol-function 'browse-url) (lambda (&rest _) (ert-fail "Automatic browser")))
            ((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Automatic network"))))
    (djot-test-with "![remote](https://example.invalid/x.svg) $`x`\n\n```elisp\n(error \"EXECUTED\")\n```\n"
      (djot-refresh)
      (djot-show-source 1)
      (djot-show-source 0)
      (insert "a")
      (font-lock-ensure)
      (should-not djot--preview-overlays))))

(ert-deftest djot-preview-missing-tools-and-nongraphical-errors ()
  (should-error (djot--run-tool "/missing/djot-tool" nil) :type 'user-error)
  (should-error (djot--executable "/ssh:host:/tool") :type 'user-error)
  (unless (display-images-p)
    (djot-test-with "$`x`\n"
      (should-error (djot-preview-math) :type 'user-error)
      (should-error (djot-display-inline-images) :type 'user-error)
      (should-not djot--preview-overlays))))

(ert-deftest djot-preview-official-export-faithful-and-atomic ()
  (let ((djot-command (djot-test-official-command)))
    (djot-test-directory
      (djot-test-with "# Faithful\n\n![local](image.svg)\n\n[link](next.djot)\n\n`<b onclick=\"alert(1)\">raw</b>`{=html}\n\n[^1]: Footnote\n\nNote[^1].\n"
        (setq buffer-file-name (expand-file-name "note.djot"))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (let* ((source (buffer-string)) (position (point))
               (expected (djot--render-html)) (file (djot-export-to-html)))
          (should (equal file (expand-file-name "note.html")))
          (should (equal expected (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (should (string-match-p "onclick=" expected))
          (should-not (string-match-p "Content-Security-Policy" expected))
          (should (equal source (buffer-string)))
          (should (= position (point)))
          (should-not buffer-undo-list)
          (should-not (buffer-modified-p))
          (dolist (djot-command '("/missing/djot" "false"))
            (should-error (djot-export-to-html file) :type 'user-error)
            (should (equal expected (with-temp-buffer (insert-file-contents file) (buffer-string)))))
          (should-error (djot-export-to-html buffer-file-name) :type 'user-error)
          (should-error (djot-export-to-html "/ssh:host:/tmp/export.html") :type 'user-error)
          (should-not (directory-files default-directory nil "\\`\\.djot-export-")))))))

(ert-deftest djot-preview-unvisited-explicit-export ()
  (let ((djot-command (djot-test-official-command)))
    (djot-test-directory
      (djot-test-with "Unvisited _source_.\n"
        (let ((noninteractive t))
          (should-error (djot-export-to-html) :type 'user-error))
        (should (file-exists-p (djot-export-to-html (expand-file-name "chosen.html"))))))))

(ert-deftest djot-preview-browser-denial-and-batch-fail-closed ()
  (djot-test-with "# Never launch\n"
    (cl-letf (((symbol-function 'djot--render-html) (lambda () (ert-fail "Rendered before consent")))
              ((symbol-function 'browse-url-of-file) (lambda (&rest _) (ert-fail "Launched before consent"))))
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
          (should-error (djot-preview-html) :type 'user-error)))
      (let ((noninteractive t))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "Batch consent prompt"))))
          (should-error (djot-preview-html) :type 'user-error)))
      (should-not djot--preview-html-files))))

(ert-deftest djot-preview-browser-confirmation-each-time-and-wrapper ()
  (let ((djot-command (djot-test-official-command)) (confirmations 0) opened)
    (djot-test-directory
      (djot-test-with "# Harmless\n\n![local](image.svg)\n\n[Other](other.djot)\n"
        (setq buffer-file-name (expand-file-name "note.djot"))
        (set-buffer-modified-p nil)
        (let ((source (buffer-string)) (html (djot--render-html)) files)
          (let ((noninteractive nil))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (cl-incf confirmations) t))
                      ((symbol-function 'browse-url-of-file) (lambda (file) (push file opened))))
              (push (djot-preview-html) files)
              (push (djot-preview-html) files)))
          (should (= 2 confirmations))
          (should (= 2 (length opened)))
          (should-not (equal (car files) (cadr files)))
          (dolist (file files)
            (should (equal (file-name-directory file) default-directory))
            (let ((content (with-temp-buffer (insert-file-contents file) (buffer-string))))
              (should (string-match-p (regexp-quote html) content))
              (should (string-match-p (regexp-quote djot--preview-csp) content))))
          (should (equal source (buffer-string)))
          (text-mode)
          (dolist (file files) (should-not (file-exists-p file))))))))

(ert-deftest djot-preview-browser-failure-never-launches ()
  (let ((djot-command "false") (noninteractive nil))
    (djot-test-directory
      (djot-test-with "Text\n"
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'browse-url-of-file) (lambda (&rest _) (ert-fail "Launched after failure"))))
          (should-error (djot-preview-html) :type 'user-error)
          (should-not djot--preview-html-files)
          (should-not (directory-files default-directory nil "\\`\\.djot-preview-")))))))

(ert-deftest djot-preview-owned-overlay-source-edit-and-teardown ()
  (djot-test-with "$`x`\n\nOutside\n"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil)
    (djot--preview-setup)
    (let ((node (djot-node-at 3 "math")) (source (buffer-string))
          (foreign (make-overlay 1 4)) (position (point)))
      ;; Lifecycle is display-independent; actual image rendering is tested below.
      (djot--preview-overlay node 'math "preview")
      (djot-show-source 1)
      (should-not (overlay-get (car djot--preview-overlays) 'display))
      (djot-show-source 0)
      (should (equal "preview" (overlay-get (car djot--preview-overlays) 'display)))
      (should (equal source (buffer-string)))
      (should (= position (point)))
      (should-not buffer-undo-list)
      (should-not (buffer-modified-p))
      (goto-char (point-max))
      (insert "Edit")
      (should-not djot--preview-overlays)
      (should (overlay-buffer foreign))
      (djot--preview-overlay (djot-node-at 3 "math") 'math "preview")
      (narrow-to-region 7 (point-max))
      (text-mode)
      (should-not djot--preview-overlays)
      (should (overlay-buffer foreign)))))

(ert-deftest djot-preview-gui-local-and-reference-images ()
  (skip-unless (display-images-p))
  (djot-test-directory
    (with-temp-file "image one.svg" (insert djot-test-image))
    (djot-test-with "![one](image%20one.svg)\n\n![reference][r]\n\n[r]: image%20one.svg\n\n![remote](https://example.invalid/x.svg)\n\n![data](data:image/png;base64,a)\n"
      (setq buffer-file-name (expand-file-name "note.djot"))
      (let ((source (buffer-string)) (position (point)))
        (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "Network fetch"))))
          (should (= 2 (djot-display-inline-images))))
        (should (= 2 (djot-test-preview-count 'image)))
        (dolist (overlay djot--preview-overlays)
          (let ((image (overlay-get overlay 'display)))
            (should (equal (plist-get (cdr image) :file) (expand-file-name "image one.svg")))
            (should (> (car (image-size image t)) 0)))
          (save-excursion
            (goto-char (overlay-start overlay))
            (should (eq (key-binding (kbd "RET")) #'djot-open-at-point))
            (should (equal '(:underline nil :strike-through nil :box nil)
                           (overlay-get overlay 'face)))))
        (djot-display-inline-images)
        (should (= 2 (djot-test-preview-count 'image)))
        (should (equal source (buffer-string)))
        (should (= position (point)))
        (setq buffer-file-name nil)
        (goto-char (point-max)) (insert "\n[r]: missing.svg\n")
        (should-not djot--preview-overlays)
        (should (= 1 (djot-display-inline-images)))
        (djot-remove-inline-images)
        (should-not djot--preview-overlays)
        (set-buffer-modified-p nil)))))

(ert-deftest djot-preview-gui-real-math-cache-and-failure ()
  (skip-unless (and (display-images-p) (image-type-available-p 'svg)))
  (djot-test-with "Inline $`x^2 + y^2` and $$``\\frac{a}{b}`` end.\n\n$`\\notARealDjotCommand`\n"
    (goto-char 10)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil)
    (let ((source (buffer-string)) (position (point)) cache file)
      (should (= 1 (djot-preview-math)))
      (setq cache djot--math-cache-directory
            file (plist-get (cdr (overlay-get (car djot--preview-overlays) 'display)) :file))
      (should (file-exists-p file))
      (should (> (car (image-size (overlay-get (car djot--preview-overlays) 'display) t)) 0))
      (let ((mtime (file-attribute-modification-time (file-attributes file))))
        (cl-letf (((symbol-function 'djot--run-tool) (lambda (&rest _) (ert-fail "Cache miss"))))
          (djot-preview-math))
        (should (equal mtime (file-attribute-modification-time (file-attributes file)))))
      (should (= 1 (djot-test-preview-count 'math)))
      (search-forward "frac")
      (should (= 1 (djot-preview-math)))
      (should (= 2 (djot-test-preview-count 'math)))
      (goto-char position)
      (should (equal source (buffer-string)))
      (should-not buffer-undo-list)
      (should-not (buffer-modified-p))
      (search-forward "notAReal")
      (let ((before (directory-files cache nil "svg\\'")))
        (should-error (djot-preview-math) :type 'user-error)
        (should (equal before (directory-files cache nil "svg\\'"))))
      (should (with-current-buffer "*Djot diagnostics*" (string-match-p "Undefined control sequence" (buffer-string))))
      (djot-clear-math-preview)
      (should-not djot--preview-overlays)
      (should (file-exists-p file))
      (text-mode)
      (should-not (file-exists-p cache)))))

(ert-deftest djot-preview-gui-unmatched-math-and-theme-invalidation ()
  (skip-unless (and (display-images-p) (image-type-available-p 'svg)))
  (djot-test-with "$``unclosed\n\nValid $`x`\n"
    (should-error (djot-preview-math) :type 'user-error)
    (search-forward "$`x")
    (djot-preview-math)
    (let ((djot-math-scale (+ djot-math-scale 0.2)))
      (djot--preview-redisplay nil)
      (should-not djot--preview-overlays)
      (djot-preview-math)
      (should (= 1 (djot-test-preview-count 'math))))))

(ert-deftest djot-preview-gui-cancel-cleans-private-work ()
  (skip-unless (and (display-images-p) (image-type-available-p 'svg)))
  (djot-test-with "$`x`\n"
    (let ((before (directory-files temporary-file-directory nil "\\`djot-math-work-")))
      (cl-letf (((symbol-function 'djot--run-tool) (lambda (&rest _) (signal 'quit nil))))
        (should (eq 'cancelled (condition-case nil (djot-preview-math) (quit 'cancelled)))))
      (should-not djot--preview-overlays)
      (should (equal before (directory-files temporary-file-directory nil "\\`djot-math-work-"))))))

(provide 'djot-preview-test)
;;; djot-preview-test.el ends here
