;;; djot-mode.el --- Structured editing of Djot documents -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jay Bonthius
;; Author: Jay Bonthius
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: text, markup, languages
;; URL: https://github.com/jaybonthius/djot-mode.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Djot editing with the tree-sitter-djot grammar.  Install that grammar
;; using `treesit-install-language-grammar', then open a .djot file.
;; Descriptive links are enabled by default; emphasis concealment is
;; opt-in.  `djot-show-source' reveals syntax explicitly, never on motion.
;; The optional djot-modern package adds presentation, not semantics.
;; See README.org for setup and the extension interface.
;; Recognition follows treeman/tree-sitter-djot and John MacFarlane's
;; djot-ts-mode.  Link interaction and native code fontification follow
;; the designs of Org's org-activate-links and org-src-font-lock-fontify-block.
;; This implementation does not load Org or execute document code.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'outline)
(require 'browse-url)
(require 'url-util)
(require 'subr-x)

(defgroup djot nil "Edit Djot documents." :group 'text)

(defcustom djot-hide-markup nil
  "Hide matched emphasis and span delimiters.
Literal, escaped, malformed and code syntax always remains visible."
  :type 'boolean :group 'djot :safe #'booleanp)

(defcustom djot-descriptive-links t
  "Show descriptive link text rather than brackets and destinations."
  :type 'boolean :group 'djot :safe #'booleanp)

(defcustom djot-src-fontify-natively t
  "Fontify code using the language's Emacs major mode, without execution."
  :type 'boolean :group 'djot :safe #'booleanp)

(defcustom djot-language-modes
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("lisp" . lisp-mode) ("sh" . sh-mode) ("bash" . sh-mode)
    ("js" . js-mode) ("javascript" . js-mode) ("python" . python-mode)
    ("json" . js-json-mode) ("c" . c-mode) ("cpp" . c++-mode)
    ("html" . html-mode) ("css" . css-mode) ("ruby" . ruby-mode))
  "Code language aliases to fontification major modes.
Other languages use LANGUAGE-mode if that function is available."
  :type '(alist :key-type string :value-type function) :group 'djot)

(defface djot-heading-1 '((t :inherit outline-1)) "Level one heading." :group 'djot)
(defface djot-heading-2 '((t :inherit outline-2)) "Level two heading." :group 'djot)
(defface djot-heading-3 '((t :inherit outline-3)) "Level three heading." :group 'djot)
(defface djot-heading-4 '((t :inherit outline-4)) "Level four heading." :group 'djot)
(defface djot-heading-5 '((t :inherit outline-5)) "Level five heading." :group 'djot)
(defface djot-heading-6 '((t :inherit outline-6)) "Level six and deeper heading." :group 'djot)
(defface djot-marker '((t :inherit shadow)) "Structural punctuation." :group 'djot)
(defface djot-emphasis '((t :inherit italic)) "Emphasis." :group 'djot)
(defface djot-strong '((t :inherit bold)) "Strong emphasis." :group 'djot)
(defface djot-highlight '((t :inherit highlight)) "Highlighted text." :group 'djot)
(defface djot-insert '((t :inherit success)) "Inserted text." :group 'djot)
(defface djot-delete '((t :inherit error :strike-through t)) "Deleted text." :group 'djot)
(defface djot-verbatim '((t :inherit (fixed-pitch font-lock-constant-face))) "Verbatim text." :group 'djot)
(defface djot-code '((t :inherit fixed-pitch)) "Code, without a token foreground override." :group 'djot)
(defface djot-language '((t :inherit font-lock-comment-face)) "Code language label." :group 'djot)
(defface djot-quote '((t :inherit font-lock-doc-face)) "Quoted prose, preserving the prose family." :group 'djot)
(defface djot-attribute '((t :inherit font-lock-preprocessor-face)) "Attributes." :group 'djot)
(defface djot-link '((t :inherit link)) "Link descriptions." :group 'djot)
(defface djot-destination '((t :inherit shadow)) "Link destinations and labels." :group 'djot)
(defface djot-footnote '((t :inherit link)) "Footnote references." :group 'djot)
(defface djot-math '((t :inherit (fixed-pitch font-lock-string-face))) "Math source." :group 'djot)
(defface djot-table '((t :inherit fixed-pitch)) "Table source." :group 'djot)
(defface djot-list-marker '((t :inherit font-lock-builtin-face)) "List markers and checkboxes." :group 'djot)

(defvar-local djot-parser nil
  "The sole Djot parser owned by this buffer's major mode.
Extensions must reuse this parser, never delete it or create another.")
(defvar-local djot-source-visible nil
  "Non-nil means explicit source display overrides concealment options.")
(defvar djot-after-fontify-hook nil
  "Functions called with BEG END after base fontification of that range.
May run repeatedly on overlapping ranges.  Extensions must replace only
their own decorations in that range, and must not change buffer text.
Neither native token faces nor base overlays belong to extensions.")
(defvar-local djot--footnote-return nil)
(defvar-local djot--index-tick nil)
(defvar-local djot--references nil)
(defvar-local djot--headings nil)
(defvar-local djot--anchors nil)
(defvar-local djot--multiline-references-p nil)

(defun djot-node-at (&optional position type)
  "Return the smallest named Djot node at POSITION, defaulting to point.
If TYPE is non-nil, ascend to an ancestor whose type equals TYPE.
Nodes are ephemeral: do not cache them across source edits."
  (when djot-parser
    (let ((node (treesit-node-at (or position (point)) djot-parser)))
      (when type
        (while (and node (not (equal type (treesit-node-type node))))
          (setq node (treesit-node-parent node))))
      node)))

(defun djot-node-children (node)
  "Return NODE's named children in source order."
  (let (children)
    (dotimes (i (treesit-node-child-count node t))
      (push (treesit-node-child node i t) children))
    (nreverse children)))

(defun djot-node-valid-p (node)
  "Return non-nil if NODE is complete and not inside an ERROR node.
A sibling's error does not invalidate an otherwise complete node."
  (and node (not (treesit-node-check node 'has-error))
       (not (treesit-node-check node 'missing))
       (let ((parent node) (valid t))
         (while parent
           (when (equal (treesit-node-type parent) "ERROR") (setq valid nil))
           (setq parent (treesit-node-parent parent)))
         valid)))

(defun djot-map-nodes (function &optional beg end types)
  "Call FUNCTION on named nodes intersecting BEG END in source order.
Bounds default to the accessible buffer.  TYPES is nil or a list of node
type strings.  Includes ancestors intersecting the range; callers must
clip decorations to BEG END.  Nodes must not be retained across edits."
  (when djot-parser
    (dolist (capture (treesit-query-capture
                      (treesit-parser-root-node djot-parser) '((_) @node)
                      (or beg (point-min)) (or end (point-max))))
      (let ((node (cdr capture)))
        (when (or (null types) (member (treesit-node-type node) types))
          (funcall function node))))))

(defun djot--field (node field)
  "Return NODE's FIELD child."
  (treesit-node-child-by-field-name node field))

(defun djot--text (node)
  "Return NODE's source text, or an empty string if absent."
  (if node (treesit-node-text node t) ""))

(defun djot--overlay (beg end &rest properties)
  "Make an owned, nonsticky overlay from BEG to END with PROPERTIES."
  (when (< beg end)
    (let ((overlay (make-overlay beg end nil t nil)))
      (overlay-put overlay 'djot-mode t)
      (overlay-put overlay 'evaporate t)
      (while properties
        (overlay-put overlay (pop properties) (pop properties)))
      overlay)))

(defun djot--clear-overlays (beg end &optional preserve-outside)
  "Delete base-mode overlays in BEG END.
With PRESERVE-OUTSIDE, retain the portions outside that range."
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'djot-mode)
      (when preserve-outside
        (when (< (overlay-start overlay) beg)
          (move-overlay (copy-overlay overlay) (overlay-start overlay) beg))
        (when (> (overlay-end overlay) end)
          (move-overlay (copy-overlay overlay) end (overlay-end overlay))))
      (delete-overlay overlay))))

(defun djot--hide (beg end start finish)
  "Conceal BEG END, clipped to fontification range START FINISH."
  (unless djot-source-visible
    (djot--overlay (max beg start) (min end finish) 'invisible 'djot-markup)))

(defun djot--face (node face start end &optional append)
  "Apply FACE to NODE within START END, optionally APPEND it."
  (when node
    (let ((beg (max start (treesit-node-start node)))
          (finish (min end (treesit-node-end node))))
      (when (< beg finish)
        (treesit-fontify-with-override beg finish face (if append 'append 'prepend))))))

(defconst djot--link-types
  '("inline_link" "inline_image" "full_reference_link" "full_reference_image"
    "collapsed_reference_link" "collapsed_reference_image" "autolink"
    "footnote_reference")
  "Grammar constructs supporting link interaction.")

(defconst djot--inline-faces
  '(("emphasis" . djot-emphasis) ("strong" . djot-strong)
    ("highlighted" . djot-highlight) ("insert" . djot-insert)
    ("delete" . djot-delete) ("superscript" . djot-emphasis)
    ("subscript" . djot-emphasis))
  "Faces for paired inline constructs.")

(defun djot--fontify-node (node _override start end)
  "Fontify a captured NODE in START END, ignoring _OVERRIDE."
  (when (djot-node-valid-p node)
    (let* ((type (treesit-node-type node))
           (content (djot--field node "content"))
           (inline (assoc type djot--inline-faces)))
      (cond
       (inline
        (djot--face content (cdr inline) start end)
        (dolist (field '("begin_marker" "end_marker"))
          (when-let* ((marker (djot--field node field)))
            (djot--face marker 'djot-marker start end)
            (when djot-hide-markup
              (djot--hide (treesit-node-start marker) (treesit-node-end marker) start end)))))
       ((equal type "heading")
        (let ((level (length (string-trim (djot--text (djot--field node "marker"))))))
          (djot--face content (intern (format "djot-heading-%d" (min 6 (max 1 level)))) start end)))
       ((equal type "block_quote") (djot--face content 'djot-quote start end t))
       ((member type '("verbatim" "raw_inline")) (djot--face node 'djot-verbatim start end))
       ((equal type "raw_block") (djot--face node 'djot-code start end))
       ((equal type "code_block") (djot--fontify-code node start end))
       ((equal type "math") (djot--face node 'djot-math start end))
       ((equal type "table") (djot--face node 'djot-table start end t))
       ((member type '("inline_attribute" "block_attribute" "raw_inline_attribute"))
        (djot--face node 'djot-attribute start end))
       ((equal type "span")
        (when (and djot-hide-markup content)
          (djot--hide (treesit-node-start node) (treesit-node-start content) start end)
          (djot--hide (treesit-node-end content) (1+ (treesit-node-end content)) start end)))
       ((member type djot--link-types) (djot--fontify-link node start end))
       ((equal type "language") (djot--face node 'djot-language start end))
       ((member type '("link_label" "link_destination" "inline_link_destination"))
        (djot--face node 'djot-destination start end))
       ((string-prefix-p "list_marker" type) (djot--face node 'djot-list-marker start end))
       ((or (string-match-p "marker\\|_begin\\|_end" type)
            (equal type "thematic_break"))
        (djot--face node 'djot-marker start end))))))

(defun djot--fontify-region (beg end &optional loudly)
  "Fontify BEG END, then notify presentation extensions.
LOUDLY is passed through to the Tree-sitter fontifier."
  (let ((result (treesit-font-lock-fontify-region beg end loudly)))
    (run-hook-with-args 'djot-after-fontify-hook beg end)
    result))

(defun djot--unfontify-region (beg end)
  "Remove base fontification in BEG END, preserving foreign overlays."
  (djot--clear-overlays beg end t)
  (font-lock-default-unfontify-region beg end))

(defun djot-refresh (&optional beg end)
  "Refontify BEG END, defaulting to the accessible buffer.
Rebuilds base decorations and runs `djot-after-fontify-hook'.  Preserves
point, source, undo and modified status.  Use after changing options."
  (interactive)
  (save-excursion
    (font-lock-flush beg end)
    (font-lock-ensure beg end)))

(defun djot-toggle-markup ()
  "Toggle concealment of matched emphasis syntax in this buffer."
  (interactive)
  (setq-local djot-hide-markup (not djot-hide-markup))
  (djot-refresh))

(defun djot-show-source (&optional arg)
  "Toggle explicit source display, revealing all concealed syntax.
With positive ARG show source; with zero or negative ARG restore the
configured document display.  Moving point never changes this setting."
  (interactive "P")
  (setq djot-source-visible
        (if arg (> (prefix-numeric-value arg) 0) (not djot-source-visible)))
  (djot-refresh))

(defun djot-edit-at-point ()
  "Reveal source explicitly and move to the current link destination."
  (interactive)
  (let* ((node (djot--link-at-point))
         (destination (and node (djot--field node "destination"))))
    (djot-show-source 1)
    (cond
     (destination (goto-char (1+ (treesit-node-start destination))))
     ((and node (string-match-p "reference_\\(?:link\\|image\\)" (treesit-node-type node)))
      (djot-goto-reference)
      (when-let* ((definition (djot-node-at (point) "link_reference_definition"))
                  (target (djot--field definition "destination")))
        (goto-char (treesit-node-start target))))
     ((and node (equal (treesit-node-type node) "autolink"))
      (goto-char (1+ (treesit-node-start node)))))))

(defun djot--fontify-code (node start end)
  "Natively fontify NODE's code, clipping copied faces to START END."
  (when-let* ((code (djot--field node "code")))
    (djot--face code 'djot-code start end t)
    (let* ((language (djot--text (djot--field node "language")))
           (mode (or (cdr (assoc language djot-language-modes))
                     (intern-soft (concat language "-mode")))))
      (when (and djot-src-fontify-natively mode (fboundp mode)
                 (not (memq mode '(djot-mode text-mode))))
        (let ((target (current-buffer))
              (offset (1- (treesit-node-start code))))
          (with-temp-buffer
            (insert (djot--text code))
            ;; Run the mode, not document locals, hooks or executable forms.
            (let ((enable-local-variables nil) (enable-local-eval nil))
              (delay-mode-hooks (funcall mode)))
            (font-lock-ensure)
            (let ((pos (point-min)))
              (while (< pos (point-max))
                (let* ((next (next-single-property-change pos 'face nil (point-max)))
                       (face (or (get-text-property pos 'face)
                                 (get-text-property pos 'font-lock-face)))
                       (beg (max start (+ offset pos)))
                       (finish (min end (+ offset next))))
                  (when (and face (< beg finish))
                    (with-current-buffer target
                      (treesit-fontify-with-override beg finish face 'prepend)))
                  (setq pos next))))))))))

(defvar djot-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'djot-open-at-point)
    (define-key map [mouse-2] #'djot-open-at-mouse)
    map)
  "Keymap for descriptive link text.")

(defun djot--link-at-point ()
  "Return the link or footnote node surrounding point."
  (let ((node (djot-node-at)))
    (while (and node (not (member (treesit-node-type node) djot--link-types)))
      (setq node (treesit-node-parent node)))
    (and (djot-node-valid-p node) node)))

(defun djot--link-text-bounds (node)
  "Return the visible description bounds for link NODE."
  (let* ((text (or (djot--field node "text") (djot--field node "description")))
         (type (treesit-node-type node)))
    (cond
     (text (cons (+ (treesit-node-start text) (if (string-suffix-p "image" type) 2 1))
                 (1- (treesit-node-end text))))
     ((equal type "autolink") (cons (1+ (treesit-node-start node)) (1- (treesit-node-end node))))
     (t (cons (treesit-node-start node) (treesit-node-end node))))))

(defun djot--fontify-link (node start end)
  "Fontify link NODE and attach interaction in START END."
  (pcase-let* ((`(,beg . ,finish) (djot--link-text-bounds node))
               (destination (djot-link-destination node)))
    ;; Empty descriptions must retain an actionable visible source link.
    (when (= beg finish)
      (setq beg (treesit-node-start node) finish (treesit-node-end node)))
    (djot--face node (if (equal (treesit-node-type node) "footnote_reference")
                         'djot-footnote 'djot-link) start end)
    (when destination
      (djot--overlay (max beg start) (min finish end)
                     'keymap djot-link-map 'mouse-face 'highlight 'follow-link t
                     'help-echo destination 'djot-link destination))
    (when (and djot-descriptive-links destination
               (not (equal (treesit-node-type node) "footnote_reference")))
      (djot--hide (treesit-node-start node) beg start end)
      (djot--hide finish (treesit-node-end node) start end))))

(defun djot--normalize-label (text)
  "Normalize whitespace in reference label TEXT, preserving case."
  (string-trim (replace-regexp-in-string "[ \t\r\n]+" " " text)))

(defun djot--unescape (text)
  "Remove Djot backslash escapes from destination TEXT."
  (replace-regexp-in-string "\\\\\\([[:punct:] ]\\)" "\\1" text))

(defun djot-link-destination (node)
  "Return the destination string of link or image NODE, or nil.
Local heading targets use #ID; footnotes use the internal footnote:LABEL
scheme.  Resolution is case-sensitive and never retrieves resources."
  (let ((type (treesit-node-type node)))
    (cond
     ((equal type "autolink")
      (let ((text (substring (djot--text node) 1 -1)))
        (cond
         ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" text) text)
         ((string-match-p "\\`[^@ <>]+@[^@ <>]+\\'" text) (concat "mailto:" text))
         (t nil))))
     ((equal type "footnote_reference")
      (concat "footnote:" (djot--text (car (cl-remove-if-not
                                          (lambda (n) (equal (treesit-node-type n) "reference_label"))
                                          (djot-node-children node))))))
     ((djot--field node "destination")
      (djot--unescape
       (replace-regexp-in-string "[ \t\r\n]+" ""
                                 (substring (djot--text (djot--field node "destination")) 1 -1))))
     (t
      (let* ((label-node (djot--field node "label"))
             (bounds (djot--link-text-bounds node))
             (label (djot--normalize-label
                     (if label-node (djot--text label-node)
                       (buffer-substring-no-properties (car bounds) (cdr bounds))))))
        (djot--ensure-index)
        (if-let* ((reference (assoc label djot--references)))
            (cdr reference)
          (cdr (assoc label (djot--heading-references)))))))))

(defun djot--reference-destination (label)
  "Find the explicit reference destination for normalized LABEL."
  (djot--ensure-index)
  (cdr (assoc label djot--references)))

(defun djot--plain-text (node)
  "Return NODE's plain inline text for implicit heading targets."
  (let ((type (treesit-node-type node)) (position (treesit-node-start node)) parts)
    (cond
     ((member type '("footnote_reference" "symbol" "inline_attribute" "marker")) "")
     ((equal type "backslash_escape") (substring (djot--text node) 1))
     ((equal type "em_dash") "—")
     ((equal type "en_dash") "–")
     ((equal type "ellipsis") "…")
     ((member type '("verbatim" "math" "raw_inline")) (djot--text (djot--field node "content")))
     ((djot--field node "content") (djot--plain-text (djot--field node "content")))
     ((member type djot--link-types)
      (if-let* ((text (or (djot--field node "text") (djot--field node "description"))))
          (substring (djot--plain-text text)
                     (if (equal (treesit-node-type text) "image_description") 2 1) -1)
        (pcase-let ((`(,beg . ,end) (djot--link-text-bounds node)))
          (buffer-substring-no-properties beg end))))
     (t
      (dolist (child (djot-node-children node))
        (push (buffer-substring-no-properties position (treesit-node-start child)) parts)
        (push (djot--plain-text child) parts)
        (setq position (treesit-node-end child)))
      (push (buffer-substring-no-properties position (treesit-node-end node)) parts)
      (apply #'concat (nreverse parts))))))

(defun djot--reference-supported-p (node)
  "Return non-nil if NODE has a complete reference destination.
Older grammars omit continuation chunks.  Reject a potentially continued
definition rather than opening a silently truncated URL."
  (or djot--multiline-references-p
      (and (djot--field node "destination")
           (save-excursion
             (goto-char (treesit-node-end node))
             (not (looking-at-p "[ \t]+[^ \t\n]"))))))

(defun djot--reference-url (node)
  "Return the complete URL from validated reference definition NODE."
  (let ((destination (djot--field node "destination")))
    (djot--unescape
     (if djot--multiline-references-p
         (mapconcat #'djot--text
                    (cl-remove-if-not
                     (lambda (child) (equal (treesit-node-type child) "link_destination_chunk"))
                     (and destination (djot-node-children destination))) "")
       (djot--text destination)))))

(defun djot--attribute-id (node)
  "Return the first identifier within attribute NODE, if any."
  (when-let* ((identifier (car (treesit-query-capture node '((identifier) @id)))))
    (substring (djot--text (cdr identifier)) 1)))

(defun djot--ensure-index ()
  "Rebuild reference and heading indexes once per source modification.
Only plain strings and positions survive the query; no nodes are cached."
  (unless (eq djot--index-tick (buffer-chars-modified-tick))
    (let ((old-signature (list djot--references (mapcar #'butlast djot--headings)))
          (used (make-hash-table :test #'equal))
          (empty-id 0) attributes headings)
      (setq djot--references nil djot--headings nil djot--anchors nil)
      (save-restriction
        (widen)
        (djot-map-nodes
         (lambda (node)
           (when (djot-node-valid-p node)
             (pcase (treesit-node-type node)
               ("identifier"
                (push (cons (substring (djot--text node) 1) (treesit-node-start node)) djot--anchors))
               ("block_attribute" (push node attributes))
               ("heading" (push node headings))
               ("link_reference_definition"
                (let ((label (djot--normalize-label (djot--text (djot--field node "label")))))
                  ;; The official renderer lets the last definition win.
                  (setf (alist-get label djot--references nil nil #'equal)
                        (and (djot--reference-supported-p node)
                             (djot--reference-url node))))))))
         nil nil '("identifier" "block_attribute" "heading" "link_reference_definition"))
        (dolist (node (nreverse headings))
          (let* ((label (djot--normalize-label (djot--plain-text (djot--field node "content"))))
                 (base (replace-regexp-in-string
                        " " "-" (replace-regexp-in-string "[^[:alnum:]_ -]" "" label)))
                 (position (treesit-node-start node))
                 explicit)
            ;; Block attributes attach immediately, and may be stacked.
            (dolist (attribute attributes)
              (when (= (treesit-node-end attribute) position)
                (setq explicit (or explicit (djot--attribute-id attribute))
                      position (treesit-node-start attribute))))
            (when (string-empty-p base)
              (setq empty-id (1+ empty-id) base (format "s-%d" empty-id)))
            (let ((id (or explicit base)) (number 1))
              (unless explicit
                (while (gethash id used)
                  (setq id (format "%s-%d" base number) number (1+ number))))
              (puthash id t used)
              (push (list label id (treesit-node-start node)) djot--headings))))
        (setq djot--headings (nreverse djot--headings)
              djot--anchors (nreverse djot--anchors)
              djot--index-tick (buffer-chars-modified-tick))
        ;; A definition edit can change a link well outside the edited range.
        ;; Invalidate those links, not all document decorations.
        (unless (equal old-signature
                       (list djot--references (mapcar #'butlast djot--headings)))
          (djot-map-nodes
           (lambda (node)
             (let ((beg (treesit-node-start node)) (end (treesit-node-end node)))
               (djot--clear-overlays beg end t)
               (with-silent-modifications (put-text-property beg end 'fontified nil))))
           nil nil '("full_reference_link" "collapsed_reference_link"
                     "full_reference_image" "collapsed_reference_image")))))))

(defun djot--after-change (_beg _end _old-length)
  "Update an already-used reference index after a source edit.
_BEG, _END and _OLD-LENGTH are the standard change-hook arguments."
  (when djot--index-tick (djot--ensure-index)))

(defun djot--heading-targets ()
  "Return heading entries (LABEL ID POSITION), including unique IDs."
  (djot--ensure-index)
  djot--headings)

(defun djot--heading-references ()
  "Return implicit heading reference names and destinations."
  (mapcar (lambda (entry) (cons (car entry) (concat "#" (cadr entry))))
          (djot--heading-targets)))

(defun djot--goto-anchor (id)
  "Go to local anchor ID, or signal an actionable error."
  (djot--ensure-index)
  (let ((position (or (nth 2 (cl-find id (djot--heading-targets) :key #'cadr :test #'equal))
                      (cdr (assoc id djot--anchors)))))
    (unless position (user-error "No Djot anchor: %s" id))
    (push-mark)
    (goto-char position)
    (djot--reveal-position)))

(defun djot--goto-footnote (label)
  "Visit footnote definition LABEL, remembering the reference location."
  (let (position)
    (djot-map-nodes
     (lambda (node)
       (when (equal label (djot--text (djot--field node "label")))
         (setq position (treesit-node-start node))))
     nil nil '("footnote"))
    (unless position (user-error "No Djot footnote: %s" label))
    (setq djot--footnote-return (point-marker))
    (push-mark)
    (goto-char position)
    (djot--reveal-position)))

(defun djot-footnote-return ()
  "Return to the reference from which a footnote was opened."
  (interactive)
  (unless (and (markerp djot--footnote-return) (marker-buffer djot--footnote-return))
    (user-error "No footnote reference to return to"))
  (goto-char djot--footnote-return)
  (djot--reveal-position))

(defun djot-open-at-point ()
  "Open the link at point, or jump to its local anchor or footnote.
Only explicit activation visits files or invokes the user's browser.
Unsafe executable URL schemes are rejected, never evaluated."
  (interactive)
  (let* ((node (djot--link-at-point))
         (destination (and node (djot-link-destination node))))
    (unless destination
      (user-error "Unresolved Djot link; check label and install the pinned grammar from README"))
    (cond
     ((string-empty-p destination) (push-mark) (goto-char (point-min)))
     ((string-prefix-p "footnote:" destination)
      (djot--goto-footnote (substring destination 9)))
     ((string-prefix-p "#" destination)
      (djot--goto-anchor (url-unhex-string (substring destination 1))))
     ((string-match-p "\\`\\(?:https?\\|mailto\\|ftp\\):" destination)
      (browse-url destination))
     ((and (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" destination)
           (not (string-prefix-p "file:" destination)))
      (user-error "Refusing unsupported Djot URL scheme: %s" destination))
     (t
      (let* ((parts (split-string (string-remove-prefix "file:" destination) "#"))
             (path (url-unhex-string (car parts)))
             (anchor (cadr parts)))
        (when (string-prefix-p "//" path)
          (if (string-prefix-p "///" path) (setq path (substring path 2))
            (user-error "Remote file URLs are not supported")))
        (setq path (expand-file-name path (if buffer-file-name
                                             (file-name-directory buffer-file-name)
                                           default-directory)))
        (when (file-remote-p path) (user-error "Remote file visits are not supported"))
        (find-file path)
        (when (and anchor (derived-mode-p 'djot-mode))
          (djot--goto-anchor (url-unhex-string anchor))))))))

(defun djot-open-at-mouse (event)
  "Open the link at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (djot-open-at-point))

;;; Structured editing
;; Org's cycle/list/table commands and markdown-mode's contextual RET are
;; interaction references.  All structural boundaries here come from Djot.

(defun djot--heading-entries ()
  "Return parsed heading entries (START END LEVEL SECTION-END)."
  (let (entries)
    (djot-map-nodes
     (lambda (node)
       (when (djot-node-valid-p node)
         (let* ((container (treesit-node-parent node))
                (level (length (string-trim (djot--text (djot--field node "marker")))))
                (end (treesit-node-end container)))
           ;; Only top-level headings have section parents.  Within a div,
           ;; quote or list, same/higher-level sibling headings bound the
           ;; subtree; headings inside a nested container do not.
           (unless (equal (treesit-node-type container) "section")
             (let ((sibling (treesit-node-next-sibling node t)))
               (while sibling
                 (if (and (equal (treesit-node-type sibling) "heading")
                          (<= (length (string-trim (djot--text (djot--field sibling "marker")))) level))
                     (setq end (treesit-node-start sibling) sibling nil)
                   (setq sibling (treesit-node-next-sibling sibling t))))))
           (push (list (treesit-node-start node) (treesit-node-end node) level end) entries))))
     nil nil '("heading"))
    (nreverse entries)))

(defun djot--heading-body-start (entry)
  "Return the beginning of the foldable body after heading ENTRY."
  (let ((end (nth 1 entry)))
    (if (eq (char-before end) ?\n) (1- end) end)))

(defun djot--current-heading ()
  "Return the heading entry enclosing point, or signal a user error."
  (or (car (last (cl-remove-if-not
                 (lambda (entry) (and (<= (car entry) (point))
                                      (< (point) (nth 3 entry))))
                 (djot--heading-entries))))
      (user-error "No Djot heading here")))

(defun djot--folds (beg end)
  "Return only Djot fold overlays intersecting BEG END."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'djot-fold))
                    (overlays-in beg end)))

(defun djot--reveal-position ()
  "Reveal the fold at point after an explicit navigation command."
  (dolist (ov (djot--folds (point) (point))) (delete-overlay ov)))

(defun djot--fold (beg end)
  "Fold BEG through END without touching semantic display overlays."
  (when (< beg end)
    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'djot-fold t)
      (overlay-put ov 'invisible 'djot-fold)
      (overlay-put ov 'isearch-open-invisible #'delete-overlay)
      (overlay-put ov 'evaporate t))))

(defun djot--guard-folded-edit (beg end)
  "Reject edits to folded source between BEG and END, like Org."
  (unless undo-in-progress
    (when (djot--folds beg end)
      (user-error "Folded Djot source: unfold with TAB or C-c C-a before editing"))))

(defun djot-show-all ()
  "Show every Djot heading body without changing markup concealment."
  (interactive)
  (dolist (ov (djot--folds (point-min) (point-max))) (delete-overlay ov)))

(defun djot-cycle ()
  "Cycle this heading through folded, children and fully open states."
  (interactive)
  (pcase-let* ((`(,beg ,head-end ,level ,end) (djot--current-heading))
               (body (djot--heading-body-start (list beg head-end)))
               (folds (djot--folds body end))
               (closed (cl-find-if (lambda (ov) (and (= (overlay-start ov) body)
                                                     (= (overlay-end ov) end))) folds)))
    (cond
     (closed
      (mapc #'delete-overlay folds)
      (let ((children nil) (child-end body))
        (dolist (entry (djot--heading-entries))
          (when (and (> (car entry) beg) (< (car entry) end)
                     (> (nth 2 entry) level) (>= (car entry) child-end))
            (push entry children)
            (setq child-end (nth 3 entry))))
        (setq children (nreverse children))
        (when children
          (djot--fold body (caar children))
          (dolist (entry children)
            (djot--fold (djot--heading-body-start entry) (nth 3 entry))))))
     (folds (mapc #'delete-overlay folds))
     (t (djot--fold body end)))))

(defun djot-cycle-global ()
  "Toggle between a heading overview and the fully visible document."
  (interactive)
  (if (djot--folds (point-min) (point-max)) (djot-show-all)
    (let ((entries (djot--heading-entries)))
      (while entries
        (djot--fold (djot--heading-body-start (car entries))
                    (or (caadr entries) (point-max)))
        (setq entries (cdr entries))))))

(defun djot-next-heading (&optional backward)
  "Move to the next parsed heading; with BACKWARD, move to the previous."
  (interactive)
  (let* ((entries (djot--heading-entries))
         (entry (cl-find-if (lambda (item) (if backward (< (car item) (point))
                                            (> (car item) (point))))
                            (if backward (reverse entries) entries))))
    (unless entry (user-error "No %s Djot heading" (if backward "previous" "next")))
    (goto-char (car entry))
    (djot--reveal-position)))

(defun djot-previous-heading ()
  "Move to the previous parsed heading, ignoring literal code."
  (interactive)
  (djot-next-heading t))

(defun djot-up-heading ()
  "Move to the parent of the current parsed heading."
  (interactive)
  (let* ((current (djot--current-heading))
         (parent (cl-find-if (lambda (entry) (and (< (car entry) (car current))
                                                 (< (nth 2 entry) (nth 2 current))
                                                 (> (nth 3 entry) (car current))))
                             (reverse (djot--heading-entries)))))
    (unless parent (user-error "No parent heading"))
    (goto-char (car parent))
    (djot--reveal-position)))

(defun djot--change-heading-level (delta)
  "Change the current heading and its descendants by DELTA levels."
  (let* ((current (djot--current-heading))
         (entries (cl-remove-if-not
                   (lambda (entry) (and (>= (car entry) (car current))
                                        (< (car entry) (nth 3 current))))
                   (djot--heading-entries))) changes)
    (when (< (+ (nth 2 current) delta) 1) (user-error "Already a top-level heading"))
    (dolist (entry entries)
      (let ((node (djot-node-at (car entry) "heading")))
        (dolist (capture (treesit-query-capture node '((marker) @marker)))
          (let ((marker (cdr capture)))
            (push (cons (treesit-node-start marker) (treesit-node-end marker)) changes)))))
    (atomic-change-group
      (save-excursion
        (dolist (range (sort changes (lambda (a b) (> (car a) (car b)))))
          (goto-char (car range))
          (if (> delta 0) (insert "#") (delete-char 1)))))))

(defun djot-promote-heading ()
  "Promote this heading and all its descendants by one level."
  (interactive)
  (djot--change-heading-level -1))

(defun djot-demote-heading ()
  "Demote this heading and all its descendants by one level."
  (interactive)
  (djot--change-heading-level 1))

(defun djot--item-at-point ()
  "Return a validated list item at point, guarding old scanner defects."
  (let* ((node (djot-node-at (if (eobp) (max (point-min) (1- (point))) (point)) "list_item"))
         (marker (and node (djot--field node "marker")))
         (text (and marker (string-trim (djot--text marker)))))
    (when (and (djot-node-valid-p node) marker
               ;; Some grammars accept arbitrary words as Roman markers.
               (or (not (string-match-p "roman" (treesit-node-type marker)))
                   (string-match-p "\\`(?[ivxlcdmIVXLCDM]+[.)]\\'" text)))
      ;; The grammar can misclassify lazy paragraph continuation as a
      ;; nested list.  Only a blank-separated nested block is editable.
      (let ((candidate node) ancestor)
        (while candidate
          (let* ((list-node (treesit-node-parent candidate))
                 (container (treesit-node-parent list-node))
                 (first (car (djot-node-children list-node)))
                 (nested (and container
                              (equal (treesit-node-type container) "list_item_content"))))
            (setq ancestor (and nested (treesit-node-parent container)))
            (when (and ancestor first
                       (save-excursion
                         (goto-char (treesit-node-start (djot--field first "marker")))
                         (forward-line -1)
                         (not (looking-at-p "[ \t>]*$"))))
              (setq node ancestor))
            (setq candidate ancestor))))
      node)))

(defun djot--item-prefix (item)
  "Return source before ITEM's marker, including quote and indent prefixes."
  (save-excursion
    (goto-char (treesit-node-start (djot--field item "marker")))
    (buffer-substring-no-properties (line-beginning-position) (point))))

(defun djot--roman (number)
  "Return the canonical lowercase Roman representation of NUMBER."
  (let ((result ""))
    (dolist (pair '((1000 . "m") (900 . "cm") (500 . "d") (400 . "cd")
                    (100 . "c") (90 . "xc") (50 . "l") (40 . "xl")
                    (10 . "x") (9 . "ix") (5 . "v") (4 . "iv") (1 . "i")))
      (while (>= number (car pair))
        (setq result (concat result (cdr pair)) number (- number (car pair)))))
    result))

(defun djot--next-list-marker (marker)
  "Return MARKER's successor, preserving its parsed style."
  (let* ((type (treesit-node-type marker))
         (text (string-trim-right (djot--text marker)))
         (token (replace-regexp-in-string "[().]" "" text))
         (case-fold-search nil) value)
    (cond
     ((equal type "list_marker_task") (concat (substring text 0 1) " [ ]"))
     ((string-match-p "decimal" type)
      (replace-regexp-in-string "[0-9]+" (number-to-string (1+ (string-to-number token))) text))
     ((string-match-p "roman" type)
      (let ((previous 0) (number 0))
        (dolist (char (reverse (string-to-list (downcase token))))
          (setq value (cdr (assq char '((?i . 1) (?v . 5) (?x . 10) (?l . 50)
                                       (?c . 100) (?d . 500) (?m . 1000)))))
          (setq number (+ number (if (< value previous) (- value) value))
                previous value))
        (setq value (djot--roman (1+ number)))
        (when (string-match-p "upper" type) (setq value (upcase value)))
        (replace-regexp-in-string "[[:alpha:]]+" value text t t)))
     ((string-match-p "alpha" type)
      (setq value (string (if (memq (aref token 0) '(?z ?Z))
                             (- (aref token 0) 25) (1+ (aref token 0)))))
      (replace-regexp-in-string "[[:alpha:]]" value text t t))
     (t text))))

(defun djot-insert-item ()
  "Insert a sibling item, continuing numbering and resetting checkboxes.
Split at point within ordinary item text.  From a definition's body or a
nested block, insert after the whole item.  On an empty item, end the list."
  (interactive)
  (let* ((item (or (djot--item-at-point) (user-error "Not in a complete Djot list item")))
         (marker (djot--field item "marker"))
         (start (treesit-node-start marker)) (marker-end (treesit-node-end marker))
         (end (treesit-node-end item))
         (prefix (djot--item-prefix item))
         (next (djot--next-list-marker marker))
         (definition (djot--field item "term"))
         (empty (string-empty-p (string-trim (buffer-substring-no-properties marker-end end)))))
    (atomic-change-group
      (cond
       (empty
        (delete-region start marker-end)
        (goto-char start))
       ((or definition (< (point) marker-end)
            (djot-node-at (point) "code_block") (djot-node-at (point) "raw_block")
            (djot-node-at (point) "table"))
        (goto-char end)
        (unless (bolp) (insert "\n"))
        (when definition (insert prefix "\n"))
        (insert prefix next " "))
       (t
        (delete-horizontal-space)
        (insert "\n" prefix next " "))))))

(defun djot--empty-item-marker ()
  "Return a parsed marker on an otherwise empty item line, even at EOF."
  (let ((node (djot-node-at (max (line-beginning-position) (1- (point))))))
    (while (and node (not (string-prefix-p "list_marker_" (treesit-node-type node))))
      (setq node (treesit-node-parent node)))
    (when (and node (<= (treesit-node-end node) (line-end-position))
               (string-empty-p (string-trim (buffer-substring-no-properties
                                             (treesit-node-end node) (line-end-position)))))
      node)))

(defun djot-newline ()
  "Continue lists or tables at point; otherwise insert an ordinary newline.
An empty list item ends the list.  In a definition term, start its body."
  (interactive)
  (let ((item (djot--item-at-point))
        (empty-marker (djot--empty-item-marker)))
    (cond
     (empty-marker
      (delete-region (treesit-node-start empty-marker) (line-end-position)))
     ((djot-node-at (point) "table") (djot-table-insert-row))
     ((and item (djot--field item "term")
           (<= (point) (treesit-node-end (djot--field item "term"))))
      (let ((prefix (djot--item-prefix item)))
        (end-of-line)
        (insert "\n" prefix "\n" prefix "  ")))
     ((and item (djot--field item "term"))
      (insert "\n" (djot--item-prefix item) "  "))
     ((and item (not (or (djot-node-at (point) "code_block")
                         (djot-node-at (point) "raw_block"))))
      (djot-insert-item))
     (t (newline)))))

(defun djot-toggle-checkbox (&optional remove)
  "Toggle the current task checkbox, or add one to a bullet item.
With prefix REMOVE, remove the checkbox while preserving the bullet."
  (interactive "P")
  (let* ((item (or (djot--item-at-point) (user-error "Not in a complete Djot list item")))
         (marker (djot--field item "marker"))
         (type (treesit-node-type marker))
         (start (treesit-node-start marker))
         (check (djot--field marker "checkmark"))
         (checked (and check (equal (treesit-node-type check) "checked"))))
    (unless (member type '("list_marker_task" "list_marker_dash" "list_marker_plus" "list_marker_star"))
      (user-error "Djot checkboxes require a bullet list"))
    (atomic-change-group
      (save-excursion
        (goto-char start)
        (cond
         ((and check remove) (delete-region (+ start 2) (treesit-node-end marker)))
         (check
          (search-forward "[" (treesit-node-end marker))
          (delete-char 1)
          (insert (if checked " " "X")))
         ((not remove) (forward-char 2) (insert "[ ] ")))))))

(defun djot--shift-item (outward)
  "Move the whole item one level OUTWARD, or under its previous sibling."
  (let* ((item (or (djot--item-at-point) (user-error "Not in a complete Djot list item")))
         (list-node (treesit-node-parent item))
         (parent (treesit-node-parent list-node))
         (previous (treesit-node-prev-sibling item t))
         (start (save-excursion (goto-char (treesit-node-start (djot--field item "marker")))
                                (line-beginning-position)))
         (end (copy-marker (treesit-node-end item)))
         (origin (point-marker))
         (prefix (djot--item-prefix item))
         (indent (length (if (string-match "[ \t]*\\'" prefix) (match-string 0 prefix) "")))
         delta)
    (if outward
        (progn
          (while (and parent (not (equal (treesit-node-type parent) "list_item")))
            (setq parent (treesit-node-parent parent)))
          (unless parent (user-error "Already a top-level item"))
          (setq delta (- (length (djot--item-prefix parent)) (length prefix))))
      (unless (and previous (equal (treesit-node-type previous) "list_item"))
        (user-error "No preceding item to nest under"))
      (setq delta 2))
    (atomic-change-group
      (save-excursion
        (goto-char start)
        ;; A blank line is required before a nested Djot block.
        (when (and (not outward) (> start (point-min))
                   (not (save-excursion (forward-line -1) (looking-at-p "[ \t>]*$"))))
          (insert (replace-regexp-in-string "[ \t]+\\'" "" prefix) "\n"))
        (while (< (point) end)
          ;; Keep quote container prefixes unchanged.
          (when (looking-at "\\(?:[ \t]*> ?\\)*") (goto-char (match-end 0)))
          (if (> delta 0) (unless (looking-at-p "[ \t]*$") (insert (make-string delta ?\s)))
            (let ((available (skip-chars-forward " \t" (line-end-position))))
              (delete-region (- (point) available)
                             (+ (- (point) available) (min available (- delta) indent)))))
          (forward-line 1))))
    (goto-char origin)
    (set-marker origin nil)
    (set-marker end nil)))

(defun djot-indent-item ()
  "Indent this item and its children beneath its previous sibling."
  (interactive)
  (djot--shift-item nil))

(defun djot-outdent-item ()
  "Outdent this item and its children one parsed list level."
  (interactive)
  (djot--shift-item t))

(defun djot-indent-line ()
  "Indent a continuation line relative to its containing Djot item.
Leave non-list indentation alone.  Use M-<right> to nest an entire item."
  (interactive)
  (when-let* ((item (djot--item-at-point))
              (marker (djot--field item "marker")))
    (unless (= (line-beginning-position)
               (save-excursion (goto-char (treesit-node-start marker)) (line-beginning-position)))
      (unless (string-match-p ">" (djot--item-prefix item))
        (indent-line-to (+ 2 (length (djot--item-prefix item))))))))

(defun djot-cycle-list-marker ()
  "Cycle the current list's bullets through -, +, *, 1., 1), and (1).
Change siblings together, leaving nested lists and checkboxes intact."
  (interactive)
  (let* ((item (or (djot--item-at-point) (user-error "Not in a complete Djot list item")))
         (items (djot-node-children (treesit-node-parent item)))
         (text (string-trim (djot--text (djot--field item "marker"))))
         (styles '("-" "+" "*" "1." "1)" "(1)"))
         (style (or (cadr (member (replace-regexp-in-string "[0-9]+" "1" text) styles)) "-"))
         (number 0) changes)
    (when (djot--field item "term") (user-error "Definition lists use the : marker"))
    (dolist (sibling items)
      (when (equal (treesit-node-type sibling) "list_item")
        (let* ((marker (djot--field sibling "marker"))
               (task (djot--field marker "checkmark")))
          (when task (user-error "Remove task checkboxes before cycling the list style"))
          (push (list (treesit-node-start marker) (treesit-node-end marker)
                      (concat (replace-regexp-in-string "1" (number-to-string (cl-incf number)) style) " ")) changes))))
    (atomic-change-group
      (save-excursion
        (dolist (change changes)
          (goto-char (car change))
          (delete-region (car change) (cadr change))
          (insert (nth 2 change)))))))

(defun djot--cell-width (beg end)
  "Measure BEG END using base concealment only, never modern padding."
  (let (visible)
    (while (< beg end)
      (unless (cl-some (lambda (ov) (and (overlay-get ov 'djot-mode)
                                         (eq (overlay-get ov 'invisible) 'djot-markup)
                                         (invisible-p beg)))
                       (overlays-at beg))
        (push (char-after beg) visible))
      (cl-incf beg))
    (string-width (apply #'string (nreverse visible)))))

(defun djot--table-rows ()
  "Return editable rows of the parsed table at point.
Each row is (START END PREFIX CELLS SEPARATOR BOUNDS WIDTHS).  Cell ranges,
including zero-width empty cells, come directly from the pinned grammar."
  (let ((table (djot-node-at (point) "table")) rows)
    (unless (djot-node-valid-p table) (user-error "Not in a complete Djot table"))
    (font-lock-ensure (treesit-node-start table) (treesit-node-end table))
    (dolist (node (djot-node-children table))
      (when (member (treesit-node-type node) '("table_header" "table_row" "table_separator"))
        (let* ((first-cell (car (djot-node-children node)))
               (start (if first-cell (1- (treesit-node-start first-cell))
                        (treesit-node-start node)))
               (end (save-excursion (goto-char start) (line-end-position)))
               (prefix (save-excursion (goto-char start)
                                       (buffer-substring-no-properties (line-beginning-position) start)))
               cells bounds widths)
          (dolist (cell (djot-node-children node))
            (let* ((beg (treesit-node-start cell)) (finish (treesit-node-end cell))
                   (text (buffer-substring-no-properties beg finish))
                   (trimmed (string-trim text)))
              (push (cons beg finish) bounds)
              (push trimmed cells)
              (push (djot--cell-width
                     (+ beg (- (length text) (length (string-trim-left text))))
                     (- finish (- (length text) (length (string-trim-right text))))) widths)))
          (unless cells (user-error "No complete table cells"))
          (push (list start end prefix (nreverse cells)
                      (equal (treesit-node-type node) "table_separator")
                      (nreverse bounds) (nreverse widths)) rows))))
    (or (nreverse rows) (user-error "No table rows"))))

(defun djot--table-location (rows)
  "Return (ROW COLUMN OFFSET) for point in ROWS."
  (let* ((row-index (or (cl-position-if (lambda (row) (and (<= (car row) (point))
                                                          (<= (point) (cadr row)))) rows) 0))
         (row (nth row-index rows))
         (bounds (nth 5 row))
         (column (or (cl-position-if (lambda (range) (<= (point) (cdr range))) bounds)
                     (1- (length bounds))))
         (range (nth column bounds))
         (content (save-excursion (goto-char (car range))
                                 (skip-chars-forward " \t" (cdr range)) (point))))
    (list row-index column (- (point) content))))

(defun djot--table-goto (row column &optional offset)
  "Go to COLUMN of table ROW, optionally OFFSET characters into the cell."
  (let* ((rows (djot--table-rows))
         (entry (nth (min row (1- (length rows))) rows))
         (bounds (nth 5 entry))
         (range (nth (min column (1- (length bounds))) bounds)))
    (goto-char (car range))
    (skip-chars-forward " \t" (cdr range))
    (when offset
      (goto-char (max (1- (car range)) (min (cdr range) (+ (point) offset)))))))

(defun djot--table-write (rows)
  "Replace ROWS' source with aligned cells, preserving captions and prefixes."
  (let* ((count (apply #'max (mapcar (lambda (row) (length (nth 3 row))) rows)))
         (widths (make-vector count 3))
         (alignments (make-vector count 'left)) changes)
    (dolist (row rows)
      (unless (nth 4 row)
        (cl-loop for width in (nth 6 row) for i from 0 do
                 (aset widths i (max (aref widths i) width)))))
    (dolist (row rows)
      (let (cells)
        (dotimes (i count)
          (let* ((cell (or (nth i (nth 3 row)) (if (nth 4 row) "---" "")))
                 (width (aref widths i)))
            (if (nth 4 row)
                (let ((left (string-prefix-p ":" cell)) (right (string-suffix-p ":" cell)))
                  (aset alignments i (cond ((and left right) 'center) (right 'right) (t 'left)))
                  (push (concat (if left ":" "-") (make-string width ?-) (if right ":" "-")) cells))
              (let* ((padding (- width (or (nth i (nth 6 row)) 0)))
                     (left (pcase (aref alignments i) ('right padding) ('center (/ padding 2)) (_ 0))))
                (push (concat " " (make-string left ?\s) cell (make-string (- padding left) ?\s) " ") cells)))))
        (push (list (car row) (cadr row) (concat "|" (mapconcat #'identity (nreverse cells) "|") "|")) changes)))
    (dolist (change changes)
      (goto-char (car change))
      (unless (equal (buffer-substring-no-properties (car change) (cadr change)) (nth 2 change))
        (delete-region (car change) (cadr change))
        (insert (nth 2 change))))))

(defun djot-table-align ()
  "Align the parsed table, preserving escaped/verbatim pipes and alignment.
Widths use current base concealment, ignoring modern display padding.
After changing source display, realign explicitly if desired."
  (interactive)
  (let* ((rows (djot--table-rows)) (location (djot--table-location rows))
         (origin (copy-marker (caar rows))))
    (atomic-change-group
      (djot--table-write rows)
      (goto-char origin)
      (apply #'djot--table-goto location))
    (set-marker origin nil)))

(defun djot-table-next-cell (&optional backward)
  "Align and move to the next table cell, or previous with BACKWARD.
Skip separator rows.  Moving past the last data row appends an empty row."
  (interactive)
  (djot-table-align)
  (let* ((rows (djot--table-rows))
         (location (djot--table-location rows))
         (row (car location)) (column (+ (cadr location) (if backward -1 1))))
    (when (or (< column 0) (>= column (length (nth 3 (nth row rows)))))
      (setq row (+ row (if backward -1 1)))
      (while (and (>= row 0) (< row (length rows)) (nth 4 (nth row rows)))
        (setq row (+ row (if backward -1 1))))
      (setq column (if backward (1- (length (nth 3 (nth row rows)))) 0)))
    (cond
     ((< row 0) (user-error "First table cell"))
     ((>= row (length rows))
      (goto-char (cadr (car (last rows))))
      (djot-table-insert-row)
      (djot--table-goto row 0))
     (t (djot--table-goto row column)))))

(defun djot-table-previous-cell ()
  "Align and move to the previous table cell, skipping separators."
  (interactive)
  (djot-table-next-cell t))

(defun djot-table-insert-row (&optional above separator)
  "Insert an empty row below point, or ABOVE with a prefix argument.
If SEPARATOR is non-nil, insert a Djot header separator instead."
  (interactive "P")
  (let* ((rows (djot--table-rows))
         (location (djot--table-location rows))
         (row (nth (car location) rows))
         (prefix (nth 2 row))
         (cells (make-list (length (nth 3 row)) (if separator "---" " "))))
    (atomic-change-group
      (goto-char (if above (car row) (cadr row)))
      (if above
          (progn (insert "|" (mapconcat #'identity cells "|") "|\n" prefix)
                 (goto-char (car row)))
        (insert "\n" prefix "|" (mapconcat #'identity cells "|") "|")
        (beginning-of-line) (forward-char (length prefix)))
      (djot-table-align)
      (djot--table-goto (+ (car location) (if above 0 1)) (cadr location)))))

(defun djot-table-insert-separator ()
  "Insert a header separator below the current table row."
  (interactive)
  (djot-table-insert-row nil t))

(defun djot-table-delete-row ()
  "Delete the current table row, including its container prefix."
  (interactive)
  (let* ((rows (djot--table-rows)) (location (djot--table-location rows))
         (row (nth (car location) rows)))
    (atomic-change-group
      (goto-char (car row))
      (delete-region (line-beginning-position) (min (point-max) (1+ (cadr row)))))))

(defun djot--table-change-column (delete after)
  "Insert a column, or DELETE it; insert AFTER point if requested."
  (let* ((rows (djot--table-rows)) (location (djot--table-location rows))
         (column (+ (cadr location) (if (and after (not delete)) 1 0)))
         (count (apply #'max (mapcar (lambda (row) (length (nth 3 row))) rows)))
         (origin (copy-marker (caar rows))))
    (when (and delete (= count 1)) (user-error "Cannot delete the only table column"))
    (dolist (row rows)
      (let ((widths (append (nth 6 row) (make-list (- count (length (nth 6 row))) 0))))
        (setf (nth 6 row) (append (cl-subseq widths 0 column)
                                  (unless delete (list 0))
                                  (nthcdr (+ column (if delete 1 0)) widths))))
      (let ((cells (append (nth 3 row) (make-list (- count (length (nth 3 row))) ""))))
        (setf (nth 3 row) (append (cl-subseq cells 0 column)
                                  (unless delete (list (if (nth 4 row) "---" "")))
                                  (nthcdr (+ column (if delete 1 0)) cells)))))
    (atomic-change-group
      (djot--table-write rows)
      (goto-char origin)
      (djot--table-goto (car location) column))
    (set-marker origin nil)))

(defun djot-table-insert-column (&optional after)
  "Insert an empty column before point, or AFTER with a prefix argument."
  (interactive "P")
  (djot--table-change-column nil after))

(defun djot-table-delete-column ()
  "Delete the current column from all rows of the parsed table."
  (interactive)
  (djot--table-change-column t nil))

(defun djot-tab ()
  "Align/advance in tables, cycle a heading, or indent the current line."
  (interactive)
  (cond
   ((djot-node-at (point) "table") (djot-table-next-cell))
   ((djot-node-at (point) "heading") (djot-cycle))
   (t (indent-for-tab-command))))

(defun djot-backtab ()
  "Move backward in tables; elsewhere toggle the heading overview."
  (interactive)
  (if (djot-node-at (point) "table") (djot-table-previous-cell)
    (djot-cycle-global)))

(defun djot--escape-link-text (text)
  "Escape delimiter characters in plain link description TEXT."
  (replace-regexp-in-string "[][\\\\]" (lambda (char) (concat "\\" char)) text t t))

(defun djot--encode-destination (destination)
  "Encode whitespace and delimiters in a literal DESTINATION string."
  (replace-regexp-in-string
   "[ \t\r\n()\\\\]"
   (lambda (char) (pcase char (" " "%20") ("\t" "%09") ("\n" "%0A")
                         ("\r" "%0D") (_ (concat "\\" char)))) destination t t))

(defun djot-insert-link (destination description)
  "Insert or replace an inline link with DESTINATION and DESCRIPTION.
Interactively use the active region as the default description.  Existing
image links remain images.  This explicit edit reveals the resulting source."
  (interactive
   (let* ((node (djot--link-at-point))
          (bounds (and node (djot--link-text-bounds node)))
          (text (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                      (bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))
                      (t ""))))
     (list (read-string "Destination: " (and node (djot-link-destination node)))
           (read-string "Description: " text))))
  (let* ((node (djot--link-at-point))
         (image (and node (string-suffix-p "image" (treesit-node-type node))))
         (beg (cond ((use-region-p) (region-beginning)) (node (treesit-node-start node)) (t (point))))
         (end (cond ((use-region-p) (region-end)) (node (treesit-node-end node)) (t (point)))))
    (atomic-change-group
      (delete-region beg end)
      (goto-char beg)
      (insert (if image "![" "[") (djot--escape-link-text description)
              "](" (djot--encode-destination destination) ")"))
    (djot-show-source 1)))

(defun djot--definition (label type)
  "Return the last parsed definition of LABEL with node TYPE."
  (let (result)
    (djot-map-nodes
     (lambda (node)
       (when (and (djot-node-valid-p node)
                  (equal label (djot--normalize-label (djot--text (djot--field node "label")))))
         (setq result node))) nil nil (list type))
    result))

(defun djot--check-label (label)
  "Reject LABEL if it cannot be a nonempty single-line reference label."
  (when (or (string-empty-p (string-trim label)) (string-match-p "[][\n\r]" label))
    (user-error "A label must be nonempty and contain no brackets or newlines")))

(defun djot-insert-reference (label destination)
  "Create or update reference LABEL with literal DESTINATION.
Use the last matching definition, as does the renderer.  New definitions
are appended as separate blocks; existing link descriptions stay unchanged."
  (interactive (list (read-string "Reference label: ") (read-string "Destination: ")))
  (djot--check-label label)
  (let* ((node (djot--definition (djot--normalize-label label) "link_reference_definition"))
         (beg (and node (treesit-node-start node)))
         (end (and node (treesit-node-end node))))
    (atomic-change-group
      (save-excursion
        (if node (progn (goto-char beg) (delete-region beg end))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (unless (or (= (point) (point-min))
                      (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
            (insert "\n")))
        (insert "[" label "]: " (djot--encode-destination destination) "\n")))))

(defun djot-insert-reference-link (label description)
  "Insert a reference link using LABEL and plain DESCRIPTION."
  (interactive
   (progn
     (djot--ensure-index)
     (list (completing-read "Reference: " (append djot--references (djot--heading-references)))
           (read-string "Description: " (when (use-region-p)
                                          (buffer-substring-no-properties (region-beginning) (region-end)))))))
  (djot--check-label label)
  (atomic-change-group
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert "[" (djot--escape-link-text description) "][" label "]")))

(defun djot-goto-reference ()
  "Visit the definition of the reference link at point, without opening it."
  (interactive)
  (let* ((node (or (djot--link-at-point) (user-error "No link here")))
         (label-node (djot--field node "label"))
         (bounds (djot--link-text-bounds node))
         (label (djot--normalize-label (if label-node (djot--text label-node)
                                        (buffer-substring-no-properties (car bounds) (cdr bounds)))))
         (definition (djot--definition label "link_reference_definition")))
    (unless definition (user-error "No explicit reference definition for %s" label))
    (push-mark)
    (goto-char (treesit-node-start definition))
    (djot--reveal-position)
    (djot-show-source 1)))

(defun djot-insert-footnote (&optional label)
  "Insert a footnote reference and visit its definition.
Reuse LABEL when supplied; otherwise allocate an unused numeric label.
On an existing reference, visit it instead.
Use `djot-footnote-return' to return to the source."
  (interactive (list (when current-prefix-arg (read-string "Footnote label: "))))
  (let ((node (djot--link-at-point)))
    (if (and node (equal (treesit-node-type node) "footnote_reference"))
        (djot-open-at-point)
      (unless label
        (let ((number 1) labels)
          (djot-map-nodes (lambda (n) (push (djot--text n) labels)) nil nil '("reference_label"))
          (while (member (number-to-string number) labels) (cl-incf number))
          (setq label (number-to-string number))))
      (djot--check-label label)
      (let* ((definition (djot--definition label "footnote"))
             (target (and definition (copy-marker (treesit-node-start definition) t)))
             (origin (point-marker)))
        (atomic-change-group
          (insert "[^" label "]")
          (if target (goto-char target)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n[^" label "]: ")))
        (setq djot--footnote-return origin)
        (djot--reveal-position)
        (when target (set-marker target nil))))))

(defun djot--outline-level ()
  "Return the current heading's outline level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "#")
    (- (point) (line-beginning-position))))

(defun djot--imenu-index ()
  "Return a flat index of parsed headings."
  (mapcar (lambda (entry) (cons (car entry) (nth 2 entry))) (djot--heading-targets)))

(defun djot--teardown ()
  "Remove only this mode's overlays and parser."
  (save-restriction
    (widen)
    (djot--clear-overlays (point-min) (point-max))
    (djot-show-all))
  (when djot-parser (treesit-parser-delete djot-parser) (setq djot-parser nil))
  (remove-from-invisibility-spec 'djot-markup)
  (remove-from-invisibility-spec '(djot-fold . t)))

(defvar djot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'djot-open-at-point)
    (define-key map (kbd "C-c C-s") #'djot-show-source)
    (define-key map (kbd "C-c C-m") #'djot-toggle-markup)
    (define-key map (kbd "C-c '") #'djot-edit-at-point)
    (define-key map (kbd "C-c C-b") #'djot-footnote-return)
    (define-key map (kbd "TAB") #'djot-tab)
    (define-key map (kbd "<backtab>") #'djot-backtab)
    (define-key map (kbd "RET") #'djot-newline)
    (define-key map (kbd "M-RET") #'djot-insert-item)
    (define-key map (kbd "M-<right>") #'djot-indent-item)
    (define-key map (kbd "M-<left>") #'djot-outdent-item)
    (define-key map (kbd "C-c C-c") #'djot-toggle-checkbox)
    (define-key map (kbd "C-c -") #'djot-cycle-list-marker)
    (define-key map (kbd "C-c C-n") #'djot-next-heading)
    (define-key map (kbd "C-c C-p") #'djot-previous-heading)
    (define-key map (kbd "C-c C-u") #'djot-up-heading)
    (define-key map (kbd "C-c C-a") #'djot-show-all)
    (define-key map (kbd "C-c <left>") #'djot-promote-heading)
    (define-key map (kbd "C-c <right>") #'djot-demote-heading)
    (define-key map (kbd "C-c C-l") #'djot-insert-link)
    (define-key map (kbd "C-c C-x r") #'djot-goto-reference)
    (define-key map (kbd "C-c C-x l") #'djot-insert-reference-link)
    (define-key map (kbd "C-c C-x d") #'djot-insert-reference)
    (define-key map (kbd "C-c C-f") #'djot-insert-footnote)
    (define-key map (kbd "C-c |") #'djot-table-align)
    (define-key map (kbd "C-c C-t i") #'djot-table-insert-row)
    (define-key map (kbd "C-c C-t d") #'djot-table-delete-row)
    (define-key map (kbd "C-c C-t c") #'djot-table-insert-column)
    (define-key map (kbd "C-c C-t k") #'djot-table-delete-column)
    (define-key map (kbd "C-c C-t -") #'djot-table-insert-separator)
    map)
  "Keymap for `djot-mode'.")

;;;###autoload
(define-derived-mode djot-mode text-mode "Djot"
  "Major mode for Djot using one Tree-sitter parser.
Install the pinned grammar specified in README.org with
`treesit-install-language-grammar' before activating this mode."
  (unless (treesit-ready-p 'djot t)
    (user-error "Install the Djot grammar: see djot-mode README, then M-x treesit-install-language-grammar RET djot"))
  (setq djot-parser (treesit-parser-create 'djot))
  (setq-local treesit-primary-parser djot-parser)
  (setq djot--multiline-references-p
        (condition-case nil
            (progn (treesit-query-compile 'djot '((link_destination_chunk) @chunk) t) t)
          (treesit-query-error nil)))
  (setq-local treesit-font-lock-settings
              (treesit-font-lock-rules :language 'djot :feature 'djot
                                       :override 'prepend '((_) @djot--fontify-node)))
  (setq-local treesit-font-lock-feature-list '((djot)))
  (treesit-major-mode-setup)
  (setq-local font-lock-defaults
              '(nil nil nil nil
                (font-lock-fontify-syntactically-function . djot--fontify-region)))
  (setq-local font-lock-unfontify-region-function #'djot--unfontify-region)
  (setq-local outline-regexp "#+[ \t]")
  (setq-local outline-level #'djot--outline-level)
  (setq-local imenu-create-index-function #'djot--imenu-index)
  (setq-local comment-start "{% ")
  (setq-local comment-end " %}")
  (add-to-invisibility-spec 'djot-markup)
  (add-to-invisibility-spec '(djot-fold . t))
  (setq-local indent-line-function #'djot-indent-line)
  (add-hook 'before-change-functions #'djot--guard-folded-edit nil t)
  (add-hook 'after-change-functions #'djot--after-change nil t)
  (add-hook 'change-major-mode-hook #'djot--teardown nil t)
  (add-hook 'kill-buffer-hook #'djot--teardown nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.djot\\'" . djot-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dj\\'" . djot-mode))

(provide 'djot-mode)
;;; djot-mode.el ends here
