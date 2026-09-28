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
    (when destination (goto-char (1+ (treesit-node-start destination))))))

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
    (when (invisible-p (point)) (outline-show-entry))))

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
    (goto-char position)))

(defun djot-footnote-return ()
  "Return to the reference from which a footnote was opened."
  (interactive)
  (unless (and (markerp djot--footnote-return) (marker-buffer djot--footnote-return))
    (user-error "No footnote reference to return to"))
  (goto-char djot--footnote-return))

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
    (djot--clear-overlays (point-min) (point-max)))
  (when djot-parser (treesit-parser-delete djot-parser) (setq djot-parser nil))
  (remove-from-invisibility-spec 'djot-markup))

(defvar djot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'djot-open-at-point)
    (define-key map (kbd "C-c C-s") #'djot-show-source)
    (define-key map (kbd "C-c C-m") #'djot-toggle-markup)
    (define-key map (kbd "C-c '") #'djot-edit-at-point)
    (define-key map (kbd "C-c C-b") #'djot-footnote-return)
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
  (add-hook 'after-change-functions #'djot--after-change nil t)
  (add-hook 'change-major-mode-hook #'djot--teardown nil t)
  (add-hook 'kill-buffer-hook #'djot--teardown nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.djot\\'" . djot-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dj\\'" . djot-mode))

(provide 'djot-mode)
;;; djot-mode.el ends here
