;;; tagaria-ui.el --- Shared UI primitives for Tagaria -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Yilin Zhang

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Faces, buffer naming, and state shared by Tagaria's user interfaces.

;;; Code:

(require 'subr-x)
(require 'tagaria-core)

(declare-function tagaria-mouse-visit-occurrence "tagaria-ref")
(declare-function tagaria-mouse-show-related "tagaria-ref")
(declare-function tagaria-mouse-edit-desc "tagaria")

(defface tagaria-tag-face
  '((t :inherit font-lock-warning-face :weight normal :underline t))
  "Face used for Tagaria tags."
  :group 'tagaria)

(defface tagaria-path-face
  '((t :inherit font-lock-comment-face))
  "Face used for paths in Tagaria buffers."
  :group 'tagaria)

(defface tagaria-section-heading-face
  '((t :inherit font-lock-variable-name-face :extend t :weight bold))
  "Face used for section headings in Tagaria buffers."
  :group 'tagaria)

(defcustom tagaria-buffer-name "*Tagaria*"
  "Base buffer name for the Tagaria management list."
  :type 'string
  :group 'tagaria)

(defcustom tagaria-detail-buffer-name "*Tagaria Detail*"
  "Base buffer name for a Tagaria detail page."
  :type 'string
  :group 'tagaria)

(defcustom tagaria-description-summary-width 80
  "Maximum display width of a description in the Tagaria list."
  :type 'natnum
  :group 'tagaria)

(defvar-local tagaria--buffer-root nil
  "Realm root represented by the current Tagaria buffer.")

(defvar tagaria-occurrence-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'tagaria-mouse-visit-occurrence)
    map)
  "Text-property keymap used on Tagaria occurrence rows.")

(defvar tagaria-related-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'tagaria-mouse-show-related)
    map)
  "Text-property keymap used on related tags.")

(defvar tagaria-description-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'tagaria-mouse-edit-desc)
    map)
  "Text-property keymap used on Tagaria descriptions.")

(defun tagaria--scoped-buffer-name (base root &optional detail)
  "Return BASE qualified by ROOT and optional DETAIL."
  (format "%s — %s%s*"
          (string-remove-suffix "*" base)
          (abbreviate-file-name (directory-file-name root))
          (if detail (format " — %s" detail) "")))

(defun tagaria--sort-numeric-third-column (left right)
  "Order tabulated entries LEFT and RIGHT by numeric column three."
  (< (string-to-number (aref (cadr left) 2))
     (string-to-number (aref (cadr right) 2))))

(defun tagaria--desc-summary (description)
  "Return a one-line display summary for DESCRIPTION.
Line breaks use the same visible marker convention as Decklet hints."
  (when description
    (truncate-string-to-width
     (replace-regexp-in-string "[\r\n]+" "↵" description)
     tagaria-description-summary-width nil nil "…")))

(defun tagaria--text-property-at-point (property)
  "Return PROPERTY at point or immediately before point."
  (or (get-text-property (point) property)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) property))))

(defun tagaria--interactive-desc-text (text tag)
  "Return a copy of TEXT with description interaction properties for TAG."
  (propertize text
              'tagaria-description-tag tag
              'mouse-face 'highlight
              'help-echo "mouse-1 or RET: edit description"
              'keymap tagaria-description-text-map
              'follow-link t))

(defun tagaria--desc-display (description tag &optional full)
  "Return DESCRIPTION as interactive display text for TAG.
Summarize it to a single line unless FULL is non-nil."
  (tagaria--interactive-desc-text
   (cond
    ((or (null description) (string-empty-p description))
     (propertize "(empty)" 'face 'shadow))
    (full description)
    (t (tagaria--desc-summary description)))
   tag))

(defun tagaria--insert-related-tags (related)
  "Insert clickable RELATED tag names at point."
  (if related
      (insert (tagaria--related-tags-string related))
    (insert (propertize "(none)" 'face 'shadow)))
  (insert "\n"))

(defun tagaria--related-tags-string (related)
  "Return RELATED as a comma-separated string of clickable tag names."
  (mapconcat
   (lambda (tag)
     (propertize tag
                 'face 'tagaria-tag-face
                 'mouse-face 'highlight
                 'help-echo "mouse-1 or RET: show related tag"
                 'keymap tagaria-related-text-map
                 'follow-link t
                 'tagaria-related-tag tag))
   related ", "))

(defun tagaria--insert-section-heading (section title &optional suffix)
  "Insert TITLE and optional unbolded SUFFIX as the heading for SECTION."
  (let ((start (point)))
    (insert (propertize "* " 'invisible 'tagaria-heading)
            (propertize title 'face 'tagaria-section-heading-face))
    (when suffix
      (insert (propertize suffix 'face 'default)))
    (insert "\n")
    (add-text-properties start (point) `(tagaria-section ,section))))

(defun tagaria--tag-text-regexp (tag)
  "Return a regexp matching TAG's rendered text plus surrounding blanks."
  (concat "[[:blank:]]*" (regexp-quote (tagaria--format-tag tag))
          "[[:blank:]]*"))

(defun tagaria--occurrence-display-context (occurrence regexp)
  "Return OCCURRENCE context with REGEXP occurrences of its own tag removed."
  (string-trim
   (replace-regexp-in-string
    regexp " " (tagaria-occurrence-context occurrence) t)))

(defun tagaria--render-tag-page (tag root description related occurrences)
  "Render TAG in ROOT with DESCRIPTION, RELATED tags, and OCCURRENCES."
  ;; This is the sole renderer for focused Detail pages.  Interactive text
  ;; properties define RET/mouse zones; section properties define folding.
  (let ((inhibit-read-only t)
        (tag-regexp (tagaria--tag-text-regexp tag)))
    (erase-buffer)
    (insert (propertize tag 'face 'tagaria-tag-face) "\n\n")
    (insert (propertize root 'face 'tagaria-path-face) "\n\n")
    (tagaria--insert-section-heading 'description "Description")
    (insert (tagaria--desc-display description tag t) "\n")
    (insert "\n")
    (tagaria--insert-section-heading 'related "Related Tags")
    (tagaria--insert-related-tags related)
    (insert "\n")
    (tagaria--insert-section-heading
     'occurrences "Occurrences" (format " (%d)" (length occurrences)))
    (if occurrences
        (dolist (occurrence occurrences)
          (let ((start (point))
                (context (tagaria--occurrence-display-context
                          occurrence tag-regexp)))
            (insert (format "%-34s "
                            (file-relative-name
                             (tagaria-occurrence-file occurrence) root))
                    (propertize
                     (format "#%d" (tagaria-occurrence-line occurrence))
                     'face 'shadow))
            (unless (string-empty-p context)
              (insert "  " context))
            (insert "\n")
            (add-text-properties
             start (point)
             `(tagaria-occurrence ,occurrence
                                  rear-nonsticky t))
            ;; Leave the newline unfontified so adjacent rows remain
            ;; separate mouse-face regions instead of one large link.
            (add-text-properties
             start (1- (point))
             `(mouse-face highlight
                          help-echo "mouse-1: visit occurrence"
                          keymap ,tagaria-occurrence-text-map
                          follow-link t))))
      (insert (propertize "(none)\n" 'face 'shadow)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(provide 'tagaria-ui)
;;; tagaria-ui.el ends here

;; Local Variables:
;; package-lint-main-file: "tagaria.el"
;; End:
