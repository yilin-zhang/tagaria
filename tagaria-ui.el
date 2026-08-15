;;; tagaria-ui.el --- User interface for Tagaria -*- lexical-binding: t; -*-

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

;; Inline integration, list and detail views, navigation, and user commands.

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'fringe)
(require 'outline)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'text-mode)
(require 'tagaria-core)
(require 'tagaria-ref)

;;; Faces and rendering

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

;;; Inline tags

(defvar tagaria-tag-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'tagaria-mouse-show-occurrences)
    map)
  "Text-property keymap installed on fontified Tagaria tags.")

(defvar tagaria-markdown-tag-text-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tagaria-tag-text-map)
    (define-key map (kbd "C-c C-o") #'tagaria-show-occurrences)
    map)
  "Tagaria tag keymap used specifically in Markdown buffers.")

(defvar tagaria-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c @") #'tagaria-show-occurrences)
    map)
  "Keymap for `tagaria-minor-mode'.")

(defconst tagaria--font-lock-keywords
  '((tagaria--font-lock-matcher (0 'tagaria-tag-face prepend)))
  "Font-lock rules used by `tagaria-minor-mode'.")

(defun tagaria--font-lock-matcher (limit)
  "Find and decorate the next Tagaria tag before LIMIT."
  (when (re-search-forward tagaria-tag-regexp limit t)
    (when (and (match-beginning 1) (match-end 1))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(tagaria-tag ,(match-string-no-properties 1)
                     mouse-face highlight
                     help-echo "mouse-1: show Tagaria references"
                     keymap ,(if (derived-mode-p 'markdown-mode)
                                 tagaria-markdown-tag-text-map
                               tagaria-tag-text-map)
                     rear-nonsticky t)))
    t))

(defun tagaria--remove-tag-properties (begin end)
  "Remove Tagaria-owned interactive properties between BEGIN and END."
  (let ((position begin))
    (while (< position end)
      (let ((next (or (next-single-property-change
                       position 'tagaria-tag nil end)
                      end)))
        (when (get-text-property position 'tagaria-tag)
          (remove-text-properties
           position next
           '(tagaria-tag nil mouse-face nil help-echo nil keymap nil
                         rear-nonsticky nil)))
        (setq position next)))))

(defun tagaria--before-change (begin end)
  "Clear Tagaria properties around a change from BEGIN to END."
  (save-excursion
    (goto-char begin)
    (let ((line-beginning (line-beginning-position)))
      (goto-char end)
      (tagaria--remove-tag-properties line-beginning (line-end-position)))))

(defun tagaria--org-open-at-point ()
  "Open a Tagaria tag at point through Org's native dispatcher."
  (when-let ((tag (tagaria--text-tag-at-point)))
    (tagaria-show-occurrences tag)
    t))

;;;###autoload
(define-minor-mode tagaria-minor-mode
  "Highlight textual Tagaria tags and make them navigable."
  :lighter " Tagaria"
  :keymap tagaria-minor-mode-map
  (if tagaria-minor-mode
      (progn
        (tagaria--check-tag-regexp)
        (font-lock-add-keywords nil tagaria--font-lock-keywords 'append)
        (add-hook 'before-change-functions #'tagaria--before-change nil t)
        (when (derived-mode-p 'org-mode)
          (add-hook 'org-open-at-point-functions
                    #'tagaria--org-open-at-point nil t)))
    (font-lock-remove-keywords nil tagaria--font-lock-keywords)
    (remove-hook 'before-change-functions #'tagaria--before-change t)
    (remove-hook 'org-open-at-point-functions #'tagaria--org-open-at-point t)
    (save-restriction
      (widen)
      (tagaria--remove-tag-properties (point-min) (point-max))))
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-flush))))

;;; Management list and commands

(defvar-local tagaria--scan nil
  "Most recent `tagaria-scan' represented by the current list buffer.")

(defvar-local tagaria--edited-tag nil
  "Tag whose description is being edited in the current buffer.")

(defvar-local tagaria--description-edit-p nil
  "Non-nil in a buffer used to edit one Tagaria description.")

(defcustom tagaria-desc-edit-mode #'text-mode
  "Function used to initialize a Tagaria description edit buffer."
  :type 'function
  :group 'tagaria)

(defun tagaria--read-root (&optional force-prompt)
  "Return the configured realm root, prompting when FORCE-PROMPT is non-nil."
  (if (or force-prompt
          (and (null tagaria-directory)
               (null (tagaria--find-enclosing-realm))))
      (tagaria--root
       (read-directory-name "Tagaria realm: " default-directory nil t))
    (tagaria--root)))

;;;###autoload
(defun tagaria-switch-realm (directory)
  "Switch the global default Tagaria realm to DIRECTORY."
  (interactive (list (read-directory-name
                      "Default Tagaria realm: " default-directory nil t)))
  (let ((root (tagaria--root directory)))
    (set-default 'tagaria-directory root)
    (message "Default Tagaria realm: %s" root)))

(defun tagaria--completion-annotation (candidate entries)
  "Return a description annotation for CANDIDATE found in ENTRIES."
  (when-let ((entry (assoc candidate entries)))
    (when-let ((description (tagaria--entry-desc entry)))
      (propertize
       (concat "  " (tagaria--desc-summary description))
       'face 'shadow))))

(defun tagaria--read-tag (entries prompt &optional require-match initial)
  "Read a tag from ENTRIES using PROMPT.
REQUIRE-MATCH and INITIAL are passed to `completing-read'."
  (let* ((completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (tagaria--completion-annotation candidate entries))))
         (value (completing-read prompt (mapcar #'car entries)
                                 nil require-match initial)))
    (setq value (string-trim value))
    (when (string-empty-p value)
      (user-error "Tagaria tag name cannot be empty"))
    value))

(defun tagaria--tag-at-point ()
  "Return the Tagaria tag represented at point, or nil."
  (cond
   ((derived-mode-p 'tagaria-mode) (tabulated-list-get-id))
   ((derived-mode-p 'tagaria-detail-mode) tagaria--detail-tag)
   (t (tagaria--text-tag-at-point))))

(defun tagaria-mouse-edit-desc (event)
  "Edit the Tagaria description clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (if-let ((tag (tagaria--text-property-at-point
                 'tagaria-description-tag)))
      (tagaria-edit-desc tag)
    (user-error "No Tagaria description at point")))

(defun tagaria--context-root ()
  "Return the Tagaria root associated with the current context."
  (or tagaria--buffer-root (tagaria--read-root)))

(defun tagaria--read-context-tag (prompt)
  "Return the tag at point or read one using PROMPT."
  (or (tagaria--tag-at-point)
      (let ((root (tagaria--context-root)))
        (tagaria--read-tag (tagaria-tags root) prompt t))))

(defun tagaria--list-entry (entry occurrence-table relation-table)
  "Build a tabulated row from ENTRY, OCCURRENCE-TABLE, and RELATION-TABLE."
  (let* ((name (car entry))
         (occurrences (length (gethash name occurrence-table)))
         (description (tagaria--entry-desc entry))
         (related (gethash name relation-table)))
    (list name
          (vector
           (propertize name
                       'face 'tagaria-tag-face
                       'mouse-face 'highlight
                       'help-echo "mouse-1: show Tagaria detail"
                       'keymap tagaria-tag-text-map
                       'follow-link t)
           (tagaria--desc-display description name)
           (propertize (number-to-string occurrences) 'face 'shadow)
           (tagaria--related-tags-string related)))))

(defun tagaria--render-entries ()
  "Build rows from this list buffer's cached scan and entries."
  (let* ((entries (tagaria-scan-entries tagaria--scan))
         (table (tagaria-scan-occurrence-table tagaria--scan))
         (relation-table (tagaria-scan-relations tagaria--scan)))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (tagaria--list-entry entry table relation-table))
                  entries))))

(defun tagaria--populate ()
  "Scan and populate the current Tagaria list."
  (unless tagaria--buffer-root
    (error "This Tagaria list has no realm root"))
  (setq tagaria--scan (tagaria-sync tagaria--buffer-root))
  (tagaria--render-entries))

(defun tagaria--goto-tag-row (tag)
  "Move to TAG in the current tabulated list and return its position.
Leave point unchanged and return nil when TAG is absent."
  (when tag
    (let (target)
      (save-excursion
        (goto-char (point-min))
        (while (and (not target) (not (eobp)))
          (when (equal (tabulated-list-get-id) tag)
            (setq target (point)))
          (unless target (forward-line 1))))
      (when target
        (goto-char target)))))

(defun tagaria--adjacent-tag-row ()
  "Return the next row's tag, or the previous row's tag at the end."
  (or (save-excursion
        (forward-line 1)
        (tabulated-list-get-id))
      (save-excursion
        (forward-line -1)
        (tabulated-list-get-id))))

(defun tagaria-refresh ()
  "Rescan and refresh the current Tagaria list."
  (interactive)
  (unless (derived-mode-p 'tagaria-mode)
    (user-error "Not in a Tagaria list"))
  (let ((tag (tabulated-list-get-id)))
    (tabulated-list-revert)
    (tagaria--goto-tag-row tag)))

(defun tagaria-list-activate ()
  "Edit the description, open a relation, or open the current row's tag."
  (interactive)
  (unless (derived-mode-p 'tagaria-mode)
    (user-error "Not in a Tagaria list"))
  ;; Tabulated List still owns row navigation, but the visible columns are
  ;; independent actions: Description edits, Related enters that tag, and
  ;; every other position enters the row tag.  Mouse properties mirror RET.
  (let ((description-tag
         (tagaria--text-property-at-point 'tagaria-description-tag)))
    (cond
     (description-tag (tagaria-edit-desc description-tag))
     ((tagaria--text-property-at-point 'tagaria-related-tag)
      (tagaria-show-related-at-point))
     (t (tagaria-show-occurrences)))))

(defun tagaria--refresh-list-buffers (root &optional rescan)
  "Refresh Tagaria list buffers for ROOT, rescanning when RESCAN is non-nil."
  (let* ((scan (and rescan (tagaria-sync root)))
         (database (and (null scan) (tagaria--read-database root))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'tagaria-mode)
                   tagaria--buffer-root
                   (file-equal-p tagaria--buffer-root root))
          (let* ((tag (tabulated-list-get-id))
                 (adjacent (tagaria--adjacent-tag-row))
                 (new-scan
                  (or scan
                      (tagaria--update-scan-data tagaria--scan database))))
            ;; The scan is the single List/Detail snapshot.  Data-only edits
            ;; update its DB slots so opening Detail cannot revive stale data.
            (setq tagaria--scan new-scan)
            (tagaria--render-entries)
            (tabulated-list-print t)
            (unless (or (tagaria--goto-tag-row tag)
                        (tagaria--goto-tag-row adjacent))
              (goto-char (point-min)))))))
    (tagaria--refresh-detail-buffer root scan database)))

(defvar tagaria-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'tagaria-list-activate)
    (define-key map (kbd "o") #'tagaria-show-occurrences)
    (define-key map (kbd "g") #'tagaria-refresh)
    (define-key map (kbd "c") #'tagaria-create)
    (define-key map (kbd "e") #'tagaria-edit-desc)
    (define-key map (kbd "E") #'tagaria-edit-desc-buffer)
    (define-key map (kbd "a") #'tagaria-add-related-tag)
    (define-key map (kbd "d") #'tagaria-delete-all-occurrences)
    (define-key map (kbd "x") #'tagaria-delete-related-tag)
    (define-key map (kbd "r") #'tagaria-rename)
    (define-key map (kbd "D") #'tagaria-delete)
    (define-key map (kbd "s") #'tagaria-search)
    (define-key map (kbd "q") #'tagaria-quit)
    map)
  "Keymap for `tagaria-mode'.")

(define-derived-mode tagaria-mode tabulated-list-mode "Tagaria"
  "Major mode for browsing and managing a Tagaria realm."
  (setq tabulated-list-format
        [("Tag" 22 t)
         ("Description" 32 t)
         ("Refs" 6 tagaria--sort-numeric-third-column)
         ("Related" 28 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Tag" . nil))
  (add-hook 'tabulated-list-revert-hook #'tagaria--populate nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun tagaria--prepare-list-buffer (root &optional scan)
  "Return ROOT's initialized Tagaria list buffer.
SCAN avoids rescanning when the caller already has current data."
  (let ((buffer
         (get-buffer-create
          (tagaria--scoped-buffer-name tagaria-buffer-name root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tagaria-mode)
        (tagaria-mode))
      (setq tagaria--buffer-root root)
      (setq default-directory root)
      (if scan
          (progn
            (setq tagaria--scan scan)
            (tagaria--render-entries)
            (tabulated-list-print t))
        (tabulated-list-revert)))
    buffer))

(defun tagaria--open-list (root)
  "Open ROOT's Tagaria list."
  (let ((buffer (tagaria--prepare-list-buffer root)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun tagaria-list (&optional choose-directory)
  "Open the Tagaria list for the configured realm.
With prefix argument CHOOSE-DIRECTORY, prompt for a realm without changing
`tagaria-directory'."
  (interactive "P")
  (tagaria--open-list (tagaria--read-root choose-directory)))

(defun tagaria--buffer-belongs-to-root-p (root)
  "Return non-nil when the current buffer belongs below ROOT."
  (let ((location (or buffer-file-name default-directory)))
    (and location
         (file-in-directory-p (file-truename location) root))))

;;;###autoload
(defun tagaria-insert ()
  "Insert an existing or newly created Tagaria tag at point."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((root (tagaria--read-root)))
    (unless (tagaria--buffer-belongs-to-root-p root)
      (user-error "Current buffer is outside the Tagaria realm: %s" root))
    (let* ((entries (tagaria-tags root))
           (name (tagaria--read-tag entries "Insert tag: "))
           (existing (assoc name entries))
           (rendered (tagaria--format-tag name)))
      (insert rendered)
      (unless existing
        (tagaria-register name nil root))
      (message "%s %s"
               (if existing "Inserted" "Created and inserted") name))))

;;;###autoload
(defun tagaria-create (name &optional description directory)
  "Create NAME in DIRECTORY with optional DESCRIPTION."
  (interactive
   (let* ((root (tagaria--context-root))
          (entries (tagaria-tags root))
          (name (tagaria--read-tag entries "Create tag: ")))
     (list name nil root)))
  (let* ((root (tagaria--root directory))
         (entries (tagaria-tags root)))
    (when (assoc name entries)
      (user-error "Tagaria tag already exists: %s" name))
    (tagaria--format-tag name)
    (tagaria-register name description root)
    (tagaria--refresh-list-buffers root)
    (message "Created Tagaria tag %s" name)))

(defun tagaria--store-desc (root tag value)
  "Store string VALUE as TAG's description under ROOT."
  (let ((value (tagaria--normalize-desc value)))
    (if (equal value (tagaria-desc tag root))
        (message "Description for %s is unchanged" tag)
      (tagaria-set-desc tag value root)
      (tagaria--refresh-list-buffers root)
      (message "Updated description for %s" tag))))

;;;###autoload
(defun tagaria-edit-desc (&optional tag)
  "Edit TAG's description in the minibuffer."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Edit description for: ")))
         (value (read-string "Description: "
                             (tagaria-desc name root))))
    (tagaria--store-desc root name value)))

;;;###autoload
(defun tagaria-edit-desc-buffer (&optional tag)
  "Edit TAG's multiline description in a regular buffer."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Edit description for: ")))
         (buffer
          (get-buffer-create
           (tagaria--scoped-buffer-name
            "*Tagaria Description*" root name))))
    (when (buffer-modified-p buffer)
      (user-error "Editing the description of %s is already in progress" name))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when-let ((description (tagaria-desc name root)))
          (insert description)))
      (funcall tagaria-desc-edit-mode)
      (local-set-key (kbd "C-c C-c") #'tagaria-desc-edit-commit)
      (local-set-key (kbd "C-c C-k") #'tagaria-desc-edit-cancel)
      (setq-local header-line-format
                  '(" C-c C-c save description   C-c C-k cancel "))
      (setq tagaria--buffer-root root
            tagaria--edited-tag name
            tagaria--description-edit-p t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun tagaria-desc-edit-commit ()
  "Save the current buffer as one Tagaria description."
  (interactive)
  (unless tagaria--description-edit-p
    (user-error "Not in a Tagaria description edit buffer"))
  (let ((root tagaria--buffer-root)
        (tag tagaria--edited-tag)
        (value (buffer-substring-no-properties (point-min) (point-max))))
    (tagaria--store-desc root tag value)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defun tagaria-desc-edit-cancel ()
  "Cancel the current Tagaria description edit."
  (interactive)
  (unless tagaria--description-edit-p
    (user-error "Not in a Tagaria description edit buffer"))
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun tagaria-add-related-tag (&optional tag related)
  "Add an undirected relation between TAG and RELATED."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Relate tag: ")))
         (database (tagaria--read-database root))
         (existing (tagaria--related-tags-in
                    name (plist-get database :related)))
         (choices (seq-remove
                   (lambda (candidate)
                     (or (string= candidate name) (member candidate existing)))
                   (mapcar #'car (plist-get database :tags))))
         (other (or related
                    (completing-read "Related tag: " choices nil t))))
    (tagaria-set-related name other t root)
    (tagaria--refresh-list-buffers root)
    (message "Related %s and %s" name other)))

(defun tagaria-delete-related-tag (&optional tag related)
  "Delete the undirected relation between TAG and RELATED."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Unrelate tag: ")))
         (database (tagaria--read-database root))
         (existing (tagaria--related-tags-in
                    name (plist-get database :related)))
         (other (or related
                    (get-text-property (point) 'tagaria-related-tag)
                    (completing-read "Remove related tag: " existing nil t))))
    (unless (member other existing)
      (user-error "%s is not related to %s" name other))
    (unless (y-or-n-p (format "Remove relation between %s and %s?" name other))
      (user-error "Tagaria relation deletion cancelled"))
    (tagaria-set-related name other nil root)
    (tagaria--refresh-list-buffers root)
    (message "Removed relation between %s and %s" name other)))

(defun tagaria-delete-occurrence ()
  "Delete the textual Tagaria occurrence represented by the current line."
  (interactive)
  (unless (derived-mode-p 'tagaria-detail-mode)
    (user-error "Not in a Tagaria detail page"))
  (let* ((occurrence (tagaria--occurrence-at-point))
         (root tagaria--buffer-root)
         (tag tagaria--detail-tag))
    (unless occurrence
      (user-error "No Tagaria occurrence on this line"))
    (unless (y-or-n-p (format "Delete this occurrence of %s?" tag))
      (user-error "Tagaria occurrence deletion cancelled"))
    (let ((buffer (find-file-noselect (tagaria-occurrence-file occurrence)))
          (file (tagaria-occurrence-file occurrence)))
      (with-current-buffer buffer
        (barf-if-buffer-read-only)
        (save-restriction
          (widen)
          (condition-case nil
              (tagaria--delete-occurrence-text occurrence)
            (error
             (user-error "Occurrence changed since the detail page was built")))))
      (unless (buffer-modified-p buffer)
        (error "Deleting the occurrence did not modify %s" file)))
    (tagaria--refresh-list-buffers root t)
    (message "Deleted one occurrence of %s; source buffer is unsaved" tag)))

;;;###autoload
(defun tagaria-show-occurrences (&optional tag)
  "Show TAG's description, related tags, and occurrences in the current realm."
  (interactive)
  (let* ((from-list (derived-mode-p 'tagaria-mode))
         (root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Show tag: ")))
         (cached (and from-list tagaria--scan))
         (scan (or cached (tagaria-sync root)))
         (list-buffer
          (if from-list
              (current-buffer)
            (tagaria--prepare-list-buffer root scan))))
    (unless from-list
      (with-current-buffer list-buffer
        (tagaria--goto-tag-row name)))
    (tagaria--open-detail root name scan list-buffer)))

;;;###autoload
(defun tagaria-search ()
  "Select a tag with annotated completion and show its detail page."
  (interactive)
  (let* ((root (tagaria--read-root))
         (scan (tagaria-sync root))
         (entries (tagaria-scan-entries scan))
         (tag (tagaria--read-tag entries "Search tag: " t))
         (list-buffer (tagaria--prepare-list-buffer root scan)))
    (with-current-buffer list-buffer
      (tagaria--goto-tag-row tag))
    (tagaria--open-detail root tag scan list-buffer)))

(defun tagaria--save-modified-occurrence-buffers (occurrences operation)
  "Offer to save modified buffers affected by OCCURRENCES for OPERATION.
Abort when any answer is not yes.  Return non-nil if one was saved."
  (let (saved)
    (dolist (buffer (tagaria--modified-buffers-for-occurrences occurrences))
      (unless (y-or-n-p
               (format "Buffer %s has unsaved changes; save before %s?"
                       (buffer-name buffer) operation))
        (user-error "Tagaria %s cancelled" operation))
      (with-current-buffer buffer
        (save-buffer))
      (setq saved t))
    saved))

(defun tagaria--settled-occurrences (root name operation)
  "Return NAME's occurrences in ROOT once buffers are settled for OPERATION.
Offer to save any modified buffer holding an occurrence, and rescan when one
was saved, so the returned set matches what is on disk."
  (let* ((scan (tagaria-sync root))
         (occurrences (gethash name (tagaria-scan-occurrence-table scan))))
    (if (tagaria--save-modified-occurrence-buffers occurrences operation)
        (gethash name (tagaria-scan-occurrence-table (tagaria-sync root)))
      occurrences)))

(defun tagaria--show-operation-preview (title root occurrences empty-message)
  "Show TITLE and OCCURRENCES in ROOT, using EMPTY-MESSAGE when empty."
  (let ((buffer (get-buffer-create "*Tagaria Operation Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize (concat title "\n\n") 'face 'bold))
        (if occurrences
            (dolist (occurrence occurrences)
              (insert (format "%s:%d\n  %s\n"
                              (file-relative-name
                               (tagaria-occurrence-file occurrence) root)
                              (tagaria-occurrence-line occurrence)
                              (tagaria-occurrence-context occurrence))))
          (insert empty-message "\n"))
        (goto-char (point-min))))
    (display-buffer buffer)
    buffer))

(cl-defun tagaria--confirm-with-preview
    (&key title root occurrences empty-message prompt confirm-function cancelled)
  "Preview an operation and confirm PROMPT with CONFIRM-FUNCTION.
TITLE, ROOT, OCCURRENCES, and EMPTY-MESSAGE describe the preview.  Signal
a user error containing CANCELLED when confirmation is declined."
  (let ((preview (tagaria--show-operation-preview
                  title root occurrences empty-message)))
    (unwind-protect
        (unless (funcall confirm-function prompt)
          (user-error "%s" cancelled))
      (when (buffer-live-p preview)
        (kill-buffer preview)))))

;;;###autoload
(defun tagaria-rename (&optional old-name new-name)
  "Rename OLD-NAME to NEW-NAME across the current Tagaria realm."
  (interactive)
  (let* ((root (tagaria--context-root))
         (old (or old-name (tagaria--read-context-tag "Rename tag: ")))
         (new (or new-name
                  (string-trim
                   (read-string (format "Rename %s to: " old) old))))
         (occurrences (tagaria--settled-occurrences root old "rename")))
    (let ((signature (tagaria--occurrence-signature occurrences)))
      (tagaria--confirm-with-preview
       :title (format "Rename %s → %s" old new)
       :root root
       :occurrences occurrences
       :empty-message "No textual occurrences; only tag data will be renamed."
       :prompt (format "Rename %s to %s in %s?"
                       old new
                       (tagaria--count-phrase
                        (length occurrences) "occurrence"))
       :confirm-function #'y-or-n-p
       :cancelled "Tagaria rename cancelled")
      (let ((result (tagaria-rename-tag old new root signature)))
        (tagaria--retarget-detail root old new)
        (tagaria--refresh-list-buffers root t)
        (message "Renamed %s to %s in %s; backup: %s"
                 old new
                 (tagaria--count-phrase
                  (plist-get result :count) "occurrence")
                 (plist-get result :backup))))))

;;;###autoload
(defun tagaria-delete-all-occurrences (&optional tag)
  "Delete every textual occurrence of TAG while preserving the tag."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Clear tag references: ")))
         (occurrences
          (tagaria--settled-occurrences root name "reference deletion")))
    (if (null occurrences)
        (message "%s already has no textual references" name)
      (tagaria--confirm-with-preview
       :title (format "Delete every reference to %s" name)
       :root root
       :occurrences occurrences
       :empty-message "No textual references."
       :prompt (format "Delete all %s to %s?"
                       (tagaria--count-phrase
                        (length occurrences) "textual reference")
                       name)
       :confirm-function #'yes-or-no-p
       :cancelled "Tagaria reference deletion cancelled")
      (let ((result
             (tagaria-delete-occurrences
              name root (tagaria--occurrence-signature occurrences))))
        (tagaria--refresh-list-buffers root t)
        (message "Deleted %s to %s; backup: %s"
                 (tagaria--count-phrase
                  (plist-get result :count) "reference")
                 name
                 (plist-get result :backup))))))

;;;###autoload
(defun tagaria-delete (&optional tag)
  "Delete TAG, its occurrences, description, and relations."
  (interactive)
  (let* ((root (tagaria--context-root))
         (name (or tag (tagaria--read-context-tag "Delete tag: ")))
         (occurrences
          (tagaria--settled-occurrences root name "tag deletion")))
    (if occurrences
        (tagaria--confirm-with-preview
         :title (format "Delete tag %s and every reference" name)
         :root root
         :occurrences occurrences
         :empty-message "No textual references."
         :prompt (format "Delete tag %s and its %s?"
                         name
                         (tagaria--count-phrase
                          (length occurrences) "reference"))
         :confirm-function #'yes-or-no-p
         :cancelled "Tagaria deletion cancelled")
      (unless (yes-or-no-p (format "Delete tag %s?" name))
        (user-error "Tagaria deletion cancelled")))
    (let ((result
           (tagaria-delete-tag
            name root (tagaria--occurrence-signature occurrences))))
      (tagaria--close-detail root name)
      (tagaria--refresh-list-buffers root)
      (message "Deleted Tagaria tag %s and %s; backup: %s"
               name
               (tagaria--count-phrase
                (plist-get result :count) "reference")
               (plist-get result :backup)))))

(easy-menu-define tagaria-menu tagaria-mode-map
  "Menu for Tagaria lists."
  '("Tagaria"
    ["Show occurrences" tagaria-show-occurrences t]
    ["Create tag" tagaria-create t]
    ["Edit description" tagaria-edit-desc t]
    ["Edit description in buffer" tagaria-edit-desc-buffer t]
    ["Add related tag" tagaria-add-related-tag t]
    ["Remove related tag" tagaria-delete-related-tag t]
    "---"
    ["Search tag" tagaria-search t]
    "---"
    ["Rename tag" tagaria-rename t]
    ["Delete all references" tagaria-delete-all-occurrences t]
    ["Delete tag" tagaria-delete t]
    ["Refresh" tagaria-refresh t]))

;;; Detail view

(defvar-local tagaria--detail-tag nil
  "Tag represented by the current detail page.")

(defvar-local tagaria--detail-scan nil
  "Scan cached by the current detail page.")

(defvar-local tagaria--detail-indicator-overlays nil
  "Visibility indicator overlays in the current detail page.")

(defvar-local tagaria--detail-list-buffer nil
  "Tagaria list buffer to show when moving up from this detail page.")

(defvar-local tagaria--detail-list-point nil
  "Marker for the List row that opened this detail page.")

(defvar-local tagaria--occurrence-preview-window nil
  "Window used to preview the occurrence selected in the detail page.")

(defvar-local tagaria--occurrence-preview-overlay nil
  "Overlay highlighting the currently previewed occurrence.")

(defvar-local tagaria--last-preview-occurrence nil
  "Last occurrence previewed from the current detail page.")

(define-fringe-bitmap 'tagaria-fringe-bitmap-right
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'tagaria-fringe-bitmap-down
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

;; Window contract: List and Detail are two states of one Tagaria window.
;; RET replaces List with Detail, `^' restores that exact List state, and `q'
;; quits only the selected Tagaria window.  A separate ordinary window may
;; preview occurrence sources, but navigation must never replace Detail with
;; that source; RET selects the already-previewed source window instead.
(defvar tagaria-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'tagaria-detail-activate)
    (define-key map (kbd "o") #'tagaria-detail-activate)
    (define-key map (kbd "^") #'tagaria-detail-up)
    (define-key map [remap next-line] #'tagaria-detail-next-line)
    (define-key map [remap previous-line] #'tagaria-detail-previous-line)
    (define-key map (kbd "TAB") #'tagaria-detail-toggle-section)
    (define-key map (kbd "<tab>") #'tagaria-detail-toggle-section)
    (define-key map (kbd "g") #'tagaria-detail-refresh)
    (define-key map (kbd "e") #'tagaria-edit-desc)
    (define-key map (kbd "E") #'tagaria-edit-desc-buffer)
    (define-key map (kbd "a") #'tagaria-add-related-tag)
    (define-key map (kbd "d") #'tagaria-delete-occurrence)
    (define-key map (kbd "D") #'tagaria-delete)
    (define-key map (kbd "x") #'tagaria-delete-related-tag)
    (define-key map (kbd "r") #'tagaria-rename)
    (define-key map (kbd "q") #'tagaria-quit)
    map)
  "Keymap for `tagaria-detail-mode'.")

(define-derived-mode tagaria-detail-mode special-mode "Tagaria-Detail"
  "Major mode for viewing one tag's description, relations, and occurrences."
  (setq-local outline-regexp "^\\* "
              outline-level (lambda () 1)
              outline-minor-mode-cycle nil
              truncate-lines t
              line-move-visual nil)
  (add-to-invisibility-spec 'tagaria-heading)
  (outline-minor-mode 1)
  (add-hook 'post-command-hook #'tagaria--preview-current-occurrence nil t)
  (add-hook 'kill-buffer-hook #'tagaria--cleanup-occurrence-preview nil t)
  (hl-line-mode 1))

(defun tagaria-detail-next-line (&optional count)
  "Move forward COUNT physical lines in the detail page."
  (interactive "p")
  (forward-line (or count 1)))

(defun tagaria-detail-previous-line (&optional count)
  "Move backward COUNT physical lines in the detail page."
  (interactive "p")
  (forward-line (- (or count 1))))

(defun tagaria--occurrence-at-point ()
  "Return the occurrence represented by the current line, or nil."
  (get-text-property (line-beginning-position) 'tagaria-occurrence))

(defun tagaria--detail-indicator-string (collapsed)
  "Return a section indicator string for COLLAPSED state."
  (if (display-graphic-p)
      (propertize
       "fringe" 'display
       `(left-fringe
         ,(if collapsed
              'tagaria-fringe-bitmap-right
            'tagaria-fringe-bitmap-down)
         shadow))
    (propertize (if collapsed "> " "v ") 'face 'shadow)))

(defun tagaria--detail-install-indicators ()
  "Install expanded visibility indicators on every section heading."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward outline-regexp nil t)
      (let ((overlay (make-overlay (line-beginning-position)
                                   (line-end-position) nil t)))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'tagaria-detail-indicator t)
        (overlay-put overlay 'before-string
                     (tagaria--detail-indicator-string nil))
        (push overlay tagaria--detail-indicator-overlays)))))

(defun tagaria--detail-update-indicator (collapsed)
  "Show the current heading's indicator in COLLAPSED state."
  (when-let ((overlay
              (seq-find
               (lambda (candidate)
                 (and (overlay-get candidate 'tagaria-detail-indicator)
                      (= (overlay-start candidate) (line-beginning-position))))
               tagaria--detail-indicator-overlays)))
    (overlay-put overlay 'before-string
                 (tagaria--detail-indicator-string collapsed))))

(defun tagaria--detail-collapsed-sections ()
  "Return section identifiers currently collapsed in this detail page."
  (save-excursion
    (goto-char (point-min))
    (let (collapsed)
      (while (re-search-forward outline-regexp nil t)
        (let ((section (get-text-property (match-beginning 0)
                                          'tagaria-section)))
          (forward-line 1)
          (when (outline-invisible-p (point))
            (push section collapsed))))
      collapsed)))

(defun tagaria--detail-position-key ()
  "Return the semantic position represented by point in a detail page."
  ;; Renders can change line lengths.  Preserve the UI object under point,
  ;; not a raw buffer position, so edits and refreshes stay on the same item.
  (let ((occurrence (tagaria--occurrence-at-point))
        (related (tagaria--text-property-at-point 'tagaria-related-tag))
        (description
         (tagaria--text-property-at-point 'tagaria-description-tag))
        (section (get-text-property (line-beginning-position)
                                    'tagaria-section)))
    (cond
     (occurrence
      (list 'occurrence (tagaria--occurrence-key occurrence)))
     (related (list 'related related))
     (description (list 'description description))
     (section (list 'section section)))))

(defun tagaria--detail-goto-occurrence (&optional key)
  "Move to the occurrence matching KEY, or the first occurrence."
  (goto-char (point-min))
  (let (first found)
    (while (and (not found) (not (eobp)))
      (when-let ((occurrence (tagaria--occurrence-at-point)))
        (unless first (setq first (point)))
        (when (equal key (tagaria--occurrence-key occurrence))
          (setq found (point))))
      (unless found (forward-line 1)))
    (goto-char (or found first (point-min)))))

(defun tagaria--detail-goto-property (property value)
  "Move to the first text whose PROPERTY equals VALUE."
  (goto-char (point-min))
  (when-let ((match (text-property-search-forward property value t)))
    (goto-char (prop-match-beginning match))
    t))

(defun tagaria--detail-restore-position (position)
  "Restore semantic POSITION, falling back to the first occurrence."
  (unless
      (pcase position
        (`(occurrence ,key)
         (tagaria--detail-goto-occurrence key)
         (tagaria--occurrence-at-point))
        (`(related ,tag)
         (tagaria--detail-goto-property 'tagaria-related-tag tag))
        (`(description ,tag)
         (tagaria--detail-goto-property 'tagaria-description-tag tag))
        (`(section ,section)
         (tagaria--detail-goto-property 'tagaria-section section)))
    (tagaria--detail-goto-occurrence)))

(defun tagaria--detail-restore-sections (collapsed)
  "Collapse section identifiers listed in COLLAPSED."
  (dolist (section collapsed)
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (re-search-forward outline-regexp nil t))
        (when (eq (get-text-property (match-beginning 0) 'tagaria-section)
                  section)
          (setq found t)))
      (when found
        (beginning-of-line)
        (outline-hide-subtree)
        (tagaria--detail-update-indicator t)))))

(defun tagaria--render-detail (description related occurrences)
  "Render DESCRIPTION, RELATED tags, and OCCURRENCES in the detail page."
  (let ((collapsed (and (derived-mode-p 'tagaria-detail-mode)
                        (tagaria--detail-collapsed-sections)))
        (position (tagaria--detail-position-key)))
    (mapc #'delete-overlay tagaria--detail-indicator-overlays)
    (setq tagaria--detail-indicator-overlays nil)
    (tagaria--render-tag-page
     tagaria--detail-tag tagaria--buffer-root description related occurrences)
    (tagaria--detail-install-indicators)
    (tagaria--detail-restore-sections collapsed)
    (tagaria--detail-restore-position position)
    (set-buffer-modified-p nil)))

(defun tagaria-detail-toggle-section ()
  "Toggle the outline section containing point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at outline-regexp)
      (outline-back-to-heading t))
    (let ((body (line-beginning-position 2)))
      (if (outline-invisible-p body)
          (progn
            (outline-show-subtree)
            (tagaria--detail-update-indicator nil))
        (outline-hide-subtree)
        (tagaria--detail-update-indicator t)))))

(defun tagaria--cleanup-occurrence-preview ()
  "Remove highlighting left by the current detail page."
  (when (overlayp tagaria--occurrence-preview-overlay)
    (delete-overlay tagaria--occurrence-preview-overlay))
  (setq tagaria--occurrence-preview-overlay nil
        tagaria--last-preview-occurrence nil))

(defun tagaria--ordinary-other-window (&optional window)
  "Return a non-Tagaria window other than WINDOW, or nil."
  (let ((excluded (or window (selected-window))))
    (seq-find
     (lambda (candidate)
       (and (not (eq candidate excluded))
            (null (window-parameter candidate 'window-side))
            (not (window-dedicated-p candidate))
            (with-current-buffer (window-buffer candidate)
              (not (derived-mode-p 'tagaria-mode 'tagaria-detail-mode)))))
     (window-list (window-frame excluded) 'nomini))))

(defun tagaria--preview-occurrence (occurrence)
  "Preview OCCURRENCE without selecting its source window."
  (unless (eq occurrence tagaria--last-preview-occurrence)
    (tagaria--cleanup-occurrence-preview)
    (let* ((detail-buffer (current-buffer))
           ;; Preview must never stop for unsafe file-local-variable questions.
           (enable-local-variables :safe)
           (enable-local-eval nil)
           (buffer (find-file-noselect (tagaria-occurrence-file occurrence)))
           (window
            (or (and (window-live-p tagaria--occurrence-preview-window)
                     tagaria--occurrence-preview-window)
                (tagaria--ordinary-other-window)
                (display-buffer
                 buffer
                 '((display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (inhibit-same-window . t))))))
      (setq tagaria--occurrence-preview-window window)
      (when (window-live-p window)
        (set-window-buffer window buffer)
        (with-current-buffer buffer
          (tagaria--goto-occurrence-position occurrence)
          (let* ((start (point))
                 (length (- (tagaria-occurrence-end occurrence)
                            (tagaria-occurrence-start occurrence)))
                 (end (min (point-max) (+ start length)))
                 (overlay (make-overlay start end buffer)))
            ;; Never select WINDOW here: preview must leave focus in Detail.
            (set-window-point window start)
            (set-window-start
             window
             (save-excursion
               (forward-line (- (/ (window-body-height window) 2)))
               (line-beginning-position)))
            (overlay-put overlay 'face 'highlight)
            (with-current-buffer detail-buffer
              (setq tagaria--occurrence-preview-overlay overlay))))
        (setq tagaria--last-preview-occurrence occurrence)))))

(defun tagaria--preview-current-occurrence ()
  "Preview the occurrence at point without leaving the detail page."
  ;; Preview is keyboard-driven.  Merely hovering a row must remain free of
  ;; file/window side effects; mouse-1 performs the explicit visit instead.
  (when (derived-mode-p 'tagaria-detail-mode)
    (if-let ((occurrence (tagaria--occurrence-at-point)))
        (tagaria--preview-occurrence occurrence)
      (tagaria--cleanup-occurrence-preview))))

(defun tagaria-detail-up ()
  "Return from Tag view to its Tagaria list in the same window."
  (interactive)
  (unless (derived-mode-p 'tagaria-detail-mode)
    (user-error "Not in a Tagaria detail page"))
  (unless (buffer-live-p tagaria--detail-list-buffer)
    (user-error "This Tag view has no Tagaria list"))
  (let ((list-buffer tagaria--detail-list-buffer)
        (list-point tagaria--detail-list-point))
    (tagaria--cleanup-occurrence-preview)
    (switch-to-buffer list-buffer)
    (when (and (markerp list-point)
               (eq (marker-buffer list-point) list-buffer))
      (goto-char list-point))))

(defun tagaria-quit ()
  "Quit the current Tagaria window."
  (interactive)
  (unless (derived-mode-p 'tagaria-mode 'tagaria-detail-mode)
    (user-error "Not in a Tagaria window"))
  (when (derived-mode-p 'tagaria-detail-mode)
    (tagaria--cleanup-occurrence-preview)
    (setq tagaria--occurrence-preview-window nil))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (quit-window)
    (when (and (window-live-p window)
               (eq (window-buffer window) buffer))
      (switch-to-prev-buffer window 'bury))))

(defun tagaria-detail-refresh ()
  "Rescan and redraw the current Tagaria detail page."
  (interactive)
  (unless (derived-mode-p 'tagaria-detail-mode)
    (user-error "Not in a Tagaria detail page"))
  (let ((scan (tagaria-sync tagaria--buffer-root)))
    (tagaria--update-detail tagaria--buffer-root tagaria--detail-tag scan)))

(defun tagaria--refresh-detail-buffer (root &optional scan database)
  "Refresh ROOT's detail page from optional SCAN or freshly read DATABASE."
  (when-let ((buffer (tagaria--detail-buffer root t)))
    (with-current-buffer buffer
      (when tagaria--detail-tag
        (unless scan
          (setq scan (tagaria--update-scan-data tagaria--detail-scan
                                                database)))
        (tagaria--update-detail
         root tagaria--detail-tag scan nil
         (null (get-buffer-window buffer t)))))))

(defun tagaria--retarget-detail (root old-name new-name)
  "Retarget ROOT's detail page from OLD-NAME to NEW-NAME."
  (when-let ((buffer (tagaria--detail-buffer root t)))
    (with-current-buffer buffer
      (when (equal tagaria--detail-tag old-name)
        (setq tagaria--detail-tag new-name)))))

(defun tagaria--close-detail (root tag)
  "Clear the detail page when it represents TAG in ROOT."
  (when-let ((buffer (tagaria--detail-buffer root t)))
    (with-current-buffer buffer
      (when (equal tagaria--detail-tag tag)
        (setq tagaria--occurrence-preview-window nil
              tagaria--detail-tag nil
              tagaria--detail-scan nil)
        (tagaria--cleanup-occurrence-preview)
        (dolist (window (get-buffer-window-list buffer nil t))
          (if (buffer-live-p tagaria--detail-list-buffer)
              (set-window-buffer window tagaria--detail-list-buffer)
            (quit-window nil window)))
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun tagaria--detail-buffer (root &optional existing-only)
  "Return ROOT's unique detail buffer.
When EXISTING-ONLY is non-nil, do not create it."
  (let* ((name (tagaria--scoped-buffer-name tagaria-detail-buffer-name root))
         (buffer (if existing-only (get-buffer name) (get-buffer-create name))))
    (when buffer
      (with-current-buffer buffer
        (unless (derived-mode-p 'tagaria-detail-mode)
          (tagaria-detail-mode))
        (setq tagaria--buffer-root root
              default-directory root)))
    buffer))

(defun tagaria--update-detail
    (root tag &optional scan list-buffer no-render)
  "Update ROOT's TAG detail from cached SCAN.
When LIST-BUFFER is non-nil, remember it as the parent list.  When NO-RENDER
is non-nil, update only the cached state."
  (let ((buffer (tagaria--detail-buffer root)))
    (with-current-buffer buffer
      (unless (equal tagaria--detail-tag tag)
        (tagaria--cleanup-occurrence-preview))
      (setq scan (or scan tagaria--detail-scan))
      (unless scan
        (error "Tagaria detail has no realm scan"))
      (setq tagaria--detail-tag tag
            tagaria--detail-scan scan)
      (when list-buffer
        (setq tagaria--detail-list-buffer list-buffer))
      (unless no-render
        (tagaria--render-detail
         (tagaria--entry-desc (assoc tag (tagaria-scan-entries scan)))
         (gethash tag (tagaria-scan-relations scan))
         (gethash tag (tagaria-scan-occurrence-table scan)))))
    buffer))

(defun tagaria--open-detail (root tag scan list-buffer)
  "Open ROOT's TAG detail in the current Tagaria window.
SCAN supplies its contents; LIST-BUFFER is shown again by `^'."
  (let* (;; Entering from List reuses its window.  Entering from prose keeps
         ;; the prose window available as the occurrence preview target.
         (from-list (derived-mode-p 'tagaria-mode))
         (origin-window (selected-window))
         (source-window (if from-list
                            (tagaria--ordinary-other-window origin-window)
                          origin-window))
         (list-point (with-current-buffer list-buffer
                       (copy-marker (point))))
         (buffer (tagaria--update-detail
                  root tag scan list-buffer)))
    (if from-list
        (switch-to-buffer buffer)
      (pop-to-buffer
       buffer
       '((display-buffer-reuse-window display-buffer-pop-up-window)
         (inhibit-same-window . t))))
    (with-current-buffer buffer
      (tagaria--detail-goto-property 'tagaria-section 'occurrences))
    (setq tagaria--occurrence-preview-window source-window
          tagaria--detail-list-point list-point)
    (with-current-buffer buffer
      (tagaria--preview-current-occurrence))
    buffer))

;;;###autoload
(defun tagaria-mouse-show-occurrences (event)
  "Show references for the Tagaria tag clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'tagaria-show-occurrences))

(defun tagaria-mouse-visit-occurrence (event)
  "Visit the Tagaria occurrence clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (tagaria-visit-occurrence))

(defun tagaria-show-related-at-point ()
  "Show the related tag at point in the same Tagaria window."
  (interactive)
  (let ((tag (tagaria--text-property-at-point 'tagaria-related-tag)))
    (unless tag
      (user-error "No related Tagaria tag at point"))
    (if (derived-mode-p 'tagaria-mode)
        (tagaria-show-occurrences tag)
      (tagaria--update-detail tagaria--buffer-root tag)
      (tagaria--detail-goto-property 'tagaria-section 'occurrences))))

(defun tagaria-mouse-show-related (event)
  "Show the related tag clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (tagaria-show-related-at-point))

(defun tagaria-detail-activate ()
  "Edit the description or follow the relation or occurrence at point."
  (interactive)
  ;; These text-property zones are the same actions used by mouse-1.  The
  ;; occurrence fallback keeps a physical row as the navigation unit.
  (let ((description
         (tagaria--text-property-at-point 'tagaria-description-tag))
        (related (tagaria--text-property-at-point 'tagaria-related-tag)))
    (cond
     (description (tagaria-edit-desc tagaria--detail-tag))
     (related (tagaria-show-related-at-point))
     (t (tagaria-visit-occurrence)))))

(defun tagaria-visit-occurrence ()
  "Visit the occurrence represented by the current row."
  (interactive)
  (unless (derived-mode-p 'tagaria-detail-mode)
    (user-error "Not in a Tagaria detail page"))
  (let ((occurrence (tagaria--occurrence-at-point)))
    (unless occurrence
      (user-error "No Tagaria occurrence on this line"))
    (tagaria--preview-occurrence occurrence)
    (let* ((window tagaria--occurrence-preview-window)
           (buffer (and (window-live-p window) (window-buffer window))))
      (unless (window-live-p window)
        (user-error "Unable to display the Tagaria occurrence"))
      (tagaria--cleanup-occurrence-preview)
      ;; Like color-rg's open-and-stay, RET enters the current Source window.
      (select-window window)
      (with-current-buffer buffer
        (tagaria--goto-occurrence-position occurrence)))))

(provide 'tagaria-ui)
;;; tagaria-ui.el ends here

;; Local Variables:
;; package-lint-main-file: "tagaria.el"
;; End:
