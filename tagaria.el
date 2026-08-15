;;; tagaria.el --- Realm-based textual tag manager -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Yilin Zhang

;; Author: Yilin Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, files
;; URL: https://github.com/yilin-zhang/tagaria

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

;; Tagaria discovers user-defined textual tags below a realm directory and
;; associates each tag with an optional description and related tags.  It provides a
;; tabulated list with a shared detail page, minibuffer search and insertion,
;; description editing, relation management, and recoverable realm-wide renames.
;; Tagaria does not depend on Org or any other major mode.

;;; Code:

(require 'easymenu)
(require 'seq)
(require 'tabulated-list)
(require 'text-mode)
(require 'tagaria-core)
(require 'tagaria-ui)
(require 'tagaria-ref)

(defvar tagaria--detail-tag)

(declare-function tagaria--close-detail "tagaria-ref")
(declare-function tagaria--open-detail "tagaria-ref")
(declare-function tagaria--occurrence-at-point "tagaria-ref")
(declare-function tagaria--goto-occurrence-position "tagaria-ref")
(declare-function tagaria--refresh-detail-buffer "tagaria-ref")
(declare-function tagaria--retarget-detail "tagaria-ref")
(declare-function tagaria-show-related-at-point "tagaria-ref")
(declare-function tagaria--text-tag-at-point "tagaria-ref")

(defvar-local tagaria--scan nil
  "Most recent `tagaria-scan' represented by the current list buffer.")

(defvar-local tagaria--entry-filter #'identity
  "Predicate selecting entries shown in the current Tagaria list.")

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
    (when-let ((description (plist-get (cdr entry) :desc)))
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
         (description (plist-get (cdr entry) :desc))
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
  (let* ((entries (seq-filter
                   tagaria--entry-filter
                   (tagaria-scan-entries tagaria--scan)))
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

(defun tagaria--prepare-list-buffer
    (root &optional predicate buffer-name scan)
  "Return ROOT's initialized Tagaria list buffer.
PREDICATE and BUFFER-NAME describe an optional filtered list.  SCAN avoids
rescanning when the caller already has current data."
  (let ((buffer
         (get-buffer-create
          (tagaria--scoped-buffer-name
           (or buffer-name tagaria-buffer-name) root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tagaria-mode)
        (tagaria-mode))
      (setq tagaria--buffer-root root
            tagaria--entry-filter (or predicate #'identity))
      (setq default-directory root)
      (if scan
          (progn
            (setq tagaria--scan scan)
            (tagaria--render-entries)
            (tabulated-list-print t))
        (tabulated-list-revert)))
    buffer))

(defun tagaria--open-list (root &optional predicate buffer-name)
  "Open ROOT's Tagaria list filtered by PREDICATE in BUFFER-NAME."
  (let ((buffer (tagaria--prepare-list-buffer
                 root predicate buffer-name)))
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
            (tagaria--prepare-list-buffer root nil nil scan))))
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
         (list-buffer (tagaria--prepare-list-buffer
                       root nil nil scan)))
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
         (scan (tagaria-sync root))
         (occurrences
          (gethash old (tagaria-scan-occurrence-table scan))))
    (when (tagaria--save-modified-occurrence-buffers occurrences "rename")
      (setq scan (tagaria-sync root)
            occurrences
            (gethash old (tagaria-scan-occurrence-table scan))))
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
         (scan (tagaria-sync root))
         (occurrences
          (gethash name (tagaria-scan-occurrence-table scan))))
    (if (null occurrences)
        (message "%s already has no textual references" name)
      (when (tagaria--save-modified-occurrence-buffers
             occurrences "reference deletion")
        (setq scan (tagaria-sync root)
              occurrences
              (gethash name (tagaria-scan-occurrence-table scan))))
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
         (scan (tagaria-sync root))
         (occurrences
          (gethash name (tagaria-scan-occurrence-table scan))))
    (when (tagaria--save-modified-occurrence-buffers
           occurrences "tag deletion")
      (setq scan (tagaria-sync root)
            occurrences
            (gethash name (tagaria-scan-occurrence-table scan))))
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

(provide 'tagaria)

;;; tagaria.el ends here
