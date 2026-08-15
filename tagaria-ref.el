;;; tagaria-ref.el --- Tag references for Tagaria -*- lexical-binding: t; -*-

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

;; This module owns inline tag navigation and the focused tag detail page.
;;
;; Window contract:
;; - Tagaria owns exactly one UI window.  List RET replaces List with Tag view
;;   in that window; `tagaria-detail-up' replaces Tag view with List again.
;; - Source preview uses another ordinary window following color-rg's model.
;;   Preview changes that window's buffer and position, then leaves focus in
;;   Tagaria.  If no other window exists, standard `display-buffer' creates it.
;; - Occurrence RET or mouse-1 selects the existing Source window.
;; - `q' delegates restoration of the Tagaria window to `quit-window'.

;;; Code:

(require 'cl-lib)
(require 'fringe)
(require 'outline)
(require 'seq)
(require 'subr-x)
(require 'tagaria-core)
(require 'tagaria-ui)

(declare-function tagaria-show-occurrences "tagaria")
(declare-function tagaria-delete "tagaria")
(declare-function tagaria-delete-occurrence "tagaria")
(declare-function tagaria-edit-desc "tagaria")
(declare-function tagaria-edit-desc-buffer "tagaria")
(declare-function tagaria-add-related-tag "tagaria")
(declare-function tagaria-delete-related-tag "tagaria")
(declare-function tagaria-rename "tagaria")
(declare-function tagaria--render-tag-page "tagaria-ui")

(defvar tagaria-detail-buffer-name)

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

(defun tagaria--text-tag-at-point ()
  "Return the textual Tagaria tag at point, or nil."
  (let ((position (point)))
    (save-excursion
      (goto-char (line-beginning-position))
      (cl-loop while (re-search-forward tagaria-tag-regexp
                                        (line-end-position) t)
               when (<= (match-beginning 0)
                        position
                        (1- (match-end 0)))
               return (match-string-no-properties 1)))))

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

(defun tagaria--detail-occurrence-key (occurrence)
  "Return a stable display key for OCCURRENCE."
  (when occurrence
    (list (tagaria-occurrence-file occurrence)
          (tagaria-occurrence-start occurrence)
          (tagaria-occurrence-end occurrence))))

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
      (list 'occurrence (tagaria--detail-occurrence-key occurrence)))
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
        (when (equal key (tagaria--detail-occurrence-key occurrence))
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

(defun tagaria--goto-occurrence-position (occurrence)
  "Move point to OCCURRENCE in the current buffer."
  (widen)
  (goto-char (point-min))
  (forward-line (1- (tagaria-occurrence-line occurrence)))
  (move-to-column (tagaria-occurrence-column occurrence)))

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
         (plist-get (cdr (assoc tag (tagaria-scan-entries scan))) :desc)
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

(provide 'tagaria-ref)
;;; tagaria-ref.el ends here

;; Local Variables:
;; package-lint-main-file: "tagaria.el"
;; End:
