;;; tagaria-ref.el --- Reference primitives for Tagaria -*- lexical-binding: t; -*-

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

;; Textual tag lookup and occurrence presentation primitives.  This module has
;; no modes, keymaps, windows, or commands; those belong to `tagaria-ui'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tagaria-core)

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

(defun tagaria--goto-occurrence-position (occurrence)
  "Move point to OCCURRENCE and reveal it in the current buffer."
  (widen)
  (goto-char (point-min))
  (forward-line (1- (tagaria-occurrence-line occurrence)))
  (move-to-column (tagaria-occurrence-column occurrence))
  ;; Let each markup mode reveal its own folds.  Keep these integrations
  ;; optional: loading Tagaria must not load Org or Markdown Mode.
  (when (invisible-p (point))
    (cond
     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-fold-show-context))
      (funcall 'org-fold-show-context 'link-search))
     ((and (derived-mode-p 'markdown-mode)
           (fboundp 'markdown-show-entry))
      (funcall 'markdown-show-entry)))))

(defun tagaria--tag-text-regexp (tag)
  "Return a regexp matching TAG's rendered text plus surrounding blanks."
  (concat "[[:blank:]]*" (regexp-quote (tagaria--format-tag tag))
          "[[:blank:]]*"))

(defun tagaria--occurrence-display-context (occurrence regexp)
  "Return OCCURRENCE context with REGEXP occurrences of its own tag removed."
  (string-trim
   (replace-regexp-in-string
    regexp " " (tagaria-occurrence-context occurrence) t)))

(provide 'tagaria-ref)
;;; tagaria-ref.el ends here

;; Local Variables:
;; package-lint-main-file: "tagaria.el"
;; End:
