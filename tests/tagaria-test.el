;;; tagaria-test.el --- Tests for Tagaria -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Yilin Zhang

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; End-to-end and unit tests for Tagaria.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'tagaria)

(declare-function markdown-follow-thing-at-point "markdown-mode")
(declare-function markdown-mode "markdown-mode")

(defun tagaria-test--write-file (root relative contents &optional literal)
  "Write CONTENTS to RELATIVE below ROOT and return its absolute path.
When LITERAL is non-nil, write without coding conversion."
  (let ((path (expand-file-name relative root)))
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (when literal (set-buffer-multibyte nil))
      (insert contents)
      (let ((coding-system-for-write (and literal 'no-conversion)))
        (write-region (point-min) (point-max) path nil 'silent)))
    path))

(defun tagaria-test--file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun tagaria-test--kill-realm-buffers (root)
  "Kill file-visiting buffers below temporary ROOT."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-local-value 'buffer-file-name buffer))
          (realm (and (local-variable-p 'tagaria--buffer-root buffer)
                      (buffer-local-value 'tagaria--buffer-root buffer))))
      (when (or (and file (file-in-directory-p file root))
                (and realm (file-equal-p realm root)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defmacro tagaria-test-with-realm (&rest body)
  "Evaluate BODY with a fresh temporary Tagaria realm bound as `root'."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "tagaria-test-" t)))
          (tagaria-directory root)
          (tagaria-tag-regexp "@{\\([[:alnum:]_][[:alnum:]_./-]*\\)}")
          (tagaria-format-function (lambda (name) (format "@{%s}" name)))
          (tagaria-file-predicate #'tagaria-default-file-predicate)
          (tagaria-excluded-directory-names
           '(".git" ".hg" ".svn" ".bzr" "CVS")))
     (unwind-protect
         (progn ,@body)
       (tagaria-test--kill-realm-buffers root)
       (when (file-directory-p root)
         (delete-directory root t)))))

(ert-deftest tagaria-database-round-trips-description-and-relations ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" "First line\nSecond line" root)
    (tagaria-register "beta" nil root)
    (tagaria-register "alpha" "Replacement" root)
    (tagaria-set-related "alpha" "beta" t root)
    (should (equal (tagaria-desc "alpha" root)
                   "First line\nSecond line"))
    (should (equal (tagaria-related-tags "alpha" root) '("beta")))
    (should (equal (tagaria-related-tags "beta" root) '("alpha")))
    (should (equal (plist-get (tagaria--read-database root) :related)
                   '(("alpha" "beta"))))
    (should (file-exists-p (expand-file-name ".tagaria.eld" root)))
    (should (equal (mapcar #'car (tagaria-tags root)) '("alpha" "beta")))))

(ert-deftest tagaria-desc-must-be-a-string ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" "keep" root)
    (should-error (tagaria-set-desc "alpha" 5 root))
    (should (equal (tagaria-desc "alpha" root) "keep"))))

(ert-deftest tagaria-migrates-legacy-description-and-relations-with-a-backup ()
  (tagaria-test-with-realm
    (tagaria-test--write-file
     root ".tagaria.eld"
     (concat
      "(:version 1 :tags ((\"old\" :desc (\"line one\" \"line two\")"
      " :score 9 :related (\"third\")) (\"peer\") (\"third\"))"
      " :related ((\"old\" \"peer\") (\"peer\" \"old\")))"))
    (should-error (tagaria-tags root))
    (let* ((result (tagaria-migrate-database root))
           (backup (plist-get result :backup)))
      (should (file-exists-p
               (expand-file-name tagaria-database-file-name backup)))
      (should-not
       (plist-member (tagaria--read-one-form
                      (expand-file-name tagaria-database-file-name root))
                     :version))
      (should (equal (tagaria-desc "old" root)
                     "line one, line two"))
      (should (equal (tagaria-related-tags "old" root)
                     '("peer" "third")))
      (should (equal (plist-get result :dropped) '(:score :version)))
      (let ((second (tagaria-migrate-database root)))
        (should (plist-get second :current))
        (should-not (plist-get second :backup))))))

(ert-deftest tagaria-malformed-database-is-not-overwritten ()
  (tagaria-test-with-realm
    (let* ((path (tagaria-test--write-file
                  root ".tagaria.eld" "(:tags ((\"bad\" :field)))"))
           (before (tagaria-test--file-string path)))
      (should-error (tagaria-tags root))
      (should (string= before (tagaria-test--file-string path))))))

(ert-deftest tagaria-database-rejects-incomplete-trailing-form ()
  (tagaria-test-with-realm
    (tagaria-test--write-file
     root ".tagaria.eld" "(:tags nil) (")
    (should-error (tagaria-tags root))))

(ert-deftest tagaria-write-refuses-modified-database-buffer ()
  (tagaria-test-with-realm
    (tagaria-register "existing" nil root)
    (let* ((path (expand-file-name ".tagaria.eld" root))
           (before (tagaria-test--file-string path))
           (buffer (find-file-noselect path)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "\n;; unsaved manual edit"))
      (should-error (tagaria-register "new" nil root) :type 'user-error)
      (should (string= before (tagaria-test--file-string path)))
      (should (buffer-modified-p buffer)))))

(ert-deftest tagaria-sync-discovers-text-and-skips-excluded-content ()
  (tagaria-test-with-realm
    (tagaria-test--write-file root "one.org" "First @{alpha}\n")
    (tagaria-test--write-file root "sub/two.md" "@{beta} then @{alpha}\n")
    (tagaria-test--write-file root ".git/ignored.txt" "@{ignored}\n")
    (tagaria-test--write-file root "binary.dat"
                              (concat "before" (string 0) "@{binary}") t)
    (let* ((scan (tagaria-sync root))
           (table (tagaria-scan-occurrence-table scan)))
      (should (= (length (gethash "alpha" table)) 2))
      (should (= (length (gethash "beta" table)) 1))
      (should-not (gethash "ignored" table))
      (should-not (gethash "binary" table))
      (should (equal (mapcar #'car (tagaria-tags root))
                     '("alpha" "beta"))))))

(ert-deftest tagaria-custom-syntax-round-trips-through-rename ()
  (tagaria-test-with-realm
    (let ((tagaria-tag-regexp "\\[\\[tag:\\([^]]+\\)\\]\\]")
          (tagaria-format-function
           (lambda (name) (format "[[tag:%s]]" name))))
      (let ((file (tagaria-test--write-file
                   root "note.txt" "See [[tag:old name]].\n")))
        (tagaria-sync root)
        (tagaria-rename-tag "old name" "new name" root)
        (should (string= (tagaria-test--file-string file)
                         "See [[tag:new name]].\n"))))))

(ert-deftest tagaria-sync-keeps-zero-occurrence-tags ()
  (tagaria-test-with-realm
    (let ((file (tagaria-test--write-file root "note.txt" "@{lasting}\n")))
      (tagaria-sync root)
      (delete-file file)
      (tagaria-sync root)
      (should (assoc "lasting" (tagaria-tags root))))))

(ert-deftest tagaria-occurrence-deletion-cleans-text-boundaries ()
  (tagaria-test-with-realm
    (dolist (case '(("  @{x}  \nnext\n" . "next\n")
                    ("@{x} tail" . "tail")
                    ("head @{x}" . "head")
                    ("a  @{x}  b" . "a b")
                    ("a @{x}b" . "a b")
                    ("a@{x}b" . "ab")
                    ("中@{x}a" . "中 a")))
      (with-temp-buffer
        (insert (car case))
        (goto-char (point-min))
        (re-search-forward tagaria-tag-regexp)
        (tagaria--delete-occurrence-text
         (tagaria-occurrence-create
          :tag "x" :file "test"
          :start (match-beginning 1) :end (match-end 1)))
        (should (equal (buffer-string) (cdr case)))))))

(ert-deftest tagaria-bulk-deletion-can-preserve-or-remove-tag-data ()
  (tagaria-test-with-realm
    (let ((file (tagaria-test--write-file
                 root "note.txt" "A @{keep} B\n@{drop}\n")))
      (tagaria-sync root)
      (tagaria-set-desc "keep" "retain" root)
      (tagaria-set-desc "drop" "remove" root)
      (tagaria-set-related "keep" "drop" t root)
      (tagaria-delete-occurrences "keep" root)
      (should (equal (tagaria-test--file-string file) "A B\n@{drop}\n"))
      (should (equal (tagaria-desc "keep" root) "retain"))
      (tagaria-delete-tag "drop" root)
      (should (equal (tagaria-test--file-string file) "A B\n"))
      (should-not (assoc "drop" (tagaria-tags root)))
      (should-not (tagaria-related-tags "keep" root)))))

(ert-deftest tagaria-deletes-multiple-occurrences-in-one-file ()
  (tagaria-test-with-realm
    (let ((file (tagaria-test--write-file
                 root "note.txt" "one @{tag} two @{tag} three\n")))
      (tagaria-sync root)
      (should (= (plist-get (tagaria-delete-occurrences "tag" root) :count)
                 2))
      (should (equal (tagaria-test--file-string file)
                     "one two three\n")))))

(ert-deftest tagaria-scan-does-not-follow-symbolic-links ()
  (tagaria-test-with-realm
    (let ((outside (make-temp-file "tagaria-outside-" t)))
      (unwind-protect
          (progn
            (tagaria-test--write-file outside "outside.txt" "@{outside}\n")
            (make-symbolic-link outside (expand-file-name "linked" root))
            (let ((scan (tagaria-sync root)))
              (should-not
               (gethash "outside" (tagaria-scan-occurrence-table scan)))))
        (delete-directory outside t)))))

(ert-deftest tagaria-insert-registers-a-new-empty-tag ()
  (tagaria-test-with-realm
    (with-temp-buffer
      (setq default-directory root)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "new-tag")))
        (tagaria-insert))
      (should (string= (buffer-string) "@{new-tag}")))
    (should (equal (assoc "new-tag" (tagaria-tags root)) '("new-tag")))))

(ert-deftest tagaria-minor-mode-fontifies-and-makes-tags-clickable ()
  (tagaria-test-with-realm
    (with-temp-buffer
      (insert "See @{flow-matching} here.")
      (let ((unrelated-map (make-sparse-keymap)))
        (put-text-property (point-min) (1+ (point-min))
                           'keymap unrelated-map)
        (fundamental-mode)
        (font-lock-mode 1)
        (tagaria-minor-mode 1)
        (font-lock-ensure)
        (goto-char 10)
        (should (equal (tagaria--tag-at-point) "flow-matching"))
        (should (eq (lookup-key (get-text-property (point) 'keymap)
                                [mouse-1])
                    #'tagaria-mouse-show-occurrences))
        (should (eq (key-binding (kbd "C-c @"))
                    #'tagaria-show-occurrences))
        (tagaria-minor-mode -1)
        (should-not (get-text-property (point) 'tagaria-tag))
        (should (eq (get-text-property (point-min) 'keymap)
                    unrelated-map))))))

(ert-deftest tagaria-open-at-point-integrates-with-org ()
  (tagaria-test-with-realm
    (let (opened)
      (cl-letf (((symbol-function 'tagaria-show-occurrences)
                 (lambda (&optional tag) (setq opened tag))))
        (with-temp-buffer
          (org-mode)
          (insert "@{native-org}")
          (goto-char 5)
          (tagaria-minor-mode 1)
          (org-open-at-point)
          (should (equal opened "native-org"))
          (setq opened nil)
          (erase-buffer)
          (insert "@{alpha}[[https://example.org][link]]")
          (font-lock-ensure)
          (goto-char 9)
          (should-not (tagaria--org-open-at-point))
          (should-not opened))))))

(ert-deftest tagaria-open-at-point-integrates-with-markdown ()
  (skip-unless (require 'markdown-mode nil t))
  (tagaria-test-with-realm
    (with-temp-buffer
      (markdown-mode)
      (font-lock-mode 1)
      (insert "@{native-markdown} plain")
      (tagaria-minor-mode 1)
      (font-lock-ensure)
      (goto-char 5)
      (should (eq (key-binding (kbd "C-c C-o"))
                  #'tagaria-show-occurrences))
      (goto-char (point-max))
      (should (eq (key-binding (kbd "C-c C-o"))
                  #'markdown-follow-thing-at-point)))))

(ert-deftest tagaria-desc-buffer-editor-preserves-newlines ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" nil root)
    (save-window-excursion
      (tagaria-edit-desc-buffer "alpha")
      (erase-buffer)
      (insert "First line\nSecond line")
      (cl-letf (((symbol-function 'tagaria-set-desc)
                 (lambda (&rest _arguments) (error "Injected write failure"))))
        (should-error (tagaria-desc-edit-commit))
        (should (equal (buffer-string) "First line\nSecond line")))
      (tagaria-desc-edit-commit))
    (should (equal (tagaria-desc "alpha" root)
                   "First line\nSecond line"))))

(ert-deftest tagaria-create-prompts-for-realm-only-once ()
  (tagaria-test-with-realm
    (let ((tagaria-directory nil)
          (directory-prompts 0))
      (with-temp-buffer
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _arguments)
                     (setq directory-prompts (1+ directory-prompts))
                     root))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _arguments) "created")))
          (call-interactively #'tagaria-create)))
      (should (= directory-prompts 1))
      (should (assoc "created" (tagaria-tags root))))))

(ert-deftest tagaria-discovers-enclosing-realm-when-default-is-unset ()
  (tagaria-test-with-realm
    (tagaria-register "known" nil root)
    (let ((tagaria-directory nil)
          (default-directory (expand-file-name "nested/deeper/" root)))
      (make-directory default-directory t)
      (should (file-equal-p (tagaria--root) root)))))

(ert-deftest tagaria-programmatic-create-does-not-prompt-for-realm ()
  (let ((tagaria-directory nil)
        (default-directory
         (file-name-as-directory (make-temp-file "tagaria-no-realm-" t))))
    (unwind-protect
        (should-error (tagaria-create "orphan") :type 'user-error)
      (delete-directory default-directory t))))

(ert-deftest tagaria-rename-rewrites-files-description-and-relations ()
  (tagaria-test-with-realm
    (let ((first (tagaria-test--write-file
                  root "one.org" "A @{old-name} and @{other}.\n"))
          (second (tagaria-test--write-file
                   root "nested/two.md" "@{old-name} twice @{old-name}\n")))
      (let ((preview (tagaria-sync root)))
        (tagaria-set-desc "old-name" "shared" root)
        (tagaria-set-related "old-name" "other" t root)
        (let* ((occurrences
                (gethash "old-name" (tagaria-scan-occurrence-table preview)))
               (signature (tagaria--occurrence-signature occurrences))
               (result (tagaria-rename-tag
                        "old-name" "new-name" root signature)))
          (should (= (plist-get result :count) 3))
          (should (file-directory-p (plist-get result :backup)))
          (should (string-match-p "@{new-name}"
                                  (tagaria-test--file-string first)))
          (should-not (string-match-p "@{old-name}"
                                      (tagaria-test--file-string first)))
          (should (string= (tagaria-test--file-string second)
                           "@{new-name} twice @{new-name}\n"))
          (should (equal (tagaria-desc "new-name" root) "shared"))
          (should (equal (tagaria-related-tags "new-name" root) '("other")))
          (should-not (assoc "old-name" (tagaria-tags root))))))))

(ert-deftest tagaria-rename-refuses-stale-preview ()
  (tagaria-test-with-realm
    (let ((file (tagaria-test--write-file root "note.txt" "@{old}\n")))
      (let* ((scan (tagaria-sync root))
             (signature
              (tagaria--occurrence-signature
               (gethash "old" (tagaria-scan-occurrence-table scan)))))
        (tagaria-test--write-file root "note.txt" "prefix @{old}\n")
        (should-error (tagaria-rename-tag "old" "new" root signature)
                      :type 'user-error)
        (should (string= (tagaria-test--file-string file)
                         "prefix @{old}\n"))))))

(ert-deftest tagaria-rename-refuses-modified-visiting-buffer ()
  (tagaria-test-with-realm
    (let* ((file (tagaria-test--write-file root "note.txt" "@{old}\n"))
           (buffer (find-file-noselect file)))
      (tagaria-sync root)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "unsaved"))
      (should-error (tagaria-rename-tag "old" "new" root)
                    :type 'user-error)
      (should (string= (tagaria-test--file-string file) "@{old}\n")))))

(ert-deftest tagaria-interactive-rename-saves-after-y-or-n-confirmation ()
  (tagaria-test-with-realm
    (let* ((file (tagaria-test--write-file root "note.txt" "@{old}\n"))
           (buffer (find-file-noselect file)))
      (tagaria-sync root)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "@{old}\n"))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'display-buffer)
                 (lambda (&rest _arguments) nil)))
        (tagaria-rename "old" "new"))
      (should-not (buffer-modified-p buffer))
      (should (string= (tagaria-test--file-string file)
                       "@{new}\n@{new}\n")))))

(ert-deftest tagaria-rename-rolls-back-files-and-data-on-error ()
  (tagaria-test-with-realm
    (let ((first (tagaria-test--write-file root "a.txt" "@{old}\n"))
          (second (tagaria-test--write-file root "b.txt" "@{old}\n")))
      (tagaria-sync root)
      (tagaria-set-desc "old" "keep" root)
      (tagaria-register "peer" nil root)
      (tagaria-set-related "old" "peer" t root)
      (let ((original-rewrite (symbol-function 'tagaria--modify-file))
            (calls 0))
        (cl-letf (((symbol-function 'tagaria--modify-file)
                   (lambda (&rest arguments)
                     (setq calls (1+ calls))
                     (if (= calls 2)
                         (error "Injected rename failure")
                       (apply original-rewrite arguments)))))
          (should-error (tagaria-rename-tag "old" "new" root))))
      (should (string= (tagaria-test--file-string first) "@{old}\n"))
      (should (string= (tagaria-test--file-string second) "@{old}\n"))
      (should (equal (tagaria-desc "old" root) "keep"))
      (should (equal (tagaria-related-tags "old" root) '("peer")))
      (should (equal (tagaria-related-tags "peer" root) '("old")))
      (should-not (assoc "new" (tagaria-tags root))))))

(ert-deftest tagaria-rename-rollback-refreshes-visiting-database-buffer ()
  (tagaria-test-with-realm
    (let ((file (tagaria-test--write-file root "note.txt" "@{old}\n")))
      (tagaria-sync root)
      (tagaria-set-desc "old" "keep" root)
      (let* ((database (expand-file-name ".tagaria.eld" root))
             (buffer (find-file-noselect database))
             (original-write (symbol-function 'tagaria--write-database)))
        (cl-letf (((symbol-function 'tagaria--write-database)
                   (lambda (&rest arguments)
                     (apply original-write arguments)
                     (error "Injected post-database failure"))))
          (should-error (tagaria-rename-tag "old" "new" root)))
        (should (string= (tagaria-test--file-string file) "@{old}\n"))
        (with-current-buffer buffer
          (should (string-match-p "old" (buffer-string)))
          (should-not (string-match-p "\"new\"" (buffer-string))))))))

(ert-deftest tagaria-successful-renames-prune-old-backups ()
  (tagaria-test-with-realm
    (let ((tagaria-backup-keep 2))
      (tagaria-test--write-file root "note.txt" "@{one}\n")
      (tagaria-sync root)
      (tagaria-rename-tag "one" "two" root)
      (tagaria-rename-tag "two" "three" root)
      (tagaria-rename-tag "three" "four" root)
      (should (= (length
                  (directory-files
                   (expand-file-name tagaria-backup-directory-name root)
                   nil directory-files-no-dot-files-regexp))
                 2)))))

(ert-deftest tagaria-list-and-detail-navigation-share-one-window ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" "Example" root)
    (tagaria-register "beta" nil root)
    (tagaria-test--write-file root "note.txt" "@{alpha}\n@{beta}\n")
    (let* ((scan (tagaria-sync root))
           (list-buffer (tagaria--prepare-list-buffer root scan)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer list-buffer)
            ;; Exercise the data-only refresh used after relation edits.
            (tagaria-set-related "alpha" "beta" t root)
            (tagaria--refresh-list-buffers root)
            (tagaria--goto-tag-row "alpha")
            (let ((tagaria-window (selected-window)) edited)
              (cl-letf (((symbol-function 'tagaria-edit-desc)
                         (lambda (tag) (setq edited tag))))
                (re-search-forward "Example" (line-end-position))
                (tagaria-list-activate))
              (should (equal edited "alpha"))
              (tagaria--goto-tag-row "alpha")
              (re-search-forward "beta" (line-end-position))
              (tagaria-list-activate)
              (should (eq (selected-window) tagaria-window))
              (should (equal tagaria--detail-tag "beta"))
              (tagaria-detail-up)
              (tagaria--goto-tag-row "alpha")
              (tagaria-list-activate)
              (should (eq (selected-window) tagaria-window))
              (should (equal tagaria--detail-tag "alpha"))
              (tagaria-detail-up)
              (should (eq (current-buffer) list-buffer))
              (should (equal (tabulated-list-get-id) "alpha"))))
        (when (buffer-live-p list-buffer) (kill-buffer list-buffer))))))

(ert-deftest tagaria-detail-refresh-preserves-folded-sections ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" "Example" root)
    (let* ((file (tagaria-test--write-file root "note.txt" "@{alpha}\n"))
           (scan (tagaria-sync root))
           (list-buffer (tagaria--prepare-list-buffer root scan)))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer list-buffer)
            (tagaria--goto-tag-row "alpha")
            (tagaria-list-activate)
            (goto-char (point-min))
            (re-search-forward "Description$")
            (beginning-of-line)
            (tagaria-detail-toggle-section)
            (with-current-buffer (find-file-noselect file)
              (erase-buffer)
              (insert "@{alpha}\n@{alpha}\n@{alpha}\n")
              (save-buffer))
            (tagaria-detail-refresh)
            (should (string-match-p "Occurrences (3)" (buffer-string)))
            (goto-char (point-min))
            (re-search-forward "Description$")
            (forward-line 1)
            (should (outline-invisible-p (point))))
        (when (buffer-live-p list-buffer) (kill-buffer list-buffer))))))

(ert-deftest tagaria-list-delete-selects-nearest-row ()
  (tagaria-test-with-realm
    (tagaria-register "alpha" nil root)
    (tagaria-register "beta" nil root)
    (let ((list-buffer (tagaria--prepare-list-buffer root (tagaria-sync root))))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer list-buffer)
            (tagaria--goto-tag-row "beta")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _arguments) t)))
              (tagaria-delete))
            (should (equal (tabulated-list-get-id) "alpha")))
        (when (buffer-live-p list-buffer) (kill-buffer list-buffer))))))

(ert-deftest tagaria-detail-previews-and-visits-in-other-window ()
  (tagaria-test-with-realm
    (tagaria-register "shared" "Common topic" root)
    (let* ((_first (tagaria-test--write-file root "a.txt" "@{shared}\n"))
           (second (tagaria-test--write-file root "b.txt" "@{shared}\n"))
           (scan (tagaria-sync root))
           (list-buffer (tagaria--prepare-list-buffer root scan))
           (origin (generate-new-buffer " *tagaria-origin-test*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer origin)
            (let ((origin-window (selected-window)))
              (let ((detail (tagaria--open-detail
                             root "shared" scan list-buffer)))
                (should (derived-mode-p 'tagaria-detail-mode))
                (should-not (eq (selected-window) origin-window))
                (tagaria-detail-next-line 2)
                (tagaria--preview-current-occurrence)
                (should (eq (selected-window)
                            (get-buffer-window (current-buffer))))
                (should (file-equal-p
                         (buffer-file-name (window-buffer origin-window))
                         second))
                (let ((detail-window (selected-window)))
                  (tagaria-visit-occurrence)
                  (should (eq (selected-window) origin-window))
                  (should (file-equal-p buffer-file-name second))
                  (should (eq (window-buffer detail-window) detail))
                  (select-window detail-window)
                  (tagaria-detail-up)
                  (should (eq (window-buffer detail-window) list-buffer))))))
        (when (buffer-live-p origin) (kill-buffer origin))
        (when (buffer-live-p list-buffer) (kill-buffer list-buffer))))))

(ert-deftest tagaria-occurrence-position-widens-visiting-buffer ()
  (tagaria-test-with-realm
    (let* ((file (tagaria-test--write-file
                  root "note.txt" "@{first}\nsecond\n"))
           (scan (tagaria-scan-realm root))
           (occurrence
            (car (gethash "first" (tagaria-scan-occurrence-table scan))))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (point-max))
        (tagaria--goto-occurrence-position occurrence)
        (should-not (buffer-narrowed-p))
        (should (= (line-number-at-pos) 1))))))

(ert-deftest tagaria-occurrence-position-reveals-folded-org-context ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\nBody @{tag}\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)
    (tagaria--goto-occurrence-position
     (tagaria-occurrence-create :tag "tag" :line 3 :column 5))
    (should (looking-at-p (regexp-quote "@{tag}")))
    (should-not (invisible-p (point)))))

(ert-deftest tagaria-occurrence-position-reveals-folded-markdown-entry ()
  (skip-unless (require 'markdown-mode nil t))
  (with-temp-buffer
    (markdown-mode)
    (insert "# Parent\n\n## Child\n\nBody @{tag}\n")
    (goto-char (point-min))
    (outline-hide-subtree)
    (tagaria--goto-occurrence-position
     (tagaria-occurrence-create :tag "tag" :line 5 :column 5))
    (should (looking-at-p (regexp-quote "@{tag}")))
    (should-not (invisible-p (point)))))

(ert-deftest tagaria-list-buffers-are-scoped-by-realm ()
  (tagaria-test-with-realm
    (let ((other-root
           (file-name-as-directory (make-temp-file "tagaria-other-" t))))
      (unwind-protect
          (save-window-excursion
            (tagaria-register "first" nil root)
            (tagaria-register "second" nil other-root)
            (let ((first (tagaria--open-list root))
                  (second (tagaria--open-list other-root)))
              (should-not (eq first second))
              (with-current-buffer first
                (goto-char (point-min))
                (should (equal (tabulated-list-get-id) "first")))
              (with-current-buffer second
                (goto-char (point-min))
                (should (equal (tabulated-list-get-id) "second")))
              (kill-buffer first)
              (kill-buffer second)))
        (when (file-directory-p other-root)
          (delete-directory other-root t))))))

(provide 'tagaria-test)

;;; tagaria-test.el ends here
