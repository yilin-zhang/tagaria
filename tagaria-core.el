;;; tagaria-core.el --- Storage and scanning for Tagaria -*- lexical-binding: t; -*-

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

;; Internal storage, scanning, and mutation primitives for Tagaria.

;;; Code:

(require 'cl-lib)
(require 'lisp-mode)
(require 'pp)
(require 'seq)
(require 'subr-x)

(defgroup tagaria nil
  "Manage textual tags inside directory-backed realms."
  :group 'convenience
  :prefix "tagaria-")

(defcustom tagaria-directory nil
  "Directory used as the default Tagaria realm.
When nil, Tagaria first looks for an enclosing realm, then prompts when needed."
  :type '(choice (const :tag "Prompt when needed" nil)
                 directory)
  :group 'tagaria)

(put 'tagaria-directory 'safe-local-variable #'stringp)

(defcustom tagaria-tag-regexp "@{\\([[:alnum:]_][[:alnum:]_./-]*\\)}"
  "Regexp used to find Tagaria tags.
The first capture group must contain the tag's database name."
  :type 'regexp
  :group 'tagaria)

(defcustom tagaria-format-function
  (lambda (name) (format "@{%s}" name))
  "Function used to turn a tag NAME into text for insertion.
The result must match `tagaria-tag-regexp', with NAME in capture group one."
  :type 'function
  :group 'tagaria)

(defcustom tagaria-database-file-name ".tagaria.eld"
  "File name used for Tagaria data inside each realm."
  :type 'string
  :group 'tagaria)

(defcustom tagaria-backup-directory-name ".tagaria-backups"
  "Directory name used for recoverable rename backups."
  :type 'string
  :group 'tagaria)

(defcustom tagaria-backup-keep 10
  "Number of successful rename backups to retain per realm.
Values below one are treated as one so the most recent rename stays
recoverable.  Nil keeps every backup."
  :type '(choice integer (const :tag "Keep every backup" nil))
  :group 'tagaria)

(defcustom tagaria-excluded-directory-names
  '(".git" ".hg" ".svn" ".bzr" "CVS")
  "Directory base names excluded from recursive scans."
  :type '(repeat string)
  :group 'tagaria)

(defcustom tagaria-file-predicate #'tagaria-default-file-predicate
  "Predicate deciding whether an absolute file name should be scanned.
Tagaria already excludes its database, excluded directories, symlinks, and
non-regular files before calling this function."
  :type 'function
  :group 'tagaria)

(defcustom tagaria-context-width 240
  "Maximum number of characters retained for an occurrence context."
  :type 'natnum
  :group 'tagaria)

(defcustom tagaria-delete-separator-function
  #'tagaria-default-delete-separator
  "Function deciding what separates characters exposed by tag deletion.
The function receives the characters immediately before and after a deleted
tag and returns a string, normally either an empty string or one space."
  :type 'function
  :group 'tagaria)

(defun tagaria--check-tag-regexp ()
  "Ensure `tagaria-tag-regexp' has capture group one."
  (unless (> (regexp-opt-depth tagaria-tag-regexp) 0)
    (user-error "Tagaria regexp must contain capture group one: %s"
                tagaria-tag-regexp)))

(cl-defstruct (tagaria-occurrence
               (:constructor tagaria-occurrence-create))
  "One textual occurrence of a Tagaria tag."
  tag file start end line column context)

(cl-defstruct (tagaria-scan (:constructor tagaria-scan-create))
  "Results from scanning one Tagaria realm."
  root occurrence-table fingerprints entries relations)

(cl-defstruct (tagaria--migration-entry
               (:constructor tagaria--migration-entry-create))
  "One converted legacy tag entry and its migration notes."
  entry related dropped)

(cl-defstruct (tagaria--migration-plan
               (:constructor tagaria--migration-plan-create))
  "A validated database migration ready to be written."
  database tag-count dropped)

(defun tagaria--find-enclosing-realm ()
  "Return the nearest Tagaria realm containing the current buffer, or nil."
  (let* ((location (or buffer-file-name default-directory))
         (directory (if (file-directory-p location)
                        location
                      (file-name-directory location))))
    (locate-dominating-file directory tagaria-database-file-name)))

(defun tagaria--root (&optional directory)
  "Return canonical realm root for DIRECTORY or `tagaria-directory'."
  (let ((candidate (or directory tagaria-directory
                       (tagaria--find-enclosing-realm))))
    (unless candidate
      (user-error "No Tagaria directory is configured"))
    (unless (file-directory-p candidate)
      (user-error "Tagaria directory does not exist: %s" candidate))
    (file-name-as-directory (file-truename candidate))))

(defun tagaria--realm-path (root name what)
  "Return NAME expanded inside ROOT, describing it as WHAT on error.
NAME must be a plain base name so that it cannot escape ROOT."
  (unless (equal name (file-name-nondirectory name))
    (error "Tagaria %s must not contain directories: %s" what name))
  (expand-file-name name root))

(defun tagaria--database-path (root)
  "Return the Tagaria database path inside ROOT."
  (tagaria--realm-path root tagaria-database-file-name "database name"))

(defun tagaria--backup-root (root)
  "Return the backup directory path inside ROOT."
  (tagaria--realm-path root tagaria-backup-directory-name "backup name"))

(defun tagaria--empty-database ()
  "Return a new empty Tagaria database value."
  (list :tags nil :related nil))

(defun tagaria--desc-data-p (data)
  "Return non-nil when DATA contain only an optional string description."
  (or (null data)
      (and (equal (proper-list-p data) 2)
           (eq (car data) :desc)
           (stringp (cadr data)))))

(defun tagaria--canonical-edge (first second)
  "Return the undirected FIRST/SECOND edge in canonical order."
  (if (string-lessp first second)
      (list first second)
    (list second first)))

(defun tagaria--edge-less-p (left right)
  "Return non-nil when edge LEFT belongs before edge RIGHT."
  (or (string-lessp (car left) (car right))
      (and (string= (car left) (car right))
           (string-lessp (cadr left) (cadr right)))))

(defun tagaria--validate-related (related tags path)
  "Validate RELATED edges against TAGS read from PATH and return them sorted."
  (unless (proper-list-p related)
    (error "Tagaria :related must be a proper list in %s" path))
  (let ((known (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        edges)
    (dolist (entry tags)
      (puthash (car entry) t known))
    (dolist (relation related)
      (unless (and (equal (proper-list-p relation) 2)
                   (stringp (car relation))
                   (stringp (cadr relation))
                   (gethash (car relation) known)
                   (gethash (cadr relation) known)
                   (not (string= (car relation) (cadr relation))))
        (error "Malformed Tagaria relation in %s: %S" path relation))
      (let ((edge (tagaria--canonical-edge (car relation) (cadr relation))))
        (when (gethash edge seen)
          (error "Duplicate Tagaria relation in %s: %S" path edge))
        (puthash edge t seen)
        (push edge edges)))
    (sort edges #'tagaria--edge-less-p)))

(defun tagaria--related-tags-in (name relations)
  "Return tags related to NAME in undirected RELATIONS."
  (sort
   (cl-loop for (first second) in relations
            if (string= name first) collect second
            else if (string= name second) collect first)
   #'string-lessp))

(defun tagaria--relation-index (relations)
  "Return a neighbor index derived from undirected RELATIONS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (edge relations)
      (push (cadr edge) (gethash (car edge) table))
      (push (car edge) (gethash (cadr edge) table)))
    (maphash (lambda (name neighbors)
               (puthash name (sort neighbors #'string-lessp) table))
             table)
    table))

(defun tagaria--validate-database (database path)
  "Validate DATABASE read from PATH and return it.
Signal an error without modifying PATH when the value is malformed."
  (unless (and (proper-list-p database)
               (plist-member database :tags))
    (error "Malformed Tagaria database in %s" path))
  (let ((tags (plist-get database :tags))
        (names (make-hash-table :test #'equal)))
    (unless (proper-list-p tags)
      (error "Tagaria :tags must be a proper list in %s" path))
    (dolist (entry tags)
      (unless (and (consp entry)
                   (stringp (car entry))
                   (not (string-empty-p (car entry))))
        (error "Malformed Tagaria entry in %s: %S" path entry))
      (unless (tagaria--desc-data-p (cdr entry))
        (error (concat "Tagaria entries may contain only a string :desc in "
                       "%s: %S.  Run M-x tagaria-migrate-database")
               path (cdr entry)))
      (when (gethash (car entry) names)
        (error "Duplicate Tagaria tag in %s: %s" path (car entry)))
      (puthash (car entry) t names)))
  (let ((tags (plist-get database :tags)))
    (list :tags tags
          :related (tagaria--validate-related
                    (or (plist-get database :related) nil) tags path))))

(defun tagaria--read-single-form (source)
  "Read the only Lisp form in the current buffer, named by SOURCE."
  (goto-char (point-min))
  (with-syntax-table lisp-data-mode-syntax-table
    (forward-comment (buffer-size))
    (if (eobp)
        (error "No Lisp form in %s" source)
      (let ((value
             (condition-case nil
                 (read (current-buffer))
               (end-of-file
                (error "Incomplete Lisp form in %s" source)))))
        (forward-comment (buffer-size))
        (unless (eobp)
          (error "More than one or an incomplete Lisp form in %s" source))
        value))))

(defun tagaria--read-one-form (path)
  "Read and return the single Lisp form stored at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (tagaria--read-single-form path)))

(defun tagaria--read-database (root)
  "Read and validate the Tagaria database in ROOT."
  (let ((path (tagaria--database-path root)))
    (if (file-exists-p path)
        (condition-case err
            (tagaria--validate-database (tagaria--read-one-form path) path)
          (error
           (error "Cannot read Tagaria database %s: %s"
                  path (error-message-string err))))
      (tagaria--empty-database))))

(defun tagaria--write-database (root database)
  "Atomically write DATABASE inside ROOT and return DATABASE."
  (let* ((path (tagaria--database-path root))
         (visiting-buffer (find-buffer-visiting path)))
    (when (and visiting-buffer
               (buffer-modified-p visiting-buffer))
      (user-error "Save or revert the modified Tagaria database buffer first"))
    (let ((temporary (make-temp-file
                      (expand-file-name ".tagaria-write-" root)
                      nil ".eld")))
      (unwind-protect
          (progn
            (with-temp-buffer
              (let ((print-circle t)
                    (coding-system-for-write 'utf-8-unix))
                (insert ";; Tagaria data.  Descriptions may contain newlines.\n")
                (pp database (current-buffer))
                (write-region (point-min) (point-max) temporary nil 'silent)))
            (rename-file temporary path t)
            (setq temporary nil)
            (when (buffer-live-p visiting-buffer)
              (with-current-buffer visiting-buffer
                (revert-buffer t t)))
            database)
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun tagaria--sort-entries (entries)
  "Return a copy of ENTRIES sorted by tag name."
  (seq-sort-by #'car #'string-lessp entries))

(defun tagaria--count-phrase (count singular &optional plural)
  "Return COUNT followed by SINGULAR or its optional PLURAL form."
  (format "%d %s" count
          (if (= count 1) singular (or plural (concat singular "s")))))

(defun tagaria--normalize-desc (description)
  "Return DESCRIPTION as a string or nil, treating an empty string as nil."
  (unless (or (null description) (stringp description))
    (error "Tagaria description must be a string or nil: %S" description))
  (and description (not (string-empty-p description)) description))

(defun tagaria--entry (database name)
  "Return NAME's entry in DATABASE, or nil."
  (assoc name (plist-get database :tags)))

(defun tagaria--set-entry (database name description)
  "Set NAME to optional string DESCRIPTION in DATABASE and return DATABASE."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "Invalid Tagaria tag name: %S" name))
  (setq description (tagaria--normalize-desc description))
  (let* ((tags (plist-get database :tags))
         (entry (assoc name tags)))
    (if entry
        (setcdr entry (and description (list :desc description)))
      (push (cons name (and description (list :desc description))) tags))
    (plist-put database :tags (tagaria--sort-entries tags))))

(defun tagaria--delete-entry (database name)
  "Delete NAME from DATABASE and return DATABASE."
  (setq database
        (plist-put database :tags
                   (seq-remove (lambda (entry) (string= (car entry) name))
                               (plist-get database :tags))))
  (plist-put
   database :related
   (seq-remove (lambda (edge) (member name edge))
               (plist-get database :related))))

(defun tagaria-tags (&optional directory)
  "Return all tag entries for DIRECTORY's realm."
  (copy-tree
   (plist-get (tagaria--read-database (tagaria--root directory)) :tags)))

(defun tagaria--entry-desc (entry)
  "Return ENTRY's description, or nil."
  (plist-get (cdr entry) :desc))

(defun tagaria--require-entry (database name)
  "Return NAME's entry in DATABASE, or signal that NAME is unknown."
  (or (tagaria--entry database name)
      (user-error "Unknown Tagaria tag: %s" name)))

(defun tagaria-desc (name &optional directory)
  "Return NAME's description in DIRECTORY, or nil."
  (when-let ((entry (tagaria--entry
                     (tagaria--read-database (tagaria--root directory)) name)))
    (tagaria--entry-desc entry)))

(defun tagaria-set-desc (name description &optional directory)
  "Set NAME's optional string DESCRIPTION in DIRECTORY and return it."
  (let* ((description (tagaria--normalize-desc description))
         (root (tagaria--root directory))
         (database (tagaria--read-database root)))
    (unless (equal description
                   (tagaria--entry-desc
                    (tagaria--require-entry database name)))
      (tagaria--write-database
       root (tagaria--set-entry database name description)))
    description))

(defun tagaria-register (name &optional description directory)
  "Register NAME with optional DESCRIPTION inside DIRECTORY.
An existing description is preserved.  Return NAME."
  (let* ((root (tagaria--root directory))
         (database (tagaria--read-database root)))
    (unless (tagaria--entry database name)
      (tagaria--write-database
       root (tagaria--set-entry database name description)))
    name))

(defun tagaria-related-tags (name &optional directory)
  "Return tags related to NAME in the realm at optional DIRECTORY."
  (tagaria--related-tags-in
   name (plist-get (tagaria--read-database (tagaria--root directory))
                   :related)))

(defun tagaria--set-related-pair (database first second present)
  "Set the undirected FIRST/SECOND relation in DATABASE to PRESENT."
  (let* ((edge (tagaria--canonical-edge first second))
         (relations (delete edge (copy-tree (plist-get database :related)))))
    (plist-put database :related
               (sort (if present (cons edge relations) relations)
                     #'tagaria--edge-less-p))))

(defun tagaria-set-related (first second present &optional directory)
  "Set the undirected relation between FIRST and SECOND to PRESENT.
Operate on DIRECTORY's realm, defaulting to the configured one."
  (when (string= first second)
    (user-error "A Tagaria tag cannot be related to itself"))
  (let* ((root (tagaria--root directory))
         (database (tagaria--read-database root))
         (edge (tagaria--canonical-edge first second)))
    (dolist (name (list first second))
      (tagaria--require-entry database name))
    (unless (eq (and (member edge (plist-get database :related)) t)
                (and present t))
      (tagaria--write-database
       root (tagaria--set-related-pair database first second present)))
    present))

(defun tagaria--binary-file-p (file)
  "Test whether the beginning of FILE has a NUL byte."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 8192)
        (goto-char (point-min))
        (search-forward "\0" nil t))
    (file-error t)))

(defun tagaria-default-file-predicate (file)
  "Return non-nil when FILE appears to contain ordinary text."
  (and (file-readable-p file)
       (not (tagaria--binary-file-p file))))

(defun tagaria--excluded-directory-p (directory)
  "Return non-nil when DIRECTORY should not be traversed."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (or (string= name tagaria-backup-directory-name)
        (member name tagaria-excluded-directory-names))))

(defun tagaria--scan-files (root)
  "Return scan-worthy files below ROOT without following symlinks."
  (let ((database (tagaria--database-path root))
        (directories (list root))
        files)
    (while directories
      (let ((directory (pop directories)))
        (condition-case err
            (dolist (path
                     (directory-files
                      directory t directory-files-no-dot-files-regexp t))
              (condition-case path-error
                  (cond
                   ((file-symlink-p path))
                   ((file-directory-p path)
                    (unless (tagaria--excluded-directory-p path)
                      (push path directories)))
                   ((and (file-regular-p path)
                         (not (string= path database))
                         (funcall tagaria-file-predicate path))
                    (push path files)))
                (file-error
                 (message "Tagaria skipped %s: %s"
                          path (error-message-string path-error)))))
          (file-error
           (message "Tagaria skipped %s: %s"
                    directory (error-message-string err))))))
    (sort files #'string-lessp)))

(defun tagaria--file-fingerprint (file)
  "Return a compact size and modification-time fingerprint for FILE."
  (let ((attributes (file-attributes file 'string)))
    (list (file-attribute-size attributes)
          (file-attribute-modification-time attributes))))

(defun tagaria--occurrence-context (start)
  "Return a bounded context excerpt around START on its current line."
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (half (/ tagaria-context-width 2))
         (excerpt-start (max line-start (- start half)))
         (excerpt-end
          (min line-end (+ excerpt-start tagaria-context-width))))
    (setq excerpt-start
          (max line-start (- excerpt-end tagaria-context-width)))
    (string-trim
     (buffer-substring-no-properties excerpt-start excerpt-end))))

(defun tagaria--scan-current-buffer (file occurrence-table)
  "Scan the current buffer as FILE into OCCURRENCE-TABLE."
  (save-excursion
    (goto-char (point-min))
    (let ((line 1)
          (line-scan-position (point-min)))
      ;; Matches are ordered, so count only new lines since the prior match.
      (while (re-search-forward tagaria-tag-regexp nil t)
        (unless (and (match-beginning 1) (match-end 1))
          (error "Tagaria regexp has no first capture group: %s"
                 tagaria-tag-regexp))
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (name (match-string-no-properties 1)))
          (save-excursion
            (goto-char line-scan-position)
            (while (search-forward "\n" start t)
              (cl-incf line)))
          (setq line-scan-position start)
          (when (string-empty-p name)
            (error "Tagaria regexp captured an empty tag in %s" file))
          (let ((occurrence
                 (tagaria-occurrence-create
                  :tag name
                  :file file
                  :start start
                  :end end
                  :line line
                  :column (save-excursion
                            (goto-char start)
                            (current-column))
                  :context (tagaria--occurrence-context start))))
            (puthash name
                     (cons occurrence (gethash name occurrence-table))
                     occurrence-table)))))))

(defun tagaria--scan-file (file occurrence-table)
  "Scan FILE and add results to OCCURRENCE-TABLE."
  (if-let ((visiting-buffer (find-buffer-visiting file)))
      (with-current-buffer visiting-buffer
        (save-restriction
          (widen)
          (tagaria--scan-current-buffer file occurrence-table)))
    (with-temp-buffer
      (insert-file-contents file)
      (tagaria--scan-current-buffer file occurrence-table))))

(defun tagaria-scan-realm (&optional directory)
  "Scan DIRECTORY and return a `tagaria-scan' value.
This function does not mutate the Tagaria database."
  (tagaria--check-tag-regexp)
  (let* ((root (tagaria--root directory))
         (files (tagaria--scan-files root))
         (occurrences (make-hash-table :test #'equal))
         (fingerprints (make-hash-table :test #'equal)))
    (dolist (file files)
      (puthash file (tagaria--file-fingerprint file) fingerprints)
      (tagaria--scan-file file occurrences))
    (maphash (lambda (name values)
               (puthash name (nreverse values) occurrences))
             occurrences)
    (tagaria-scan-create
     :root root
     :occurrence-table occurrences
     :fingerprints fingerprints)))

(defun tagaria-sync (&optional directory)
  "Scan DIRECTORY, register newly discovered tags, and return the scan."
  (let* ((root (tagaria--root directory))
         (scan (tagaria-scan-realm root))
         (database (tagaria--read-database root))
         (known (make-hash-table :test #'equal))
         new-entries)
    (dolist (entry (plist-get database :tags))
      (puthash (car entry) t known))
    (maphash
     (lambda (name _occurrences)
       (unless (gethash name known)
         (push (cons name nil) new-entries)))
     (tagaria-scan-occurrence-table scan))
    (when new-entries
      (plist-put database :tags
                 (tagaria--sort-entries
                  (append new-entries (plist-get database :tags)))))
    (when (or new-entries
              (not (file-exists-p (tagaria--database-path root))))
      (tagaria--write-database root database))
    (tagaria--update-scan-data scan database)
    scan))

(defun tagaria--update-scan-data (scan database)
  "Update SCAN with the tag data in freshly read DATABASE and return SCAN."
  (setf (tagaria-scan-entries scan) (plist-get database :tags)
        ;; The file stores each undirected edge once.  Views use an adjacency
        ;; index because they repeatedly ask for one tag's neighbours.
        (tagaria-scan-relations scan)
        (tagaria--relation-index (plist-get database :related)))
  scan)

(defun tagaria--format-tag (name)
  "Return validated insertion text for NAME."
  (let ((rendered (funcall tagaria-format-function name)))
    (unless (stringp rendered)
      (user-error "Tagaria formatter did not return a string for %s" name))
    (unless (and (string-match tagaria-tag-regexp rendered)
                 (match-beginning 1)
                 (string= (match-string 1 rendered) name))
      (user-error "Formatted tag does not round-trip through the regexp: %s"
                  rendered))
    rendered))

(defun tagaria-default-delete-separator (left right)
  "Return a separator for adjacent LEFT and RIGHT characters.
Different Unicode scripts receive one space.  Line boundaries, punctuation,
and characters from the same script receive no separator."
  (if (or (null left) (null right)
          (eq (char-syntax left) ?.)
          (eq (char-syntax right) ?.)
          (eq (aref char-script-table left)
              (aref char-script-table right)))
      ""
    " "))

(defun tagaria--occurrence-text-bounds (occurrence)
  "Return full textual bounds for OCCURRENCE in the current buffer."
  (let ((text (tagaria--format-tag (tagaria-occurrence-tag occurrence))))
    (string-match tagaria-tag-regexp text)
    (let* ((start (- (tagaria-occurrence-start occurrence)
                     (match-beginning 1)))
           (end (+ start (length text))))
      (unless (and (<= (point-min) start end (point-max))
                   (string= text (buffer-substring-no-properties start end)))
        (error "Tagaria occurrence changed in %s"
               (tagaria-occurrence-file occurrence)))
      (cons start end))))

(defun tagaria--delete-occurrence-text (occurrence)
  "Delete OCCURRENCE from the current buffer with boundary cleanup."
  (pcase-let* ((`(,start . ,end)
                (tagaria--occurrence-text-bounds occurrence))
               (line-start (save-excursion
                             (goto-char start)
                             (line-beginning-position)))
               (line-end (save-excursion
                           (goto-char end)
                           (line-end-position)))
               (before (buffer-substring-no-properties line-start start))
               (after (buffer-substring-no-properties end line-end)))
    (if (and (string-match-p "\\`[[:blank:]]*\\'" before)
             (string-match-p "\\`[[:blank:]]*\\'" after))
        (delete-region
         (if (and (= line-end (point-max))
                  (> line-start (point-min)))
             (1- line-start)
           line-start)
         (min (point-max) (1+ line-end)))
      (let ((left-space-start
             (save-excursion
               (goto-char start)
               (skip-chars-backward " \t" line-start)
               (point)))
            (right-space-end
             (save-excursion
               (goto-char end)
               (skip-chars-forward " \t" line-end)
               (point))))
        (cond
         ((and (< left-space-start start) (> right-space-end end))
          (delete-region left-space-start right-space-end)
          (goto-char left-space-start)
          (insert " "))
         ((and (= start line-start) (> right-space-end end))
          (delete-region start right-space-end))
         ((and (= end line-end) (< left-space-start start))
          (delete-region left-space-start end))
         ((or (< left-space-start start) (> right-space-end end))
          (delete-region start end))
         (t
          (let ((separator
                 (funcall tagaria-delete-separator-function
                          (char-before start) (char-after end))))
            (unless (stringp separator)
              (error "Tagaria delete separator did not return a string"))
            (delete-region start end)
            (goto-char start)
            (insert separator)))))))
  1)

(defun tagaria--occurrence-key (occurrence)
  "Return the identity tuple of OCCURRENCE, or nil when it is nil."
  (when occurrence
    (list (tagaria-occurrence-file occurrence)
          (tagaria-occurrence-start occurrence)
          (tagaria-occurrence-end occurrence))))

(defun tagaria--occurrence-signature (occurrences)
  "Return a stable comparison signature for OCCURRENCES."
  (mapcar #'tagaria--occurrence-key occurrences))

(defun tagaria--occurrence-files (occurrences)
  "Return the unique files represented by OCCURRENCES."
  (delete-dups (mapcar #'tagaria-occurrence-file occurrences)))

(defun tagaria--modified-buffers-for-occurrences (occurrences)
  "Return modified visiting buffers for files in OCCURRENCES."
  (seq-filter #'buffer-modified-p
              (seq-keep #'find-buffer-visiting
                        (tagaria--occurrence-files occurrences))))

(defun tagaria--make-backup-directory (root)
  "Create and return a unique rename backup directory below ROOT."
  (let* ((parent (tagaria--backup-root root))
         (base (format-time-string "%Y%m%dT%H%M%S"))
         (candidate (expand-file-name base parent))
         (suffix 1))
    (make-directory parent t)
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name (format "%s-%d" base suffix) parent)
            suffix (1+ suffix)))
    (make-directory candidate)
    candidate))

(defun tagaria--backup-files (root files)
  "Back up FILES and the database from ROOT, returning the backup directory."
  (let ((backup (tagaria--make-backup-directory root))
        (database (tagaria--database-path root)))
    (dolist (file (delete-dups files))
      (let ((destination
             (expand-file-name (file-relative-name file root) backup)))
        (make-directory (file-name-directory destination) t)
        (copy-file file destination t t t t)))
    (when (file-exists-p database)
      (copy-file database
                 (expand-file-name tagaria-database-file-name backup)
                 t t t t))
    backup))

(defun tagaria--legacy-value-string (value)
  "Convert legacy description VALUE to a string."
  (cond
   ((stringp value) value)
   ((null value) "")
   ((proper-list-p value)
    (mapconcat #'tagaria--legacy-value-string value ", "))
   (t (format "%s" value))))

(defun tagaria--migrate-legacy-entry (entry path)
  "Return migration data for legacy ENTRY read at PATH."
  (unless (and (consp entry)
               (stringp (car entry))
               (not (string-empty-p (car entry))))
    (error "Malformed legacy Tagaria entry in %s: %S" path entry))
  (let* ((data (cdr entry))
         (length (proper-list-p data)))
    (unless (and length (cl-evenp length))
      (error "Malformed legacy Tagaria entry data in %s: %S" path data))
    (let* ((keys (cl-loop for (key _value) on data by #'cddr
                          unless (keywordp key)
                          do (error
                              "Legacy Tagaria field is not a keyword in %s: %S"
                              path key)
                          collect key))
           (description
            (and (plist-member data :desc)
                 (tagaria--legacy-value-string (plist-get data :desc))))
           (related-value (and (plist-member data :related)
                               (plist-get data :related)))
           (related (and (proper-list-p related-value)
                         (seq-filter #'stringp related-value)))
           (invalid-related
            (and (plist-member data :related)
                 (or (not (proper-list-p related-value))
                     (seq-some (lambda (value) (not (stringp value)))
                               related-value))))
           (dropped (seq-remove (lambda (key) (memq key '(:desc :related)))
                                keys)))
      (tagaria--migration-entry-create
       :entry (cons (car entry)
                    (and description
                         (not (string-empty-p description))
                         (list :desc description)))
       :related related
       :dropped (if invalid-related (cons :related dropped) dropped)))))

(defun tagaria--migration-edges (migration)
  "Return all per-entry relation edges described by MIGRATION."
  (cl-loop for item in migration
           for name = (car (tagaria--migration-entry-entry item))
           append (mapcar (lambda (other)
                            (tagaria--canonical-edge name other))
                          (tagaria--migration-entry-related item))))

(defun tagaria--legacy-top-level-edges (related path)
  "Convert legacy top-level RELATED adjacency data read at PATH to edges."
  (unless (proper-list-p related)
    (error "Legacy Tagaria :related must be a proper list in %s" path))
  (cl-loop
   for entry in related
   unless (and (consp entry)
               (stringp (car entry))
               (proper-list-p (cdr entry)))
   do (error "Malformed legacy Tagaria relation in %s: %S" path entry)
   append
   (mapcar
    (lambda (other)
      (unless (stringp other)
        (error "Malformed legacy Tagaria relation in %s: %S" path entry))
      (tagaria--canonical-edge (car entry) other))
    (cdr entry))))

(defun tagaria--valid-migration-edge-p (edge names)
  "Return non-nil when relation EDGE connects two distinct known NAMES."
  (and (member (car edge) names)
       (member (cadr edge) names)
       (not (equal (car edge) (cadr edge)))))

(defun tagaria--add-migration-edge (database edge)
  "Add relation EDGE to DATABASE and return the resulting database."
  (tagaria--set-related-pair database (car edge) (cadr edge) t))

(defun tagaria--current-database-p (database path)
  "Return non-nil when DATABASE already has the canonical format at PATH."
  (condition-case nil
      (equal database (tagaria--validate-database database path))
    (error nil)))

(defun tagaria--finish-migration-plan (path migrated edges dropped-fields)
  "Build a migration plan at PATH from converted data.
MIGRATED contains new entries, EDGES contains legacy relations, and
DROPPED-FIELDS records legacy keys that cannot be represented."
  (let* ((names (mapcar #'car migrated))
         (valid-edges
          (seq-filter (lambda (edge)
                        (tagaria--valid-migration-edge-p edge names))
                      edges))
         (dropped
          (sort (delete-dups
                 (if (= (length valid-edges) (length edges))
                     dropped-fields
                   (cons :related dropped-fields)))
                #'string-lessp))
         (base-database
          (tagaria--validate-database
           (list :tags migrated :related nil) path)))
    (tagaria--migration-plan-create
     :database (seq-reduce #'tagaria--add-migration-edge
                           valid-edges base-database)
     :tag-count (length migrated)
     :dropped dropped)))

(defun tagaria--migration-plan (database path)
  "Return a canonical migration plan for legacy DATABASE read at PATH."
  (unless (and (proper-list-p database) (plist-member database :tags))
    (error "Malformed legacy Tagaria database in %s" path))
  (let ((tags (plist-get database :tags))
        (top-level-fields
         (cl-loop for (key _value) on database by #'cddr
                  unless (memq key '(:tags :related))
                  collect key)))
    (unless (proper-list-p tags)
      (error "Legacy Tagaria :tags must be a proper list in %s" path))
    (let ((migration (mapcar (lambda (entry)
                               (tagaria--migrate-legacy-entry entry path))
                             tags)))
      (tagaria--finish-migration-plan
       path
       (mapcar #'tagaria--migration-entry-entry migration)
       (append (tagaria--legacy-top-level-edges
                (or (plist-get database :related) nil) path)
               (tagaria--migration-edges migration))
       (append top-level-fields
               (cl-loop for item in migration
                        append (tagaria--migration-entry-dropped item)))))))

;;;###autoload
(defun tagaria-migrate-database (&optional directory)
  "Migrate legacy data in DIRECTORY to descriptions and related tags.
The original database is copied to a timestamped backup before conversion."
  (interactive
   (list (read-directory-name
          "Migrate Tagaria realm: "
          (or tagaria-directory default-directory) nil t)))
  (let* ((root (tagaria--root directory))
         (path (tagaria--database-path root)))
    (unless (file-exists-p path)
      (user-error "No Tagaria database exists in %s" root))
    (let ((database (tagaria--read-one-form path)))
      (if (tagaria--current-database-p database path)
          (progn
            (message "Tagaria database is already current")
            (list :backup nil
                  :tags (length (plist-get database :tags))
                  :dropped nil
                  :current t))
        (let* ((plan (tagaria--migration-plan database path))
               (backup (tagaria--backup-files root nil))
               (dropped (tagaria--migration-plan-dropped plan)))
          (tagaria--write-database
           root (tagaria--migration-plan-database plan))
          (message "Migrated %d tags; dropped fields: %s; backup: %s"
                   (tagaria--migration-plan-tag-count plan)
                   (if dropped (mapconcat #'symbol-name dropped ", ") "none")
                   backup)
          (list :backup backup :tags (tagaria--migration-plan-tag-count plan)
                :dropped dropped :current nil))))))

(defun tagaria--prune-backups (root)
  "Keep only the newest `tagaria-backup-keep' backups below ROOT."
  (let ((parent (tagaria--backup-root root)))
    (when (and tagaria-backup-keep (file-directory-p parent))
      (let* ((keep (max 1 tagaria-backup-keep))
             (directories
              (seq-filter
               (lambda (path)
                 (and (file-directory-p path) (not (file-symlink-p path))))
               (directory-files parent t directory-files-no-dot-files-regexp)))
             (old (nthcdr keep (sort directories #'string-greaterp))))
        (dolist (directory old)
          (delete-directory directory t))))))

(defun tagaria--replace-name-in-buffer (old-name new-name)
  "Replace OLD-NAME tag captures with NEW-NAME in the current buffer.
Return the number of replacements."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward tagaria-tag-regexp nil t)
          (unless (match-beginning 1)
            (error "Tagaria regexp has no first capture group"))
          (when (string= (match-string-no-properties 1) old-name)
            (replace-match new-name t t nil 1)
            (setq count (1+ count))))))
    count))

(defun tagaria--apply-file-modification (file expected-count modify)
  "Run MODIFY in the current buffer; require EXPECTED-COUNT edits to FILE."
  (let ((count (funcall modify)))
    (unless (= count expected-count)
      (error "Tagaria occurrence count changed in %s" file))
    count))

(defun tagaria--modify-file (file expected-count modify)
  "Use MODIFY to edit FILE exactly EXPECTED-COUNT times.
MODIFY runs in a buffer containing FILE and returns its number of changes."
  (if-let ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (when (buffer-modified-p)
          (error "Refusing to overwrite modified buffer: %s" (buffer-name)))
        (prog1 (tagaria--apply-file-modification file expected-count modify)
          (save-buffer)))
    (with-temp-buffer
      (insert-file-contents file)
      (prog1 (tagaria--apply-file-modification file expected-count modify)
        (write-region (point-min) (point-max) file nil 'silent)))))

(defun tagaria--restore-backup (root backup files)
  "Restore FILES and the database in ROOT from BACKUP."
  (dolist (file (delete-dups files))
    (let ((source (expand-file-name (file-relative-name file root) backup)))
      (when (file-exists-p source)
        (copy-file source file t t t t))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (revert-buffer t t)))))
  (let ((database-backup
         (expand-file-name tagaria-database-file-name backup)))
    (when (file-exists-p database-backup)
      (let* ((database (tagaria--database-path root))
             (buffer (find-buffer-visiting database)))
        (copy-file database-backup database t t t t)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (revert-buffer t t)))))))

(defun tagaria--rename-occurrences
    (scan occurrences files old-name new-name)
  "Rewrite FILES in SCAN for OCCURRENCES from OLD-NAME to NEW-NAME."
  (let ((counts (make-hash-table :test #'equal))
        (total 0))
    (dolist (occurrence occurrences)
      (cl-incf (gethash (tagaria-occurrence-file occurrence) counts 0)))
    (dolist (file files)
      (unless (equal (gethash file (tagaria-scan-fingerprints scan))
                     (tagaria--file-fingerprint file))
        (error "File changed during Tagaria rename: %s" file))
      (cl-incf total
               (tagaria--modify-file
                file (gethash file counts)
                (lambda ()
                  (tagaria--replace-name-in-buffer old-name new-name)))))
    total))

(defun tagaria--delete-occurrences-in-buffer (occurrences)
  "Delete OCCURRENCES from the current buffer, from last to first."
  (let ((count 0))
    (dolist (occurrence
             (seq-sort-by #'tagaria-occurrence-start #'> occurrences))
      (cl-incf count (tagaria--delete-occurrence-text occurrence)))
    count))

(defun tagaria--delete-tag-transaction
    (name directory expected-signature remove-entry)
  "Delete NAME references in DIRECTORY and optionally REMOVE-ENTRY.
EXPECTED-SIGNATURE, when non-nil, must match the current references."
  (let* ((root (tagaria--root directory))
         (scan (tagaria-sync root))
         (occurrences
          (copy-sequence
           (gethash name (tagaria-scan-occurrence-table scan))))
         (signature (tagaria--occurrence-signature occurrences))
         (database (tagaria--read-database root)))
    (tagaria--require-entry database name)
    (when (and expected-signature
               (not (equal expected-signature signature)))
      (user-error "Tagaria occurrences changed after confirmation; retry"))
    (when-let ((buffers
                (tagaria--modified-buffers-for-occurrences occurrences)))
      (user-error "Modified buffers prevent deletion: %s"
                  (mapconcat #'buffer-name buffers ", ")))
    (let* ((files (tagaria--occurrence-files occurrences))
           (backup (and (or files remove-entry)
                        (tagaria--backup-files root files))))
      (condition-case err
          (let ((by-file (make-hash-table :test #'equal))
                (total 0))
            (dolist (occurrence occurrences)
              (push occurrence
                    (gethash (tagaria-occurrence-file occurrence) by-file)))
            (dolist (file files)
              (unless (equal (gethash file (tagaria-scan-fingerprints scan))
                             (tagaria--file-fingerprint file))
                (error "File changed during Tagaria deletion: %s" file))
              (let ((file-occurrences (gethash file by-file)))
                (cl-incf total
                         (tagaria--modify-file
                          file (length file-occurrences)
                          (lambda ()
                            (tagaria--delete-occurrences-in-buffer
                             file-occurrences))))))
            (when remove-entry
              (tagaria--write-database
               root (tagaria--delete-entry database name)))
            (when backup (tagaria--prune-backups-safely root))
            (list :count total :backup backup))
        (error
         (when backup
           (tagaria--restore-backup-safely root backup files))
         (signal (car err) (cdr err)))))))

(defun tagaria-delete-occurrences
    (name &optional directory expected-signature)
  "Delete every textual reference to NAME throughout DIRECTORY.
EXPECTED-SIGNATURE, when non-nil, guards the confirmed reference set."
  (tagaria--delete-tag-transaction
   name directory expected-signature nil))

(defun tagaria-delete-tag (name &optional directory expected-signature)
  "Delete NAME, its references, description, and relations in DIRECTORY.
EXPECTED-SIGNATURE, when non-nil, guards the confirmed reference set."
  (tagaria--delete-tag-transaction
   name directory expected-signature t))

(defun tagaria--rename-relations (relations old-name new-name)
  "Rename OLD-NAME to NEW-NAME in undirected RELATIONS."
  (sort
   (delete-dups
    (mapcar
     (lambda (edge)
       (tagaria--canonical-edge
        (if (string= (car edge) old-name) new-name (car edge))
        (if (string= (cadr edge) old-name) new-name (cadr edge))))
     relations))
   #'tagaria--edge-less-p))

(defun tagaria--rename-database-entry
    (root database old-name new-name)
  "Move OLD-NAME and its relations to NEW-NAME in DATABASE below ROOT."
  (let* ((old-entry (tagaria--entry database old-name))
         (description (tagaria--entry-desc old-entry))
         (relations (tagaria--rename-relations
                     (plist-get database :related) old-name new-name)))
    (setq database (tagaria--delete-entry database old-name))
    (setq database (tagaria--set-entry database new-name description))
    (setq database (plist-put database :related relations))
    (tagaria--write-database root database)))

(defun tagaria--prune-backups-safely (root)
  "Prune old backups below ROOT without failing a completed rename."
  (condition-case err
      (tagaria--prune-backups root)
    (error
     (message "Tagaria could not prune old backups: %s"
              (error-message-string err)))))

(defun tagaria--restore-backup-safely (root backup files)
  "Restore BACKUP for FILES below ROOT, reporting rollback failure."
  (condition-case err
      (tagaria--restore-backup root backup files)
    (error
     (message "Tagaria rollback also failed: %s"
              (error-message-string err)))))

(defun tagaria-rename-tag (old-name new-name &optional directory expected-signature)
  "Rename OLD-NAME to NEW-NAME throughout DIRECTORY.
EXPECTED-SIGNATURE, when non-nil, must equal the current occurrence signature.
Refuse modified visiting buffers.  Return a plist containing :count and
:backup."
  (let ((root (tagaria--root directory)))
    (when (string= old-name new-name)
      (user-error "The old and new Tagaria names are identical"))
    (tagaria--format-tag new-name)
    (let* ((scan (tagaria-sync root))
           (table (tagaria-scan-occurrence-table scan))
           (occurrences (copy-sequence (gethash old-name table)))
           (current-signature (tagaria--occurrence-signature occurrences))
           (database (tagaria--read-database root)))
      (tagaria--require-entry database old-name)
      (when (tagaria--entry database new-name)
        (user-error "Tagaria tag already exists: %s" new-name))
      (when (and expected-signature
                 (not (equal expected-signature current-signature)))
        (user-error "Tagaria occurrences changed after preview; retry rename"))
      (when-let ((buffers
                  (tagaria--modified-buffers-for-occurrences occurrences)))
        (user-error "Modified buffers prevent rename: %s"
                    (mapconcat #'buffer-name buffers ", ")))
      (let* ((files (tagaria--occurrence-files occurrences))
             (backup (tagaria--backup-files root files)))
        (condition-case err
            (let ((total
                   (tagaria--rename-occurrences
                    scan occurrences files old-name new-name)))
              (tagaria--rename-database-entry
               root database old-name new-name)
              (tagaria--prune-backups-safely root)
              (list :count total :backup backup))
          (error
           (tagaria--restore-backup-safely root backup files)
           (signal (car err) (cdr err))))))))

(provide 'tagaria-core)

;;; tagaria-core.el ends here

;; Local Variables:
;; package-lint-main-file: "tagaria.el"
;; End:
