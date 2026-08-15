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

;; Load Tagaria's storage, reference, and user-interface modules.

;;; Code:

(require 'tagaria-core)
(require 'tagaria-ref)
(require 'tagaria-ui)

(provide 'tagaria)
;;; tagaria.el ends here
