;;; awa5x-mode.el --- Major mode for editing awa5x code   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Stark Night

;; Author: Stark Night <starknightawawa@gmail.com>
;; Keywords: languages
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
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

;; awa5x Mode is a major mode for editing awa5x programs.

;;; Code:

(require 'prog-mode)

(defgroup awa5x-mode nil
  "Major mode for editing awa5x code."
  :tag "awa5x Mode"
  :prefix "awa5x-mode-"
  :group 'languages)

(defface awa5x-mode-opener-face
  '((default :inherit font-lock-builtin-face)
    (t :inherit font-lock-builtin-face))
  "Face to highlight the opening 'awa'."
  :group 'awa5x-mode)

(defconst awa5x-mode-opener-face 'awa5x-mode-opener-face
  "Face name to use for the opening 'awa'.")

(defvar awa5x-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `awa5x-mode'.")

(defvar awa5x-mode-map
  (let ((map (make-sparse-keymap "awa5x")))
    map))

(defvar-local awa5x-mode--last-known-opener nil
  "Where the last known opening 'awa' was located at.")

(defun awa5x-mode--font-lock-open-tag (limit)
  "Apply Font Lock to the opening awa in a file."
  (if (and (not (null awa5x-mode--last-known-opener))
           (>= (point) awa5x-mode--last-known-opener))
      nil
    (let ((match nil))
      (while (and (setq match (re-search-forward "\\<awa\\>" limit t))
                  (nth 4 (syntax-ppss))))
      (when match
        (setq awa5x-mode--last-known-opener match))
      match)))

(defvar awa5x-mode--font-lock-defaults
  (list
   (list
    (list #'awa5x-mode--font-lock-open-tag 0 'awa5x-mode-opener-face))))

(define-derived-mode awa5x-mode prog-mode "awa5x"
  "Major mode for editing awa5x code.

\\{awa5x-mode-map}"
  :syntax-table awa5x-mode-syntax-table
  (setq-local case-fold-search nil)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local font-lock-defaults awa5x-mode--font-lock-defaults))

(provide 'awa5x-mode)
;;; awa5x.el ends here
