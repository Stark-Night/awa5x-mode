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

(defface awa5x-mode-preprocessor-face
  '((default :inherit font-lock-preprocessor-face)
    (t :inherit font-lock-preprocessor-face))
  "Face to highlight preprocessor directives."
  :group 'awa5x-mode)

(defconst awa5x-mode-opener-face 'awa5x-mode-opener-face
  "Face name to use for the opening 'awa'.")

(defconst awa5x-mode-preprocessor-face 'awa5x-mode-preprocessor-face
  "Face name to use for preprocessor directives.")

(defvar awa5x-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `awa5x-mode'.")

(defconst awa5x-mode--opcode-alist
  '((NOP . #x00) (PRN . #x01) (PR1 . #x02) (RED . #x03) (R3D . #x04)
    (BLO . #x05) (SBM . #x06) (POP . #x07) (DPL . #x08) (SRN . #x09)
    (MRG . #x0A) (4DD . #x0B) (SUB . #x0C) (MUL . #x0D) (DIV . #x0E)
    (CNT . #x0F) (LBL . #x10) (JMP . #x11) (EQL . #x12) (LSS . #x13)
    (GR8 . #x14) (EQZ . #x15) (TLB . #x16) (JTL . #x17) (CLL . #x18)
    (RET . #x19) (LDO . #x1A) (CDO . #x1B)
    (TRM . #x1F))
  "Alist of opcode names and their hexadecimal value.")

(defun awa5x-mode-insert-opcode ()
  "Insert the specified opcode at point, in its awatism form."
  (interactive)
  (let ((names (mapcar #'car awa5x-mode--opcode-alist)))
    (let ((given (completing-read "Opcode name: " names nil t nil t)))
      (let ((numberval (cdr (assoc (intern given)
                                   awa5x-mode--opcode-alist))))
        (let ((stringed
               (concat (if (zerop (logand numberval 16)) "awa" "~wa")
                       (if (zerop (logand numberval 8)) " awa" "wa")
                       (if (zerop (logand numberval 4)) " awa" "wa")
                       (if (zerop (logand numberval 2)) " awa" "wa")
                       (if (zerop (logand numberval 1)) " awa " "wa "))))
          (insert stringed))))))

(defun awa5x-mode-insert-parameter (parameter)
  "Insert the given number in its awatism form."
  (interactive "nParameter to insert: ")
  (let ((stringed
         (concat (if (zerop (logand parameter 128)) "awa" "~wa")
                 (if (zerop (logand parameter 64)) " awa" "wa")
                 (if (zerop (logand parameter 32)) " awa" "wa")
                 (if (zerop (logand parameter 16)) " awa" "wa")
                 (if (zerop (logand parameter 8)) " awa" "wa")
                 (if (zerop (logand parameter 4)) " awa" "wa")
                 (if (zerop (logand parameter 2)) " awa" "wa")
                 (if (zerop (logand parameter 1)) " awa " "wa "))))
    (insert stringed)))

(defvar awa5x-mode-map
  (let ((map (make-sparse-keymap "awa5x")))
    (define-key map (kbd "C-c C-o") #'awa5x-mode-insert-opcode)
    (define-key map (kbd "C-c C-p") #'awa5x-mode-insert-parameter)
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

(defconst awa5x-mode--font-lock-0
  (list #'awa5x-mode--font-lock-open-tag 0 'awa5x-mode-opener-face)
  "Font Lock base level.")

(defconst awa5x-mode--font-lock-1
  '("#[[:upper:]]+" 0 font-lock-preprocessor-face)
  "Font Lock level 1.")

(defconst awa5x-mode--font-lock-keywords
  (list awa5x-mode--font-lock-0 awa5x-mode--font-lock-1)
  "Font Lock keywords levels.")

(defvar awa5x-mode--font-lock-defaults
  (list awa5x-mode--font-lock-keywords)
  "Font Lock defaults.")

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
