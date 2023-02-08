;;; 25_lsp.el --- LSP and other autocompletion things  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nino Maruszewski

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: convenience, languages

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

;; 

;;; Code:

;; Snippets
(use-package yasnippet
  :after (yasnippet-snippets)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Company
(use-package company
  :hook (after-init . global-company-mode))

;; Tree sitter (for syntax highlighting)
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; Flymake (for showing errors)
(use-package flymake
  :bind (:map flymake-mode-map
              ("C->" . flymake-goto-next-error)
              ("C-<" . flymake-goto-prev-error)))

;; Unbind the STUPID 2-column mode from F2
;; Still bound to C-x 6 regardless
(global-set-key (kbd "<f2>") nil)

;; Eglot (the magic LSP thing)
(use-package eglot
  :after (company yasnippet)
  :commands (eglot)
  :bind ("<f2>" . eglot-rename))


(provide '25_lsp)
;;; 25_lsp.el ends here
