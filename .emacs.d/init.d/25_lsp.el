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
  :defer 3
  :after (yasnippet-snippets)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer 3)

;; Company
(use-package company
  :defer 3
  :config
  (global-company-mode))

;; Tree sitter (for syntax highlighting)
(use-package tree-sitter
  :defer 2
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer 2)

;; Flymake (for showing errors)
(use-package flymake
  :defer 3
  :bind (:map flymake-mode-map
              ("C->" . flymake-goto-next-error)
              ("C-<" . flymake-goto-prev-error)))

;; Unbind the STUPID 2-column mode from F2
;; Still bound to C-x 6 regardless
(global-set-key (kbd "<f2>") nil)

;; Eglot (the magic LSP thing)
(use-package eglot
  :after (company yasnippet)
  :commands (eglot eglot-format eglot-rename)
  :autoload (eglot-current-server)
  :init
  (defun my/eglot-start-or-format ()
    (interactive)
    (call-interactively (if (eglot-current-server)
                            'eglot-format-buffer
                          'eglot)))
  (defun my/eglot-rename ()
    (interactive)
    (when (not (eglot-current-server))
      (call-interactively 'eglot))
    (call-interactively 'eglot-rename))
  ;; For hooks
  ;; https://github.com/joaotavora/eglot/issues/574#issuecomment-1249316625
  (defun my/eglot-organize-imports ()
    (call-interactively 'eglot-code-action-organize-imports))
  (defun my/eglot-mode-hook ()
    (when (eglot-managed-p)
      (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
      (add-hook 'before-save-hook #'my/eglot-organize-imports nil t)))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-mode-hook)
  :bind
  (("<f2>" . my/eglot-rename)
   ("<f1>" . my/eglot-start-or-format)))


(provide '25_lsp)
;;; 25_lsp.el ends here
