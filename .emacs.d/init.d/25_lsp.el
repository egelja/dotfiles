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

(use-package yasnippet-radical-snippets ; python and cpp docstring snippets
  :straight (:host github
             :repo "Xaldew/yasnippet-radical-snippets"
             :files (:defaults "snippets" "yasnippet-radical-snippets.el"))
  :after yasnippet
  :config
  (yasnippet-radical-snippets-initialize))

;; Company
(use-package company
  :config
  (global-company-mode))

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
  :commands (eglot eglot-format eglot-rename)
  :autoload (eglot-current-server)
  :init
  (defun my/eglot-start-or-format ()
    (interactive)
    (if (eglot-current-server)
        (call-interactively 'eglot-format-buffer)
      (call-interactively 'eglot)))
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
      ;; In python-mode, import organization is done in eglot-format-buffer
      (unless (or (eq major-mode 'python-mode)
                  (eq major-mode 'python-ts-mode))
        (add-hook 'before-save-hook #'my/eglot-organize-imports nil t))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-mode-hook)
  :config
  ;; Set server variables
  (setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:jedi_completion (:include_params t 
                                                    :fuzzy t)
                                  :jedi_rename     (:enabled t)
                                  :rope_autoimport (:enabled :json-false)
                                  :rope_completion (:enabled :json-false)
                                  :rope_rename     (:enabled :json-false)
                                  )
                        :rope    (:ropeFolder      ".ropeproject"))))
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :bind
  (("<f2>" . my/eglot-rename)
   ("<f1>" . my/eglot-start-or-format)
   ("C-'" . eglot-code-actions)))

(use-package eldoc)


(provide '25_lsp)
;;; 25_lsp.el ends here
