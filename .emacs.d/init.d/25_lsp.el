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

;;
;; Snippets
;;
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

;;
;; Corfu (for completion)
;;
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-quit-no-match 'separator)  ;; quit early if possible
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  (corfu-popupinfo-delay '(1.0 . 1.0))

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :config
  ;; https://github.com/minad/corfu/wiki
  (with-eval-after-load 'eglot
    ;; enable cache busting for eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    ;; use orderless completion for eglot
    (setq completion-category-overrides '((eglot (styles orderless))
                                          (eglot-capf (styles orderless))))))

(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;
;; Tree sitter (for syntax highlighting)
;;
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;;
;; Flymake (for showing errors)
;;
(use-package flymake
  :bind (:map flymake-mode-map
              ("C->" . flymake-goto-next-error)
              ("C-<" . flymake-goto-prev-error)))


;;
;; Eglot (the magic LSP thing)
;;

;; unbind F1 from help
(global-set-key (kbd "<f1>") nil)

;; Unbind the STUPID 2-column mode from F2
;; Still bound to C-x 6 regardless
(global-set-key (kbd "<f2>") nil)

(use-package eglot
  :after (corfu yasnippet)
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
