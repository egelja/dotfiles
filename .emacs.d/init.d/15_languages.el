;;; 15_languages.el --- Language mode configs        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: languages

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

;; C indent style
(setq-default c-basic-offset 4
              c-default-style "k&r")

(defun my/c-mode-common-hook ()
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inher-intro 0)
  (c-set-offset 'template-args-cont '++)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 4)
  (c-set-offset 'arglist-close 0))
(add-hook 'c-mode-common-hook #'my/c-mode-common-hook)

;; PYTHON
(use-package pet
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-mode-hook
            #'(lambda ()
                ;; Python interpreter
                (setq-local python-shell-interpreter (pet-executable-find "python")
                            python-shell-virtualenv-root (pet-virtualenv-root))
                ;; Pytest (for python-pytest)
                ;;(setq-local python-pytest-executable (pet-executable-find "pytest"))
                ;; Eglot
                (require 'eglot)
                (setq-local eglot-server-programs
                            (cons `((python-mode python-ts-mode)
                                    . (,(pet-executable-find "pylsp")))
                                  eglot-server-programs))                              
                ))
  :custom
  (pet-toml-to-json-program-arguments '("-f" "-" "-r" "toml" "-w" "json"))
  (pet-yaml-to-json-program-arguments '("-f" "-" "-r" "yaml" "-w" "json")))
                
;; Language modes
(use-package ahk-mode
  :mode "\\.ahk\\'")

(use-package cmake-mode
  :mode ("\\.cmake\\'" "CMakeLists.txt\\'"))

(use-package gcode-mode
  :mode "\\.gcode\\'"
  :hook (gcode-mode . eldoc-mode))

(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")

(use-package llvm-mode
  :straight nil)

(use-package qasm-mode
  :straight nil
  :mode "\\.qasm\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode))

(use-package powershell
  :mode ("\\.psm?1\\'" . powershell-mode))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package ssh-config-mode
  :defer t)

(use-package typescript-mode
  :mode "\\.tsx?\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (setq-local fill-column 79)
  (add-hook 'yaml-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode))

(provide '15_languages)
;;; 15_languages.el ends here
