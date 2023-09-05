;;; 10_editing.el --- Editing tools                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: convenience

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

;; Window jumping
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (push "*eldoc*" aw-ignored-buffers)
  (ace-window-display-mode 1))

;; Monitor keys pressed
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Highlight todo messages in code
(use-package hl-todo
  :hook ((prog-mode LaTeX-mode TeX-mode) . hl-todo-mode)
  :config
  (push '("\\todo" . "#cc9393")
        hl-todo-keyword-faces))

;; Line moving
(use-package move-dup
  :bind
  (("M-p" . move-dup-move-lines-up)
   ("M-n" . move-dup-move-lines-down)
   ("M-P" . move-dup-duplicate-up)
   ("M-N" . move-dup-duplicate-down)))

;; Line jumping
(use-package avy
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-:" . avy-goto-line)
  ("C-c C-j" . avy-resume))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-}" . mc/mark-next-like-this)
  ("C-{" . mc/mark-previous-like-this))

;; Search
(use-package ivy
  :config
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure-system-package (rg . ripgrep)
  :custom
  (counsel-rg-base-command
   "rg -S -M 240 --with-filename --no-heading --line-number --color never %s .")
  ;; https://oremacs.com/2017/08/04/ripgrep/
  (counsel-grep-base-command
   "rg -i -M 240 --no-heading --line-number --color never '%s' %s")
  :config
  (counsel-mode)
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-r" . counsel-rg)))

(use-package ivy-prescient ; improve search ordering
  :config
  (ivy-prescient-mode t)
  ;; https://github.com/radian-software/prescient.el/issues/43
  (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

;; Zoxide
(use-package zoxide.el
  :bind
  (("C-x C-S-f" . zoxide-find-file)))

;; Better help menus
(use-package helpful
  :after (counsel)
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;;
;; GIT stuff
;;
(use-package magit
  :commands (magit-status))

(use-package git-modes)

(use-package forge
  :after magit)

(use-package age
  :disabled
  :custom
  (age-default-identity (expand-file-name "~/.ssh/id_ed25519"))
  (age-default-recipient
   (mapcar 'expand-file-name             
           '("~/.ssh/id_ed25519.pub")))
  :config
  ;(setq age-program "rage")
  (age-file-enable)
  (push "~/.authinfo.age" auth-sources))


(provide '10_editing)
;;; 10_editing.el ends here
