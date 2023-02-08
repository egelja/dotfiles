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
  :config (ace-window-display-mode 1))

;; Monitor keys pressed
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Highlight todo messages in code
(use-package hl-todo
  :hook (prog-mode))

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
  :init
  (unless (executable-find "rg")
    (warn
     "\nWARNING: Could not find the ripgrep executable. It is recommended you install ripgrep."))
  :config
  (counsel-mode)
  :bind
  (("C-s" . swiper-isearch)))

(use-package ivy-prescient ; improve search ordering
  :config
  (ivy-prescient-mode t))

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


(provide '10_editing)
;;; 10_editing.el ends here
