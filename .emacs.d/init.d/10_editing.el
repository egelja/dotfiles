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

;; TURN OFF THE MOUSE
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;; Code screenshots
(use-package screenshot
  :disabled
  :if (not (is-windows-p))
  :straight
  (:type git :host github :repo "tecosaur/screenshot"))

;; Window jumping
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  ;(push "*eldoc*" aw-ignored-buffers)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (ace-window-display-mode 1))

;; Monitor keys pressed
(use-package keyfreq
  :defer 10
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Highlight "TODO" messages in code
(use-package hl-todo
  :hook ((prog-mode LaTeX-mode TeX-mode) . hl-todo-mode)
  :config
  (push '("\\todo" . "#cc9393")         ; lowercase TODOs too (i.e., latex)
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
  :after (verilog-mode)
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-'" . avy-goto-line)
  ("C-c C-j" . avy-resume)
  (:map verilog-mode-map
        ("C-;" . avy-goto-char-timer)))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-}" . mc/mark-next-like-this)
  ("C-{" . mc/mark-previous-like-this))

;; Zoxide
(use-package zoxide.el
  :bind
  (("C-x C-S-f" . zoxide-find-file)))

;; Better help menus
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))


;;
;; GIT stuff
;;
(use-package sqlite3
  :demand t
  :if (version< emacs-version "29.1"))

(use-package magit)

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

(use-package smartparens
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))


(provide '10_editing)
;;; 10_editing.el ends here
