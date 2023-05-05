;;; init.el --- Emacs initialization file            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nino Maruszewski

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: c, convenience

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

(add-to-list 'load-path
             (expand-file-name (concat user-emacs-directory "site-lisp/")))

;; Utility function for checking if we are on windows
(defun is-windows-p ()
  (eq system-type 'windows-nt))

;; TODO:
;;   diff-hl-mode
;;   scroll bar?
;;   magit forges
;;   eglot-format on save
;;   eglot start hook/keybinding
;;   Continue comments on indent
;;   C++ doxygen comments
;;   org roam
;;   hide-ifdef-mode: need to run `hide-ifdefs' every so often
;;       https://www.emacswiki.org/emacs/HideIfDef
;;       https://emacs.stackexchange.com/questions/31631/gray-out-preprocessor-conditionals
;;   PDF-tools


;;;
;;; PACKAGE LOADING
;;;

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure straight.el
(use-package
  straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-protocol 'https))

(use-package use-package-ensure-system-package
  :ensure t)

;; Sub-file loading
(use-package init-loader
  :custom
  (init-loader-directory
   (expand-file-name (concat user-emacs-directory "init.d/")))
  :config
  (init-loader-load))

;; TODO(nino): Whitespace cleanup package
;; https://github.com/purcell/whitespace-cleanup-mode


(provide 'init)
;;; init.el ends here
