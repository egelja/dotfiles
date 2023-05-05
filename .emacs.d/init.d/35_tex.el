;;; 35_tex.el --- LaTeX config                       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@ARTHUR>
;; Keywords: languages, tex, tex

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

;; Utilities and other things for LaTeX.

;;; Code:

; https://www.mail-archive.com/bug-auctex@gnu.org/msg04164.html 	
(use-package tex-mik
  :straight auctex
  :defer 1
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq-default TeX-master nil)
  ;; Hooks
  (defun my/tex-common-hook ()
    (display-line-numbers-mode 1)
    ;; Fill column stuff
    (setq-local fill-column 79)
    (auto-fill-mode 1)
    (display-fill-column-indicator-mode 1))
  (add-hook 'tex-mode-hook #'my/tex-common-hook)
  (add-hook 'TeX-mode-hook #'my/tex-common-hook)
  (add-hook 'latex-mode-hook #'my/tex-common-hook)
  (add-hook 'LaTeX-mode-hook #'my/tex-common-hook)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

(use-package procress
  :straight (:host github :repo "haji-ali/procress")
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

(use-package pdf-tools
  :defer 2
  :config
  (pdf-loader-install))

(provide '35_tex)
;;; 35_tex.el ends here
