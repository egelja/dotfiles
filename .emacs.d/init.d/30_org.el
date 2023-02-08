;;; 30_org.el --- Org mode configs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: 

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

(use-package org
  :config
  ;; Auto break lines
  (add-hook 'org-mode-hook #'(lambda () (setq fill-column 79)))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'display-fill-column-indicator-mode))

;; Alerts (for org agenda)
;; TODO
(use-package alert
  :commands (alert)
  :config (setq alert-default-style (if is-windows
                                        'toast
                                      'libnotify)))

(when (is-windows-p)
  (use-package alert-toast
    :after alert))


(provide '30_org)
;;; 30_org.el ends here
