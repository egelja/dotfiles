;;; 05_theming.el --- Emacs theme configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: faces

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

;; Emacs theme setup.

;;; Code:

;; Turn on line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq column-number-mode t)

;; Show battery percentage in mode line
(display-battery-mode 1)

;; Cursor type
(setq-default cursor-type 'bar)

;; Yeet bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode 1)

;; Icons!
(use-package all-the-icons
  :if (display-graphic-p))

;; Emojis!
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Doom modeline
(use-package shrink-path ; have to preload this to get right path
  :straight
  (:host gitlab
   :repo "bennya/shrink-path.el"
   :local-repo "shrink-path"))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Highlight indents
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method (if (display-graphic-p)
                                      'bitmap
                                    'character))
  (highlight-indent-guides-responsive 'stack))

;; Highlight parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Parrot!
(use-package parrot
  ;; :after (magit)
  :hook
  (magit-post-commit . parrot-start-animation)
  ;; (mu4e-index-updated . parrot-start-animation)
  :config
  (parrot-mode)
  (parrot-set-parrot-type 'science))

;;
;; DASHBOARD
;;
(use-package page-break-lines)

(use-package dashboard
  :ensure t
  :after page-break-lines
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; https://github.com/emacs-dashboard/emacs-dashboard/issues/430#issuecomment-1376768467
  (defun dashboard-string-pixel-width (str)
    "..."
    (require 'shr)
    (shr-string-pixel-width str))
  :custom
  ;;  (dashboard-center-content t)
  ;;  (dashboard-startup-banner 'logo)
  (dashboard-set-navigator t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-projects-backend 'project-el)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents  . 10)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-navigator-buttons
   ;; Format: "(icon title help action face prefix suffix)"
   `(;; line1
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Github"
       "Open Github"
       (lambda (&rest _) (browse-url "https://github.com/MrAwesomeRocks?tab=repositories")))
      (,(all-the-icons-material "school" :height 1.1 :v-adjust -0.25)
       "Canvas"
       "Open Canvas"
       (lambda (&rest _) (browse-url "https://canvas.northwestern.edu"))))
     ;; line 2
     ;; ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
     ;;   "Linkedin"
     ;;   ""
     ;;   (lambda (&rest _) (browse-url "homepage")))
     ;;  (" " nil "Show flags" (lambda (&rest _) (message "flag")) error)))))
     )))

;;
;; Zone mode
;; https://www.emacswiki.org/emacs/ZoneMode
;;
(use-package zone
  :config
  ;; Define a new Zone program
  (defun zone-pgm-md5 ()
    "MD5 the buffer, then recursively checksum each hash."
    (let ((prev-md5
           (buffer-substring-no-properties ;; Initialize.
            (point-min) (point-max))))
      ;; Whitespace-fill the window.
      (zone-fill-out-screen (window-width) (window-height))
      (random t)
      (goto-char (point-min))
      (when (eobp)
        (while (not (input-pending-p))
          (goto-char (point-min)))
        (while (not (eobp))
          (delete-region (point) (line-end-position))
          (let ((next-md5 (md5 prev-md5)))
            (insert next-md5)
            (setq prev-md5 next-md5))
          (forward-line 1)
          (zone-park/sit-for (point-min) 0.1)))))
  ;; Push it to the list
  (unless (memq 'zone-pgm-md5 (append zone-programs nil))
    (setq zone-programs (vconcat zone-programs [zone-pgm-md5])))
  ;; And enable zone mode
  (zone-when-idle 300))
  
(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive (list
                (completing-read
                 "Program: "
                 (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))

;;
;; ELCORD
;;
(straight-use-package 'elcord)

(setq elcord-editor-icon nil
      elcord-use-major-mode-as-main-icon t)

;; Hack for using Daemon
;; https://github.com/Mstrodl/elcord/issues/17#issuecomment-571383324
(defun elcord--disable-elcord-if-no-frames (f)
  (declare (ignore f))
  (when (let ((frames (delete f (visible-frame-list))))
	  (or (null frames)
	      (and (null (cdr frames))
		   (eq (car frames) terminal-frame))))
    (elcord-mode -1)
    (add-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)))

(defun elcord--enable-on-frame-created (f)
  (declare (ignore f))
  (elcord-mode +1))

(defun my/elcord-mode-hook ()
  (if elcord-mode
      (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
    (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)))

(add-hook 'elcord-mode-hook 'my/elcord-mode-hook)

;; Schedule turning off elcord mode in 10 sec, after it should have connected
(run-at-time "10 sec" nil 'elcord--disable-elcord-if-no-frames -1)

;; Start elcord
(elcord-mode)


(provide '05_theming)
;;; 05_theming.el ends here
