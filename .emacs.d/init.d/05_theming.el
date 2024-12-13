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
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode 1)

;; Icons!
(use-package all-the-icons
  ;; :straight (all-the-icons
  ;;            :type git
  ;;            :host github
  ;;            :repo "domtronn/all-the-icons.el"
  ;;            :branch "svg"
  ;;            :files (:defaults "svg"))
;  :if (or (display-graphic-p)
;          (is-windows-p))
)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Emojis!
(use-package emojify
  :if (or (display-graphic-p)
          (is-windows-p))
  :custom
  (emojify-emoji-styles '(unicode github))
  :hook
  (after-init . global-emojify-mode))

;; Which-key
(use-package which-key
  :defer 5
  :config
  (which-key-mode))

;; Keycast
;; https://github.com/seagle0128/doom-modeline/issues/122#issuecomment-1133838869
(use-package keycast
  :disabled
  :config
  (defun my/toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON")))
  ;;:hook
  ;;(doom-modeline-mode . my/toggle-keycast)
  )

;; Doom modeline
(use-package shrink-path ; have to preload this to get right path
  :straight
  (:host gitlab
   :repo "bennya/shrink-path.el"
   :local-repo "shrink-path"))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-env-load-string "...")
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
  ;; (doom-themes-neotree-config)
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
  :after (doom-modeline)
  :defer 10
  :hook
  (magit-post-commit . parrot-start-animation)
  ;; (mu4e-index-updated . parrot-start-animation)
  :config
  (parrot-mode)
  (parrot-set-parrot-type 'science)
  (message "Parrot mode enabled"))

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
  :custom
  ;;  (dashboard-center-content t)
  (dashboard-startup-banner "~/.emacs.d/fancy.png")
  (dashboard-set-navigator t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-projects-backend 'project-el)
  ;(dashboard-icon-type 'all-the-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents  . 10)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  (dashboard-agenda-prefix-format " %s ")
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-agenda-sort-strategy '(time-up priority-down))
  (dashboard-navigator-buttons
   ;; Format: "(icon title help action face prefix suffix)"
   `(;; line1
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Github"
       "Open Github"
       (lambda (&rest _) (browse-url "https://github.com/egelja?tab=repositories")))
      (,(all-the-icons-material "school" :height 1.1 :v-adjust -0.20)
       "Canvas"
       "Open Canvas"
       (lambda (&rest _) (browse-url "https://canvas.northwestern.edu")))
      (,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
       "Linkedin"
       ""
       (lambda (&rest _) (browse-url "https://www.linkedin.com/in/nikola-maruszewski")))))))

;;
;; Zone mode
;; https://www.emacswiki.org/emacs/ZoneMode
;;
(use-package zone
  :defer 10
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

;; ;;
;; ;; ELCORD
;; ;;
;; (straight-use-package 'elcord)

;; (setq elcord-editor-icon nil
;;       elcord-use-major-mode-as-main-icon t)

;; ;; Hack for using Daemon
;; ;; https://github.com/Mstrodl/elcord/issues/17#issuecomment-571383324
;; (defun elcord--disable-elcord-if-no-frames (f)
;;   (declare (ignore f))
;;   (when (let ((frames (delete f (visible-frame-list))))
;; 	  (or (null frames)
;; 	      (and (null (cdr frames))
;; 		   (eq (car frames) terminal-frame))))
;;     (elcord-mode -1)
;;     (add-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)))

;; (defun elcord--enable-on-frame-created (f)
;;   (declare (ignore f))
;;   (elcord-mode +1))

;; (defun my/elcord-mode-hook ()
;;   (if elcord-mode
;;       (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
;;     (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)))

;; (add-hook 'elcord-mode-hook 'my/elcord-mode-hook)

;; ;; Schedule turning off elcord mode in 10 sec, after it should have connected
;; (run-at-time "10 sec" nil 'elcord--disable-elcord-if-no-frames -1)

;; ;; Start elcord
;; (elcord-mode)


(provide '05_theming)
;;; 05_theming.el ends here
