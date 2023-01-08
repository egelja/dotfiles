;;
;; Emacs builtins config
;;

;; Trailing newline
(setq require-final-newline t)

;; Window setup history
(winner-mode 1)

;; Deleting selections by typing over them
(delete-selection-mode 1)

;; URLs are files
(url-handler-mode 1)

;; Space indents
(setq-default indent-tabs-mode nil)

;; Yeet bars
(tool-bar-mode -1)
(scroll-bar-mode -1) ; TODO find a better scrollbar

;; Configure autosaves
(setq
 backup-directory-alist
 '(("." . "~/.emacs.d/backup")) ; Autosave in one place
 backup-by-copying t ; Backup by copying - slower, but preserves links
 version-control t ; Use version numbers on backups,
 delete-old-versions t ; Clean up the backups
 kept-new-versions 5 ; keep some new versions
 kept-old-versions 2) ; and some old ones, too

;; Zone mode
;; https://www.emacswiki.org/emacs/ZoneMode
(require 'zone)

(defun zone-pgm-md5 ()
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5
         (buffer-substring-no-properties ;; Initialize.
          (point-min) (point-max))))
    ;; Whitespace-fill the window.
    (zone-fill-out-screen (window-width) (window-height))
    (random t)
    (goto-char (point-min))
    (while (not (input-pending-p))
      (when (eobp)
        (goto-char (point-min)))
      (while (not (eobp))
        (delete-region (point) (line-end-position))
        (let ((next-md5 (md5 prev-md5)))
          (insert next-md5)
          (setq prev-md5 next-md5))
        (forward-line 1)
        (zone-park/sit-for (point-min) 0.1)))))

(eval-after-load "zone"
  '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
     (setq zone-programs (vconcat zone-programs [zone-pgm-md5]))))

(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive (list
                (completing-read
                 "Program: "
                 (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))

(zone-when-idle 120)

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

;; Config straight.el
(use-package
 straight
 :custom
 (straight-use-package-by-default t)
 (straight-vc-git-default-protocol 'https))

;; Preloads
(use-package
 shrink-path
 :straight
 (:host
  gitlab
  :repo "bennya/shrink-path.el"
  :local-repo "shrink-path"))


;; Packages
(use-package ace-window :bind ("M-o" . ace-window))

(use-package ahk-mode :mode "\\.ahk\\'")

(use-package all-the-icons :if (display-graphic-p))

(use-package
 counsel
 :init
 (if (executable-find "rg")
     ;; use ripgrep instead of grep because it's way faster
     (setq
      counsel-grep-base-command (append counsel-rg-base-command '("%s")))
   (warn
    "\nWARNING: Could not find the ripgrep executable. It "
    "is recommended you install ripgrep."))
 :config (ivy-mode 1) (counsel-mode 1)
 :custom (ivy-use-virtual-buffers t) (ivy-count-format "(%d/%d) ")
 :bind
 (("C-s" . swiper-isearch)))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package doom-modeline :config (doom-modeline-mode 1))

(use-package
 dimmer
 :custom
 (dimmer-fraction 0.3)
 (dimmer-buffer-exclusion-regexps '("^ \\*Minibuf-[0-9]+\\*$"
                                    "^ \\*Echo.*\\*$"
                                    "^ \\*Completions\\*$"
                                    "^ \\*Backtrace\\*$"))
 :config (dimmer-mode t))

(use-package dracula-theme)

(use-package elcord)

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package emojify :hook (after-init . global-emojify-mode))

(use-package
 gcode-mode
 :mode "\\.gcode\\'"
 :hook (gcode-mode . eldoc-mode))

(use-package
 highlight-indent-guides
 :hook (prog-mode . highlight-indent-guides-mode)
 :custom
 (highlight-indent-guides-method 'bitmap)
 (highlight-indent-guides-responsive 'stack))


(use-package julia-mode :mode "\\.jl\\'" :interpreter "julia")

(use-package markdown-mode :mode "\\.md\\'")

(use-package
 move-dup
 :bind
 (("M-p" . move-dup-move-lines-up)
  ("M-n" . move-dup-move-lines-down)
  ("M-P" . move-dup-duplicate-up)
  ("M-N" . move-dup-duplicate-down)))

(use-package powershell :mode ("\\.psm?1\\'" . powershell-mode))

(use-package
 pulsar
 :init
 (setq
  pulsar-pulse t
  pulsar-delay 0.100
  pulsar-iterations 10
  pulsar-face 'pulsar-magenta
  pulsar-highlight-face 'pulsar-yellow)
 :config (pulsar-global-mode 1)
 :hook (next-error . pulsar-pulse-line)
 :bind
 ("C-x l" . pulsar-pulse-line)
 ("C-X L" . pulsar-highlight-dwim))

(use-package scratch-pop :bind ("C-M-s" . scratch-pop))

(use-package ssh-config-mode :defer t)

(use-package typescript-mode :mode "\\.tsx?\\'")

;; TODO(nino): Whitespace cleanup package
;; https://github.com/purcell/whitespace-cleanup-mode

(use-package yaml-mode :mode "\\.ya?ml\\'")

;;;
;;; ELCORD
;;;
;; (require 'elcord)
;;(elcord-mode)				; Connect to discord


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
;;   (if elcord-mode (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
;;     (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)))

;; (add-hook 'elcord-mode-hook 'my/elcord-mode-hook)

;; ;; Schedule turning off elcord mode in 10 sec, after it should have connected
;; (run-at-time "10 sec" nil 'elcord--disable-elcord-if-no-frames -1)

;;;
;;; THEME SETTINGS
;;;
(setq column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type t)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176"
     default))
 '(doc-view-continuous t)
 '(elcord-editor-icon nil)
 '(elcord-use-major-mode-as-main-icon t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
