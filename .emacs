;;;
;;; MELPA
;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;;;
;;; AUTOSAVE
;;;
(setq backup-directory-alist '(("." . "~/.emacs-autosaves")) ; Autosave in one place
      backup-by-copying t ; Backup by copying - slower, but preserves links
      delete-old-versions t	     ; Clean up the backups
      version-control t		     ; Use version numbers on backups,
      kept-new-versions 5	     ; keep some new versions
      kept-old-versions 2)	     ; and some old ones, too

;;;
;;; PACKAGE LOADING
;;;

;; Elisp formatter
(require 'elisp-format)

;; Emoji support
(add-hook 'after-init-hook #'global-emojify-mode)

(url-handler-mode 1)
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
   '("1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176" default))
 '(elcord-editor-icon nil)
 '(elcord-use-major-mode-as-main-icon t)
 '(package-selected-packages
   '(yaml-mode ahk-mode restart-emacs julia-mode ssh-config-mode emojify logview markdown-mode typescript-mode powershell gcode-mode elisp-format spacemacs-theme dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
