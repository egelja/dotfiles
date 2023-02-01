;; Profile startup time
;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Raise GC threshold
;; https://github.com/doomemacs/doomemacs/issues/310#issuecomment-354424413
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;;
;; UTILS
;;
(defun is-windows-p ()
  (eq system-type 'windows-nt))

;; TODO:
;;   https://github.com/emacs-jp/init-loader

;;
;; Emacs builtin packages config
;;

;; Auto set executable bit when file starts with `#!'
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Fill column
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default fill-column 79) ; zero indexed

;; Use UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Unbind the STUPID 2-column mode from F2
;; Still bound to C-x 6 regardless
(global-set-key (kbd "<f2>") nil)

;; Set the correct find path
(if (is-windows-p)
    (push "~/scoop/apps/findutils/current/bin/"
          exec-path))

;; Buffer reverting
(defun revert-all-buffers ()
  (interactive)
  (dolist (buff (buffer-list))
    (ignore-errors  ; for buffers w/o files
      (revert-buffer buff))))

;; Flymake
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C->") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-<") 'flymake-goto-prev-error))

;; Flyspell
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case"
                          "--sug-mode=ultra"
                          "--lang=en_US"))

(setq-default ispell-silently-savep t) ;; auto-save dictionary

(with-eval-after-load "flyspell"
  ;; Unbind confusing and bad keybinding
  (define-key flyspell-mode-map (kbd "C-;") nil))

;; TODO is this ok to always have on?
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-mode)

;; Org-mode auto break
(add-hook 'org-mode-hook #'(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook #'auto-fill-mode)

;; Move custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;; TRAMP
(setq tramp-default-method
      (if (is-windows-p)
          "plink"
        "ssh"))

(with-eval-after-load "tramp"
  (add-to-list 'tramp-remote-path
               'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path
               "~/.local/bin"))

;; Trailing newline
(setq require-final-newline t)
(setq tab-width 4)

(setq-default c-basic-offset 4
              c-default-style "k&r")
(defun my/c-mode-common-hook ()
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inher-intro 0)
  (c-set-offset 'template-args-cont '++)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 4))
(add-hook 'c-mode-common-hook #'my/c-mode-common-hook)

;; Turn on line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq column-number-mode t)

;; Window setup history
(winner-mode 1)

;; Show battery percentage
(display-battery-mode 1)

;; Deleting selections by typing over them
(delete-selection-mode 1)

;; URLs are files
(url-handler-mode 1)

;; Space indents
(setq-default indent-tabs-mode nil)

;; For PDFs use continuous view
(setq doc-view-continuous t)

;; Cursor type
(setq-default cursor-type 'bar)

;; Yeet bars
(tool-bar-mode -1)
(scroll-bar-mode -1) ; TODO find a better scrollbar

;; Highlight current line
(global-hl-line-mode 1)

;; Configure autosaves
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ; Autosave in one place
      backup-by-copying      t                              ; Backup by copying - slower, but preserves links
      version-control        t                              ; Use version numbers on backups,
      delete-old-versions    t                              ; Clean up the backups
      kept-new-versions      5                              ; keep some new versions
      kept-old-versions      2)                             ; and some old ones, too

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

(zone-when-idle 300)

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

;; Preloads
(use-package
  shrink-path
  :straight
  (:host
   gitlab
   :repo "bennya/shrink-path.el"
   :local-repo "shrink-path"))


;; Packages
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (ace-window-display-mode 1))

(use-package ahk-mode :mode "\\.ahk\\'")

(use-package all-the-icons :if (display-graphic-p))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package avy
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-:" . avy-goto-line)
  ("C-c C-j" . avy-resume))

(use-package
  ivy
  :config
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("<backtab>" . centaur-tabs-backward)
;;   ("C-<tab>" . centaur-tabs-forward))

(use-package
  counsel
  :init
  (unless (executable-find "rg")
    (warn
     "\nWARNING: Could not find the ripgrep executable. It is recommended you install ripgrep."))
  :config
  (counsel-mode)
  :bind
  (("C-s" . swiper-isearch)))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package doom-modeline :config (doom-modeline-mode 1))

;; (use-package dimmer
;;   :custom
;;   (dimmer-fraction 0.2)
;;   (dimmer-buffer-exclusion-regexps '("^ \\*Minibuf-[0-9]+\\*$"
;;                                      "^ \\*Echo.*\\*$"
;;                                      "^ \\*Completions\\*$"
;;                                      "^ \\*Backtrace\\*$"))
;;   :config (dimmer-mode t))

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

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (push '(emacs-lisp-mode . lisp-indent) apheleia-mode-alist)
  (push '(racket-mode . lisp-indent) apheleia-mode-alist)
  (push '(lisp-interaction-mode . lisp-indent) apheleia-mode-alist)
  ;; :custom
  ;; (apheleia-remote-algorithm 'remote)
  )

(use-package gcode-mode
  :mode "\\.gcode\\'"
  :hook (gcode-mode . eldoc-mode))

(use-package helpful
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


(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method (if (display-graphic-p)
                                      'bitmap
                                    'character))
  (highlight-indent-guides-responsive 'stack))


(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package move-dup
  :bind
  (("M-p" . move-dup-move-lines-up)
   ("M-n" . move-dup-move-lines-down)
   ("M-P" . move-dup-duplicate-up)
   ("M-N" . move-dup-duplicate-down)))

(use-package parrot
  :after (magit)
  :hook
  (magit-post-commit . parrot-start-animation)
  ;; (mu4e-index-updated . parrot-start-animation)
  :config
  (parrot-mode)
  (parrot-set-parrot-type 'science))

(use-package powershell :mode ("\\.psm?1\\'" . powershell-mode))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package scratch-pop :bind ("C-M-s" . scratch-pop))

(use-package ssh-config-mode :defer t)

(use-package typescript-mode :mode "\\.tsx?\\'")

;; TODO(nino): Whitespace cleanup package
;; https://github.com/purcell/whitespace-cleanup-mode

(use-package yaml-mode :mode "\\.ya?ml\\'")

;;
;; Custom functions
;;
(defun find-init-file ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-`") 'find-init-file)

;;;
;;; ELCORD
;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 LSP THINGS                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-exists-p (font)
  (if (null (x-list-fonts font))
      nil
    t))

;; Fira code mode
;; CONFLICTS with all the icons
;; NOTE: run M-x fira-code-mode-install-fonts RET afterwards
;; (use-package fira-code-mode
;;   :hook prog-mode ;; Enables fira-code-mode automatically for programming major modes
;;   :custom
;;   (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
;;   (prettify-symbols-unprettify-at-point 'right-edge)  ;; turn off ligatures on hover
;;   :config
;;   (fira-code-mode-set-font)
;;   (unless (font-exists-p "Fira Code Symbol5323")
;;     (warn "Fira Code Symbol font missing, run \"M-x fira-code-mode-install-fonts RET\" to install")))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-}" . mc/mark-next-like-this)
  ("C-{" . mc/mark-previous-like-this))

(use-package magit)

(use-package yasnippet
  :after (yasnippet-snippets)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package eglot
  :after (company yasnippet)
  :commands (eglot)
  :bind ("<f2>" . eglot-rename))
                                        ;  :hook (prog-mode . eglot))

(use-package company
  :hook (after-init . global-company-mode))

(defun my/treemacs-ignore-predicate (filename absolute-path)
  (or (string-search "__pycache__" absolute-path)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (push #'my/treemacs-ignore-predicate
        treemacs-ignored-file-predicates)
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

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

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package alert
  :commands (alert)
  :config (setq alert-default-style (if is-windows
                                        'toast
                                      'libnotify)))

(when (is-windows-p)
  (use-package alert-toast
    :after alert))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-feeds
   '(("https://xkcd.com/rss.xml" webcomic)
     ("https://hnrss.org/frontpage" news))))

;; TODO move this to its own file
;; https://robbmann.io/posts/emacs-eglot-pyrightconfig/
(defun pyrightconfig-write-with-venv (virtualenv)
  (interactive "DPath to venv: ")

  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above .venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `.venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json' is in the project root
         ;; folder.
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

         ;; Finally, get a string with the JSON payload.
         (out-contents (json-encode (list :venvPath venvPath
                                          :venv venv
                                          :typeCheckingMode "basic")))) ; TODO mypy

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
    (with-temp-file out-file (insert out-contents))))

(defun pyrightconfig-write-with-system ()
  (interactive)
  (let* ((base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :typeCheckingMode "basic")))) ; TODO mypy
    (with-temp-file out-file (insert out-contents))))


;; Restore GC threshhold
;; https://github.com/doomemacs/doomemacs/issues/310#issuecomment-354424413
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist last-file-name-handler-alist)))
