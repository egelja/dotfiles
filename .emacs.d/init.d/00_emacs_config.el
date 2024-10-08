;;; 00_emacs_config.el --- Emacs builtins config      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <Nino Maruszewski@NINO-ASUS-G15>
;; Keywords: convenience, convenience, convenience

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

;; https://github.com/travitch/dotfiles/blob/master/home/dot_emacs.d/early-init.el
;; Edit GUI things as early as possible
(when (or (display-graphic-p)
          (is-windows-p))
  ;; Disable ctrl Z, but only in GUI mode.  This is still useful
  ;; in a terminal
  (global-unset-key (kbd "C-z")))

;; Use UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix) ; UNIX newlines

;; Give custom its own file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;; Delete selections by typing over them
(delete-selection-mode 1)

;; URLs are files
(url-handler-mode 1)

;; Set the correct path for find.exe on Windows
(if (is-windows-p)
    (push "~/scoop/apps/findutils/current/bin/"
          exec-path))

;; Auto set executable bit when file starts with `#!'
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Fill column
(setq-default fill-column 99) ; zero indexed
(add-hook 'prog-mode-hook
          #'display-fill-column-indicator-mode)

;; Whitespace
;; TODO whitespace mode
(setq require-final-newline t)

(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; indent with spaces

;; Configure auto-saves
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ; Autosave in one place
      backup-by-copying      t                              ; Backup by copying - slower, but preserves links
      version-control        t                              ; Use version numbers on backups,
      delete-old-versions    t                              ; Clean up the backups
      kept-new-versions      5                              ; keep some new versions
      kept-old-versions      2)                             ; and some old ones, too

;; Resizing windows
;; https://old.reddit.com/r/emacs/comments/b13n39/how_do_you_manage_window_sizes_in_emacs/
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Window layout history
(use-package winner
  :config
  (winner-mode 1))

;; Flyspell and ispell
(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--camel-case"
                       "--sug-mode=ultra"
                       "--lang=en_US"))
  (ispell-silently-savep t) ; auto save dictionary
  )

(use-package flyspell
  :bind (:map flyspell-mode-map
              ; Unbind confusing and bad keybindings
              ("C-;" . nil)
              ("C-M-i" . nil))
  :hook ((prog-mode text-mode) . flyspell-mode))

;; TRAMP
(use-package tramp
  :demand t
  ;; https://github.com/radian-software/straight.el/issues/236#issuecomment-886324625
  :straight `(:build t :pre-build (; Generate autoloads
                                   ("make" "autoloads")))
  :custom
  (tramp-default-method (if (is-windows-p)
                            "plink"
                          "ssh"))
  :config
  (add-to-list 'tramp-remote-path
               'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path
               "~/.local/bin")
  (add-to-list 'tramp-remote-path
               "~/go/bin"))

;; Add a command to revert all buffers
(defun revert-all-buffers ()
  (interactive)
  (dolist (buff (buffer-list))
    (ignore-errors  ; for buffers w/o files
      (revert-buffer buff))))

;; enable narrowing
(put 'narrow-to-region 'disabled nil)

;; Give backspace a better mapping to save my pinky
(progn
  ;; map backspace [delete-backward-char] to C-h
  (define-key key-translation-map [?\C-?] [?\C-h]) 

  ;; map M-backspace [backward-kill-word] to M-h
  (define-key key-translation-map [?\M-\d] [?\M-h])

  ;; map C-h to backspace
  (define-key key-translation-map [?\C-h] [?\C-?])

  ;; map M-h [mark-paragraph] to M-backspace
  (define-key key-translation-map [?\M-h] [?\M-\d]))

(provide '00_emacs_config)
;;; 00_emacs_config.el ends here
