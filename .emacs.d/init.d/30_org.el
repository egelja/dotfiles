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
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq fill-column 79)))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'display-line-numbers-mode)
  (add-hook 'org-mode-hook #'display-fill-column-indicator-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c o" . org-capture)
   ("C-c a" . org-agenda))
  :custom
  (org-log-done 'time)
  (org-todo-keywords '("TODO(t)" "STARTED(s)" "WAITING(w)"
                       "|" "DONE(d)" "CANCELED(c)"))
  (org-tag-alist '((:startgroup . nil)
                   ("@home"   . ?h)
                   ("@school" . ?s)
                   ("@work"   . ?w)
                   (:endgroup . nil)
                   (:newline  . nil)
                   ("laptop"  . ?l)
                   ("phone"   . ?p)
                   ("offline" . ?o)))
  (org-blank-before-new-entry '((heading         t)
                                (plain-list-item auto))))

;;
;; Org-roam
;;
(use-package org-roam
  :defer 1
  :custom
  (org-roam-directory "~/notes")
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  ;; Dailies
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;
;; ORG-JOURNAL
;;

(use-package org-journal
  :demand t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :custom
  (org-journal-dir "~/journal")
  (org-journal-file-format "%F.org") ; ISO 8601
  (org-journal-file-type 'daily) ; daily notes
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"|TODO=\"WAITING\"")
  :config
  ;; Set file header based on file type
  (defun my/org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: content")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
  (setq org-journal-file-header 'my/org-journal-file-header-func)
  ;; Work with org-capture
  (defvar org-journal--date-location-scheduled-time nil)
  
  (defun my/org-journal-location-journal ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    ;; Search for "Journal" headline
    (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote "Journal"))
                           nil t)
        (goto-char (point-max))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n\n" "* Journal" "\n")))
  
  (defun my/org-journal-location-todo (&optional scheduled-time)
    (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
      (setq org-journal--date-location-scheduled-time scheduled-time)
      (org-journal-new-entry t (org-time-string-to-time scheduled-time))
      (unless (eq org-journal-file-type 'daily)
        (org-narrow-to-subtree))
      ;; Search for "Journal" headline, and start right before
      (goto-char (point-min))
      (if (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote "Maybe do today"))
                             nil t)
          (progn
            (forward-line 0)
            (open-line 1))
        (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote "Journal"))
                               nil t)
            (progn
              (forward-line 0)
              (open-line 1))
          (goto-char (point-max))))))
  
  (defun my/org-journal-location-maybe-todo (&optional scheduled-time)
    (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
      (setq org-journal--date-location-scheduled-time scheduled-time)
      (org-journal-new-entry t (org-time-string-to-time scheduled-time))
      (unless (eq org-journal-file-type 'daily)
        (org-narrow-to-subtree))
      ;; Search for "Journal" headline, and start right before
      (goto-char (point-min))
      (let ((has-header (re-search-forward (format org-complex-heading-regexp-format
                                     (regexp-quote "Maybe do today"))
                             nil t)))
        (if (re-search-forward (format org-complex-heading-regexp-format
                                       (regexp-quote "Journal"))
                               nil t)
            (progn
              (forward-line 0)
              (open-line 1)
              (unless has-header
                (unless (bolp) (insert "\n"))
                (insert "\n\n" "* Maybe do today" "\n")))
          (goto-char (point-max))
          (unless has-header
            (unless (bolp) (insert "\n"))
            (insert "\n\n" "* Maybe do today" "\n"))))))

  (setq org-capture-templates
        '(("j"
           "Journal entry"
           plain
           (function my/org-journal-location-journal)
           "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
           :jump-to-captured t
           :immediate-finish t)
          ("t"
           "Task"
           plain
           (function my/org-journal-location-todo)
           "** TODO %?\n <%(princ org-journal--date-location-scheduled-time)>\n"
           :jump-to-captured t)
          ("m"
           "Possible task"
           plain
           (function my/org-journal-location-maybe-todo)
           "** TODO %?\n <%(princ org-journal--date-location-scheduled-time)>\n"
           :jump-to-captured t))))

(message "%s" (org-agenda-files))


;;
;; Alerts (for org agenda)
;;
;; TODO
(use-package alert
  :defer 3
  :after alert-toast
  :commands (alert)
  :config (setq alert-default-style (if (is-windows-p)
                                        'toast
                                      'libnotify)))

(use-package alert-toast
  :defer 3
  :if (is-windows-p))

(use-package pomidor
  :defer 3
  :after alert
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

(provide '30_org)
;;; 30_org.el ends here
