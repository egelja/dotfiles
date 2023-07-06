;;; 35_misc.el --- Misc emacs configs                -*- lexical-binding: t; -*-

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

;; Things I can't find a better place for.

;;; Code:

;; Keybinding to open the init file (dir)
(defun my/find-init-file ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.d/"))

(global-set-key (kbd "C-`") 'my/find-init-file)

;; For PDFs use continuous view
(setq doc-view-continuous t)

;; RSS feeds
;; https://www.youneedfeeds.com/starter-packs
(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-feeds
   '(("https://xkcd.com/rss.xml" webcomic)
     ("https://hnrss.org/frontpage" news)
     ("https://web3isgoinggreat.com/feed.xml" news)
     ("https://fasterthanli.me/index.xml" tech article)
     ("https://chipsandcheese.com/feed/" tech article))))


(provide '35_misc)
;;; 35_misc.el ends here
