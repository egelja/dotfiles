(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-print-command "start \"\" %f")
 '(age-debug t)
 '(current-language-environment "UTF-8")
 '(elfeed-feeds
   '("https://building.open-home.io/rss/"
     ("https://xkcd.com/rss.xml" webcomic)
     ("https://hnrss.org/frontpage" news)
     ("https://web3isgoinggreat.com/feed.xml" news)
     ("https://fasterthanli.me/index.xml" tech article)
     ("https://chipsandcheese.com/feed/" tech article)) nil nil "Customized with use-package elfeed")
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-prefix-format
   '((dashboard-agenda . " %i %-12:c %s ")
     (agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(package-enable-at-startup nil)
 '(preview-gs-command "gs.exe")
 '(straight-package-neutering-mode t)
 '(straight-use-package-mode t)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Iosevka NF")))))