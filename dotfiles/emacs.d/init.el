;; This file tries to do as little as possible before calling into a
;; literate org-mode file that actually configures my emacs.
;;
;; First thing I do here is configure use-package such that I can
;; update org-mode before loading babel.
;;
;; A lot of this comes from here: https://github.com/hrs/dotfiles/blob/master/emacs/dot-emacs.d/init.e

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package org
  :commands org-babel-load-file)

;; load real config
(require 'org)
(org-babel-load-file "~/.emacs.d/README.org")

;;; V.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (Greymatters-light)))
 '(custom-safe-themes
   (quote
    ("323964cdaee8528eeff0b721974bb033c44a2b73d795a31ceba554f764109a20" "0da70fb7c272d8e87b9343a6f36d7f9eed520ef64beaab03478113968fbed1d9" "0aa971daea004737402f4cd0575cfa6dc2fa00206cd03c77ee2c3b5d3b5f19d6" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "0a8b8aad17f3bd4af1839a49cd97048c90db98a088b1f6f6d0d2187a3c35321d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "319bf1bab5d05e3a4c4a165efe69d27b3d975759034074f15fe61e92c7304884" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(display-battery-mode t)
 '(electric-pair-mode t)
 '(global-hl-line-mode t)
 '(hscroll-margin 8)
 '(hscroll-step 1)
 '(org-agenda-files
   (quote
    ("/Users/ricky/notes/2020-05-21.org" "/Users/ricky/notes/2020-05-24.org" "/Users/ricky/notes/2020-05-25.org" "/Users/ricky/notes/2020-05-26.org" "/Users/ricky/notes/2020-05-27.org" "/Users/ricky/notes/2020-05-28.org" "/Users/ricky/notes/2020-05-30.org" "/Users/ricky/notes/2020-06-01.org" "/Users/ricky/notes/2020-06-02.org" "/Users/ricky/notes/2020-06-03.org" "/Users/ricky/notes/2020-06-04.org" "/Users/ricky/notes/2020-06-05.org" "/Users/ricky/notes/2020-06-06.org" "/Users/ricky/notes/2020-06-07.org" "/Users/ricky/notes/2020-06-08.org" "/Users/ricky/notes/2020-06-09.org" "/Users/ricky/notes/2020-06-10.org" "/Users/ricky/notes/2020-06-11.org" "/Users/ricky/notes/2020-06-12.org" "/Users/ricky/notes/2020-06-14.org" "/Users/ricky/notes/2020-06-15.org" "/Users/ricky/notes/2020-06-16.org" "/Users/ricky/notes/2020-06-17.org" "/Users/ricky/notes/2020-06-18.org" "/Users/ricky/notes/2020-06-19.org" "/Users/ricky/notes/2020-06-20.org" "/Users/ricky/notes/2020-06-21.org" "/Users/ricky/notes/2020-06-22.org" "/Users/ricky/notes/2020-06-23.org" "/Users/ricky/notes/2020-06-24.org" "/Users/ricky/notes/2020-06-25.org" "/Users/ricky/notes/2020-06-28.org" "/Users/ricky/notes/20200520182622-org_roam.org" "/Users/ricky/notes/20200520190206-org_mode.org" "/Users/ricky/notes/20200521120244-roam.org" "/Users/ricky/notes/20200521162530-dotfiles.org" "/Users/ricky/notes/20200524111223-marketing.org" "/Users/ricky/notes/20200524145601-voxbox.org" "/Users/ricky/notes/20200524151408-github_com_pion_webrtc.org" "/Users/ricky/notes/20200524161045-distributed_systems.org" "/Users/ricky/notes/20200524183413-engineering_success.org" "/Users/ricky/notes/20200526142341-computer_setup.org" "/Users/ricky/notes/20200526142520-zsh.org" "/Users/ricky/notes/20200526192638-emacs.org" "/Users/ricky/notes/20200527113921-mysql.org" "/Users/ricky/notes/20200527123204-object_storage.org" "/Users/ricky/notes/20200528112512-minio.org" "/Users/ricky/notes/20200528120600-ghes.org" "/Users/ricky/notes/20200528190438-chrome_el.org" "/Users/ricky/notes/20200602122131-anti_racism.org" "/Users/ricky/notes/20200604210440-quotes.org" "/Users/ricky/notes/20200606145854-prom2collectd.org" "/Users/ricky/notes/20200608100500-reviews.org" "/Users/ricky/notes/20200610164517-golang.org" "/Users/ricky/notes/articles.org" "/Users/ricky/notes/books.org" "/Users/ricky/notes/emacs-config.org" "/Users/ricky/notes/github.org" "/Users/ricky/notes/inbox.org" "/Users/ricky/notes/projects.org" "/Users/ricky/notes/quotes.org" "/Users/ricky/notes/resume.org" "/Users/ricky/notes/rules.org" "/Users/ricky/notes/supplements.org" "/Users/ricky/notes/vocab.org")))
 '(org-roam-server-mode nil)
 '(package-selected-packages
   (quote
    (perfect-margin helm-org atomic-chrome edit-server golint gotest go-test go-rename use-package)))
 '(perfect-margin-mode t)
 '(tramp-syntax (quote simplified) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
