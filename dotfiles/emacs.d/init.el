;; This file tries to do as little as possible before calling into a
;; literate org-mode file that actually configures my emacs.
;;
;; First thing I do here is configure use-package such that I can
;; update org-mode before loading babel.
;;
;; A lot of this comes from here: https://github.com/hrs/dotfiles/blob/master/emacs/dot-emacs.d/init.e

;; for native compilation in emacs
;; https://github.com/jimeh/build-emacs-for-macos 
(setq comp-speed 2)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; from https://github.com/quelpa/quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

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
 '(custom-enabled-themes '(Greymatters-light))
 '(custom-safe-themes
   '("45ceba3998a6e95a6c225fd68598c3bd78f644726af186cb4781444a41c49740" "4453f05bcdcf14a1f6b0d0a033a20a2edb207e76cf615a04424205e82ba6e76a" "a39c8a757b00de52f90a18d613a4035cf8f4d56cd13d95e133f691a0d97a4bdd" "5950d9968ae2a3fc9e448b99a061f9092c02354bb2e6a881c7c7f15f59b9bae7" "6109efe7296c9c481738bce09d51bd2b0b7d893e76b9ba3c78c4d8bcf1588480" "160b7ab82951531516af0e66edc43dca2c4d7713e5cf57dd3823e90e81a31421" "93826faafe9e0927b644bb39fa4c20a97feaa38f12f02b8ca966e765e5ac5e38" "4141a890d44a138e910c95de775c914aba245d3137dc6601f660d42970fb7eb5" "370554981c9358ddd09e9c0a50567f51ae1586303f2ea1226a15b934fb2d3bcd" "9b02a4fd5a0fa3d0b5d326057ecbf6e4f46186c16c6c42312b3ac29f6ee35548" "fb5a4e1730c349ca59a6955cb513e00ec6adb545db79fae0dbafc28af82c59ff" "323964cdaee8528eeff0b721974bb033c44a2b73d795a31ceba554f764109a20" "0da70fb7c272d8e87b9343a6f36d7f9eed520ef64beaab03478113968fbed1d9" "0aa971daea004737402f4cd0575cfa6dc2fa00206cd03c77ee2c3b5d3b5f19d6" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "0a8b8aad17f3bd4af1839a49cd97048c90db98a088b1f6f6d0d2187a3c35321d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "319bf1bab5d05e3a4c4a165efe69d27b3d975759034074f15fe61e92c7304884" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(display-battery-mode t)
 '(electric-pair-mode t)
 '(global-hl-line-mode t)
 '(hscroll-margin 8)
 '(hscroll-step 1)
 '(org-agenda-files '("~/notes"))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-tags-column 'auto)
 '(org-roam-server-mode t t)
 '(package-selected-packages
   '(centered-window focus graphql-mode helm-posframe browse-at-remote bufler magit adaptive-wrap folding pullover yasnippet yaml-mode switch-window quelpa-use-package protobuf-mode plantuml-mode perfect-margin pbcopy paredit ox-reveal ox-pandoc ox-hugo ox-gfm org-roam-bibtex org-reverse-datetree org-ref org-ql org-pomodoro org-plus-contrib org-cv org-bullets ob-go lsp-ui json-mode helm-projectile helm-flyspell heaven-and-hell hcl-mode gotest golint golden-ratio go-rename go-imports go-guru git-link flycheck-gometalinter expand-region edit-server dockerfile-mode diff-hl company-lsp calfw-org calfw beacon auto-package-update auto-compile atomic-chrome ace-jump-mode))
 '(perfect-margin-mode t)
 '(tramp-syntax 'simplified nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
