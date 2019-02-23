;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Load main config file
(require 'org)
(org-babel-load-file
 (expand-file-name
	(concat user-emacs-directory "config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-selected-packages
	 (quote
		(pretty-mode vdiff expand-region jsx-mode rjsx-mode dashboard helm-ag helm-projectile projectile emmet-mode csv-mode centered-window visual-regexp go-mode auto-complete multi-web-mode lorem-ipsum web-mode use-package markdown-mode+ helm doom-themes)))
 '(tab-width 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#282c34")))))
(put 'scroll-left 'disabled nil)
