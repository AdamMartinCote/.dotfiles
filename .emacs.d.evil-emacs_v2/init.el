(require 'package)

(require 'org)
(require 'ob-tangle)

;; Loading main configuration file
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "configs.org" init-dir))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(use-package elpy :ensure t)
(use-package neotree :ensure t)
(use-package undo-tree :ensure t)
(use-package swiper :ensure t)
(use-package counsel :ensure t)
(use-package helm :ensure t)
(use-package easy-kill :ensure t)
(use-package emmet-mode :ensure t)

(elpy-enable)
(company-mode 1)
(add-to-list 'company-backends 'company-tern)
(global-set-key (kbd "C-c e") 'emmet-expand-line)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)


(setq auto-revert-interval 0.5)
(setq-default fill-column 95)

(global-auto-revert-mode 1)

(global-linum-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai-alt)))
 '(custom-safe-themes
   (quote
    ("d1ede12c09296a84d007ef121cd72061c2c6722fcb02cb50a77d9eae4138a3ff" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "5f4e4c9f5de8156f964fdf8a1b8f8f659efbfeff88b38f49ce13953a84272b77" default)))
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#37474f")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (evil-nerd-commenter dashboard doom-modeline spaceline golden-ratio rjsx-mode monokai-alt-theme company-tern json-mode scss-mode nlinum-relative evil-org buffer-move fish-mode pdf-tools csv-mode neotree emmet-mode php-mode easy-kill ivy swiper counsel markdown-mode+ django-mode gitignore-mode material-theme eshell-did-you-mean eshell-prompt-extras eshell-up fish-completion elpy evil-escape helm monokai-theme evil-magit evil-mc evil-smartparens evil-tutor evil-vimish-fold format-all magit undo-tree which-key use-package try evil)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#262626" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(org-level-1 ((t (:inherit outline-1 :background "#455A64" :weight bold :height 1.3)))))

(electric-pair-mode t)
(setq select-enable-clipboard t)
(fset 'yes-or-no-p 'y-or-n-p)




(setq c-default-style "linux"
      c-basic-offset 8)
(global-hl-line-mode 1)
(helm-mode t)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)


(global-set-key (kbd "C-c t") 'neotree)
(global-set-key (kbd "C-c v") 'evil-mode)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq inhibit-startup-message t) ;; hide the startup message

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-undo-tree-mode)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    ;; (setq undo-tree-visualizer-timestamps t)
    ;; (setq undo-tree-visualizer-diff t)
    ))
(global-set-key (kbd "C-S-?") 'undo-tree-redo)

(show-paren-mode t)
(electric-pair-mode t)

(global-set-key (kbd "C-x u") 'undo-tree-visualize)

(setq save-interprogram-paste-before-kill t)
(global-set-key [remap kill-ring-save] 'easy-kill)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") 'ibuffer)


(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(ivy-mode 1)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ; Load Dired X when Dired is loaded.
(setq dired-omit-mode t) ; Turn on Omit mode.

(global-set-key (kbd "C-c m") 'magit-status)

(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)
(defalias 'o 'find-file-other-window)
(defalias 'emacs 'find-file)
(defalias 'em 'find-file)
(defalias 'e 'find-file)

(keyboard-translate ?\C-j ?\C-x)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   3)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 3)) )
				
(global-set-key (kbd "C-c o") 'counsel-imenu)
(global-set-key (kbd "M-o") 'other-window)

(defun dfeich/ansi-terminal (&optional path name)
  "Opens an ansi terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path.
The ansi-term buffer is named based on `name' "
  (interactive)
  (unless path (setq path default-directory))
  (unless name (setq name "ansi-term"))
  (ansi-term "/bin/bash" name)
  (let ((path (replace-regexp-in-string "^file:" "" path))
	(cd-str
	 "fn=%s; if test ! -d $fn; then fn=$(dirname $fn); fi; cd $fn;")
	(bufname (concat "" name "" )))
    (if (tramp-tramp-file-p path)
	(let ((tstruct (tramp-dissect-file-name path)))
	  (cond
	   ((equal (tramp-file-name-method tstruct) "ssh")
	    (process-send-string bufname (format
					  (concat  "ssh -t %s '"
						   cd-str
						   "exec bash'; exec bash; clear\n")
					  (tramp-file-name-host tstruct)
					  (tramp-file-name-localname tstruct))))
	   (t (error "not implemented for method %s"
		     (tramp-file-name-method tstruct)))))
      (process-send-string bufname (format (concat cd-str " exec bash;clear\n")
					   path)))))
