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

;; text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (global-linum-mode t)

;; Disable C-z in GUI
(when (display-graphic-p)
	(global-unset-key "\C-z")
	(global-set-key "\C-z" 'advertised-undo)
	)

;; (global-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(global-hl-line-mode t)

;; org-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; reading mode
(use-package centered-window :ensure t)

;; ⚠tests
(setq foo (kbd "C-c w"))
(global-set-key foo 'next-line)


(use-package visual-regexp
	:ensure t
	:bind ("C-c r" . vr/replace))

;; Dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-items '((recents   . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook))

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 90))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))


;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  :bind ("C-x C-f" . helm-find-files)
  :bind ("C-x C-b" . helm-buffers-list)

  ;; Tab completion in helm
  :bind (:map helm-map
							("TAB" . #'helm-execute-persitent-action))
  :bind (:map helm-map
							("<tab>"  . #'helm-execute-persistent-action))
  :bind (:map helm-map
							("C-z" . #'helm-select-action))
  )

;; delete all tabs on backspace
(setq backward-delete-char-untabify-method 'hungry)

;; Special Unicode Chars
(define-key key-translation-map (kbd "C-c a !") (kbd "⚠"))
(define-key key-translation-map (kbd "C-c a s") (kbd "✰"))

(defalias 'yes-or-no-p 'y-or-n-p)
(auto-fill-mode t)

(setq tab-width 2)
(global-auto-complete-mode t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; replace selected text like modern editor
(delete-selection-mode 1)

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize))

;; last buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle
   between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)

;; web-mode for .html files
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-selected-packages
	 (quote
		(emmet-mode csv-mode centered-window visual-regexp go-mode auto-complete multi-web-mode lorem-ipsum web-mode use-package markdown-mode+ helm doom-themes dashboard)))
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
