(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(defalias 'yes-or-no-p 'y-or-n-p)
(auto-fill-mode t)

(require 'auto-complete)
(global-auto-complete-mode t)

(setq tab-width 2)

(setq backward-delete-char-untabify-method 'hungry)

;; (global-set-key (kbd "TAB") 'self-insert-command)

(when (display-graphic-p)
	(global-unset-key "\C-z")
	(global-set-key "\C-z" 'advertised-undo)
	)

(global-hl-line-mode t)

;; (global-whitespace-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-linum-mode t)

(delete-selection-mode 1)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq show-paren-delay 0)
(show-paren-mode 1)

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle
   between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq inhibit-startup-screen t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 90))

(require 'dashboard)
(use-package dashboard
  :ensure t
  :preface
  (defun my/dashboard-banner ()
  """Set a dashboard banner including information on package initialization
   time and garbage collections."""
   (setq dashboard-banner-logo-title
        (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  ;; (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  ;; (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
  :config
  ;; (setq dashboard-startup-banner 'logo)
  (when (display-graphic-p)
   (dashboard-setup-startup-hook))
)

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
							("TAB"   . #'helm-execute-persitent-action))
  :bind (:map helm-map
							("<tab>" . #'helm-execute-persistent-action))
  :bind (:map helm-map
							("C-z"   . #'helm-select-action))
  )

(setq projectile-key		(kbd "C-c p"))
(use-package projectile
	:ensure t
	:config
	(projectile-global-mode)
	(define-key projectile-mode-map projectile-key
	'projectile-command-map)
	(setq projectile-project-search-path '("~/code/tp/"))
	)

(use-package helm-projectile
	:ensure t
	:config
	(helm-projectile-on)
	)

(global-set-key (kbd "C-c e") 'emmet-expand-line)

(setq visual-regexp-key (kbd "C-c r"))
(use-package visual-regexp
	:ensure t
	:config
	(global-set-key visual-regexp-key 'vr/replace)
	)

(use-package centered-window :ensure t)

(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c d") vdiff-mode-prefix-map)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize))

(global-set-key (kbd "C-c l s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c l p") 'lorem-ipsum-insert-paragraphs)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; (global-set-key (kbd "TAB") 'self-insert-command)

;; (when window-system
;;       (use-package pretty-mode
;;       :ensure t
;;       :config
;;       (global-pretty-mode t)))

(define-key key-translation-map (kbd "C-c a !") (kbd "⚠"))
(define-key key-translation-map (kbd "C-c a s") (kbd "✰"))
