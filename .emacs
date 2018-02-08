;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .
				 "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" "8eafb06bf98f69bfb86f0bfcbe773b44b465d234d4b95ed7fa882c99d365ebfd" "61003d455ba1bad9a3bf8be7342e848ca3febe899319e95a9dc3d804d9697608" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
			 (:color "#808080"))
     (implicitParams :underline
		     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(package-selected-packages
   (quote
    (paredit irony helm-fuzzy-find maxframe scala-mode latex-extra toml toml-mode rust-mode dumb-jump 2048-game better-shell magit disaster god-mode darcula-theme projectile monokai-theme evil pdf-tools auctex-latexmk auctex multi-term auto-complete markdown-mode better-defaults)))
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; remove menus and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Key mapping
(global-set-key (kbd "C-,") ctl-x-map)
(keyboard-translate ?\C-j ?\C-x)

;; move by a greater increment
(global-set-key (kbd "M-n") (lambda() (interactive) (next-line 8)))
(global-set-key (kbd "M-p") (lambda() (interactive) (previous-line 8)))

 ;; other window backward
(global-set-key (kbd "C-x O") (lambda() (interactive) (other-window -1)))
 ;; line numbers everywhere
(global-linum-mode t)

;; size temporary buffer to content
(temp-buffer-resize-mode)

(global-auto-complete-mode t)

(when window-system (set-frame-size (selected-frame) 130 55))
(when window-system (set-frame-position (selected-frame) 350 0))
(put 'upcase-region 'disabled nil)

(setq TeX-PDF-mode t)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

;; Ditch Splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; get hints vertically in C-x b
(ido-mode t)
(setq ido-separator "\n")

;; Org mode C-x
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-f") nil))

;; C settings
(setq-default c-basic-offset 8)

(global-set-key (kbd "C-x C-b") 'bs-show)

(global-hl-line-mode 1)

(dumb-jump-mode 1)
(put 'narrow-to-region 'disabled nil)

;; windows flash on error
(setq visible-bell 1)

;; auto fill in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(put 'scroll-left 'disabled nil)

;; Window resize shortcuts
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "S-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-j") 'shrink-window)
(global-set-key (kbd "S-C-k") 'enlarge-window)

;; Make transparent
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 75))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))

;; open from eshell
(defalias 'ff 'find-file)
(defalias 'emacs 'find-file)
(defalias 'ffo 'find-file-other-window)




(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c r") 'query-replace)

(global-set-key (kbd "C-c a s") 'eshell-other-window)
(global-set-key (kbd "C-c a d") 'dired)

(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(show-paren-mode t)
(electric-pair-mode t)
