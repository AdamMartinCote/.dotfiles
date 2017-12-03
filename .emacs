;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Emacs Configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;; remap C-x to C-,
(global-set-key (kbd "C-,") ctl-x-map)

(setq frame-title-format "Emacs")

;; remove menus and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers everywhere
;; (global-linum-mode t)
(linum-relative-global-mode)
;; display line number on current line
(setq linum-relative-current-symbol "")

;; Emacs is always a single instance
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Evil Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'evil)
;; activate
(evil-mode)
;; toggle comments with "gcc"
(evil-commentary-mode)
;;Exit insert mode
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

;; Auto-complete
(global-auto-complete-mode)

(ido-mode)
(column-number-mode)
(show-paren-mode)

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

;; Ace Jump keybinding (jump anywhere on the screen easily)
(add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; whole line on cursor
(global-hl-line-mode)

;; better window switching
(global-set-key (kbd "M-p") 'ace-window)

;; switch window with shift-arrow
(windmove-default-keybindings) 

;; Package Manager
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (linum-relative typescript-mode smex smart-mode-line monokai-theme monokai-alt-theme molokai-theme key-chord evil-magit evil-escape evil-commentary eproject elpy darkokai-theme auto-complete ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; smart-mode-line : a better bottom bar with colors
(sml/setup)
