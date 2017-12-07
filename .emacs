;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Emacs Configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;; remap C-x to C-,
(global-set-key (kbd "C-,") ctl-x-map)

(setq frame-title-format "Evil-Vim")

;; remove menus and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers everywhere
;; (global-linum-mode t) ;; normal line numbering
(linum-relative-global-mode)
;; display line number on current line
(setq linum-relative-current-symbol "")

;; Emacs is always a single instance
(server-start)

;; Prevent opening stuff in other window
(set-frame-parameter nil 'unsplittable t)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-x") 'smex-major-mode-commands)
;; This is your old M-x
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-x") 'execute-extended-command)

;; Powerline (bottom bar inspired by vim)
(powerline-center-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Evil Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-want-C-u-scroll t)
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

(global-set-key (kbd "C-c y") 'company-yasnippet)

;; mini-buffer with completion
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow my custom keybinding in org-mode
(with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-,") nil))
(with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-p") nil))


;; Package Manager
(require 'package)

;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/")
;; 	     t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(fci-rule-color "#3C3D37")
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
 '(ivy-mode t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (dired+ sane-term web-mode powerline yasnippet auctex pdf-tools gruber-darker-theme ivy org linum-relative typescript-mode smex smart-mode-line monokai-theme monokai-alt-theme molokai-theme key-chord evil-magit evil-escape evil-commentary eproject elpy darkokai-theme auto-complete ace-window ace-jump-mode)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; smart-mode-line : a better bottom bar with colors
;; need to be after ... so emacs knows it is safe
(sml/setup)
