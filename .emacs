;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("8eafb06bf98f69bfb86f0bfcbe773b44b465d234d4b95ed7fa882c99d365ebfd" "61003d455ba1bad9a3bf8be7342e848ca3febe899319e95a9dc3d804d9697608" default)))
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
    (god-mode darcula-theme projectile monokai-theme evil pdf-tools auctex-latexmk auctex multi-term auto-complete markdown-mode better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 ;; remap C-x to C-,
(global-set-key (kbd "C-,") ctl-x-map)

 ;; line numbers everywhere
(global-linum-mode t)

;; size temporary buffer to content
(temp-buffer-resize-mode)

(global-auto-complete-mode t)

(when window-system (set-frame-size (selected-frame) 85 40))
(put 'upcase-region 'disabled nil)

(setq TeX-PDF-mode t)

 ;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

 ;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

 ;; Remove splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(define-key global-map (kbd "RET") 'newline-and-indent)

 ;; get hints vertically in C-x b
(setq ido-separator "\n")
