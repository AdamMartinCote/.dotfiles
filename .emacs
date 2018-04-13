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
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" "53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" "8eafb06bf98f69bfb86f0bfcbe773b44b465d234d4b95ed7fa882c99d365ebfd" "61003d455ba1bad9a3bf8be7342e848ca3febe899319e95a9dc3d804d9697608" default)))
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
 '(linum-format (quote "%3d"))
 '(package-selected-packages
   (quote
    (multiple-cursors autotetris-mode smart-mode-line pyenv-mode virtualenvwrapper flycheck-plantuml js-comint inf-mongo google transpose-frame elpy nlinum flymake-json json-mode linum-relative go-mode flycheck evil-numbers evil-search-highlight-persist evil-tutor evil-vimish-fold gitignore-mode atomic-chrome function-args modern-cpp-font-lock plantuml-mode csv csv-mode cyberpunk-theme deferred epc python-environment jedi-direx jedi jedi-core paredit irony helm-fuzzy-find maxframe scala-mode latex-extra toml toml-mode rust-mode dumb-jump 2048-game better-shell magit disaster god-mode darcula-theme projectile monokai-theme evil pdf-tools auctex-latexmk auctex multi-term auto-complete markdown-mode better-defaults)))
 '(python-shell-interpreter "python3")
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(server-start)
(load "server")
(unless (server-running-p) (server-start))
;; remove menus and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Key mapping
(global-set-key (kbd "C-,") ctl-x-map)

;; move by a greater increment
(global-set-key (kbd "M-n") (lambda() (interactive) (next-line 8)))
(global-set-key (kbd "M-p") (lambda() (interactive) (previous-line 8)))

(windmove-default-keybindings)
(global-set-key (kbd "C-x O") (lambda() (interactive) (other-window -1)))
(global-set-key (kbd "C-M-o") (lambda() (interactive) (other-window  1)))
(global-set-key (kbd "C-M-1") 'delete-other-windows)
(global-set-key (kbd "C-M-2") 'split-window-below)
(global-set-key (kbd "C-M-3") 'split-window-right)
(global-set-key (kbd "C-M-0") 'delete-window)

 ;; line numbers everywhere (nlinum)
(global-nlinum-mode t)

(setq hl-line-sticky-flag nil) ;; hl-line only in current window
(temp-buffer-resize-mode) ;; size temporary buffer to content

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

(smart-mode-line-enable)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; C settings
(setq-default c-basic-offset 4)

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
(set-frame-parameter (selected-frame) 'alpha '(97 . 90))
(add-to-list 'default-frame-alist '(alpha . (97 . 90)))

;; eshell aliases
(defalias 'ff 'find-file)
(defalias 'emacs 'find-file)
(defalias 'ffo 'find-file-other-window)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c s s") 'eshell)
(global-set-key (kbd "C-c s o") 'eshell-other-window)
(global-set-key (kbd "C-c s p") 'elpy-shell-switch-to-shell)
(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c c") 'calc)
(global-set-key (kbd "C-c t") 'transpose-frame)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(show-paren-mode t)
(electric-pair-mode t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(re-search-forward "glob" nil t)

;; emulate vim %
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise go to next parenthesis"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (re-search-forward "[\(\)]") (backward-char 1) )))

(global-set-key	(kbd "C-;") 'goto-match-paren)

;; system clipboard goes to kill ring
(setq select-enable-clipboard t)

;; zap-UP-to-char
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR. \(fn arg char)"
    'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Preset `nlinum-format' for minimum width.
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

(elpy-enable)

;; # commented out for slow startup??
;; (defvar myPackages
;;   '(better-defaults
;;     elpy
;;     flycheck ;; add the flycheck package
;;     material-theme))

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(global-set-key (kbd "M-,") 'xref-pop-marker-stack)


(defun toggle-maximize-buffer () "Maximize buffer temporarly"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key (kbd "C-x x") 'toggle-maximize-buffer)

(helm-mode 1)

(setq-default fill-column 100)

(setq elpy-rpc-python-command "python3")

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

(global-set-key (kbd "C-c p") 'whitespace-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(global-undo-tree-mode)
