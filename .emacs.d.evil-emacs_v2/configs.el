(require 'cl)
(setq tls-checktrust t)

(setq python (or (executable-find "py.exe")
                 (executable-find "python")
                 ))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(setq user-full-name "Adam Martin-Côté")
(setq user-mail-address "me@adammartincote.com")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(put 'scroll-left 'disabled nil)
(column-number-mode 1)

(add-hook 'html-mode-hook 'my-html-mode-hook)
(defun my-html-mode-hook ()
  (goto-address-mode))

(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

(package-initialize)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(global-set-key (kbd "C-é")   (kbd "C-/"))
(global-set-key (kbd "M-¨")   (kbd "M-{"))
(global-set-key (kbd "M-Ç")   (kbd "M-}"))
(global-set-key (kbd "M-'")   (kbd "M-<"))
(global-set-key (kbd "M-\\")  (kbd "M->"))
(global-set-key (kbd "C-M-à") (kbd "C-M-\\"))

(setq-default js-indent-level 2)
(setq-default python-indent-offset 4)

(setq backward-delete-char-untabify-method 'hungry)
(setq-default electric-indent-inhibit t)

;; (setq evil-normal-state-tag   (propertize " COMMAND " 'face '(
;; 	(:background "dark khaki" :foreground "black")))
;;       evil-emacs-state-tag    (propertize "  EMACS  " 'face '(
;; 	(:background "turquoise" :foreground "black")))
;;       evil-insert-state-tag   (propertize " ------- " 'face '(
;; 	(:background "dark sea green" :foreground "black")))
;;       evil-replace-state-tag  (propertize " REPLACE " 'face '(
;; 	(:background "dark orange" :foreground "black")))
;;       evil-motion-state-tag   (propertize "  MOTION " 'face '(
;;         (:background "khaki" :foreground "black")))
;;       evil-visual-state-tag   (propertize "  VISUAL " 'face '(
;;         (:background "light salmon" :foreground "black")))
;; ;;     
;; evil-operator-state-tag (propertize " OPERATE " 'face '(
;; ;
                                        ; 	(:background "sandy brown" :foreground "black"))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; (evil-mode)
;; (evil-vimish-fold-mode 1)
;; (global-set-key (kbd "C-x C-;") 'evilnc-comment-or-uncomment-lines)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
