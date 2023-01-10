(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(paredit orderless flycheck company direnv poetry exec-path-from-shell lsp-ui lsp-mode rainbow-delimiters eyebrowse fira-code-mode kaolin-themes projectile vertico magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Prefix non-unique buffer names with their parent dir
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Better window movement
(windmove-default-keybindings)

;; UI customization
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq-default frame-title-format "%b (%f)")
(setq ring-bell-function 'ignore)
(setq-default left-fringe-width  10)
;; Split windows vertically instead of horizontally
(setq split-width-threshold 0)
(setq split-height-threshold nil)
;; Clickable links
(goto-address-mode)
(show-paren-mode 1)
; Delete trailing whitespace on save
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-message t)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; MISC

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq create-lockfiles nil)


;; Global Key Bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; ;; Packages and config
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :if (memq window-system '(mac ns x))
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH"))
;;   (exec-path-from-shell-initialize))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package direnv
 :config
 (direnv-mode))

(use-package magit)

(use-package vertico
  :init
  (vertico-mode))

(use-package projectile)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package kaolin-themes
  :config
  (setq monokai-comments "#919191")
  (load-theme 'kaolin-galaxy t))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "x"))
  :hook prog-mode)

(use-package eyebrowse
  :init
  (eyebrowse-mode))

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode)
  :hook prog-mode)

(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :hook
  ((python-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package poetry)

(use-package paredit
  ;; See https://suvratapte.com/configuring-emacs-from-scratch-use-package/
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
