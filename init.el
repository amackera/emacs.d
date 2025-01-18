(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:jupyter-server-use-subcommand "server")
 '(ein:output-area-inlined-images t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(web-mode eruby-mode minitest enh-ruby-mode robe robe-mode prettier-js ox-hugo clojure-mode vterm logview python-black robot-mode yaml-mode auto-dim-other-buffers :pyvenv :transient paredit orderless flycheck company direnv poetry exec-path-from-shell lsp-ui lsp-mode rainbow-delimiters eyebrowse fira-code-mode kaolin-themes projectile vertico magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "light slate gray" :slant normal)))))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(defvar personal-keybindings nil
  "A list to store personal keybindings.")
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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq inhibit-startup-message t)

;; Select the help window when it opens
(setq help-window-select t)

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

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

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

(use-package paredit
  :ensure t)

(use-package direnv
 :config
 (direnv-mode))

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package vertico
  :init
  (vertico-mode))

(use-package projectile)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-galaxy t))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "x"))
  :hook prog-mode)

(use-package eyebrowse
  :init
  (eyebrowse-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package transient
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package poetry
  :config (poetry-tracking-mode))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package auto-dim-other-buffers
  :init
  (add-hook 'after-init-hook (auto-dim-other-buffers-mode t)))

(use-package clojure-mode
  :demand t)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package yaml-mode)

(use-package robot-mode)

(use-package logview)

(use-package vterm
  :ensure t
  :config
  (setq vterm-environment
        (append vterm-environment '("VISUAL=emacsclient"))))

(with-eval-after-load 'ox
  (require 'ox-hugo))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(setq org-directory "~/Library/CloudStorage/Dropbox/org")

(add-to-list 'load-path "/Users/amackera/.emacs.d/asdf")
(require 'asdf)

(asdf-enable)

(use-package prettier-js
  :ensure t
  :init
  (add-hook 'js-mode-hook 'prettier-js-mode))

(setq js-indent-level 2)

(use-package enh-ruby-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (eglot-ensure)
              (flyckeck-select-checker 'ruby-standard)
              (flycheck-mode 1)
              (setq ruby-indent-level 2)  ; Use 2 spaces for indentation
              (setq ruby-indent-tabs-mode nil) ; Use spaces instead of tabs
              (setq ruby-deep-indent-paren nil) ; Disable deep indentation inside parentheses

              ;; Customize hash indentation
              (setq ruby-align-chained-calls nil) ; Prevent chaining alignment
              (setq ruby-align-to-stmt-keyword nil) ; No alignment to statement keywords
              )))

(with-eval-after-load 'flycheck
  ;; Disable RuboCop for Ruby files
  (setq-default flycheck-disabled-checkers '(ruby-rubocop)))

(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

(require 'eglot)
;; Add solargraph as the server for enh-ruby-mode
(add-to-list 'eglot-server-programs
             '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))

;; (use-package minitest
;;   :ensure t
;;   :init
;;   (add-hook 'enh-ruby-mode-hook 'minitest-mode))

(use-package minitest
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'minitest-mode)
  :config
  (defun my-minitest-run-test-at-point ()
    "Run the test containing the point, handling special characters like #."
    (interactive)
    (save-excursion
      (or (re-search-backward "^ *test ['\"]\\(.*?\\)['\"]" nil t)
          (re-search-forward "^ *test ['\"]\\(.*?\\)['\"]" nil t))
      (let ((test-name (match-string 1)))
        (if test-name
            (let ((default-directory (minitest-project-root)))
              (compile (format "ruby -Itest %s --name='%s'"
                               (shell-quote-argument (buffer-file-name))
                               test-name)))
          (message "Could not find a test definition near point.")))))
  (advice-add 'minitest-verify-single :override #'my-minitest-run-test-at-point))

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(enh-ruby 2))
  (add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
  :init
  (add-hook 'prog-mode-hook 'copilot-mode))


(use-package ein
  :ensure t
  :config
  (setq ein:password "abc123")
  (setq ein:output-area-inlined-images t))

(image-type-available-p 'png)

;; Make grep results open in the current window
(add-to-list 'display-buffer-alist
             '("\\*grep\\*"
               (display-buffer-reuse-window
                display-buffer-same-window)))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '("^\\*Minitest\\( \\| .*\\)\\*$"
          :regexp t
          :position bottom
          :height 0.3
          :dedicated nil
          :stick t) ;; Reuse a single window for all Minitest buffers
        popwin:special-display-config))
