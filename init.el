;;; --- bootstrap package.el ---
(setq package-enable-at-startup t)
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;; --- UI / basics ---
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-message t ring-bell-function 'ignore)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-;") (lambda () (interactive)
                               (comment-or-uncomment-region (line-beginning-position)
                                                            (line-end-position))))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq mac-command-modifier 'meta)
(when (display-graphic-p) (set-frame-font "Fira Code-12" t t))
(server-start)

;; Centralize backup and auto-save files
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "lock-files/" user-emacs-directory) t)))

;; Create directories if they don't exist
(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)
(make-directory (expand-file-name "lock-files/" user-emacs-directory) t)

;;; --- performance niceties ---
(pixel-scroll-precision-mode -1)
(setq gc-cons-threshold 100000000 gc-cons-percentage 0.6)
(add-hook 'focus-out-hook #'garbage-collect)
(run-with-idle-timer 5 t #'garbage-collect)
(dolist (hook '(org-mode-hook term-mode-hook vterm-mode-hook shell-mode-hook
                              treemacs-mode-hook eshell-mode-hook pdf-view-mode-hook
                              claude-code-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;; --- asdf setup ---
(add-to-list 'load-path "~/.emacs.d/site-lisp/asdf-vm")

(use-package asdf-vm
  :ensure nil            ;; don’t try to fetch from MELPA
  :hook (after-init . asdf-vm-init))

;;; --- env / shell paths ---
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables
        '("PATH" "ASDF_DIR" "ASDF_DATA_DIR" "PYENV_ROOT" "NVM_DIR"))
  :config
  (exec-path-from-shell-initialize))

;;; --- completion stack ---
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

;;; --- projects / git / terminals ---
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
          (setq projectile-indexing-method 'native
                projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Auto-open vterm buffers when switching projects
(defun my-projectile-open-vterms ()
  "Open three vterm buffers (server, shell, console) for the current project."
  (let* ((project-name (projectile-project-name))
         (project-root (projectile-project-root))
         (buffers '("server" "shell" "console")))
    (dolist (buf-name buffers)
      (let ((vterm-buffer-name (format "*vterm-%s-%s*" project-name buf-name)))
        (unless (get-buffer vterm-buffer-name)
          (let ((vterm-shell (getenv "SHELL")))
            (with-current-buffer (vterm vterm-buffer-name)
              (rename-buffer vterm-buffer-name))))))))

(add-hook 'projectile-after-switch-project-hook #'my-projectile-open-vterms)

(use-package magit
  :config (setq magit-display-buffer-function
                'magit-display-buffer-same-window-except-diff-v1))

(use-package vterm
  :ensure t
  :bind (("C-x v" . vterm))
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :config (setq vterm-environment (append vterm-environment '("VISUAL=emacsclient"))))

;;; --- visuals / misc ---
(use-package kaolin-themes :config (load-theme 'kaolin-galaxy t))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; Dim inactive buffers to make active buffer obvious
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode t)
  ;; Adjust the dimming intensity (0.0 = no dim, 1.0 = very dim)
  (setq dimmer-fraction 0.35)
  ;; Don't dim transient popup menus (like magit's ? menu)
  (setq dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (with-eval-after-load 'transient
    (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*transient\\*")))

;; Highlight active mode line
(set-face-attribute 'mode-line nil
                    :background "#3a5a7a"  ;; slightly darker blue for better contrast
                    :foreground "#f0f0f0"  ;; bright but not harsh white
                    :weight 'semi-bold     ;; make text bolder for readability
                    :box '(:line-width 2 :color "#3a5a7a"))
(set-face-attribute 'mode-line-inactive nil
                    :background "#2e3440"  ;; slightly darker gray
                    :foreground "#8a8f98"  ;; slightly brighter for readability
                    :weight 'normal
                    :box '(:line-width 1 :color "#232831"))
(use-package yaml-mode)
(use-package logview)
(use-package web-mode
  :mode (("\\.erb\\'" . web-mode) ("\\.phtml\\'" . web-mode) ("\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode) ("\\.as[cp]x\\'" . web-mode) ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :custom (web-mode-markup-indent-offset 2)
          (web-mode-css-indent-offset 2)
          (web-mode-code-indent-offset 2))
(use-package markdown-mode :mode "\\.md\\'")

;;; --- Org (minimal you used) ---
(use-package org
  :config
  (setq org-directory "~/Library/CloudStorage/Dropbox/org"
        org-agenda-files '("~/Library/CloudStorage/Dropbox/org/tasks.org"
                           "~/Library/CloudStorage/Dropbox/org/calendar.org"
                           "~/Library/CloudStorage/Dropbox/org/inbox.org")
        org-default-notes-file "~/Library/CloudStorage/Dropbox/org/inbox.org"
        org-log-done 'time org-startup-indented t org-hide-leading-stars t)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda))
(use-package org-bullets :hook (org-mode . org-bullets-mode))

;; elixir / erlang OTP
(use-package elixir-ts-mode
  :ensure t
  :mode "\\.exs?\\'"
  :hook ((elixir-ts-mode . eglot-ensure)
         (elixir-ts-mode . (lambda ()
                             (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  :init
  ;; Prefer ts mode when available
  (setq major-mode-remap-alist
        (append '((elixir-mode . elixir-ts-mode)) major-mode-remap-alist))
  :config
  (setq elixir-ts-indent-offset 2))

(use-package heex-ts-mode
  :ensure t
  :mode "\\.heex\\'"
  :hook ((heex-ts-mode . eglot-ensure)
         (heex-ts-mode . (lambda ()
                           (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))

;;; --- JavaScript/TypeScript ---
(use-package js-mode
  :ensure nil  ;; built-in
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;;; --- Python: Eglot + Pyright (single source of truth) ---
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Auto-activate virtualenv (works with uv's .venv)
(use-package pyvenv
  :ensure t
  :hook (python-ts-mode . (lambda ()
                            (let ((venv-path (concat (projectile-project-root) ".venv")))
                              (when (file-directory-p venv-path)
                                (pyvenv-activate venv-path))))))

(use-package eglot
  :ensure t
  :hook ((python-ts-mode python-ts-mode js-mode js-ts-mode json-ts-mode
                         typescript-mode tsx-ts-mode) . eglot-ensure)
  :config
  ;; Pyright
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  ;; JS/TS
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  ;; JSON
  (add-to-list 'eglot-server-programs
               '((json-ts-mode) . ("vscode-json-language-server" "--stdio")))
  ;; Elixir
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("/Users/amackera/elixir-ls-v0.29.3/language_server.sh")))  
    ;; Ruby (optional, keep if you use it)
  (add-to-list 'eglot-server-programs
               '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport))))

;; Ruff + Black via pylsp (disable overlapping linters)
(setq eglot-workspace-configuration
      '((pylsp . ((plugins .
                    ((ruff . ((enabled . t)))
                     (black . ((enabled . t)))
                     (pylsp-mypy . ((enabled . t)
                                   (live_mode . t)
                                   (strict . t)))
                     (pyflakes . ((enabled . nil)))
                     (mccabe . ((enabled . nil)))
                     (pylint . ((enabled . nil)))
                     (flake8 . ((enabled . nil)))
                     (yapf . ((enabled . nil)))
                     (autopep8 . ((enabled . nil)))))))))

;; (optional) format on save
(add-hook 'python-ts-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; If you use Flycheck elsewhere, avoid double diagnostics
(with-eval-after-load 'flycheck
  (add-hook 'python-ts-mode-hook (lambda () (flycheck-mode -1))))

;;; --- Ruby extras you rely on (kept) ---
(use-package enh-ruby-mode
  :mode (("\\.rb\\'" . enh-ruby-mode) ("\\.ru\\'" . enh-ruby-mode))
  :hook (enh-ruby-mode . eglot-ensure))

;; Use tree-sitter for JSON
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;; Hook Eglot to JSON
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((json-ts-mode) . ("vscode-json-language-server" "--stdio"))))

;; Treesitter recipies
(with-eval-after-load 'treesit
  (setq treesit-language-source-alist
        (append treesit-language-source-alist
                '((json   "https://github.com/tree-sitter/tree-sitter-json")
                  (python "https://github.com/tree-sitter/tree-sitter-python")
                  (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                  (heex "https://github.com/phoenixframework/tree-sitter-heex")))))


(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :custom
  (claude-code-terminal-backend 'vterm)
  :config
  ;; (claude-code-mode)
  ;; Disable line numbers in claude-code buffers
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (string-prefix-p "*claude:" (buffer-name))
                (display-line-numbers-mode 0))))
  :bind-keymap ("C-c l" . claude-code-command-map)
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;;; --- keep what you actually use often; comment others out for now ---
;; (use-package treemacs :bind (("C-c t" . treemacs)))
;; (use-package window-purpose :config (purpose-mode))
;; (use-package poetry :config (poetry-tracking-mode))
(use-package direnv :config (direnv-mode))


;; (add-to-list 'load-path "/Users/amackera/.emacs.d/asdf")
;; (require 'asdf)

;; (asdf-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
