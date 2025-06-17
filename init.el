(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:jupyter-server-use-subcommand "server")
 '(ein:output-area-inlined-images t t)
 '(grep-command "grep --color=auto -nH --null -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +" . 54))
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "builds"))
 '(grep-find-ignored-files
   '("*.o" "*.elc" "*.so" "*.a" "*.la" "*.lo" "*.dll" "*.exe" "*.obj" "*.pyo" "*.pyc" ".aider.chat.history.md" ".byebug_history" ".aider.input.history"))
 '(grep-find-template
   "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
 '(grep-find-use-xargs 'exec-plus)
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH --null -e <R> <F>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(web-mode eruby-mode enh-ruby-mode robe robe-mode prettier-js ox-hugo clojure-mode vterm logview python-black robot-mode yaml-mode auto-dim-other-buffers :pyvenv :transient paredit orderless flycheck company poetry exec-path-from-shell lsp-ui lsp-mode rainbow-delimiters eyebrowse fira-code-mode projectile vertico magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "light slate gray" :slant normal)))))

(setq mac-command-modifier 'meta)  ;; Use Command as Meta
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(set-frame-font "Fira Code-12" t t)  ;; Set Fira Code as the default font
(server-start)

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

;; Move buffers around
(use-package buffer-move
  :ensure t
  :bind (("C-c <up>"    . buf-move-up)
         ("C-c <down>"  . buf-move-down)
         ("C-c <left>"  . buf-move-left)
         ("C-c <right>" . buf-move-right)))

;; UI customization
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(global-hl-line-mode nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq-default frame-title-format "%b (%f)")
(setq ring-bell-function 'ignore)
(setq-default left-fringe-width  10)

;; Scrolling settings
;; Touchpad - try more extreme values
(pixel-scroll-precision-mode 1)

;; Mouse wheel
(setq mouse-wheel-progressive-speed nil)               ;; Disable acceleration
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))

;; Performance
(setq inhibit-compacting-font-caches t)
(setq-default bidi-display-reordering nil)
(setq jit-lock-defer-time 0.05)

;; Increase garbage collection threshold
(setq gc-cons-threshold 100000000) ;; 100MB
(setq gc-cons-percentage 0.6)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Run garbage collection when Emacs is idle or out of focus
(add-hook 'focus-out-hook #'garbage-collect)
(run-with-idle-timer 5 t #'garbage-collect)

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

;; Store auto-save files in ~/.saves too
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.saves/") t)))

;; Make sure the directory exists
(make-directory "~/.saves/" t)


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
  ;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setenv "GIT_CONFIG_GLOBAL" (expand-file-name "~/.gitconfig")))

(use-package vertico
  :init
  (vertico-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-indexing-method 'native) ;; Use Git for indexing
  (setq projectile-enable-caching t) ;; Enable caching for faster performance
  (setq projectile-git-command "git ls-files -zco --exclude-standard ':!logs/**'") ;; Use Git to respect .gitignore
  (setq projectile-globally-ignored-directories '("node_modules" "vendor" ".cache" "log" "tmp" "builds"))
  (setq projectile-globally-ignored-files '("*.log" "*.tmp" "*.map"))

  ;; Enable Projectile globally
  (projectile-mode +1)

  (defun my/projectile-setup-vterm ()
    "Open multiple vterm buffers with specific names when switching to a project."
    (when (projectile-project-p)
      (let* ((project-name (projectile-project-name))
             (term1-name (format "*vterm-%s-shell*" project-name))
             (term2-name (format "*vterm-%s-server*" project-name))
             (term3-name (format "*vterm-%s-console*" project-name)))
        (unless (get-buffer term1-name)
          (with-current-buffer (vterm)
            (rename-buffer term1-name)
            (vterm-send-string (format "cd %s\n" (projectile-project-root)))))
        (unless (get-buffer term2-name)
          (with-current-buffer (vterm)
            (rename-buffer term2-name)
            (vterm-send-string (format "cd %s\n" (projectile-project-root)))))
        (unless (get-buffer term3-name)
          (with-current-buffer (vterm)
            (rename-buffer term3-name)
            (vterm-send-string (format "cd %s\n" (projectile-project-root))))))))

  ;; Attach hook to run when switching projects
  (add-hook 'projectile-after-switch-project-hook #'my/projectile-setup-vterm)

  ;; Optional: Set up a keymap prefix for Projectile commands
  :bind-keymap
  ("C-c p" . projectile-command-map) )

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-galaxy t))

;; (use-package fira-code-mode
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "x"))
;;   :hook prog-mode)

;; (use-package eyebrowse
;;   :init
;;   (eyebrowse-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;; (use-package lsp-mode
;;   :hook
;;   ((python-mode . lsp))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

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
  (global-set-key (kbd "C-x v") #'vterm)
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
(add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))

(use-package eglot
  :ensure t
  :hook
  ((enh-ruby-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure))
  :config
  ;; Add Solargraph as the server for enh-ruby-mode
  (add-to-list 'eglot-server-programs
               '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))

  ;; Use typescript-language-server for JS/TS/JSX/TSX
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; Ensure eglot is the xref backend
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local xref-backend-functions '(eglot-xref-backend)))))

;; Unbind js-find-symbol in js-mode and force xref-find-definitions
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-mode-map (kbd "M-.") #'xref-find-definitions))

;; Global fallback for M-. to use xref
(global-set-key (kbd "M-.") #'xref-find-definitions)

(setq warning-minimum-level :error)

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

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)
  (setq markdown-hide-markup nil))

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


(use-package all-the-icons
  :if (display-graphic-p))

(use-package ligature
  :straight t  ;; Or use `:ensure t` if you're using straight.el or package.el respectively
  :hook (prog-mode . ligature-mode)
  :config
  ;; Enable specific ligatures for programming modes
  (ligature-set-ligatures 'prog-mode
                          '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                            ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "-> " "->>"
                            "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                            "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                            "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                            "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                            "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                            "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                            "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                            "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enable globally if needed
  (global-ligature-mode t))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :bind (("C-c C-a" . aidermacs-transient-menu))

  :config
  (aidermacs-setup-minor-mode)
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  ;; Ensure Aidermacs buffers open in the side window
  (setq aidermacs-display-buffer-function
        (lambda (buffer)
          (display-buffer buffer '(display-buffer-reuse-window shackle-display-buffer)))))

(use-package ediff
  :ensure nil  ;; `ediff` is built-in, so no need to install it
  :custom
  ;; Show diffs side by side instead of stacked
  (ediff-split-window-function 'split-window-horizontally)
  ;; Prevent ediff from making a new frame
  (ediff-window-setup-function 'ediff-setup-windows-plain))


;; Treemacs configuration added
(use-package treemacs
  :ensure t
  :bind
  (("C-c t" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package window-purpose
  :ensure t
  :config
  ;; Enable purpose-mode globally
  (purpose-mode)

  ;; Associate major modes with purposes
  (setq purpose-user-mode-purposes
        '((emacs-lisp-mode . code)
          (python-mode . code)
          (mardown-mode . code)
          (conf-toml-mode . code)
          (js-mode . code)
          (typescript-mode . code)
          (web-mode . code)
          (compilation-mode . work)
          (help-mode . work)
          (eshell-mode . work)
          (magit-status-mode . work)
          (vterm . work)
          (markdown-mode . code)
          (yaml-mode . code)
          (org-mode . code)
          (text-mode . code)
          (enh-ruby-mode . code)
          (dockerfile-mode . code)))

  ;; Associate buffer names with purposes
  (setq purpose-user-name-purposes
        '(("*Help*" . work)
          ("*compilation*" . work)
          ("*Org Agenda*" . work)
          ("*Agenda Commands*" . work)
          ("*eshell*" . work)
          ("*Messages*" . work)
          ("COMMIT_EDITMSG" . code)
          ("*scratch*" . work)))

  (setq purpose-user-regexp-purposes
        '(("\\*aider.*" . work)
          ("\\*vterm.*" . work)
          ("magit.*" . work)))

  (purpose-compile-user-configuration))

(use-package org
  :config
  ;; Where Org looks for agenda & refile targets
  (setq org-directory "~/Dropbox/org"
        org-agenda-files '("~/Dropbox/org/tasks.org"
                           "~/Dropbox/org/calendar.org"
                           "~/Dropbox/org/inbox.org")
        org-default-notes-file "~/Dropbox/org/inbox.org")

  ;; Quality‑of‑life defaults
  (setq org-log-done 'time          ; time‑stamp when DONE
        org-startup-indented t      ; pretty outline
        org-hide-leading-stars t)   ; no leading ★★★

  ;; Quick global keys
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda))

;; ───────────────────────────
;;  Nicer bullets (optional)
;; ───────────────────────────
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ───────────────────────────
;;  Capture templates
;; ───────────────────────────
(setq org-capture-templates
      '(("t" "🗒️  Todo" entry
         (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  %U\n  %a")
        ("n" "📝 Note" entry
         (file+datetree "~/org/inbox.org")
         "* %?\n  Entered on %U\n  %a")))

(use-package yard-mode
  :ensure t
  :hook (enh-ruby-mode . yard-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))
