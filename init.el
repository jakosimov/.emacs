(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(load "~/.emacs.d/terminal-thing")
(setq custom-file "~/.emacs.d/customs.el")

(defvar on-laptop
  (equal (system-name) "yoga"))
(defvar is-evening
  (let ((current-hour (decoded-time-hour (decode-time (current-time)))))
    (or (< current-hour 7)
        (> current-hour 19))))
(defvar org-directory "~/Sync/notes/")
(defvar journal-directory (concat org-directory "journal/"))
(defvar client-enabled nil)

(use-package emacs
  :bind (("C-c j" . toggle-terminal-horizontal)
         ("C-c C-j" . toggle-terminal-vertical)
         ("C-c J" . new-terminal)
         ("C-'" . comment-line)
         ("C-z" . nil)
         ("C-x w" . kill-ring-save)
         ("C-§" . projectile-previous-project-buffer)
         ("M-§" . projectile-next-project-buffer))
  :config
  (defun sm-greek-lambda ()
       (font-lock-add-keywords nil `(("\\<lambda\\>"
           (0 (progn (compose-region (match-beginning 0) (match-end 0)
           ,(make-char 'greek-iso8859-7 107))
                     nil))))))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'emacs-lisp-mode-hook 'sm-greek-lambda)
  (tool-bar-mode -1) ;; The thing with big icons.
  (scroll-bar-mode -1)
  (menu-bar-mode -1) ;; The ordinary menu bar.
  (global-set-key (kbd "C-M-+") (lambda ()
                                  (interactive)
                                  (let ((window (selected-window)))
                                    (make-frame)
                                    (delete-window window))))
  (setq-default fill-column 80)
  (setq confirm-kill-processes nil)
  (setq inhibit-splash-screen t)
  (setq frame-title-format '("emacs"))
  (defvar gdb-show-main t)
  (defvar gdb-display-io-nopopup t)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

(use-package lcr
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t)
  :config
  (defun new-line-under ()
    (interactive)
    (save-excursion (evil-open-below 1)
                    (evil-normal-state)))
  (defun new-line-above ()
    (interactive)
    (save-excursion (evil-open-above 1)
                    (evil-normal-state)))
  (defun config-special-modes ()
    (add-hook 'special-mode-hook
              (lambda ()
                (evil-emacs-state)
                (if (not (or (equal major-mode 'vterm-mode)))
                    (progn (evil-local-set-key 'emacs (kbd "j") 'evil-next-line)
                           (evil-local-set-key 'emacs (kbd "k") 'evil-previous-line)
                           (evil-local-set-key 'emacs (kbd "l") 'evil-forward-char)
                           (evil-local-set-key 'emacs (kbd "h") 'evil-backward-char))))))
  (evil-define-key 'normal 'global (kbd "å") 'evil-first-non-blank)
  (evil-define-key 'normal 'global (kbd "C-SPC") (lambda ()
                                                 (interactive)
                                                 (save-excursion (insert " "))))
  (evil-define-key 'normal 'global (kbd "<backspace>") (lambda ()
                                                 (interactive)
                                                 (delete-backward-char 1)))
  (with-eval-after-load 'evil-maps
    (evil-define-key 'normal 'global (kbd "RET") 'new-line-under)
    (evil-define-key 'normal 'global (kbd "<S-return>") 'new-line-above)
    (evil-define-key 'normal org-mode-map (kbd "RET") nil)
    (define-key evil-normal-state-map (kbd "M-p") 'evil-paste-pop))
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
  (add-hook 'dashboard-mode-hook (lambda ()
                                   (add-hook 'evil-insert-state-entry-hook 'evil-emacs-state nil t)
                                   (add-hook 'evil-normal-state-entry-hook 'evil-emacs-state nil t)))
  (config-special-modes)
  (evil-define-key 'emacs magit-mode-map (kbd "C-k") 'magit-discard)
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(load "~/.emacs.d/org-config.el")

(use-package magit
  :ensure t)

(use-package centered-window
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd "SPC s") 'swiper)
  (add-hook 'special-mode-hook (lambda () (local-set-key (kbd "C-s") 'swiper))))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t
  :hook ((emacs-lisp-mode) . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-idle-change-delay 1.0))

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook 'racket-xp-mode)
  (add-hook 'racket-mode-hook 'sm-greek-lambda))

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode rustic-mode python-mode racket-mode racket-repl-mode
                          haskell-mode haskell-interactive-mode) . company-mode)
  :config)

(add-to-list 'load-path "~/.emacs.d/pomodoro/")
(require 'pomodoro)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c C-f" . projectile-find-file)
              ("C-c b" . projectile-switch-to-buffer))
  :config
  (projectile-mode 1)
  (evil-define-key 'normal 'projectile-mode-map (kbd "SPC p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "♦")))

(use-package yasnippet
  :ensure t
  :hook ((org-mode . yas-minor-mode)
         (latex-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package imenu-anywhere
  :ensure t
  :config
  (evil-define-key 'normal 'lsp-mode-map (kbd "SPC d") 'imenu-anywhere)
  (setq-default imenu-anywhere-preprocess-entry-function
                (lambda (entry parent-name) entry)))

(use-package ccls
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode c++-mode python-mode c-mode rustic-mode) . lsp)
  :bind (:map lsp-mode-map
              ("M-+" . lsp-ui-sideline-toggle-symbols-info))
  :config
  ;; (defvar lsp-clients-clangd-args '("-cross-file-rename"))
  (add-hook 'xref--xref-buffer-mode-hook 'evil-emacs-state)
  (defun setup-python ()
    (defvar strict-python-enabled nil)
    (defvar incorrect-python-warnings
      (vector "W503"))
    (defvar strict-python-warnings
      (vector "W503" "E303" "E302" "E305" "W391" "E226" "E111"))
    (if (not strict-python-enabled)
        (defvar lsp-pyls-plugins-pycodestyle-ignore strict-python-warnings)
      (defvar lsp-pyls-plugins-pycodestyle-ignore incorrect-python-warnings)))
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-snippet nil)
  (evil-define-key 'normal 'lsp-mode-map (kbd "M-.") 'lsp-find-definition)
  (evil-define-key 'normal 'lsp-mode-map (kbd "SPC l") 'lsp-ui-imenu)
  (evil-define-key 'normal 'lsp-mode-map (kbd "SPC h") 'lsp-ui-doc-glance)
  (evil-define-key 'normal 'lsp-mode-map (kbd "SPC e") 'lsp-treemacs-errors-list)
  (evil-define-key 'normal 'lsp-mode-map (kbd "SPC SPC") (lambda ()
                                                           (interactive)
                                                           (let ((n (buffer-size)))
                                                             (lsp-on-change 0 n n))))
  (setup-python)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 1.0))

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-doc-delay 0.1)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'at-point))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm
    (setq-local buffer-save-without-query t))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-chaining-hints t))

(use-package haskell-mode
  :ensure t
  :config
  (defvar is-dante-project nil)
  (defun enable-haskell-check ()
    (if is-dante-project
        (dante-mode)
      (lsp)))
  (add-hook 'haskell-mode-hook 'enable-haskell-check)
  (add-hook 'haskell-literate-mode-hook 'enable-haskell-check)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-interactive-mode-hook 'evil-emacs-state)
  (setq haskell-interactive-popup-errors nil)
  :custom
  (haskell-interactive-prompt "λ "))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :bind (:map dante-mode-map
              ("C-c d" . xref-find-definitions))
  :init
  (add-hook 'dante-mode-hook 'flycheck-mode)
  (put 'dante-repl-command-line 'safe-local-variable (lambda (_) t))
  (put 'haskell-process-type 'safe-local-variable (lambda (_) t))
  (put 'dante-methods 'safe-local-variable (lambda (_) t))

  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

(use-package lsp-haskell
  :ensure t)

(if (and (not on-laptop) nil)
    (use-package direnv
      :ensure t
      :config
      (direnv-mode)))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (latex-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (require 'smartparens-org)
  (defun configure-org-mode ()
    (sp-with-modes 'org-mode
      (sp-local-pair "\\(" "\\)")
      (sp-local-pair "\\[" "\\]")
      (sp-local-pair "$" "$"))
    (sp-local-pair 'org-mode "*" "*" :actions :rem)
    (sp-local-pair 'org-mode "*" nil :actions :rem)
    (sp-local-pair 'org-mode "=" "=" :actions :rem))
  (configure-org-mode)
  (sp-local-pair 'rustic-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package treemacs
  :ensure t
  :bind ("C-c C-b" . treemacs))

(use-package python
  :config
  (unbind-key "C-c C-f" python-mode-map)
  (unbind-key "C-c C-j" python-mode-map)
  (setq-default python-indent-levels 4)
  (setq-default python-indent-offset 4))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

(use-package typescript-mode
  :ensure t
  :config
  (setq-default typescript-indent-level 2))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 5)
                          (recents . 5)))

  (if client-enabled
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))

(use-package emojify
  :ensure t
  :config
  (emojify-set-emoji-styles '(github))
  (add-hook 'prog-mode-hook 'emojify-mode)
  (add-hook 'text-mode-hook 'emojify-mode)) ;; Fixar typ bullets i org-mode

(load "~/.emacs.d/theme-config")

(use-package vi-tilde-fringe
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (global-git-gutter-mode))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-posframe-display-at-window-bottom-left)
          (t      . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 24)
          (right-fringe . 24)))
  (ivy-posframe-mode 1))

(use-package counsel
  :ensure t
  :config
  (counsel-mode))

(use-package emacs
  :bind (("C-c C-j" . toggle-terminal-vertical)))

(provide 'init)
(put 'scroll-left 'disabled nil)
