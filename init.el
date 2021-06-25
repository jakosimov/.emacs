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
  (with-eval-after-load 'evil-maps
    (evil-define-key 'normal 'global (kbd "RET") 'new-line-under)
    (evil-define-key 'normal 'global (kbd "<S-return>") 'new-line-above)
    (evil-define-key 'normal org-mode-map (kbd "RET") nil)
    (define-key evil-normal-state-map (kbd "M-p") 'evil-paste-pop))
  (add-hook 'dashboard-mode-hook (lambda ()
                                   (add-hook 'evil-insert-state-entry-hook 'evil-emacs-state nil t)
                                   (add-hook 'evil-normal-state-entry-hook 'evil-emacs-state nil t)))
  (config-special-modes)
  (evil-define-key 'emacs magit-mode-map (kbd "C-k") 'magit-discard)
  (add-hook 'vterm-mode-hook 'evil-emacs-state)
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

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper))

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
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode rustic-mode python-mode) . company-mode)
  :config)

;; (use-package unicode-fonts
;;    :ensure t
;;    :config
;;     (unicode-fonts-setup))


(add-to-list 'load-path "~/.emacs.d/pomodoro/")
(require 'pomodoro)

(use-package doom-modeline
  :ensure t
  :init ;; Run `all-the-icons-install-fonts' as well
  (if on-laptop
      (setq doom-modeline-height 38)
    (setq doom-modeline-height 31))
  (doom-modeline-mode 1)
  (if client-enabled
      (add-hook 'after-make-frame-functions
                #'enable-doom-modeline-icons))
  :config
  (defun enable-doom-modeline-icons (_frame) ;; For emacsclient
    (setq doom-modeline-icon t))
  (setq doom-modeline-percent-position nil)

  (doom-modeline-def-segment doom-pomodoro
    (concat
     (doom-modeline-spc)
     (propertize pomodoro-mode-line-string 'face
                 'doom-modeline-urgent)
     (doom-modeline-spc)))

  (doom-modeline-def-modeline 'my-doom-mode-line
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info doom-pomodoro)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info major-mode process checker vcs " "))

  (defun setup-custom-doom-modeline ()
    (interactive)
    (doom-modeline-set-modeline 'my-doom-mode-line 'default))
  (setup-custom-doom-modeline))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c C-f" . projectile-find-file)
              ("C-c b" . projectile-switch-to-buffer))
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
  :bind (:map prog-mode-map
              ("C-c s" . imenu-anywhere)))

(use-package ccls
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode c++-mode python-mode c-mode rustic-mode) . lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-find-definition)
              ("C-c r" . lsp-ui-peek-find-references)
              ("C-c h" . lsp-ui-doc-show)
              ("C-c C-d" . lsp-find-declaration)
              ("C-c e" . lsp-treemacs-errors-list)
              ("C-c g" . (lambda ()
                           (interactive)
                           (let ((n (buffer-size)))
                             (lsp-on-change 0 n n))))
              ("M-+" . lsp-ui-sideline-toggle-symbols-info))
  :config
  ;; (defvar lsp-clients-clangd-args '("-cross-file-rename"))
  (defvar strict-python-enabled nil)
  (defvar incorrect-python-warnings
    (vector "W503"))
  (defvar strict-python-warnings
    (vector "W503" "E303" "E302" "E305" "W391" "E226" "E111"))
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  (if (not strict-python-enabled)
      (defvar lsp-pyls-plugins-pycodestyle-ignore strict-python-warnings)
    (defvar lsp-pyls-plugins-pycodestyle-ignore incorrect-python-warnings))
  :custom
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t)))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-interactive-mode-hook 'evil-emacs-state)
  (setq haskell-interactive-popup-errors nil))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :bind (:map haskell-mode-map
              ("C-c d" . xref-find-definitions))
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (put 'dante-repl-command-line 'safe-local-variable (lambda (_) t))
  (put 'haskell-process-type 'safe-local-variable (lambda (_) t))
  (put 'dante-methods 'safe-local-variable (lambda (_) t))

  (add-hook 'haskell-mode-hook 'dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

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
  :hook (after-init . global-emojify-mode)
  :config
  (emojify-set-emoji-styles '(github))
  (add-hook 'vterm-mode-hook (lambda ()
                               (emojify-mode -1)))) ;; Fixar typ bullets i org-mode

(load "~/.emacs.d/theme-config")

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-file-type 'monthly)
  (setq org-journal-dir "~/Sync/notes/journal/")
  (global-set-key (kbd "C-c å") (lambda ()
                                  (interactive)
                                  (org-journal-new-entry 1)))
  (global-set-key (kbd "C-c C-å") (lambda ()
                                  (interactive)
                                  (org-journal-new-entry nil)
                                  (evil-insert-state)))
  (add-hook 'org-journal-mode-hook
            (lambda ()
              (local-set-key (kbd "C-s") (lambda ()
                                           (interactive)
                                           (org-schedule nil "+0d")))))
  (setq org-extend-today-until 4
        org-journal-date-format "%a, %d-%m-%Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-time-format ""))

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

(use-package emacs
  :bind (("C-c C-j" . toggle-terminal-vertical)))

(provide 'init)
(put 'scroll-left 'disabled nil)
