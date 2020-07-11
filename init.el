(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; ---------------------------------------------------
;; --------- Auto-generated --------------------------
;; ---------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "a41b81af6336bd822137d4341f7e16495a49b06c180d6a6417bf9fd1001b6d2b" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default)))
 '(package-selected-packages
   (quote
    (typescript-mode use-package racket-mode ivy-posframe yasnippet yaml-mode web-mode smartparens rust-mode pretty-mode org-bullets magit lsp-ui lsp-treemacs lsp-ivy lsp-haskell hasky-stack flycheck dracula-theme doom-themes doom-modeline company-lsp cmake-project cmake-mode cmake-ide autothemer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/terminal-thing")

(use-package emacs
  :bind (("C-c j" . toggle-terminal-horizontal)
         ("C-c C-j" . toggle-terminal-vertical)
         ("C-j" . newline-and-indent)
         ("C-'" . comment-line)
         ("C-z" . nil)
         ("C-x w" . kill-ring-save))
  :config
  (tool-bar-mode -1) ;; The thing with big icons.
  (scroll-bar-mode -1)
  (menu-bar-mode -1) ;; The ordinary menu bar.
  (setq confirm-kill-processes nil)
  (setq inhibit-splash-screen t)
  (setq frame-title-format '("emacs"))
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (add-hook 'text-mode-hook 'visual-line-mode))

(use-package doom-themes
  :config
  (defvar preferred-theme 'doom-dracula)  ;; doom-one-light, dracula and doom-gruvbox is nice
  (defun load-preferred-theme ()
    (load-theme preferred-theme t)
    (when (eq preferred-theme 'doom-dracula)
      (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
      (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
      (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)))
  (defun invert-theme ()
    (interactive)
    (if (eq preferred-theme 'doom-dracula)
        (setq preferred-theme 'doom-one-light)
      (setq preferred-theme 'doom-dracula))
    (load-preferred-theme))
  (load-preferred-theme))

(use-package magit)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package lsp-ivy)

(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package company
  :config
  (global-company-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package org
  :config
  (setq org-hide-emphasis-markers t))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package yasnippet)

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode c++-mode haskell-mode python-mode c-mode emacs-lisp-mode) . lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-find-definition)
              ("C-c r" . lsp-ui-peek-find-references)
              ("C-c s" . imenu)
              ("C-c C-d" . lsp-find-declaration)
              ("C-c e" . lsp-treemacs-errors-list)
              ("C-c g" . (lambda ()
                           (interactive)
                           (let ((n (buffer-size)))
                             (lsp-on-change 0 n n)))))
  :config
  (defvar lsp-clients-clangd-args '("-cross-file-rename"))
  (defvar strict-python-enabled nil)
  (defvar strict-python-warnings
    (list "E302" "E305" "W391" "E226"))
  (defvar lsp-ui-doc-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)

  (if (not strict-python-enabled)
      (defvar lsp-pyls-plugins-pycodestyle-ignore strict-python-warnings)))

(use-package company-lsp
  :config
  (defvar company-lsp-enable-snippet nil))

(use-package lsp-haskell)

(use-package lsp-ui)

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package display-line-numbers
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package paren
  :config
  (add-hook 'prog-mode-hook 'show-paren-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-common-hook (lambda () (smartparens-mode -1))))

(use-package elec-pair
  :config
  (add-hook 'c-mode-common-hook 'electric-pair-mode))

(use-package treemacs
  :bind ("C-c b" . treemacs))

(use-package python
  :config
  (setq-default python-indent-levels 2)
  (setq-default python-indent-offset 2))

(use-package typescript-mode
  :config
  (setq-default typescript-indent-level 2))

(defun c-emacs ()
  "Opens the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(provide 'init)
