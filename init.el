(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; To install, just run: `package-install' `use-package'

(load "~/.emacs.d/terminal-thing")

(defvar on-laptop nil)

(use-package emacs
  :bind (("C-c j" . toggle-terminal-horizontal)
         ("C-c C-j" . toggle-terminal-vertical)
         ("C-c J" . new-terminal)
         ("C-j" . newline-and-indent)
         ("C-'" . comment-line)
         ("C-z" . nil)
         ("C-x w" . kill-ring-save))
  :hook (text-mode . visual-line-mode)
  :config
  (tool-bar-mode -1) ;; The thing with big icons.
  (scroll-bar-mode -1)
  (menu-bar-mode -1) ;; The ordinary menu bar.
  (setq confirm-kill-processes nil)
  (setq inhibit-splash-screen t)
  (setq frame-title-format '("emacs"))
  (defvar gdb-show-main t)
  (defvar gdb-display-io-nopopup t)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  (if on-laptop
      (set-face-attribute 'default nil :font "Source Code Pro")
    (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
  (if on-laptop
      (set-face-attribute 'default nil :height 102)
    (set-face-attribute 'default nil :height 100)))

(use-package doom-themes
  :ensure t
  :config
  (defvar dark-theme 'doom-dracula) ;; doom-dracula and doom-gruvbox
  (defvar light-theme 'doom-one-light)  ;; doom-one-light
  (defvar preferred-theme dark-theme)
  (defun load-preferred-theme ()
    (load-theme preferred-theme t)
    (when (eq preferred-theme dark-theme)
      (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
      (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
      (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)))
  (defun invert-theme ()
    (interactive)
    (if (eq preferred-theme dark-theme)
        (setq preferred-theme light-theme)
      (setq preferred-theme dark-theme))
    (load-preferred-theme))
  (load-preferred-theme)
  (doom-themes-org-config))

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  (setq posframe-mouse-banish nil)
  (ivy-posframe-mode 1))

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package doom-modeline
  :ensure t
  :init ;; Run `all-the-icons-install-fonts' as well
  (if on-laptop
      (setq doom-modeline-height 31))
  (doom-modeline-mode 1))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c C-f" . projectile-find-file)
              ("C-c p" . projectile-switch-project)
              ("C-c b" . projectile-switch-to-buffer))
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (defun export-macro ()
    (interactive)
    (insert "#+BEGIN_EXPORT latex\n")
    (save-excursion (insert "\n#+END_EXPORT")))
  (defvar org-capture-templates
    '(("t" "Todo" entry (file "~/Documents/notes/todos.org")
       "* TODO %?\n%U" :empty-lines 1)
      ("n" "Note" entry (file "~/Documents/notes/captures.org")
       "* NOTE %?\n%U" :empty-lines 1)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files (list "~/Documents/notes/"))
  (setq org-log-done t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-return-follows-link t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package yasnippet
  :ensure t
  :hook (org-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode c++-mode haskell-mode python-mode c-mode) . lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-find-definition)
              ("C-c r" . lsp-ui-peek-find-references)
              ("C-c h" . lsp-ui-doc-show)
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
  (defvar incorrect-python-warnings
    (vector "W503"))
  (defvar strict-python-warnings
    (vector "W503" "E303" "E302" "E305" "W391" "E226" "E111"))
  (defvar lsp-ui-doc-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  (if (not strict-python-enabled)
      (defvar lsp-pyls-plugins-pycodestyle-ignore strict-python-warnings)
    (defvar lsp-pyls-plugins-pycodestyle-ignore incorrect-python-warnings)))

(use-package company-lsp
  :ensure t
  :config
  (defvar company-lsp-enable-snippet nil))

(use-package lsp-haskell
  :ensure t)

(use-package lsp-ui
  :ensure t)

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
         (org-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (defun configure-org-mode ()
    (sp-local-pair 'org-mode "\\(" "\\)")
    (sp-local-pair 'org-mode "$" "$"))
  (configure-org-mode))

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

(defun c-emacs ()
  "Opens the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "1f4b51dcecc5bdd2d4dc462a185de4d9e7845ccfbcbbf30a9fb3952e84f9e876" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" default))
 '(package-selected-packages
   '(yaml-mode rust-mode smartparens lsp-treemacs lsp-ui lsp-haskell company-lsp yasnippet org-bullets doom-modeline company flycheck lsp-ivy ivy-posframe ivy magit doom-themes vterm use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
