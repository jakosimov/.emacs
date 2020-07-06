(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; ---------------------------------------------------
;; --------- Imports ---------------------------------
;; ---------------------------------------------------

(require 'smartparens-config)
(require 'lsp-mode)
(require 'yasnippet)
(require 'org)
(require 'projectile)
(require 'doom-modeline)
(require 'magit)
(require 'doom-themes)
(require 'ivy)
(require 'ivy-posframe)
(require 'flycheck)
(require 'company)
(load "~/.emacs.d/terminal-thing")


;; ---------------------------------------------------
;; --------- Variables -------------------------------
;; ---------------------------------------------------

(defvar preferred-theme 'doom-dracula)  ;; doom-one-light, dracula and doom-gruvbox is nice


;; ---------------------------------------------------
;; --------- Functions -------------------------------
;; ---------------------------------------------------

(defun load-preferred-theme ()
  (load-theme preferred-theme t)
  (when (eq preferred-theme 'doom-dracula)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
    (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)))

(defun initiate-modes ()
  (setq org-hide-emphasis-markers t)
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-window-center)))
  (setq inhibit-splash-screen t)
  (tool-bar-mode -1) ;; The thing with big icons.
  (scroll-bar-mode -1)
  (menu-bar-mode -1) ;; The ordinary menu bar.
  (ivy-mode 1)
  (projectile-mode +1)
  (doom-modeline-mode 1)
  (ivy-posframe-mode 1)
  (load-preferred-theme)
  (doom-themes-org-config)
  (global-flycheck-mode)
  (global-company-mode))

(defun initiate-hooks ()
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (electric-pair-mode 1)
                                  (smartparens-mode -1)))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'latex-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (local-set-key (kbd "C-c C-e") 'eval-buffer)))
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'haskell-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp))

(defun set-global-keys ()
  (global-set-key (kbd "C-c j") 'toggle-terminal-horizontal)
  (global-set-key (kbd "C-c C-j") 'toggle-terminal-vertical)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-set-key (kbd "C-'") 'comment-line)
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-c b") 'treemacs)
  (global-set-key (kbd "C-x w") 'kill-ring-save))

(defun my-lsp-setup ()
  (defvar lsp-ui-doc-enable nil)
  (defvar lsp-clients-clangd-args '("-cross-file-rename"))

  (defvar company-lsp-enable-snippet nil)
  (defvar strict-python-enabled nil)
  (defvar strict-python-warnings
    (list "E302" "E305" "W391" "E226"))
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  (lsp-treemacs-sync-mode 1)
  (if (not strict-python-enabled)
      (defvar lsp-pyls-plugins-pycodestyle-ignore strict-python-warnings))
  (add-hook 'lsp-mode-hook (lambda ()
                             (local-set-key (kbd "C-c d") 'lsp-find-definition)
                             (local-set-key (kbd "C-c h") 'lsp-ui-doc-show)
                             (local-set-key (kbd "C-c r") 'lsp-ui-peek-find-references)
                             (local-set-key (kbd "C-c s") 'imenu)
                             (local-set-key (kbd "C-c C-d") 'lsp-find-declaration)
                             (local-set-key (kbd "C-c e") 'lsp-treemacs-errors-list)
                             (local-set-key (kbd "C-c g") (lambda ()
                                                            (interactive)
                                                            (let ((n (buffer-size)))
                                                              (lsp-on-change 0 n n)))))))

(defun set-defaults ()
  (setq-default typescript-indent-level 2)
  (setq-default python-indent-levels 2)
  (setq-default python-indent-offset 2)
  (setq-default truncate-lines t)
  (setq-default indent-tabs-mode nil)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))


;; ---------------------------------------------------
;; --------- Interactives ----------------------------
;; ---------------------------------------------------

(defun invert-theme ()
  (interactive)
  (if (eq preferred-theme 'doom-dracula)
      (setq preferred-theme 'doom-one-light)
    (setq preferred-theme 'doom-dracula))
  (load-preferred-theme))

(defun c-emacs ()
  "Opens the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; ---------------------------------------------------
;; --------- Initiation ------------------------------
;; ---------------------------------------------------

(initiate-modes)
(initiate-hooks)
(set-global-keys)
(my-lsp-setup)
(set-defaults)


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
    (racket-mode ivy-posframe yasnippet yaml-mode web-mode smartparens rust-mode projectile pretty-mode org-bullets magit lsp-ui lsp-treemacs lsp-ivy lsp-haskell hasky-stack flycheck dracula-theme doom-themes doom-modeline company-lsp cmake-project cmake-mode cmake-ide autothemer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
