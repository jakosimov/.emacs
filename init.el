

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "a41b81af6336bd822137d4341f7e16495a49b06c180d6a6417bf9fd1001b6d2b" "28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" default)))
 '(package-selected-packages
   (quote
    (lsp-haskell company-lsp lsp-ui lsp-mode hasky-stack cmake-ide cmake-project rtags yaml-mode zenburn-theme doom-modeline rust-mode magit pretty-mode treemacs mood-line racket-mode atom-one-dark-theme web-mode projectile yasnippet typescript-mode dracula-theme org-bullets haskell-mode smartparens company flycheck ivy))))

;; Yoga:
;;   FF: Source Code Pro
;;   Height: 102

(require 'smartparens-config)
(require 'lsp-mode)
(require 'yasnippet)
(require 'org)
(require 'projectile)
(require 'doom-modeline)
(require 'ace-window)
(require 'magit)
(load "~/.emacs.d/terminal-thing")

(defvar enable-c++-lsp 1)
(defvar enable-haskell-lsp 1)

(defun customize-emacs ()
  "Opens the custom.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun initiate-modes ()
  (load-theme 'dracula)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (ivy-mode 1)
  (projectile-mode +1)
  (global-flycheck-mode)
  (global-company-mode)
  (doom-modeline-mode 1))

(defun initiate-hooks ()
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (if (not (eq major-mode 'c++-mode))
                                  (smartparens-mode 1))))
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (electric-pair-mode 1)
                                  (smartparens-mode -1)))
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)))
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'latex-mode-hook 'visual-line-mode)
  (add-hook 'racket-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (local-set-key (kbd "C-c C-e") 'eval-buffer)))
  (if enable-c++-lsp
      (add-hook 'c++-mode-hook 'lsp))
  (if enable-haskell-lsp
      (add-hook 'haskell-mode-hook 'lsp))

  )

(defun set-global-keys ()
  (global-set-key (kbd "C-c j") 'toggle-terminal-horizontal)
  (global-set-key (kbd "C-c C-j") 'toggle-terminal-vertical)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-set-key (kbd "C-'") 'comment-line)
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-c b") 'treemacs)
  (global-set-key (kbd "C-x w") 'kill-ring-save)
  (global-set-key (kbd "C-x o") 'ace-window))

(defun my-lsp-setup ()
 (setq lsp-ui-doc-enable nil)
 (setq lsp-enable-symbol-highlighting nil)
 (add-hook 'lsp-mode-hook (lambda ()
                            (local-set-key (kbd "C-c d") 'lsp-find-definition)
                            (local-set-key (kbd "C-c h") 'lsp-ui-doc-show)
                            (local-set-key (kbd "C-c r") 'lsp-ui-peek-find-references))))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq-default typescript-indent-level 2)
(setq-default python-indent-levels 2)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)

(setq inhibit-splash-screen t)
(setq org-hide-emphasis-markers t)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(initiate-modes)
(initiate-hooks)
(set-global-keys)
(my-lsp-setup)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
