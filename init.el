
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
    ("a41b81af6336bd822137d4341f7e16495a49b06c180d6a6417bf9fd1001b6d2b" "28caf31770f88ffaac6363acfda5627019cac57ea252ceb2d41d98df6d87e240" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" default)))
 '(package-selected-packages
   (quote
    (doom-modeline rust-mode magit pretty-mode treemacs mood-line racket-mode lsp-haskell atom-one-dark-theme web-mode projectile company-lsp lsp-ui yasnippet typescript-mode dracula-theme org-bullets lsp-mode haskell-mode smartparens company flycheck ivy))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))

(require 'smartparens-config)
(require 'lsp-mode)
(require 'yasnippet)
(require 'org)
(require 'projectile)
(require 'doom-modeline)
(require 'ace-window)

(defvar mini-term-name "Terminal")
(defvar actual-term-name (concat "*" mini-term-name "*"))
(defvar term-mode-line-enabled nil)

(defun position-if-helper (alist i pred)
  (if (null alist)
      nil
    (if (funcall pred (car alist))
	i
      (position-if-helper (cdr alist) (+ i 1) pred))))

(defun position-if (pred lst)
  (position-if-helper lst 0 pred))

(defun find-string-index (target alist)
  "Does something with TARGET and ALIST."
  (position-if (lambda (s) (string= s target)) alist))

(defun get-terminal-buffer ()
  "Does something."
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (term-index (find-string-index actual-term-name buffer-names)))
    (if term-index
        (switch-to-buffer (nth term-index (buffer-list)))
      (ansi-term "/bin/bash" mini-term-name))))

(defun open-terminal ()
  (let ((new-window (split-window (frame-root-window))))
    (window-resize new-window -10)
    (select-window new-window)
    (get-terminal-buffer)
    (if (not term-mode-line-enabled)
	(setq mode-line-format nil))))

(defun toggle-terminal ()
  "Toggle a small terminal window."
  (interactive)
  (let ((term-window (get-buffer-window actual-term-name)))
    (if term-window
        (delete-window term-window)
      (open-terminal))))

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
  ;; (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (if (not (eq major-mode 'c++-mode))
                                  (smartparens-mode 1))))
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (electric-pair-mode 1)
                                  (smartparens-mode -1)))
  (add-hook 'haskell-mode-hook (lambda ()
                                 (flycheck-mode -1)))
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)))
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'latex-mode 'visual-line-mode)
  (add-hook 'racket-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (local-set-key (kbd "C-c C-e") 'eval-buffer))))

(defun set-global-keys ()
  (global-set-key (kbd "C-c j") 'toggle-terminal)
  (global-set-key (kbd "C-j") 'newline-and-indent)
  (global-set-key (kbd "C-'") 'comment-line)
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-c b") 'treemacs)
  (global-set-key (kbd "C-x w") 'kill-ring-save)
  (global-set-key (kbd "C-x o") 'ace-window))



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


