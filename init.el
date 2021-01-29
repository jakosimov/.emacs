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
(defvar org-directory "~/Sync/notes/")
(defvar client-enabled nil)

(use-package emacs
  :bind (("C-c j" . toggle-terminal-horizontal)
         ("C-c C-j" . toggle-terminal-vertical)
         ("C-c J" . new-terminal)
         ;; ("C-j" . newline-and-indent)
         ("C-'" . comment-line)
         ("C-z" . nil)
         ("C-x w" . kill-ring-save)
         ("C-§" . projectile-previous-project-buffer)
         ("M-§" . projectile-next-project-buffer))
  :hook (text-mode . visual-line-mode)
  :config
  (tool-bar-mode -1) ;; The thing with big icons.
  (scroll-bar-mode -1)
  (menu-bar-mode -1) ;; The ordinary menu bar.
  (global-set-key (kbd "C-M-+") (lambda ()
                                  (interactive)
                                  (let ((window (selected-window)))
                                    (make-frame)
                                    (delete-window window))))
  (setq confirm-kill-processes nil)
  (setq inhibit-splash-screen t)
  (setq frame-title-format '("emacs"))
  (defvar gdb-show-main t)
  (defvar gdb-display-io-nopopup t)
  (setq-default indent-tabs-mode nil)
  (setq-default truncate-lines t)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  (defvar preferred-face-font
    (if on-laptop
        "Source Code Pro"
      "DejaVu Sans Mono"))
  (defvar preferred-face-size
    (if on-laptop
        102
      100))
  (set-face-attribute 'default nil :font preferred-face-font) ;; Source Code Pro, DejaVu Sans Mono
  (set-face-attribute 'default nil :height preferred-face-size))

(use-package lcr
  :ensure t)

(use-package key-chord
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
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jj" 'evil-normal-state)
  (evil-define-key 'normal prog-mode-map (kbd "å") 'evil-first-non-blank)
  (with-eval-after-load 'evil-maps
    (evil-define-key 'normal org-mode-map (kbd "RET") nil)
    (evil-define-key 'normal prog-mode-map (kbd "RET") 'new-line-under)
    (evil-define-key 'normal prog-mode-map (kbd "<S-return>") 'new-line-above)
    (define-key evil-normal-state-map (kbd "M-p") 'evil-paste-pop))
  (add-hook 'vterm-mode-hook 'evil-emacs-state)
  (add-hook 'dashboard-mode-hook 'evil-emacs-state)
  (add-hook 'haskell-interactive-mode-hook 'evil-emacs-state)
  (add-hook 'haskell-error-mode-hook 'evil-emacs-state)
  (evil-set-undo-system 'undo-tree)
  (key-chord-mode 1)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c h" . insert-http-link))
  :hook (org-mode . auto-revert-mode)
  :config
  (defun export-macro ()
    (interactive)
    (insert "#+BEGIN_EXPORT latex\n")
    (save-excursion (insert "\n#+END_EXPORT")))
  (defun prettify-checkboxes ()
    (add-hook 'org-mode-hook (lambda ()
                               "Beautify Org Checkbox Symbol"
                               (push '("[ ]" . "☐" ) prettify-symbols-alist)
                               (push '("[X]" . "☑" ) prettify-symbols-alist)
                               (push '("[-]" . "❍" ) prettify-symbols-alist)
                               (prettify-symbols-mode)))
    (defface org-checkbox-done-text
      '((t (:foreground "#71696A" :strike-through t)))
      "Face for the text part of a checked org-mode checkbox.")
    (font-lock-add-keywords
     'org-mode
     `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
        1 'org-checkbox-done-text prepend))
     'append))
  (defun insert-http-link (url &optional start)
    "Insert org link where default description is set to html title."
    (interactive "sURL: ")
    (let* ((response (get-html-title-from-url url))
           (title (car response))
           (description (cdr response))
           (emphasis-marker "="))
      (if (not start)
          (insert "+ "))
      (org-insert-link nil url (concat title))
      (if description
          (insert (concat "\n  " emphasis-marker description emphasis-marker)))))
  (defun get-html-title-from-url (url)
    "Return content in <title> tag."
    (let (x1 x2 x3 x4 (download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        (set-buffer-multibyte t)
        (beginning-of-buffer)
        (setq x1 (search-forward "<title>"))
        (search-forward "</title>")
        (setq x2 (search-backward "<"))
        (let ((title (buffer-substring-no-properties x1 x2)))
          (beginning-of-buffer)
          (if (not (search-forward "property=\"og:description\"" nil t))
              (cons title nil)
            (progn (setq x3 (search-forward "content=\""))
                   (search-forward "\"")
                   (setq x4 (search-backward "\""))
                   (cons title (buffer-substring-no-properties x3 x4))))))))
  (defun config-org-evil ()
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (local-set-key (kbd "j") 'org-agenda-next-line)
                (local-set-key (kbd "k") 'org-agenda-previous-line)
                (local-set-key (kbd "l") 'evil-forward-char)
                (local-set-key (kbd "h") 'evil-backward-char)
                (local-set-key (kbd "M-l") 'org-agenda-log-mode))))
  (defvar org-captures-path (concat org-directory "captures.org"))
  (defvar org-todos-path (concat org-directory "todos.org"))
  (defvar org-school-path (concat org-directory "skola.org"))
  (defvar org-capture-templates
    '(("t" "Todo" entry (file+headline org-todos-path "Aktiva")
       "* TODO %?\n%U" :empty-lines 1)
      ("n" "Note" entry (file+headline org-captures-path "Inte skräp")
       "* NOTE %?\n%U" :empty-lines 1)
      ("l" "Läxa/prov" entry (file+headline org-school-path "Prov _o_ sånt")
       "* TODO %^{Beskrivning} %^g\n DEADLINE: %^t" :empty-lines 1)
      ("c" "Quick note" entry (file+headline org-captures-path "Inte skräp")
       "* %? %^g\n%U" :empty-lines 1)))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (defvar latex-prefix-size 1.3)
  (if on-laptop
      (setq latex-prefix-size 1.5))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "FUTURE(f)" "|" "DONE(d!)" "CANCELLED(c)")))
  (setq org-tags-column -55)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale latex-prefix-size))
  (setq org-startup-indented t)
  (setq org-startup-with-latex-preview t)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files (list org-directory))
  (setq org-log-done t)
  (setq org-ellipsis ;; ⬎, ⤵, ↴, ⤵, ⤷, ⮷, ⮷, », ▼, ☟
        (if on-laptop
            " ↴"
          " ⤵"))
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "okular %s")))
  (setq org-deadline-warning-days 7)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-return-follows-link t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-M-RET-may-split-line nil)
  ;; (setq org-log-into-drawer t)
  (prettify-checkboxes)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-show-habits t)
  (add-hook 'org-agenda-mode-hook (lambda ()
                                    (local-set-key (kbd "d")
                                                   (lambda ()
                                                     (interactive)
                                                     (org-agenda-todo 'done)))))
  (config-org-evil))

(use-package org
  :config
  ;; Fix latex fragments in org.
  (defvar org-latex-fragment-last nil
    "Holds last fragment/environment you were on.")
  (defvar org-latex-fragment-delay 0.4)

  (defun my/org-latex-fragment--get-current-latex-fragment ()
    "Return the overlay associated with the image under point."
    (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))

  (defun my/org-in-latex-fragment-p ()
    "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
    (let* ((el (org-element-context))
           (el-type (car el)))
      (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
           (org-element-property :begin el))))

  (defun org-latex-fragment-toggle-auto ()
    ;; Wait for the s
    (interactive)
    (while-no-input
      (run-with-idle-timer org-latex-fragment-delay nil 'org-latex-fragment-toggle-helper)))

  (defun org-latex-fragment-toggle-helper ()
    "Toggle a latex fragment image "
    (condition-case nil
        (and (eq 'org-mode major-mode)
             (let* ((begin (my/org-in-latex-fragment-p)))
               (cond
                ((and
                  org-latex-fragment-last
                  begin
                  (not (= begin
                          org-latex-fragment-last)))
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p) (org-latex-preview))
                   (goto-char begin)
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   (setq org-latex-fragment-last begin)))
                ((and
                  (not begin)
                  org-latex-fragment-last)
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p)(org-latex-preview)))
                 (setq org-latex-fragment-last nil))
                ((and
                  (not org-latex-fragment-last)
                  begin)
                 (save-excursion
                   (goto-char begin)
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   (setq org-latex-fragment-last begin)))
                ((not begin)
                 (setq org-latex-fragment-last nil)))))
      (error nil)))
  (add-hook 'post-command-hook 'org-latex-fragment-toggle-auto)
  (setq org-latex-fragment-toggle-helper (byte-compile 'org-latex-fragment-toggle-helper))
  (setq org-latex-fragment-toggle-auto (byte-compile 'org-latex-fragment-toggle-auto)))

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; (use-package counsel
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x))

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((t . ivy-posframe-display-at-frame-center)))
;;   (setq posframe-mouse-banish nil)
;;   (ivy-posframe-mode 1))

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t
  :hook ((emacs-lisp-mode) . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package company
  :ensure t
  :config
  (global-company-mode))

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

  (doom-modeline-def-modeline 'my-doom-mode-line
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info major-mode process checker vcs " "))

  (defun setup-custom-doom-modeline ()
    (interactive)
    (doom-modeline-set-modeline 'my-doom-mode-line 'default))
  (setup-custom-doom-modeline))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c C-f" . projectile-find-file)
              ("C-c p" . projectile-switch-project)
              ("C-c b" . projectile-switch-to-buffer))
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "♦")))

(use-package yasnippet
  :ensure t
  :hook ((org-mode . yas-minor-mode)
         (latex-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package imenu-anywhere
  :ensure t
  :bind (:map prog-mode-map
              ("C-c s" . imenu-anywhere)))

(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode c++-mode python-mode c-mode) . lsp)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-find-definition)
              ("C-c r" . lsp-ui-peek-find-references)
              ("C-c h" . lsp-ui-doc-show)
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

(use-package lsp-haskell
  :ensure t)

(use-package company-lsp
  :ensure t
  :config
  (defvar company-lsp-enable-snippet nil))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

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

  (add-hook 'haskell-mode-hook 'dante-mode))

(use-package direnv
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
    (sp-local-pair 'org-mode "*" nil :actions :rem))
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

(use-package doom-themes
  :ensure t
  :config
  (defvar dark-theme 'doom-monokai-classic) ;; doom-dracula, doom-gruvbox, doom-monokai-classic
  (defvar light-theme 'doom-one-light)  ;; doom-one-light
  (defvar preferred-theme dark-theme)
  (defun switch-theme (theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme 'no-confirm))
  (defun is-dark-theme ()
    (eq preferred-theme dark-theme))
  (defun fix-org-headlines ()
    (with-eval-after-load 'org
      (set-face-attribute 'org-level-1 nil :height 1.3)
      (set-face-attribute 'org-level-2 nil :height 1.2)
      (set-face-attribute 'org-level-3 nil :height 1.1)
      (set-face-attribute 'org-level-4 nil :height 1.0)
      (set-face-attribute 'org-level-5 nil :height 1.0)))
  (defun fix-org-blocks ()
    (with-eval-after-load 'org
      (let* ((alpha (if (is-dark-theme) 0.1 0.03))
             (orig (face-attribute 'default :background))
             (c (doom-darken orig alpha))
             (begin-fg
              (if (is-dark-theme)
                  (doom-lighten c 0.2)
                (doom-darken c 0.3))))
        (set-face-attribute 'org-block-begin-line nil
                            :background c
                            :foreground begin-fg)
        (set-face-attribute 'org-block-end-line nil
                            :background c
                            :foreground begin-fg)
        (set-face-attribute 'org-block nil
                            :background c))))
  (defun fix-doom-dracula ()
    (if (eq preferred-theme 'doom-dracula)
        (progn (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
               (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
               (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold))))
  (defun load-preferred-theme ()
    (switch-theme preferred-theme)
    (fix-org-blocks)
    (fix-org-headlines)
    (fix-doom-dracula))
  (defun invert-theme ()
    (interactive)
    (if (eq preferred-theme dark-theme)
        (setq preferred-theme light-theme)
      (setq preferred-theme dark-theme))
    (load-preferred-theme))
  (if client-enabled
      (add-hook 'after-make-frame-functions (lambda (frame)
                                              (load-preferred-theme))))

  (load-preferred-theme)
  (doom-themes-org-config))

(provide 'init)
