
(use-package doom-themes
  :ensure t
  :config
  (defvar dark-theme 'doom-monokai-classic) ;; doom-dracula, doom-gruvbox, doom-monokai-classic
  (defvar light-theme 'doom-one-light)  ;; doom-one-light
  (defvar preferred-theme
    (if is-evening dark-theme light-theme))
  (defun is-dark-theme ()
    (eq preferred-theme dark-theme))
  (defun latex-image-directory-name ()
    (if (is-dark-theme) "dark-ltximg/" "light-ltximg/"))
  (defun set-preferred-latex-image-directory ()
    (setq org-preview-latex-image-directory (latex-image-directory-name)))
  (defun fix-emojis ()
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font
       t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)))
  (defvar source-code "Source Code Pro") ;; Om light font "Source Code Pro:demibold"
  (defvar deja-vu "DejaVu Sans Mono")
  (defvar preferred-face-font
    (if on-laptop
        source-code
      deja-vu))
  (defvar preferred-face-size
    (if on-laptop
        102
      102)) ;; Om source-code: 107
  (set-face-attribute 'default nil :font preferred-face-font) ;; Source Code Pro, DejaVu Sans Mono
  (set-face-attribute 'default nil :height preferred-face-size)
  (defun switch-theme (theme)
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme 'no-confirm))
  (defun fix-org-headlines ()
    (with-eval-after-load 'org
      (set-face-attribute 'org-level-1 nil :height 1.2)
      (set-face-attribute 'org-level-2 nil :height 1.1)
      (set-face-attribute 'org-level-3 nil :height 1.1)
      (set-face-attribute 'org-level-4 nil :height 1.1)
      (set-face-attribute 'org-level-5 nil :height 1.1)
      (set-face-attribute 'org-headline-done nil :strike-through t))
    (with-eval-after-load 'ivy
      (set-face-attribute 'ivy-org nil :height (face-attribute 'default :height))))
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
               (set-face-attribute 'font-lock-keyword-face nil       :weight 'bold)
               (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold))))
  (defun fix-light-themes ()
    (if (and (eq preferred-theme light-theme)
             on-laptop)
        (progn (set-face-attribute 'font-lock-function-name-face nil :weight 'semibold)
               (set-face-attribute 'font-lock-keyword-face nil       :weight 'semibold)
               (set-face-attribute 'font-lock-variable-name-face nil :weight 'semibold))))
  (defun load-preferred-theme ()
    (switch-theme preferred-theme)
    (fix-org-blocks)
    (fix-org-headlines)
    (fix-doom-dracula)
    (fix-light-themes))
  (defun invert-theme ()
    (interactive)
    (if (eq preferred-theme dark-theme)
        (setq preferred-theme light-theme)
      (setq preferred-theme dark-theme))
    (load-preferred-theme)
    (set-preferred-latex-image-directory))
  (if client-enabled
      (add-hook 'after-make-frame-functions (lambda (frame)
                                              (load-preferred-theme))))
  (load-preferred-theme)
  (fix-emojis)
  (doom-themes-org-config)
  (global-set-key (kbd "M-<f12>") 'invert-theme)
  (set-preferred-latex-image-directory))
