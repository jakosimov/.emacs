(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-link-frame-setup '((file . find-file))))
;; -------------------------------------------------------------------

;; (defvar good-font "Roboto Mono Light 10")
(defvar good-font "Source Code Pro 10")

;; Font and frame size
(set-face-font 'default good-font)
(setq default-frame-alist
      (append (list '(width  . 60) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Source Code Pro 10"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
;; (set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; No Tooltips
(tooltip-mode 0)

;; Paren mode is part of the theme
(show-paren-mode t)

;; No fringe but nice glyphs for truncated and wrapped lines
(fringe-mode '(0 . 0))
(defface fallback '((t :family "Fira Code Light"
                       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))


;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))

;; A theme is fully defined by these six faces 
(defgroup elegance nil
  "Faces for the elegance theme"
  :prefix "face-")

;; Do not show prefix when displaying the elegance group
(setq custom-unlispify-remove-prefixes t)

(defface face-critical nil :group 'elegance)

(defface face-popout nil :group 'elegance)

(defface face-strong nil :group 'elegance)

(defface face-salient nil :group 'elegance)

(defface face-faded nil :group 'elegance)

(defface face-subtle nil :group 'elegance)


;; Mode line (this might be slow because of the "☰" that requires substitution)
;; This line below makes things a bit faster
(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " %b "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line
        (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))


;; Comment if you want to keep the modeline at the bottom
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))

              
;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;; Modeline
(defun set-modeline-faces ()

  ;; Mode line at top
  (set-face 'header-line                                 'face-strong)
  (set-face-attribute 'header-line nil
                                :underline (face-foreground 'default))
  (set-face-attribute 'mode-line nil
                      :height 10
                      :underline (face-foreground 'default)
                      :overline nil
                      :box nil 
                      :foreground (face-background 'default)
                      :background (face-background 'default))
  (set-face 'mode-line-inactive                            'mode-line)

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'mode-line))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground (face-background 'default))
  )

;; Buttons
(defun set-button-faces ()
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-faded)
                             :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :foreground (face-foreground 'default)
                      ;; :background (face-foreground 'face-faded)
                      :inherit 'custom-button
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-subtle)
                             :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground (face-background 'default)
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-salient)
                             :style nil)
                      :inverse-video nil))

(defun elegance-monokai ()
    (setq frame-background-mode 'dark)
    (set-background-color "#272822")
    (set-foreground-color "#F8F8F2")
    (set-face-attribute 'default nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default))
    (set-face-attribute 'face-strong nil :foreground "#dcdccc"
                                         :weight 'regular)
    (set-modeline-faces)
    ;; (with-eval-after-load 'cus-edit (set-button-faces))
    )

;; Set theme
(elegance-monokai)
;; (setq lsp-headerline-breadcrumb-enable nil)


(provide 'elegance)

