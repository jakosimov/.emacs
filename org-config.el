
(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c h" . insert-http-link)
         ("C-x n s" . my-narrow-to-subtree)
         ("C-c 1" . my-toggle-narrowed)
         ("C-M-q" . auto-fill-mode))
  :hook ((org-mode . auto-revert-mode)
         (org-mode . auto-fill-mode))
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
          (org-insert-heading))
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
        (setq x1 (search-forward "<title>" nil t))
        (if (not x1)
            (cons "Link (no title)" nil)
          (progn (search-forward "</title>")
                 (setq x2 (search-backward "<"))
                 (let ((title (buffer-substring-no-properties x1 x2)))
                   (beginning-of-buffer)
                   (if (not (search-forward "property=\"og:description\"" nil t))
                       (cons title nil)
                     (progn (setq x3 (search-forward "content=\""))
                            (search-forward "\"")
                            (setq x4 (search-backward "\""))
                            (cons title (buffer-substring-no-properties x3 x4))))))))))
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
  (defun narrow-width-to-move ()
    (let* ((level (org-current-level))
           (indentation-width 2)
           (width-to-move (* (- level 1) indentation-width)))
      width-to-move))
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (setq fringe-indicator-alist '())))
  (defun my-narrow-to-subtree ()
    (interactive)
    (org-narrow-to-subtree)
    (push '(truncation nil right-arrow) fringe-indicator-alist)
    (set-window-hscroll (selected-window) 0)
    (scroll-left (narrow-width-to-move) t))
  (defun my-widen-from-subtree ()
    (interactive)
    (widen)
    ;; (pop fringe-indicator-alist)
    (set-window-hscroll (selected-window) 0))
  (defun my-toggle-narrowed ()
    (interactive)
    (if (buffer-narrowed-p)
        (my-widen-from-subtree)
      (my-narrow-to-subtree)))
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
  (setq org-agenda-files (list org-directory journal-directory))
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-startup-folded t)
  ;; (setq org-log-done t)
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
   'org-babel-load-languages
   '((python . t)))
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
