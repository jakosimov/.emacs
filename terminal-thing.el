
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-w" . kill-ring-save)))
(defvar mini-term-name "console")
(defvar actual-term-name mini-term-name)
(defvar term-mode-line-enabled nil)
(defvar term-width -60)
(defvar term-height -15)

(defun position-if-helper (alist i pred)
  (if (null alist)
      nil
    (if (funcall pred (car alist))
	i
      (position-if-helper (cdr alist) (+ i 1) pred))))

(defun position-if (pred lst)
  "Returns the index of the first element in LST that satisfies PRED."
  (position-if-helper lst 0 pred))

(defun find-string-index (target alist)
  "Returns the index of the first string in ALIST with prefix TARGET."
  (position-if (lambda (s) (string-prefix-p target s)) alist))

(defun initialize-terminal-buffer (name dir)
  (with-temp-buffer
    (if dir
        (cd dir))
    (vterm)
    (evil-emacs-state)
    (rename-buffer (concat mini-term-name "<" name ">") t)
    (if (not term-mode-line-enabled)
        (setq mode-line-format nil))))

(defun find-last-used-terminal-buffer ()
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (term-index (find-string-index actual-term-name buffer-names)))
    (if term-index
        (nth term-index (buffer-list))
      nil)))

(defun get-terminal-directory ()
  (if (not (projectile-project-p))
      (expand-file-name default-directory)
    (projectile-project-root)))

(defun get-terminal-name ()
  (if (not (projectile-project-p))
      (buffer-name)
    (projectile-project-name)))

(defun open-terminal-buffer-in-current-window ()
  (let* ((term-buffer (find-last-used-terminal-buffer)))
    (if term-buffer
        (switch-to-buffer term-buffer)
      (initialize-terminal-buffer (get-terminal-name) (get-terminal-directory)))))

(defun create-new-empty-window (is-horizontal)
  (let* ((size (if is-horizontal
                   term-height
                 term-width))
         (side (not is-horizontal)))
    (split-window (frame-root-window) size side)))

(defun open-terminal-window (is-horizontal)
  (let ((new-window (create-new-empty-window is-horizontal)))
    (select-window new-window)
    (open-terminal-buffer-in-current-window)
    (set-window-dedicated-p new-window t)))

(defun is-dedicated-terminal-window-p (window)
  (and (window-dedicated-p window)
       (string-prefix-p actual-term-name (buffer-name (window-buffer window)))))

(defun find-existing-terminal-window ()
  (let* ((window-index (position-if 'is-dedicated-terminal-window-p
                                    (window-list))))
    (if window-index
        (nth window-index (window-list))
      nil)))

(defun toggle-terminal (is-horizontal)
  (let ((term-window (find-existing-terminal-window)))
    (if term-window
        (delete-window term-window)
      (open-terminal-window is-horizontal))))


;; Denna och `open-terminal-window' gör lite för lika saker.
(defun create-new-terminal (dir name)
  (let ((terminal-window (find-existing-terminal-window)))
    (if terminal-window
        (select-window terminal-window)
      (let ((window (create-new-empty-window t)))
        (select-window window)))
    (set-window-dedicated-p (selected-window) nil)
    (initialize-terminal-buffer name dir)
    (set-window-dedicated-p (selected-window) t)))



(defun new-terminal ()
  (interactive)
  (create-new-terminal (get-terminal-directory) (get-terminal-name)))

(defun toggle-terminal-horizontal ()
  "Toggle a small horizontal terminal window."
  (interactive)
  (toggle-terminal t))

(defun toggle-terminal-vertical ()
  "Toggle a small vertical terminal window."
  (interactive)
  (toggle-terminal nil))

(defun ivy-switch-terminal-buffer ()
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (buffers (seq-filter (lambda (s) (string-prefix-p actual-term-name s))
                              buffer-names))
         (name (ivy-read "Switch to terminal: "
                         buffers
                         :preselect 1)))
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer name)
    (set-window-dedicated-p (selected-window) t)))

(defun from-terminal-switch-buffer ()
  (interactive)
  (if (not (window-dedicated-p))
      (ivy-switch-buffer)
    (ivy-switch-terminal-buffer)))

(defun toggle-dedicated ()
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-x d" . toggle-dedicated)
              ("C-x b" . from-terminal-switch-buffer)))

(provide 'terminal-thing)
