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
  (position-if-helper lst 0 pred))

(defun find-string-index (target alist)
  "Does something with TARGET and ALIST."
  (position-if (lambda (s) (string-prefix-p target s)) alist))

(defun initialize-terminal-buffer (name)
  (interactive)
  (vterm)
  (rename-buffer name t)
  (if (not term-mode-line-enabled)
      (setq mode-line-format nil)))

(defun find-terminal-buffer-index ()
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (term-index (find-string-index actual-term-name buffer-names)))
    term-index))

(defun get-a-terminal-buffer ()
  "Does something."
  (let* ((term-index (find-terminal-buffer-index)))
    (if term-index
        (switch-to-buffer (nth term-index (buffer-list)))
      (initialize-terminal-buffer mini-term-name))))

(defun create-new-window (is-horizontal)
  (let* ((size (if is-horizontal
                   term-height
                 term-width))
         (side (not is-horizontal)))
         (split-window (frame-root-window) size side)))

(defun open-terminal-window (is-horizontal)
  (let ((new-window (create-new-window is-horizontal)))
    (select-window new-window)
    (get-a-terminal-buffer)))

(defun get-terminal-window ()
  (let* ((window-names (mapcar (lambda (win) (buffer-name (window-buffer win)))
                               (window-list)))
         (window-index (find-string-index actual-term-name window-names)))
    (if window-index
        (nth window-index (window-list))
      nil)))

(defun toggle-terminal (is-horizontal)
  (let ((term-window (get-terminal-window)))
    (if term-window
        (delete-window term-window)
      (open-terminal-window is-horizontal))))

(defun toggle-terminal-horizontal ()
  "Toggle a small horizontal terminal window."
  (interactive)
  (toggle-terminal t))

(defun toggle-terminal-vertical ()
  "Toggle a small vertical terminal window."
  (interactive)
  (toggle-terminal nil))

(defun cd-to-current ()
  "CDs to the current buffer directory."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (terminal-window (get-terminal-window)))
    (if (not terminal-window)
        (open-terminal-window t)
      (select-window terminal-window))
    (vterm-send-string (concat "cd " dir) t)
    (vterm-send-return)
    (vterm-clear)))

(defun change-terminal-buffer ()
  (interactive)
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (buffers (seq-filter (lambda (s) (string-prefix-p actual-term-name s))
                              buffer-names))
         (name (ivy-read "Switch to terminal: "
                         buffers)))
    (switch-to-buffer name)))

(defun create-new-terminal (dir name)
  (let ((terminal-window (get-terminal-window)))
    (if (not terminal-window)
        (open-terminal-window t)
      (select-window terminal-window))
    (initialize-terminal-buffer (concat mini-term-name "<" name ">"))
    (vterm-send-string (concat "cd " dir) t)
    (vterm-send-return)
    (vterm-clear)
    (cd dir)))

(defun create-local-terminal ()
  (interactive)
  (create-new-terminal (expand-file-name default-directory) (buffer-name)))

(defun create-project-terminal ()
  (interactive)
  (create-new-terminal (projectile-project-root) (projectile-project-name)))

(defun new-terminal ()
  (interactive)
  (let ((project-name (projectile-project-name)))
    (if (string= () "-")
        (create-local-terminal)
      (create-project-terminal))))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-x b" . change-terminal-buffer)))

(provide 'terminal-thing)
