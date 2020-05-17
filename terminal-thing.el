(defvar mini-term-name "Terminal")
(defvar actual-term-name (concat "*" mini-term-name "*"))
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
  (position-if (lambda (s) (string= s target)) alist))

(defun initialize-terminal-buffer ()
  (ansi-term "/bin/bash" mini-term-name)
  (local-set-key (kbd "C-x w") 'kill-ring-save)
  (local-set-key (kbd "C-x y") 'yank))

(defun get-terminal-buffer ()
  "Does something."
  (let* ((buffer-names (mapcar (function buffer-name) (buffer-list)))
         (term-index (find-string-index actual-term-name buffer-names)))
    (if term-index
        (switch-to-buffer (nth term-index (buffer-list)))
      (initialize-terminal-buffer))))

(defun create-new-window (is-horizontal)
  (let* ((size (if is-horizontal
                   term-height
                 term-width))
         (side (not is-horizontal)))
         (split-window (frame-root-window) size side)))

(defun open-terminal (is-horizontal)
  (let ((new-window (create-new-window is-horizontal)))
    (select-window new-window)
    (get-terminal-buffer)
    (if (not term-mode-line-enabled)
	(setq mode-line-format nil))))

(defun toggle-terminal (is-horizontal)
  (let ((term-window (get-buffer-window actual-term-name)))
    (if term-window
        (delete-window term-window)
      (open-terminal is-horizontal))))

(defun toggle-terminal-horizontal ()
  "Toggle a small horizontal terminal window."
  (interactive)
  (toggle-terminal t))

(defun toggle-terminal-vertical ()
  "Toggle a small vertical terminal window."
  (interactive)
  (toggle-terminal nil))

(provide 'terminal-thing)
