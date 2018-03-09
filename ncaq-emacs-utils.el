;; -*- lexical-binding: t -*-

(fset 'newline-under [end return])
(fset 'newline-upper [home return up])

(defun quoted-newline ()
  (interactive)
  (insert "\n"))

(defun smart-move-beginning-of-line ()
  "Visual StudioライクなC-a,通常はインデントに従い,後ろが空白のみなら先頭"
  (interactive)
  (if (looking-back "^[ 　\t]+")
      (move-beginning-of-line nil)
    (back-to-indentation)))

(defun smart-delete-whitespace-backward ()
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (when (looking-back "[ 　\t]+" nil t)
      (replace-match ""))))

(defun smart-delete-whitespace-forward ()
  (interactive)
  (if (looking-at "\n")
      (delete-char 1)
    (when (looking-at "[ 　\t]+")
      (replace-match ""))))

(defun isearch-exit-previous ()
  "通常isearchは終了するとき,マッチした文字列の末尾に移動するが,これは先頭に移動する."
  (interactive)
  (let ((found-string-length (length isearch-string))
        (forward isearch-forward))
    (progn
      (isearch-exit)
      (goto-char (if forward
                     (-(point) found-string-length)
                   (+(point) found-string-length))))))

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point-min)(point-max))))

(defun sort-lines-auto-mark-paragrah ()
  (interactive)
  (save-excursion
    (if (use-region-p)
        (sort-lines nil (region-beginning)(region-end))
      (progn
        (mark-paragraph)
        (sort-lines nil (region-beginning)(region-end))))))

(defun sort-lines-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (sort-lines nil (region-beginning)(region-end))))

(defun align-space (beg end)
  (interactive (list (region-beginning) (region-end)))
  (align-regexp beg end "\\(\\s-*\\) " 1 0 t))

(defun kill-region-or-word-at-point ()
  (interactive)
  (apply 'kill-region (region-or-thing-at-point 'word)))

(defun kill-region-or-sexp-at-point ()
  (interactive)
  (apply 'kill-region (region-or-thing-at-point 'sexp)))

(defun kill-ring-save-region-or-word-at-point ()
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'word)))

(defun region-or-thing-at-point (thing)
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (let ((b (bounds-of-thing-at-point thing)))
      (list (car b) (cdr b)))))

(defun kill-ring-save-whole ()
  (interactive)
  (save-excursion
    (kill-ring-save (buffer-end 0) (buffer-end 1))))

(defun insert-register-@ ()
  (interactive)
  (insert-register ?@))

(defun copy-to-register-@ (start end &optional delete-flag region)
  (interactive
   (list
    (region-beginning)
    (region-end)
    current-prefix-arg
    t))
  (copy-to-register ?@ start end delete-flag region))

(defun scroll-down-one ()
  (interactive)
  (scroll-down 1))

(defun scroll-up-one ()
  (interactive)
  (scroll-up 1))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun other-window-fallback-split (COUNT &optional ALL-FRAMES)
  (interactive "p")
  (when (one-window-p) (split-window-dwim))
  (other-window COUNT ALL-FRAMES))

(defun other-window-backward ()
  (interactive)
  (other-window -1))

(defun split-window-dwim ()
  (interactive)
  (split-window nil nil (suggest-window-locate)))

(defun split-window-dwim-and-other ()
  (interactive)
  (split-window nil nil (suggest-window-locate))
  (other-window 1))

(defun suggest-window-locate ()
  (if (< (window-pixel-width) (window-pixel-height))
      'below
    'right))

(provide 'ncaq-emacs-utils)
