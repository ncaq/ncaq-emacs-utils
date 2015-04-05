;; -*- lexical-binding: t -*-

(fset 'newline-under [end return])
(fset 'newline-upper [home return up])

(defun quoted-newline ()
  (interactive)
  (insert "\n"))

(defun smart-move-beginning-of-line ()
  "Visual StudioライクなC-a,通常はインデントに従いHomeへ,もう一度押すと本来のHome"
  (interactive)
  (if (looking-back "^[ 　	]+")
      (move-beginning-of-line nil)
    (let ((oldpoint (point)))
      (back-to-indentation)
      (if (eq oldpoint (point))
          (move-beginning-of-line nil)))))

(defun smart-delete-whitespace-backward ()
  (interactive)
  (if (bolp)
      (backward-delete-char-untabify 1)
    (delete-whitespace-backward)))

(defun delete-whitespace-backward ()
  (interactive)
  (when (looking-back "[ 　	]" 0)
    (backward-delete-char-untabify 1)
    (delete-whitespace-backward)))

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

(defun kill-ring-save-whole ()
  (interactive)
  (save-excursion
    (kill-ring-save (buffer-end 0) (buffer-end 1))))

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

(provide 'ncaq-emacs-utils)
