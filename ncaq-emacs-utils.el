;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(fset 'newline-under [end return])
(fset 'newline-upper [home return up])

(defun quoted-newline ()
  (interactive)
  (insert "\n"))

(defun smart-move-beginning-of-line ()
  "Visual StudioライクなC-a,通常はインデントに従い,後ろが空白のみなら先頭"
  (interactive)
  (if (looking-back "^[ 　\t]+" -1000)
      (move-beginning-of-line nil)
    (back-to-indentation)))

(defun smart-delete-whitespace-backward ()
  (interactive)
  (if (looking-back "\n" -1000)
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
                     (- (point) found-string-length)
                   (+ (point) found-string-length))))))

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun sort-dwim ()
  "1行選択している時は単語のソート,選択してない時はパラグラフの行ソート,選択している時はその範囲の行ソート"
  (interactive)
  (save-excursion
    (if (and (use-region-p)
             (eq (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end))))
        (progn
          (kill-region (region-beginning) (region-end))
          (let ((line (car kill-ring)))
            (setq kill-ring (cdr kill-ring))
            (insert (string-join (sort (split-string line) (lambda (a b) (string< a b))) " "))))
      (progn (unless (use-region-p) (mark-paragraph))
             (sort-lines nil (region-beginning) (region-end))))))

(defun sort-lines-whole-buffer ()
  "バッファ全体をソートします"
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max))))

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

(defun kill-ring-save-region-or-sexp-at-point ()
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'sexp)))

(defun copy-whole-line ()
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'line)))

(defun region-or-thing-at-point (thing)
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (let ((b (bounds-of-thing-at-point thing)))
      (list (car b) (cdr b)))))

(defun mark-whole-word ()
  "単語をマークします."
  (interactive)
  (mark-whole-thing 'word))

(defun mark-whole-sexp ()
  "S式をマークします."
  (interactive)
  (mark-whole-thing 'sexp))

(defun mark-whole-thing (thing)
  "mark-word, mark-sexp用.
前方にも探索します."
  (interactive)
  (pcase (bounds-of-thing-at-point thing)
    (`(,a . ,d)
     (goto-char a)
     (set-mark d))))

(defun kill-ring-save-whole ()
  (interactive)
  (save-excursion
    (kill-ring-save (buffer-end 0) (buffer-end 1))))

(defun copy-to-register-@ (start end &optional delete-flag region)
  (interactive
   (list
    (region-beginning)
    (region-end)
    current-prefix-arg
    t))
  (copy-to-register ?@ start end delete-flag region))

(defun yank-register-@ ()
  (interactive)
  (insert-register ?@)
  (goto-char (prog1 (mark t) (set-marker (mark-marker) (point) (current-buffer)))))

(defun scroll-down-one ()
  (interactive)
  (scroll-down 1))

(defun scroll-up-one ()
  (interactive)
  (scroll-up 1))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-file-or-dired-buffers ()
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if-not
         (lambda (buffer)
           (or
            (buffer-file-name buffer)
            (buffer-dired-p buffer)))
         (buffer-list))))

(defun buffer-dired-p (buffer)
  (eq (buffer-local-value 'major-mode buffer) 'dired-mode))

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

(defun revert-buffer-safe-confirm ()
  "更新されていないファイルなら確認をしないrevert-buffer"
  (interactive)
  (revert-buffer nil (not (buffer-modified-p))))

(provide 'ncaq-emacs-utils)
