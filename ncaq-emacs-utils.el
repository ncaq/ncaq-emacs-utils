;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(fset 'newline-under [end return])
(fset 'newline-upper [home return up])

(defun quoted-newline ()
  "強制的に改行を入力."
  (interactive)
  (insert "\n"))

(defun smart-move-beginning-of-line ()
  "Visual Studioライクなmove-beginning-of-line, 通常はインデントに従い, 後ろが空白のみなら先頭."
  (interactive)
  (if (looking-back "^[ 　\t]+" -1000)
      (move-beginning-of-line nil)
    (back-to-indentation)))

(defun smart-delete-whitespace-backward ()
  "スペースだけではなく段階的に改行も削除します."
  (interactive)
  (if (looking-back "\n" -1000)
      (delete-char -1)
    (when (looking-back "[ 　\t]+" nil t)
      (replace-match ""))))

(defun smart-delete-whitespace-forward ()
  "スペースだけではなく段階的に改行も削除します."
  (interactive)
  (if (looking-at "\n")
      (delete-char 1)
    (when (looking-at "[ 　\t]+")
      (replace-match ""))))

(defun delete-whitespace-backward ()
  "前にある空白文字を全削除します."
  (interactive)
  (when (looking-back "[[:blank:]\n]+" nil t)
    (replace-match "")))

(defun delete-whitespace-forward ()
  "後ろにある空白文字を全削除します."
  (interactive)
  (when (looking-at "[[:blank:]\n]+")
    (replace-match "")))

(defun isearch-exit-previous ()
  "通常isearchは終了するとき, マッチした文字列の末尾に移動しますが, これは先頭に移動します."
  (interactive)
  (let ((found-string-length (length isearch-string))
        (forward isearch-forward))
    (progn
      (isearch-exit)
      (goto-char (if forward
                     (- (point) found-string-length)
                   (+ (point) found-string-length))))))

(defun indent-whole-buffer ()
  "バッファ全体をインデントします."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun sort-dwim ()
  "1行選択している時は単語のソート, 選択してない時はパラグラフの行ソート, 選択している時はその範囲の行ソート."
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
  "バッファ全体をソートします."
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max))))

(defun align-space (BEG END)
  "Align by space.
スペースを基準に整形します。
始まりのスペース、つまりインデントのスペースは触りません。
BEG and END mark the limits of the region."
  (interactive (list (region-beginning) (region-end)))
  (align-regexp BEG END "[^^]\\(\\s-*\\) " 1 0 t))

(defun kill-region-or-word-at-point ()
  "選択範囲か単語を切り取ります."
  (interactive)
  (apply 'kill-region (region-or-thing-at-point 'word)))

(defun kill-region-or-sexp-at-point ()
  "選択範囲か式を切り取ります."
  (interactive)
  (apply 'kill-region (region-or-thing-at-point 'sexp)))

(defun kill-region-or-symbol-at-point ()
  "選択範囲かシンボルを切り取ります."
  (interactive)
  (apply 'kill-region (region-or-thing-at-point 'symbol)))

(defun kill-ring-save-region-or-word-at-point ()
  "選択範囲か単語をコピーします."
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'word)))

(defun kill-ring-save-region-or-sexp-at-point ()
  "選択範囲か式をコピーします."
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'sexp)))

(defun kill-ring-save-region-or-symbol-at-point ()
  "選択範囲かシンボルをコピーします."
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'symbol)))

(defun copy-whole-line ()
  "現在の行をコピーします."
  (interactive)
  (apply 'kill-ring-save (region-or-thing-at-point 'line)))

(defun region-or-thing-at-point (thing)
  "選択範囲か `THING' を取得します."
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
  "バッファ全体をコピーします."
  (interactive)
  (save-excursion
    (kill-ring-save (buffer-end 0) (buffer-end 1))))

(defun copy-to-register-@ (start end &optional delete-flag region)
  "レジスタ@に選択範囲をコピーします."
  (interactive
   (list
    (region-beginning)
    (region-end)
    current-prefix-arg
    t))
  (copy-to-register ?@ start end delete-flag region))

(defun yank-register-@ ()
  "レジスタ@から文字列を貼り付けます."
  (interactive)
  (insert-register ?@)
  (goto-char (prog1 (mark t) (set-marker (mark-marker) (point) (current-buffer)))))

(defun scroll-down-one ()
  "一行下にスクロールします."
  (interactive)
  (scroll-down 1))

(defun scroll-up-one ()
  "一行上にスクロールします."
  (interactive)
  (scroll-up 1))

(defun kill-all-buffers ()
  "バッファを全て閉じます."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-file-or-dired-buffers ()
  "ファイルとディレクトリのバッファを全て閉じます."
  (interactive)
  (mapc 'kill-buffer
        (cl-remove-if-not
         (lambda (buffer)
           (or
            (buffer-file-name buffer)
            (buffer-dired-p buffer)))
         (buffer-list))))

(defun buffer-dired-p (buffer)
  "現在のバッファがdiredか."
  (eq (buffer-local-value 'major-mode buffer) 'dired-mode))

(defun text-scale-reset ()
  "拡大縮小をリセットします."
  (interactive)
  (text-scale-set 0))

(defun other-window-fallback-split (COUNT &optional ALL-FRAMES)
  "次のウィンドウに移動するか新しいウィンドウを作成します."
  (interactive "p")
  (when (one-window-p) (split-window-dwim))
  (other-window COUNT ALL-FRAMES))

(defun other-window-backward ()
  "後ろのウインドウに移動します."
  (interactive)
  (other-window -1))

(defun split-window-dwim ()
  "適当な位置に新しいウィンドウを作ります."
  (interactive)
  (split-window nil nil (suggest-window-locate)))

(defun split-window-dwim-and-other ()
  "ウインドウを作った後にそのウインドウに移動します."
  (interactive)
  (split-window nil nil (suggest-window-locate))
  (other-window 1))

(defun suggest-window-locate ()
  "新しいウィンドウを開くべき位置をサジェストします."
  (if (< (window-pixel-width) (window-pixel-height))
      'below
    'right))

(defun revert-buffer-safe-confirm ()
  "更新されていないファイルなら確認をしないrevert-buffer."
  (interactive)
  (revert-buffer nil (not (buffer-modified-p))))

(provide 'ncaq-emacs-utils)
