(fset 'newline-under [end return])
(fset 'newline-upper [home return up])

(defun smart-move-beginning-of-line ()
  "Visual StudioライクなC-a,通常はインデントに従いHomeへ,もう一度押すと本来のHome"
  (interactive)
  (if (bolp)
      (move-beginning-of-line nil))
  (back-to-indentation))

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

(defun sort-lines-auto-mark-paragrah ()
  (interactive)
  (if (use-region-p)
      (sort-lines nil (region-beginning)(region-end))
    (progn
      (mark-paragraph)
      (sort-lines nil (region-beginning)(region-end)))))

(defun sort-lines-whole-buffer ()
  (interactive)
  (mark-whole-buffer)
  (sort-lines nil (region-beginning)(region-end)))

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point-min)(point-max))))

(defun indent-whole-buffer-and-brackets ()
  "並括弧を揃える(いい加減)"
  (interactive)
  (save-excursion
    (while (re-search-forward ")\\W*{" nil t)
      (replace-match ")\n{"))
    (while (re-search-forward "=\\W*{" nil t)
      (replace-match "=\n{"))
    (indent-whole-buffer)
    ))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(require'text-adjust)
(defun text-adjust-selective ()
  (interactive)
  (text-adjust-hankaku-buffer)
  (text-adjust-kutouten-buffer))

(provide 'ncaq-emacs-utils)