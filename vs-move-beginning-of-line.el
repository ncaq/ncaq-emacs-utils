;;(require 'vs-move-beginning-of-line)
(defun is-reverse-point-whitespace-all ()
  "カーソルの位置の前には空白文字しかありません"
  (looking-back "^[\t ]+"))

(defun vs-move-beginning-of-line ()
  "Visual StdioライクなC-a,通常はインデントに従いHomeへ,もう一度押すと本来のHome"
  (interactive)
  (cond
   ((is-reverse-point-whitespace-all) (move-beginning-of-line nil))
   (t (back-to-indentation))))

(provide 'vs-move-beginning-of-line)
