(defun show-file-path ()
  "Show the full path file name of current buffer in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun show-file-name ()
  "Show the file name of current buffer in the minibuffer."
  (interactive)
  (message (file-name-nondirectory buffer-file-name)))

(defun insert-file-name ()
  "Insert file name of current buffer to current cursor position"
  (interactive)
  (insert (file-name-nondirectory buffer-file-name)))

(defun clear-whitespace ()
  "Delete trailing white space, and replace tabs with spaces."
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun paste-from-clipboard ()
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

(defun copy-to-clipboard ()
  (interactive)
  (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (buffer-substring (region-beginning) (region-end)))
        (process-send-eof proc))))

(defun generate-tags ()
  "Generate tags of current project"
  (interactive)
  (dolist (tags-table tags-table-list)
    (start-process "tags" "*Messages*" "etags" "-R" (concat "-f" tags-table) (substring tags-table 0 -4))))
