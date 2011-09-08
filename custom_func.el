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

(defun generate-tags ()
  "Generate tags of current project"
  (interactive)
  (message "Generating tags...")
  (if (boundp 'tags-ignore-list)
      (setq temp-tags-ignore-list tags-ignore-list)
    (dolist (tags-table tags-table-list)
      (shell-command (concat "cd " tags-table "; etags -R -V"))))
  (if (boundp 'temp-tags-ignore-list)
      (dolist (tags-table tags-table-list)
        (setq temp-tags-ignore-file (car temp-tags-ignore-list))
        (if (car temp-tags-ignore-list)
            (shell-command (concat "cd " tags-table "; etags -R --exclude=@" (car temp-tags-ignore-list) " -V"))
          (shell-command (concat "cd " tags-table "; etags -R -V")))
        (setq temp-tags-ignore-list (cdr temp-tags-ignore-list))))
  (message "Done")
)

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))


(if (equal system-type 'darwin)
    (progn
      (defun dired-open-file-osx ()
        (interactive)
        (shell-command (concat "open " (dired-get-file-for-visit))))

      (add-hook 'dired-mode-hook
                '(lambda()
                   (local-set-key "\M-\r" 'dired-open-file-osx)))


      (defun paste-from-clipboard ()
        (interactive)
        (insert (shell-command-to-string "pbpaste")))

      (global-set-key "\C-cv" 'paste-from-clipboard)

      (defun copy-to-clipboard ()
        (interactive)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc (buffer-substring (region-beginning) (region-end)))
            (process-send-eof proc))))
      (global-set-key "\C-cc" 'copy-to-clipboard)))
