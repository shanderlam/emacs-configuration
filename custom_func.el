(defun show-buffer-file-name ()
  "Show the full path file name of current buffer in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun buffer-file-name-nondirectory()
  (file-name-nondirectory buffer-file-name))

(defun show-buffer-file-name-nondirectory ()
  "Show the file name of current buffer in the minibuffer."
  (interactive)
  (message (buffer-file-name-nondirectory)))

(defun insert-buffer-file-name-nondirectory ()
  "Insert file name of current buffer to current cursor position"
  (interactive)
  (insert (buffer-file-name-nondirectory)))

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
      (progn
        (let ((temp-tags-ignore-list tags-ignore-list))
          (dolist (tags-table-dir tags-table-list)
            (let ((temp-tags-ignore-file (car temp-tags-ignore-list)))
              (if temp-tags-ignore-file
                  (shell-command (concat "cd " tags-table-dir " && etags -R --exclude=@" temp-tags-ignore-file " -V"))
                (shell-command (concat "cd " tags-table-dir " && etags -R -V"))))
            (setq temp-tags-ignore-list (cdr temp-tags-ignore-list)))))
    (dolist (tags-table-dir tags-table-list)
      (shell-command (concat "cd " tags-table-dir " && etags -R -V"))))
  (message "Done"))

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))


(if (equal system-type 'darwin)
    (progn
      (defun dired-open-file-osx ()
        (interactive)
        (setq coding-system-for-write 'utf-8)
        (shell-command (concat "open \"" (dired-get-file-for-visit) "\"")))

      (add-hook 'dired-mode-hook
                '(lambda()
                   (local-set-key "\M-\r" 'dired-open-file-osx)))

      (if (equal window-system nil)
          (progn
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
            (global-set-key "\C-cc" 'copy-to-clipboard)))))
