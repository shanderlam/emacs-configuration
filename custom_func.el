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
                  (shell-command (concat "cd " tags-table-dir " && /usr/local/bin/ctags -e -R --exclude=@" temp-tags-ignore-file " -V"))
                (shell-command (concat "cd " tags-table-dir " && /usr/local/bin/ctags -e -R -V"))))
            (setq temp-tags-ignore-list (cdr temp-tags-ignore-list)))))
    (dolist (tags-table-dir tags-table-list)
      (shell-command (concat "cd " tags-table-dir " && /usr/local/bin/ctags -e -R -V"))))
  (message "Done"))

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(require 'dired)
(if (equal system-type 'darwin)
    (progn
      (defun cus-say-current-word()
        (interactive)
        (shell-command (concat "say \"" (current-word) "\"")))

      (defun open-in-macvim ()
        "Open current file in Macvim"
        (interactive)
        (shell-command (concat "open -a Macvim \"" (buffer-file-name) "\"")))

      (defun dired-open-file-osx ()
        (interactive)
        (setq coding-system-for-write 'utf-8)
        (shell-command (concat "open \"" (dired-get-file-for-visit) "\"")))

      (defun reveal-in-finder ()
        (interactive)
        (setq coding-system-for-write 'utf-8)
        (shell-command (concat "open -R \"" (dired-get-file-for-visit) "\"")))

      (add-hook 'dired-mode-hook
                '(lambda()
                   (local-set-key "\M-\r" 'dired-open-file-osx)
                   (local-set-key "J" 'reveal-in-finder)))

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
                  (process-send-eof proc)
                  (message "Copy successfully!"))))
            (global-set-key "\C-cc" 'copy-to-clipboard)))))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
