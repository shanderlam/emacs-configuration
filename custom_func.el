
(defun cus-buffer-file-name-nondirectory ()
  "Get buffet file name without directory path."
  (file-name-nondirectory buffer-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for inserting string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cus-insert-buffer-file-name ()
  "Insert the full path file name of current buffer to current cursor position."
  (interactive)
  (insert (buffer-file-name)))

(defun cus-insert-buffer-file-name-nondirectory ()
  "Insert file name of current buffer to current cursor position."
  (interactive)
  (insert (cus-buffer-file-name-nondirectory)))

(defun cus-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun cus-clear-whitespace ()
  "Delete trailing white space, and replace tabs with spaces."
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for osx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(if (equal system-type 'darwin)
    (progn
      (defun cus-say-current-word()
        (interactive)
        (shell-command (concat "say \"" (current-word) "\"")))

      (defun cus-open-in-macvim ()
        "Open current file in Macvim"
        (interactive)
        (shell-command (concat "open -a Macvim \"" (buffer-file-name) "\"")))

      (defun cus-dired-open-file-osx ()
        (interactive)
        (setq coding-system-for-write 'utf-8)
        (shell-command (concat "open \"" (dired-get-file-for-visit) "\"")))

      (defun cus-reveal-in-finder ()
        (interactive)
        (setq coding-system-for-write 'utf-8)
        (shell-command (concat "open -R \"" (dired-get-file-for-visit) "\"")))

      (add-hook 'dired-mode-hook
                '(lambda()
                   (local-set-key "\M-\r" 'cus-dired-open-file-osx)
                   (local-set-key "J" 'reveal-in-finder)))

      (if (equal window-system nil)
          (progn
            (defun cus-paste-from-clipboard ()
              (interactive)
              (insert (shell-command-to-string "pbpaste")))

            (defun cus-copy-to-clipboard ()
              (interactive)
              (let ((process-connection-type nil))
                (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
                  (process-send-string proc (buffer-substring (region-beginning) (region-end)))
                  (process-send-eof proc)
                  (message "Copy successfully!"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cus-generate-tags ()
  "Generate tags of current project."
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

(defun cus-set-exec-path-from-shell-path ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
