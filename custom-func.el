(defun cus-locate-project (search-string)
  "Locate files within the project if `cus-project' is set,
otherwise locate files within `default-directory'."
  (interactive "MLocate in Project: ")
  (if (boundp 'cus-project)
	  (locate search-string (concat cus-project "/*"))
	(locate search-string (concat default-directory "/*"))))

(defun cus-buffer-file-name-nondirectory ()
  "Get buffer file name without directory path."
  (file-name-nondirectory buffer-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for inserting string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cus-insert-buffer-file-name (arg)
  "With no prefix argument, insert the full path file name of current buffer
to current cursor position.

With prefix argument, Insert file name of current buffer to current cursor
position."
  (interactive "P")
  (push-mark)
  (if arg
	  (insert (cus-buffer-file-name-nondirectory))
	(insert (buffer-file-name))))

(defun cus-insert-timestamp ()
  (interactive)
  (push-mark)
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
      (defun cus-say()
        (interactive)
		(if (and transient-mark-mode mark-active)
			(shell-command
			 (concat "say \""
					 (buffer-substring (region-beginning) (region-end)) "\""))
		  (shell-command (concat "say \"" (current-word) "\""))))

      (defun cus-open-in-macvim ()
        "Open current file in Macvim"
        (interactive)
        (shell-command (concat "open -a ~/Applications/MacVim \"" (buffer-file-name) "\"")))

	  (defun cus-open-in-textmate ()
		"Open current file in TextMate"
		(interactive)
		(shell-command (concat "open -a TextMate \"" (buffer-file-name) "\"")))

	  (defun cus-open-in-sublime ()
		"Open current file in Sublime Text"
		(interactive)
		(shell-command (concat "open -a \"Sublime Text 2\" \"" (buffer-file-name) "\"")))

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
                   (local-set-key "J" 'cus-reveal-in-finder)))

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
                  (message "Copy successfully!"))))
            ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cus-eval-dwim (arg)
  "Call eval command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
`eval-region'. Else, call `eval-last-sexp'."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
	  (eval-region (region-beginning) (region-end))
	(eval-last-sexp arg)))

(defun cus-show-manual ()
  "Show manual for current word."
  (interactive)
  (manual-entry (current-word)))

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
