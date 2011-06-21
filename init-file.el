;; -*- mode: Lisp; -*-

;; Make backup file save in ~/EmacsBackupFiles Directory
(setq backup-directory-alist '(("." . "~/emacs/backup-files")))

;; Add ~/emacs/elisp to load-path list
(add-to-list 'load-path "~/emacs/elisp")

;; Set default abbrev definition file
(setq abbrev-file-name "~/emacs/abbrev_defs")

;; Enable global-linum-mode
(global-linum-mode 1)

;; Show column number in the mode line
(column-number-mode 1)

;; Show Trailing Whitespace
(setq-default show-trailing-whitespace t)

;; Make emacs delete a directory by moving it to trash
(setq delete-by-moving-to-trash t)

;; Enable iswitchb mode
(iswitchb-mode t)

;; Make buffer name unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable scroll-left function
(put 'scroll-left 'disabled nil)

;; Setup bookmarks file
(setq bookmark-default-file "~/emacs/bookmarks"
      bookmark-save-flag 1)

;; Make usual search commands matches only file name when point was on a
;; file name initially
(setq dired-isearch-filenames 'dwim)

;; Set agenda files
(setq org-agenda-files "~/emacs/org/agenda.lst")

;; Automatically activate hs-minor-mode for some programing mode initialization
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)

;; Automatically activate abbrev-mode for some major mode
(add-hook 'js-mode-hook 'abbrev-mode)
(add-hook 'js2-mode-hook 'abbrev-mode)


;; Set tab width
(setq-default tab-width 4)

;; Not use tab for indentation
(setq-default indent-tabs-mode nil)

;; Set C mode indentation to 4
(setq c-basic-offset 4)

;; Set indentation of c mode
(setq c-offsets-alist
      '((substatement-open . 0)
        (arglist-close c-lineup-arglist-operators)
        (arglist-intro +)
        (case-label +))
)

;; Set indentation of sgml basic offset
(setq sgml-basic-offset 4)

;; Load directory specified setting
(load-file "~/emacs/conf/directories.el")

;; Load python configurations
(load-file "~/emacs/prog/python.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user's full name in Email's From header
(setq user-full-name "Shander Lam")

;; Make mail user agent use the SMTP library
(setq send-mail-function 'smtpmail-send-it)

;; Set the SMTP server's hostname
(setq smtpmail-smtp-server "smtp.gmail.com")

;; Set the port on the SMTP server to contact
(setq smtpmail-smtp-service 587)

;; Set information of SASL authentication
(setq smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "shanderlam@gmail.com" nil)))

;; Make connect to the server using STARTTLS
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set calendar location
(setq calendar-latitude 26.07459)
(setq calendar-longitude 119.29659)

;; Mark today's date if current date is visible
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; Customize calendar
(add-hook 'calendar-initial-window-hook
      '(lambda()
         ;; Hide trailing whitespace
         (setq show-trailing-whitespace nil)
         ;; Mark all visible dates that have diary entries
         (diary-mark-entries)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable php mode
(require 'php-mode)
;; Automatically activate flymake mode for php mode
(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))

;; Load js2 mode
(autoload 'js2-mode "js2" nil t)

;; Configure etags-select
(load-file "~/emacs/elisp/etags-select.el")
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(add-hook 'etags-select-mode-hook
          '(lambda()
             (local-set-key "\r" 'etags-select-goto-tag)
             (local-set-key "o" 'etags-select-goto-tag-other-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for custom function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Set F1 key for manual entry of current word
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

(defun paste-from-clipboard()
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

(defun copy-to-clipboard()
  (interactive)
  (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (buffer-substring (region-beginning) (region-end)))
        (process-send-eof proc))))

(defun generate-tags()
  "Generate tags of current project"
  (interactive)
  (dolist (tags-table tags-table-list)
    (start-process "tags" "*Messages*" "etags" "-R" (concat "-f" tags-table) (substring tags-table 0 -4))))