;;  Make backup file save in ~/EmacsBackupFiles Directory
(setq backup-directory-alist '(("." . "~/emacs/backup-files")))

;; Add ~/emacs/elisp to load-path list
(add-to-list 'load-path "~/emacs/elisp")

;; Enable global-linum-mode
(global-linum-mode 1)

;; Show column number in the mode line
(column-number-mode 1)

;;  Show Trailing Whitespace
(setq-default show-trailing-whitespace t)

;;  Make emacs delete a directory by moving it to trash
(setq delete-by-moving-to-trash t)

;;  Enable iswitchb mode
(iswitchb-mode t)

;;  Make buffer name unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable scroll-left function
(put 'scroll-left 'disabled nil)

;; Setup bookmarks file
(setq bookmark-default-file "~/emacs/bookmarks" bookmark-save-flag 1)

;; Make usual search commands matches only file name when point was on a
;; file name initially
(setq dired-isearch-filenames 'dwim)

;; Automatically activate hs-minor-mode for some programing mode initialization
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)

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

;; Set indentation of html mode
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

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

(require 'etags-table)
;; Automatically search tags table base on current file
(setq etags-table-search-up-depth 10)
;; Set tags table alist for specified project
(setq etags-table-alist
      (list
       '("/Users/shanderlam/Workspace/Projects/YupooForWordpress/.*"
         "/Users/shanderlam/Workspace/Open Source/Wordpress/trunk/TAGS"
         "/Users/shanderlam/Workspace/Projects/YupooForWordpress/TAGS"
         )
       '("/Users/shanderlam/Workspace/Projects/sprout/.*"
         "/Users/shanderlam/Workspace/Open Source/mootools-core.git/Source/TAGS"
         "/Users/shanderlam/Workspace/Projects/sprout/TAGS"
         "/Users/shanderlam/Workspace/Projects/workforme/TAGS"
         )
       ))

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

;; Define a function to show full path name in the minibuffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key "\C-cs" 'show-file-name)
