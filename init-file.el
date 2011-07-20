;; -*- mode: Emacs-Lisp; -*-

;; Make backup file save in ~/EmacsBackupFiles Directory
(setq backup-directory-alist '(("." . "~/emacs/backup-files")))

;; Add ~/emacs/elisp to load-path list
(add-to-list 'load-path "~/emacs/elisp")

;; Set default abbrev definition file
(setq abbrev-file-name "~/emacs/abbrev_defs")

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

;; Set tab width
(setq-default tab-width 4)

;; Not use tab for indentation
(setq-default indent-tabs-mode nil)

;; Set indentation of sgml basic offset
(setq sgml-basic-offset 4)

;; Set F1 key for manual entry of current word
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

;; Load directory specified setting
(load-file "~/emacs/conf/directories.el")

;; Load custom functions
(load-file "~/emacs/custom_func.el")

;; Load mode specific configurations
(load-file "~/emacs/modes/c.el")
(load-file "~/emacs/modes/python.el")
(load-file "~/emacs/modes/html.el")
(load-file "~/emacs/modes/javascript.el")
(load-file "~/emacs/modes/php.el")
(load-file "~/emacs/modes/man.el")
(load-file "~/emacs/modes/info.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user's full name in Email's From header
(setq user-full-name "Shander Lam")

;; Make mail user agent use the SMTP library
;; (setq send-mail-function 'smtpmail-send-it)

;; Set the SMTP server's hostname
;; (setq smtpmail-smtp-server "smtp.gmail.com")

;; Set the port on the SMTP server to contact
;; (setq smtpmail-smtp-service 587)

;; Set information of SASL authentication
;; (setq smtpmail-auth-credentials
;;       '(("smtp.gmail.com" 587 "shanderlam@gmail.com" nil)))

;; Make connect to the server using STARTTLS
;; (setq smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil)))


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

;; Configure etags-select
(load-file "~/emacs/elisp/etags-select.el")
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(add-hook 'etags-select-mode-hook
          '(lambda()
             (local-set-key "\r" 'etags-select-goto-tag)
             (local-set-key "o" 'etags-select-goto-tag-other-window)))

;; Let completion in a popup menu instead of new buffer
(defcustom complete-in-region-use-popup t
  "If non-NIL, complete-in-region will popup a menu with the possible completions."
  :type 'boolean
  :group 'completion)
(autoload 'popup-menu* "popup" "Show a popup menu" nil)
(defun popup-complete-in-region (next-func start end collection &optional predicate)
  (if (not complete-in-region-use-popup)
      (funcall next-func start end collection predicate)
    (let* ((prefix (buffer-substring start end))
           (completion (try-completion prefix collection predicate))
           (choice (and (stringp completion)
                        (string= completion prefix)
                        (popup-menu* (all-completions prefix collection predicate))))
           (replacement (or choice completion))
           (tail (and (stringp replacement)
                      (not (string= prefix replacement))
                      (substring replacement (- end start)))))
      (cond ((eq completion t)
             (goto-char end)
             (message "Sole completion")
             nil)
            ((null completion)
             (message "No match")
             nil)
            (tail
             (goto-char end)
             (insert tail)
             t)
            (choice
             (message "Nothing to do")
             nil)
            (t
             (message "completion: something failed!")
             (funcall next-func start end collection predicate))))))
(add-hook 'completion-in-region-functions 'popup-complete-in-region)
(provide 'popup-complete)