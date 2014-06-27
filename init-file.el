(defvar emacs-config-dir (file-name-directory load-file-name))
(defvar emacs-doc-dir (concat emacs-config-dir "docs"))

;; Load dir settings
(load (concat emacs-config-dir "dir-settings.el") t)

;; Add melpa repository to package archives
(when (require 'package nil t)
  (progn
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (setq package-user-dir (concat emacs-config-dir "elpa"))
    (setq package-enable-at-startup nil)
    (package-initialize)))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Setting backup directory
(setq backup-directory-alist (list (cons "." (concat emacs-config-dir ".backup-files"))))

;; Setting user directory
(setq user-emacs-directory (concat emacs-config-dir ".user-dir"))

;; Add elisp to load-path list
(add-to-list 'load-path (concat emacs-config-dir "elisp"))

;; Enable mark commend repeat pop
(setq set-mark-command-repeat-pop t)

;; Show column number in the mode line
(column-number-mode 1)

;; Show time in the mode line
(display-time-mode 1)

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
(require 'bookmark)
(setq bookmark-default-file (concat emacs-doc-dir "bookmarks")
      bookmark-save-flag 1)

;; Make usual search commands matches only file name when point was on a
;; file name initially
(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)

;; Set agenda files
(require 'org)
(setq org-agenda-files (concat emacs-config-dir "agenda.lst"))

;; Set tab width
(setq-default tab-width 4)

;; Do not allow split window vertically by `split-window-sensibly' function
(setq split-height-threshold nil)

;; Set indentation of sgml basic offset
(require 'sgml-mode)
(setq sgml-basic-offset 2)

;; Reload directory variables after major mode change
(when (fboundp 'hack-local-variables-apply )
  (add-hook 'after-change-major-mode-hook
            '(lambda ()
               (hack-dir-local-variables)
               (hack-local-variables-apply))))

;; Color theme
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (concat emacs-config-dir "themes"))
  (load-theme 'osx-terminal-pro t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake-cursor nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user's full name in Email's From header
(setq user-full-name "Shander Lam")

(setq user-mail-address "shanderlam@gmail.com")

;; Make mail user agent use the SMTP library
(setq send-mail-function 'smtpmail-send-it)

(require 'smtpmail)

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
(require 'solar)
(setq calendar-latitude 26.07459)
(setq calendar-longitude 119.29659)

;; Mark today's date if current date is visible
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setq diary-file (concat emacs-doc-dir "diary"))

;; Customize calendar
(add-hook 'calendar-initial-window-hook
      '(lambda()
         ;; Mark all visible dates that have diary entries
         (diary-mark-entries)))

(load-file (concat emacs-config-dir "ext-settings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom functions and aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (concat emacs-config-dir "custom-func.el"))
(load-file (concat emacs-config-dir "aliases.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for OS X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (equal system-type 'darwin)
	(progn
	  (setq locate-make-command-line
			'(lambda(search-string)
			   (list "mdfind" "-name" search-string)))
	  (setq trash-directory "~/.Trash")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for different window systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
    (progn
      ;; Set this env variable to avoid hg log in GUI version Emacs display
      ;; non-graphic characters as "?"
      (setenv "LANG" "en_US.UTF-8")
      (setq default-process-coding-system '(utf-8 . utf-8)))
  (progn
      (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))))

;; Setting for Chinese Characters under OS X
(if (equal window-system 'ns)
    (progn
      (create-fontset-from-fontset-spec (concat "-ns-*-*-*-*-*-12-*-*-*-*-*-fontset-custom,"
                                                "latin:-*-Menlo-*-*-*-*-*-*-*-*-*-*-iso10646-1,"
                                                "han:-*-STHeiti-medium-*-*-*-*-*-*-*-*-*-iso10646-1"))

      ;; If Emacs not launched from shell, user PATH not be inherited by Emacs
      (cus-set-exec-path-from-shell-path)

      (add-to-list 'default-frame-alist '(font . "fontset-custom"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load external settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load mode specific configurations
(dolist (file (directory-files (concat emacs-config-dir "modes") t ".+\.el$"))
  (load-file file))

;; Load key bindings
(load-file (concat emacs-config-dir "key-bindings.el"))
