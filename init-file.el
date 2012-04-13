;; -*- mode: Emacs-Lisp; -*-

(defvar emacs-config-dir "~/emacs/")
(defvar emacs-doc-dir "~/Documents/Emacs/")

;; Setting backup directory
(setq backup-directory-alist (list (cons "." (concat emacs-config-dir ".backup-files"))))

;; Setting user directory
(setq user-emacs-directory (concat emacs-config-dir ".user-dir"))

;; Add elisp to load-path list
(add-to-list 'load-path (concat emacs-config-dir "elisp"))

;; Set default abbrev definition file
(setq abbrev-file-name (concat emacs-config-dir "abbrev_defs"))

;; Enable mark comment repeat pop
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
(setq bookmark-default-file (concat emacs-doc-dir "bookmarks")
      bookmark-save-flag 1)

;; Make usual search commands matches only file name when point was on a
;; file name initially
(setq dired-isearch-filenames 'dwim)

;; Set agenda files
(setq org-agenda-files (concat emacs-config-dir "agenda.lst"))

;; Set tab width
(setq-default tab-width 4)

;; Not use tab for indentation
(setq-default indent-tabs-mode nil)

;; Set indentation of sgml basic offset
(setq sgml-basic-offset 4)

;; Set F1 key for manual entry of current word
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure flymake for javascript using closure compiler
(when (load "flymake" t)
  (defun flymake-closure-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat emacs-config-dir "shell-scripts/closure.sh")
            (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js$\\'" flymake-closure-init)))

(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set user's full name in Email's From header
(setq user-full-name "Shander Lam")

(setq user-mail-address "shanderlam@gmail.com")

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

(setq diary-file (concat emacs-doc-dir "diary"))

;; Customize calendar
(add-hook 'calendar-initial-window-hook
      '(lambda()
         ;; Mark all visible dates that have diary entries
         (diary-mark-entries)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete
(if (>= emacs-major-version 23)
    (progn
      (add-to-list 'load-path (concat emacs-config-dir "elisp/auto-complete"))
      (require 'auto-complete-config)
      (add-to-list 'ac-dictionary-directories (concat emacs-config-dir "elisp/auto-complete/dict"))
      (ac-config-default)))

;; etags-select
(load-file (concat emacs-config-dir "elisp/etags-select.el"))
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(add-hook 'etags-select-mode-hook
          '(lambda()
             (local-set-key "\r" 'etags-select-goto-tag)
             (local-set-key "o" 'etags-select-goto-tag-other-window)))

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(global-set-key (kbd "C-x p") popwin:keymap)
(setq popwin:special-display-config '(("*Completions*" :noselect t)
                                      ("*compilation*" :noselect t)
                                      ("*Occur*" :noselect t)))

;; color theme
(add-to-list 'load-path (concat emacs-config-dir "elisp/themes"))
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)

(add-to-list 'color-themes
             '(color-theme-blackboard
               "TextMate Blackboard"
               "JD Huntington <jdhuntington@gmail.com>"))
(add-to-list 'color-themes
             '(color-theme-twilight
               "TextMate Twilight"
               "Marcus Crafter <crafterm@redartisan.com>"))

;; yasnippet
(add-to-list 'load-path
             (concat emacs-config-dir "elisp/yasnippet"))
(require 'yasnippet) ;; not yasnippet-bundle
(setq yas/snippet-dirs (concat emacs-config-dir "elisp/yasnippet/snippets"))
(yas/load-directory yas/snippet-dirs)
(yas/initialize)

;; anything
(require 'anything-config)

;; Load magit if installed
(require 'magit nil t)

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
      (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
      (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

(if (equal window-system 'ns)
    (color-theme-twilight)
  (color-theme-shanderlam))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load external settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load custom functions
(load-file (concat emacs-config-dir "custom_func.el"))

;; Load mode specific configurations
(dolist (file (directory-files (concat emacs-config-dir "modes") t ".+\.el$"))
  (load-file file))
