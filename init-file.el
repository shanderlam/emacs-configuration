(defvar emacs-config-dir (file-name-directory (file-truename load-file-name)))

;; Add melpa repository to package archives
(when (require 'package nil t)
  (progn
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (setq package-enable-at-startup nil)
    (package-initialize)))

;; Enable semantic mode
(semantic-mode 1)

(setq backup-directory-alist (list (cons "." (concat emacs-config-dir ".backup-files"))))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

;; Make usual search commands matches only file name when point was on a
;; file name initially
(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)

;; Do not allow split window vertically by `split-window-sensibly' function
(setq split-height-threshold nil)

;; Reload directory variables after major mode change
(when (fboundp 'hack-local-variables-apply )
  (add-hook 'after-change-major-mode-hook
            '(lambda ()
               (hack-dir-local-variables)
               (hack-local-variables-apply))))

;; Color theme
(when (>= emacs-major-version 24)
  (add-to-list 'custom-theme-load-path (concat emacs-config-dir "themes"))
  (if window-system
      (load-theme 'solarized-light t)
    (load-theme 'osx-terminal-pro t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake-cursor nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load extension settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (concat emacs-config-dir "ext-settings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom functions and aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (concat emacs-config-dir "functions.el"))
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
