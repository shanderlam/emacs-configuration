;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations for extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (defun ac-js-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'js-mode-hook 'ac-js-mode-setup))

;; mark-multiple
(when (require 'mark-multiple nil t)
  (require 'inline-string-rectangle)
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

  (require 'mark-more-like-this)
  (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
  (add-hook 'sgml-mode-hook
			(lambda ()
			  (require 'rename-sgml-tag)
			  (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag))))

;; etags-select
(load-file (concat emacs-config-dir "elisp/etags-select.el"))
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(add-hook 'etags-select-mode-hook
          '(lambda()
             (local-set-key "\r" 'etags-select-goto-tag)
             (local-set-key "o" 'etags-select-goto-tag-other-window)))

;; popwin
(when (require 'popwin nil t)
  (progn
    (setq display-buffer-function 'popwin:display-buffer)
    (global-set-key (kbd "C-x p") popwin:keymap)
    (setq popwin:special-display-config '(("*Completions*" :noselect t)
                                          ("*compilation*" :noselect t)
                                          ("*Occur*" :noselect t)))))

;; yasnippet
(when (require 'yasnippet nil t)
  (progn
    (add-to-list 'yas/snippet-dirs (concat emacs-config-dir "elisp/yasnippet-snippets"))
    (yas-global-mode 1)))

;; anything
(require 'anything-config)

;; Load git.el
(when (file-exists-p "/usr/local/share/git-core/contrib/emacs")
  (add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs")
  (require 'git)
  (require 'git-blame))

;; Load magit if installed
(require 'magit nil t)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
