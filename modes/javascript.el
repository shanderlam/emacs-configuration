;; Load js3 mode
(autoload 'js3-mode "js3" nil t)
(setq-default js3-indent-level 4)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
;; (add-hook 'js-mode-hook '(lambda()
;;                            (flymake-mode t)))
