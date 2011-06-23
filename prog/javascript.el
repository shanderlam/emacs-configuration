;; Load js2 mode
(autoload 'js2-mode "js2" nil t)

(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'js2-mode-hook 'hs-minor-mode)

(add-hook 'js-mode-hook 'abbrev-mode)
