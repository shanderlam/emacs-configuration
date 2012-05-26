(add-to-list 'load-path (concat emacs-config-dir "elisp/jade-mode.git"))
(require 'sws-mode)
(require 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))