;; Enable php mode
(add-to-list 'load-path (concat emacs-config-dir "elisp/php-mode.git"))
(require 'php-mode)

;; Automatically activate flymake mode for php mode
(add-hook 'php-mode-hook '(lambda ()
                            (flymake-mode 1)))
