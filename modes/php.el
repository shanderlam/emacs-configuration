;; Enable php mode
(require 'php-mode)

;; Automatically activate flymake mode for php mode
(add-hook 'php-mode-hook '(lambda ()
                            (linum-mode 1)
                            (flymake-mode 1)))