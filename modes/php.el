(when (require 'php-mode nil t)
  ;; Automatically activate flymake mode for php mode
  (add-hook 'php-mode-hook '(lambda ()
                              (flymake-mode 1))))
