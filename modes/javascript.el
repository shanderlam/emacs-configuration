(add-hook 'js-mode-hook '(lambda()
                           (if (string-match "\\.jade$" buffer-file-name)
                               (setq js-indent-level 2)
                             (setq js-indent-level 4))))
