(add-hook 'js-mode-hook
          '(lambda()
             (if (string-match "\\.jade$" buffer-file-name)
                 (set (make-local-variable 'js-indent-level) 2))))
