(dolist (mode '(c-mode-common-hook
                css-mode-hook
                emacs-lisp-mode-hook
                erlang-mode-hook
                html-mode-hook
                jade-mode-hook
                js-mode-hook
                php-mode-hook
                python-mode-hook))
  (add-hook mode
            '(lambda ()
               (if (>= emacs-major-version 23) (linum-mode 1))
               (show-paren-mode t)
               (abbrev-mode 1)
               (setq show-trailing-whitespace t)
               (hs-minor-mode 1))))
