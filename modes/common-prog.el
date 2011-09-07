(setq prog-modes
     '(c-mode-common-hook
       css-mode-hook
       emacs-lisp-mode-hook
       erlang-mode-hook
       html-mode-hook
       js-mode-hook
       js2-mode-hook
       php-mode-hook
       python-mode-hook))

(dolist (mode prog-modes)
  (add-hook mode
            '(lambda ()
               (hs-minor-mode 1)
               (linum-mode 1)
               (idle-highlight-mode t)
               (show-paren-mode t)
               (abbrev-mode 1))))

