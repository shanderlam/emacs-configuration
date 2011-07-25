;; Automatically activate hs-minor-mode and linum-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (linum-mode 1)))