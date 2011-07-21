;; Load js2 mode
(autoload 'js2-mode "js2" nil t)

(add-hook 'js-mode-hook
          '(lambda ()
             (hs-minor-mode)
             (abbrev-mode)
             (linum-mode 1)))

(add-hook 'js2-mode-hook
          '(lambda ()
             (hs-minor-mode)
             (abbrev-mode)
             (linum-mode 1)))
