;; Load js2 mode
(autoload 'js2-mode "js2" nil t)

(defun js-conf ()
  (hs-minor-mode 1)
  (abbrev-mode 1)
  (linum-mode 1))

(add-hook 'js-mode-hook 'js-conf)

(add-hook 'js2-mode-hook 'js-conf)
