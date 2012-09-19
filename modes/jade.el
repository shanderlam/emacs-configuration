(when (require 'sws-mode nil t)
  (progn
    (require 'jade-mode)
    (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
    (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))))
