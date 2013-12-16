(when (require 'js2-mode nil t)
  (progn
	(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	(setq js2-strict-inconsistent-return-warning nil)))
