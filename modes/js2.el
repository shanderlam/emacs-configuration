(when (require 'js2-mode nil t)
  (progn
	(add-hook 'js-mode-hook 'js2-minor-mode)
	(setq js2-strict-inconsistent-return-warning nil)))
