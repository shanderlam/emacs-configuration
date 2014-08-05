(add-hook 'prog-mode-hook
	  '(lambda ()
	     (if (>= emacs-major-version 23) (linum-mode 1))
	     (show-paren-mode t)
	     (abbrev-mode 1)
	     (setq show-trailing-whitespace t)
	     (hs-minor-mode 1)))
