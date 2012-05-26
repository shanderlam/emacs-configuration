(add-to-list 'load-path (concat emacs-config-dir "elisp/yaml-mode.git"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))