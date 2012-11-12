;; erlang mode
(when (require 'erlang-start nil t)
  (add-to-list 'auto-mode-alist '("rebar.config$" . erlang-mode)))
