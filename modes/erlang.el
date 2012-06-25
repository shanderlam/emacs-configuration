;; erlang mode
(when (file-exists-p "/usr/local/lib/erlang/lib/tools-2.6.6.1/emacs")
  (add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.6.1/emacs")
  (setq erlang-root-dir "/usr/local/lib/erlang")
  (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
  (require 'erlang-start))
