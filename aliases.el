(defalias 'sb 'speedbar)
(defalias 'rb 'revert-buffer)

(when (fboundp 'list-packages) (defalias 'lp 'list-packages))

(when (fboundp 'helm-locate) (defalias 'hlc 'helm-locate))
(when (fboundp 'helm-imenu) (defalias 'him 'helm-imenu))
(when (fboundp 'magit-status) (defalias 'ms 'magit-status))

(defalias 'cls 'cus-clear-whitespace)
(defalias 'ctags 'cus-generate-tags)
(defalias 'fn 'cus-insert-buffer-file-name)
(defalias 'lcp 'cus-locate-project)
(defalias 'ts 'cus-insert-timestamp)

(when (fboundp 'cus-copy-to-clipboard)
  (defalias 'cp 'cus-copy-to-clipboard))
(when (fboundp 'cus-open-in-macvim) (defalias 'mvim 'cus-open-in-macvim))
(when (fboundp 'cus-open-in-textmate) (defalias 'mate 'cus-open-in-textmate))
(when (fboundp 'cus-open-in-sublime) (defalias 'subl 'cus-open-in-sublime))
(when (fboundp 'cus-paste-from-clipboard)
  (defalias 'pst 'cus-paste-from-clipboard))
(when (fboundp 'cus-say) (defalias 'say 'cus-say))
