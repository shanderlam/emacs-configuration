(defalias 'sb 'speedbar)
(defalias 'fn 'cus-insert-buffer-file-name)
(defalias 'ts 'cus-insert-timestamp)
(defalias 'cls 'cus-clear-whitespace)
(defalias 'ctags 'cus-generate-tags)

(when (fboundp 'cus-say) (defalias 'say 'cus-say))
(when (fboundp 'cus-open-in-macvim) (defalias 'macvim 'cus-open-in-macvim))
(when (fboundp 'cus-paste-from-clipboard)
  (defalias 'pst 'cus-paste-from-clipboard))
(when (fboundp 'cus-copy-to-clipboard)
  (defalias 'cp 'cus-copy-to-clipboard))
