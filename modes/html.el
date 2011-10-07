;; Hide show for html mode
(add-to-list 'hs-special-modes-alist
             '(html-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'html-mode-hook '(lambda()
                             (auto-fill-mode 0)))