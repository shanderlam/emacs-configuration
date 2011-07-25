(add-hook 'html-mode-hook
          '(lambda ()
             (hs-minor-mode 1)
             (linum-mode 1)))

;; Hide show for html mode
(add-to-list 'hs-special-modes-alist
             '(html-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
