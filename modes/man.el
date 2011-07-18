(add-hook 'man-mode-hook
          '(lambda()
             (linum-mode 0)
             (hl-line-mode 0)))