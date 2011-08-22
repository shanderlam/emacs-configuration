;; Set C mode indentation to 4
(setq c-basic-offset 4)

;; Set indentation of c mode
(setq c-offsets-alist
      '((substatement-open . 0)
        (arglist-close c-lineup-arglist-operators)
        (arglist-intro +)
        (case-label +))
)