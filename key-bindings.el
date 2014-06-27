;; Set F1 key for manual entry of current word
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

(when (fboundp 'cus-eval-dwim)
  (global-set-key (kbd "C-x C-e") 'cus-eval-dwim))
