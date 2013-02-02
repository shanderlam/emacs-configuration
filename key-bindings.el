;; Set F1 key for manual entry of current word
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

(when (equal window-system 'ns)
  ;; Set Command-control-f for toggle fullscreen
  (global-set-key (kbd "<C-s-268632070>") 'ns-toggle-fullscreen))
