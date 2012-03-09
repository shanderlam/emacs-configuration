(eval-when-compile    (require 'color-theme))
(defun color-theme-shanderlam ()
  "Color theme by Shander Lam, created 2011-12-27."
  (interactive)
  (color-theme-install
   '(color-theme-shanderlam
     ((background-mode . dark))

     (hl-line ((t (:background "#212121"))))

     ;; magit
     (magit-item-highlight ((t (:background "#191930"))))
     (magit-diff-add ((t (:foreground "green"))))
     (magit-diff-del ((t (:foreground "red"))))

     ;; diff
     (diff-added ((t (:foreground "green"))))
     (diff-removed ((t (:foreground "red")))))))

(add-to-list 'color-themes '(color-theme-shanderlam  "My Color Theme" "Shander Lam"))

(provide 'color-theme-shanderlam)