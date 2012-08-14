(eval-when-compile    (require 'color-theme))
(defun color-theme-original ()
  "Color theme inherit from original theme, created 2011-12-27."
  (interactive)
  (color-theme-install
   '(color-theme-original
     ((background-mode . dark))

     (hl-line ((t (:background "#212121"))))

     ;; magit
     (magit-item-highlight ((t (:background "#191930"))))
     (magit-diff-add ((t (:foreground "green"))))
     (magit-diff-del ((t (:foreground "red"))))

     ;; diff
     (diff-added ((t (:foreground "green"))))
     (diff-removed ((t (:foreground "red")))))))

(add-to-list 'color-themes '(color-theme-original  "Color Theme Inherit From Original Theme" "Shander Lam"))

(provide 'color-theme-original)
