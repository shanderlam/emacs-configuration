;;; original-theme.el --- Additional themes for original faces.

;;; Code:

(deftheme original
  "Customize faces for magit, diff.")

(custom-theme-set-faces
 'original

 '(hl-line ((t (:background "#212121"))))

 ;; magit
 '(magit-item-highlight ((t (:background "#191930"))))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "red"))))

 ;; diff
 '(diff-added ((t (:foreground "green"))))
 '(diff-removed ((t (:foreground "red")))))

(provide-theme 'original)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; original-theme.el ends here
