;;; osx-terminal-pro-theme.el --- Additional themes for original faces.

;;; Code:

(deftheme osx-terminal-pro
  "Customize faces for magit, diff.")

(custom-theme-set-faces
 'osx-terminal-pro

 '(hl-line ((t (:background "#212121"))))

 ;; magit
 '(magit-item-highlight ((t (:background "#191930"))))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "red"))))

 ;; ediff
 '(ediff-current-diff-A ((t (:background "yellow" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:background "brightblack" :foreground "white"))))
 '(ediff-even-diff-A ((t (:background "brightblack" :foreground "white"))))
 '(ediff-fine-diff-A ((t (:background "cyan" :foreground "black"))))

 '(ediff-current-diff-B ((t (:background "yellow" :foreground "black"))))
 '(ediff-odd-diff-B ((t (:background "brightblack" :foreground "white"))))
 '(ediff-even-diff-B ((t (:background "brightblack" :foreground "white"))))
 '(ediff-fine-diff-B ((t (:background "cyan" :foreground "black"))))

 ;; diff
 '(diff-added ((t (:foreground "green"))))
 '(diff-removed ((t (:foreground "red")))))

(provide-theme 'osx-terminal-pro)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; osx-terminal-pro-theme.el ends here
