(defun python-reinstate-current-directory ()
  "When running Python, add the current directory ('') to the head of sys.path.
For reasons unexplained, run-python passes arguments to the
interpreter that explicitly remove '' from sys.path. This means
that, for example, using `python-send-buffer' in a buffer
visiting a module's code will fail to find other modules in the
same directory.

Adding this function to `inferior-python-mode-hook' reinstates
the current directory in Python's search path."
  (python-send-string "sys.path[:0] = ['']"))

(add-hook 'inferior-python-mode-hook 'python-reinstate-current-directory)
(add-hook 'python-mode-hook
          '(lambda ()
             (linum-mode 1)
             (hs-minor-mode)))

(define-skeleton python-insert-property "Insert Python property template"
  "Name: "
  "@property" \n
  "def " str "(self):" \n
  "\"\"\"" - "\"\"\"" \n
  "return self._" str \n
  "@" str ".setter" \n
  "def " str "(self, value):" \n
  "self._" str " = value" \n
  < "@" str ".deleter" \n
  "def " str "(self):" \n
  "del self._" str \n)
