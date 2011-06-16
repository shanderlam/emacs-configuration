(add-hook 'python-mode-hook 'hs-minor-mode)

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
