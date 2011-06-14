;; -*- coding: emacs-mule; mode: Lisp; -*-
(define-abbrev-table 'js-mode-abbrev-table
  '(
    ("ahw" "alert('Hello, world');" nil 0)
   ))

(define-abbrev-table 'php-mode-abbrev-table
  '(
    ("ehw" "echo \"Hello, world\";" nil 1)
    ("el" "error_log(__FILE__ . PHP_EOL, 3, '/var/tmp/php.log');" nil 0)
   ))
