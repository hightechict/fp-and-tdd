(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz (lambda (x) x))

(check (fizzbuzz 1) => 1)
(check (fizzbuzz 2) => 2)

(check-report)
