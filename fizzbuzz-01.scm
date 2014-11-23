(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz 
  (lambda (x) x))

(define check-fizzbuzz 
  (lambda (value-to-check expected) 
    (check (fizzbuzz value-to-check) => expected)))

(check-fizzbuzz 1 1)
(check-fizzbuzz 2 2)

(check-report)
