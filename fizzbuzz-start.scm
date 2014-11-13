(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz 
  (lambda (x) 
    (cond ((= 0 (modulo x 3)) "fizz")
          ((= 5 x) "buzz")
          (else x))))

(define check-fizzbuzz 
  (lambda (value-to-check expected)
    (check (fizzbuzz value-to-check) => expected)))

(check-fizzbuzz 1 1)
(check-fizzbuzz 2 2)
(check-fizzbuzz 3 "fizz")
(check-fizzbuzz 5 "buzz")
(check-fizzbuzz 6 "fizz")

(check-report)
