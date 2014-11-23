(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz 
  (lambda (x)
    (cond ((= x 3) "fizz")
          ((= x 5) "buzz")
          (else x))))

(define check-fizzbuzz 
  (lambda (test-case expected) 
    (check (fizzbuzz test-case) => expected)))

(check-fizzbuzz 1 1)
(check-fizzbuzz 2 2)
(check-fizzbuzz 3 "fizz")
(check-fizzbuzz 5 "buzz")

(check-report)
