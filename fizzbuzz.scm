(load "check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz 
  (lambda (x) 
    (define divisible-by? 
      (lambda (number divisor)
        (= 0 (modulo number divisor))))

    (cond ((divisible-by? x 15) "fizzbuzz")
          ((divisible-by? x 3) "fizz")
          ((divisible-by? x 5) "buzz")
          (else x))))

(define check-fizzbuzz 
  (lambda (check-value expected)
    (check (fizzbuzz check-value) => expected)))

(define test-cases 
  '((1 1)
    (2 2)
    (3 "fizz")
    (5 "buzz")
    (9 "fizz")
    (10 "buzz")
    (15 "fizzbuzz")
    (30 "fizzbuzz")
    (37 37)))

(map (lambda (x) (apply check-fizzbuzz x)) test-cases)

(check-report)
