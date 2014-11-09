(load "check.scm")
(check-set-mode! 'report-failed)

(define fizzbuzz 
  (lambda (x) 
    (let ((divisible-by? 
      (lambda (divisor number)
        (= 0 (modulo number divisor)))))
    (cond 
        ((divisible-by? 15 x) "fizzbuzz")
        ((divisible-by? 3 x) "fizz")
        ((divisible-by? 5 x) "buzz")
          (else x)))))

(check (fizzbuzz 1) => 1)
(check (fizzbuzz 2) => 2)
(check (fizzbuzz 3) => "fizz")
(check (fizzbuzz 5) => "buzz")
(check (fizzbuzz 6) => "fizz")
(check (fizzbuzz 7) => 7)
(check (fizzbuzz 10) => "buzz")
(check (fizzbuzz 15) => "fizzbuzz")
(check (fizzbuzz 30) => "fizzbuzz")

(check-report)
