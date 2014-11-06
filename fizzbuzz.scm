(load "check.scm")

(define fizzbuzz 
  (lambda (x) 
    (cond ((= 0 (modulo x 15)) "fizzbuzz")
          ((= 0 (modulo x 3)) "fizz")
          ((= 0 (modulo x 5)) "buzz")
          (else x))))
(let (
      (check-fizzbuzz 
        (lambda (check-value expected)
          (check (fizzbuzz check-value) => expected)))
      (test-cases (list
                '(1 1)
                '(2 2)
                '(3 "fizz")
                '(5 "buzz")
                '(9 "fizz")
                '(10 "buzz")
                '(15 "fizzbuzz")
                '(30 "fizzbuzz")
                '(37 37))))
  (map (lambda (x) (apply check-fizzbuzz x)) test-cases))

(check-report)
