(define fizzbuzz 
  (lambda (x) 
    (cond ((= 0 (modulo x 15)) "fizzbuzz")
          ((= 0 (modulo x 3)) "fizz")
          ((= 0 (modulo x 5)) "buzz")
          (else x))))

(let (
    (assert-fizzbuzz 
      (lambda (expected parameter)
        (let ((actual (fizzbuzz parameter)))
        (test (format "(fizzbuzz ~a) = ~a expected ~a" parameter actual expected) 
              expected (fizzbuzz parameter)))))
    (test-cases 
      (list 
        '(1 1) 
        '(2 2)
        '(31 31)
        '("fizz" 3)
        '("buzz" 5 )
        '("buzz" 10)
        '("fizz" 6)
        '("fizzbuzz" 15)
        '("fizzbuzz" 30))))
  (map (lambda (x) (apply assert-fizzbuzz x)) test-cases))
