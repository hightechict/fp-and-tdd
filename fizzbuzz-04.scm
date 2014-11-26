(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define divisible-by?
  (lambda (divisor number)
    (= 0 (modulo number divisor))))

(define digits
  (lambda (x) 
    (define last-digit 
      (lambda (n) (remainder n 10)))
    (define digits-rec 
      (lambda (n accumulator)
        (if (< n 10) (cons n accumulator)
          (digits-rec
            (/ (- n (last-digit n)) 10) 
            (cons (last-digit n) accumulator)))))
    (digits-rec x '())))

(define contains? 
  (lambda (element alist)
    (if (null? alist) #f
      (or (eq? (car alist) element) 
          (contains? element (cdr alist))))))

(define contains-digit? 
  (lambda (digit number) 
    (contains? digit (digits number))))

(define fizzbuzz 
  (lambda (x) 
    (cond ((or 
             (divisible-by? 15 x)
             (and 
               (contains-digit? 3 x)
               (contains-digit? 5 x))) "fizzbuzz")
          ((or (divisible-by? 3 x) (contains-digit? 3 x)) "fizz")
          ((or (divisible-by? 5 x) (contains-digit? 5 x)) "buzz")
          (else x))))

(define check-fizzbuzz 
  (lambda (value-to-check expected)
    (check (fizzbuzz value-to-check) => expected)))

(check-fizzbuzz 1 1)
(check-fizzbuzz 2 2)
(check-fizzbuzz 3 "fizz")
(check-fizzbuzz 5 "buzz")
(check-fizzbuzz 6 "fizz")
(check-fizzbuzz 7 7)
(check-fizzbuzz 10 "buzz")
(check-fizzbuzz 15 "fizzbuzz")
(check-fizzbuzz 30 "fizzbuzz")
(check-fizzbuzz 31 "fizz")
(check-fizzbuzz 37 "fizz")
(check-fizzbuzz 52 "buzz")
(check-fizzbuzz 53 "fizzbuzz")

(check (digits 1) => '(1))
(check (digits 2) => '(2))
(check (digits 11) => '(1 1))
(check (digits 123) => '(1 2 3))
(check (digits 12345) => '(1 2 3 4 5))

(check (contains? 1 '()) => #f)
(check (contains? 1 '(1)) => #t)
(check (contains? 1 '(2 3 1)) => #t)
(check (contains? 1 '(2 3 4)) => #f)
(check (contains? 3 '(2 3 4)) => #t)
(check (contains? 3 '(2 7 4)) => #f)

(check (contains-digit? 1 1) => #t)
(check (contains-digit? 1 2313) => #t)

(check-report)
