(load "check.scm")
(check-set-mode! 'report-failed)

(define unary-operator? 
  (lambda (expression)
    (eq? 'sqrt (car expression))))

(define operator? 
  (lambda (expression)
    (let ((token (car expression)))
      (or (eq? '+ token)
          (eq? '- token)
          (eq? '* token)
          (eq? '^ token)
          (eq? 'gcd token)
          (unary-operator? expression)
          (eq? '/ token)))))

(define operator 
  (lambda (expression) 
    (define oper (car expression))
    (cond ((eq? '^ oper) 'expt)
          ((eq? 'gcd oper) 'greatest-common-divisor)
          (else oper))))

(define rpn 
  (lambda (expression)
    (define evaluate 
      (lambda (expression . operands)
        (eval (cons (operator expression) operands))))

    (define calculate 
      (lambda (expr stack)
        (cond ((null? expr) (car stack))
              ((unary-operator? expr)
               (define first-operand (car stack))
               (define result (evaluate expr first-operand))
               (calculate (cdr expr) (cons result (cdr stack))))
              ((operator? expr)
               (define first-operand (cadr stack))
               (define second-operand (car stack))
               (define result (evaluate expr first-operand second-operand))
                 (calculate (cdr expr) (cons result (cddr stack))))
              (else (calculate (cdr expr) (cons (car expr) stack))))))
    (if (null? expression)
      "there is nothing to calculate!"
      (calculate expression '()))))

(define greatest-common-divisor 
  (lambda (a b) 
    (if (< a b) (greatest-common-divisor b a) 
        (if (= b 0) a
          (greatest-common-divisor b (remainder a b))))))

(check (operator? '(+)) => #t)
(check (operator? '(-)) => #t)
(check (operator? '(*)) => #t)
(check (operator? '(/)) => #t)
(check (operator? '(^)) => #t)
(check (operator? '(sqrt)) => #t)

(check (unary-operator? '(sqrt)) => #t)
(check (unary-operator? '(+)) => #f)

(check (operator '(+)) => '+)
(check (operator '(-)) => '-)
(check (operator '(*)) => '*)
(check (operator '(/)) => '/)
(check (operator '(^)) => 'expt)
(check (operator '(sqrt)) => 'sqrt)

(check (rpn '(2 3 +)) => 5)
(check (rpn '(2 3 *)) => 6)
(check (rpn '(6 2 /)) => 3)
(check (rpn '(3 2 -)) => 1)
(check (rpn '(2 3 ^ )) => 8)
(check (rpn '(4 sqrt)) => 2.0)
(check (rpn '(6 4 gcd)) => 2)
(check (rpn '(5 3 gcd)) => 1)

(check (greatest-common-divisor 1 1) => 1)
(check (greatest-common-divisor 4 2) => 2)
(check (greatest-common-divisor 2 4) => 2)
(check (greatest-common-divisor 6 4) => 2)
(check (greatest-common-divisor 5 3) => 1)

(check-report)
