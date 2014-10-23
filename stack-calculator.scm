(load "check.scm")

(define unary-operator? (lambda (expression)
  (eqv? 'sqrt (car expression))))

(define operator? (lambda (expression)
  (let ((token (car expression)))
    (or (eqv? '+ token)
        (eqv? '- token)
        (eqv? '* token)
        (eqv? '^ token)
        (unary-operator? expression)
        (eqv? '/ token)))))

(define operator 
  (lambda (expression) 
    (let ((oper (car expression)))
      (cond ((eqv? '^ oper) 'expt)
            (else oper)))))

(define (rpn expression)
  (define evaluate 
    (lambda (operator . operands)
      (eval (cons operator operands))))

  (define calculate (lambda (exp stack)
    (if (null? expression)
        "there is nothing to calculate!"
          (cond ((null? exp) (car stack))
                ((unary-operator? exp)
                 (let* ((first-operand (car stack))
                        (result (evaluate (operator exp) first-operand)))
                   (calculate (cdr exp) (cons result (cdr stack)))))
                ((operator? exp)
                 (let* ((first-operand (cadr stack))
                        (second-operand (car stack))
                        (result (evaluate
                                  (operator exp)
                                  first-operand 
                                  second-operand)))
                   (calculate (cdr exp) (cons result (cddr stack)))))
                (else (calculate (cdr exp) (cons (car exp) stack)))))))
  (calculate expression '()))

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

(check-report)
