(load "check.scm")

(define operator? (lambda (exp)
  (let ((token (car exp)))
    (or (eqv? '+ token)
        (eqv? '- token)
        (eqv? '* token)
        (eqv? '^ token)
        (eqv? '/ token)))))

(define operator 
  (lambda (exp) 
    (let ((oper (car exp)))
      (cond ((eqv? '^ oper) 'expt)
            (else oper)))))

(define (rpn expression)
  
  (define calculate (lambda (exp stack)
    (if (null? expression)
        "there is nothing to calculate!"
          (cond ((null? exp) (car stack))
                ((operator? exp)
                 (let* ((first-operand (cadr stack))
                        (second-operand (car stack))
                        (result (eval (list (operator exp)
                                            first-operand second-operand))))
                   (calculate (cdr exp) (cons result (cddr stack)))))
                (else (calculate (cdr exp) (cons (car exp) stack)))))))
  (calculate expression '()))

(check (rpn '(2 3 +)) => 5)
(check (rpn '(2 3 *)) => 6)
(check (rpn '(6 2 /)) => 3)
(check (rpn '(3 2 -)) => 1)
(check (rpn '(2 3 ^ )) => 8)

(check (operator? '(+)) => #t)
(check (operator? '(-)) => #t)
(check (operator? '(*)) => #t)
(check (operator? '(/)) => #t)
(check (operator? '(^)) => #t)

(check (operator '(+)) => '+)
(check (operator '(-)) => '-)
(check (operator '(*)) => '*)
(check (operator '(/)) => '/)
(check (operator '(^)) => 'expt)

(check-report)
