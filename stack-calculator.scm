(load "check.scm")

(define unary-operator? 
  (lambda (expression)
    (eqv? 'sqrt (car expression))))

(define operator? 
  (lambda (expression)
    (let ((token (car expression)))
      (or (eq? '+ token)
          (eq? '- token)
          (eq? '* token)
          (eq? '^ token)
          (unary-operator? expression)
          (eq? '/ token)))))

(define operator 
  (lambda (expression) 
    (let ((oper (car expression)))
      (cond ((eq? '^ oper) 'expt)
            (else oper)))))

(define rpn 
  (lambda (expression)
    (letrec (
      (evaluate 
        (lambda (expression . operands)
          (eval (cons (operator expression) operands))))

      (calculate 
        (lambda (expr stack)
            (cond ((null? expr) (car stack))
                  ((unary-operator? expr)
                   (let* ((first-operand (car stack))
                          (result (evaluate expr first-operand)))
                     (calculate (cdr expr) (cons result (cdr stack)))))
                  ((operator? expr)
                   (let* ((first-operand (cadr stack))
                          (second-operand (car stack))
                          (result (evaluate expr first-operand second-operand)))
                     (calculate (cdr expr) (cons result (cddr stack)))))
                  (else (calculate (cdr expr) (cons (car expr) stack)))))))
    (if (null? expression)
      "there is nothing to calculate!"
      (calculate expression '())))))

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
(check (rpn '(4 sqrt)) => 2)

(check-report)
