(define (rpn expression)
  (define operator? 
    (lambda (expression)
      (let ((token (car expression)))
        (or (eqv? '+ token)
            (eqv? '- token)
            (eqv? '* token)
            (eqv? '/ token)))))
  (define operator 
    (lambda (expression) 
      (car expression)))
  (define calculate 
    (lambda (exp stack)
      (if (null? expression)
        "there is nothing to calculate!"
        (cond ((null? exp) (car stack))
              ((operator? exp)
               (let* ((first-operand (cadr stack))
                      (second-operand (car stack))
                      (result (eval (list (operator exp) first-operand second-operand))))
                 (calculate (cdr exp) (cons result (cddr stack)))))
              (else (calculate (cdr exp) (cons (car exp) stack)))))))
  (calculate expression '()))
