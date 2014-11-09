(define (rpn expression)
  (letrec (
    (operator? 
      (lambda (expression)
        (let ((token (car expression)))
          (or (eq? '+ token)
              (eq? '- token)
              (eq? '* token)
              (eq? '/ token)))))
    (operator 
      (lambda (expression) 
        (car expression)))
    (calculate 
      (lambda (expr stack)
        (cond ((null? expr) (car stack))
              ((operator? expr)
               (let* ((first-operand (cadr stack))
                      (second-operand (car stack))
                      (result (eval (list 
                                      (operator expr) 
                                      first-operand 
                                      second-operand))))
                 (calculate (cdr expr) (cons result (cddr stack)))))
              (else (calculate (cdr expr) (cons (car expr) stack)))))))
    (if (null? expression)
      "there is nothing to calculate!"
      (calculate expression '()))))
