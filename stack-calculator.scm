(load "~/.bin/check.scm")
(check-set-mode! 'report-failed)

(define operator
  (lambda (expression) 
    (car expression)))

(define operator? 
  (lambda (expression)
    (define token (car expression))
    (or (eq? '+ token)
        (eq? '- token)
        (eq? '* token)
        (eq? '/ token))))

(define calculate 
  (lambda (expression stack)
    (cond ((null? expression) (car stack))
          ((operator? expression)
            (define first-operand (cadr stack))
            (define second-operand (car stack))
            (define result (eval (list 
                                   (operator expression) 
                                   first-operand 
                                   second-operand)))
            (calculate 
              (cdr expression) 
              (cons result (cddr stack))))
          (else (calculate 
                  (cdr expression) 
                  (cons (car expression) stack))))))

(define rpn 
  (lamdba (expression)
    (if (null? expression)
      "there is nothing to calculate!"
      (calculate expression '()))))

(check (rpn '()) => )

(check-report)
