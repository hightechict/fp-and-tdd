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

(define calculate (lambda (expr stack)
    (cond ((null? expr) (car stack))
          ((operator? expr)
            (define first-operand (cadr stack))
            (define second-operand (car stack))
            (define result (eval (list 
                                   (operator expr) 
                                   first-operand 
                                   second-operand)))
            (calculate (cdr expr) (cons result (cddr stack))))
          (else (calculate (cdr expr) (cons (car expr) stack))))))

(define (rpn expression)
    (if (null? expression)
      "there is nothing to calculate!"
      (calculate expression '())))

(check (rpn '(12 12 *)) => 144)

(check-report)
