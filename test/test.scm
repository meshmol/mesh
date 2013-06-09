;;quasi-quote transfer

(define (quasi x)
  (quasi-transfer x 0)) 

(define (quasi-transfer x n)
  (cond ((null? x) '())
        ((atom? x)
         (list 'quote x))
        ((and (pair? x)(eqv? (car x) 'quasiquote))
         (quasi-transfer (cadr x) (+ n 1)))
        ((and (pair? x)(eqv? (car x) 'unquote))
         (if (= n 0)
             (cadr x)
             (quasi-transfer (cadr x) (- n 1))))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote))
         (list (quasi-transfer (cadar x) (- n 1)) (quasi-transfer (cdr x) n)))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote-splicing))
         (list (quasi-transfer (cadar x) (- n 1)) (quasi-transfer (cdr x) n)))
        (else
          (list 'cons (quasi-transfer (car x) n) (quasi-transfer (cdr x) n)))))


;;quasi-quote transfer
(define (*quasi-transfer x)
  (cond ((null? x) '())
        ((atom? x)
         (list 'quote x))
        ((and (pair? x)(eqv? (car x) 'unquote))
         (cadr x))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote))
         (list 'cons (cadar x) (*quasi-transfer (cdr x))))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote-splicing))
         (list 'append (cadar x) (*quasi-transfer (cdr x))))
        (else
          (list 'cons (*quasi-transfer (car x)) (*quasi-transfer (cdr x))))))
