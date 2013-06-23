
(define (deref x vars back)
  (cond ((null? vars) x)
        ((eqv? x (caar vars))
         (if (variable? (cdar vars))
             (deref (cdar vars) (reverse back) '())
             (cdar vars)))
        (else
          (deref x (cdr vars) (cons (car vars) back)))))


(define (variable? x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\_)))


(define vars-assoc '((_x . 1)(_y . _x)))

(define trail '())

(define-syntax push!
  (syntax-rules ()
    ((_ x y)
     (set! x (cons y x)))))

(define (unify x y)
  (cond ((eq? x y) #t)
        ((variable? x) (unifyv x y))
        ((variable? y) (unifyv y x))
        ((and (pair? x) (pair? y))
         (unify (cdr x) (cdr y))
         (unify (car x) (car y)))
        (else #f)))


(define (unifyv x y)
  (let ((r (deref x vars-assoc '())))
    (cond ((variable? r)
           (set! vars-assoc (cons (cons x y) vars-assoc)) #t)
          ((variable? y)
           (set! vars-assoc (cons (cons y x) vars-assoc)) #t)
          (else
            (equal? r y)))))

(define (unifyc x y)
  (if (variable? y)
      (begin (set! vars-assoc (cons (cons y x) vars-assoc)) #t)
      (equal? x y)))

