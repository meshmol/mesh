
(assert (append () _l _l))
(assert (append (_x1 . _l1) _l2 (_x . _l3)) (append _l1 _l2 _l3))


(import (normal compile)
        (normal system))

(define (deref x vars)
  (cond ((null? vars) x)
        ((eqv? x (caar vars))
         (if (variable? (cdar vars))
             (deref (cdar vars) (cdr vars))
             (cdar vars)))
        ((eqv? x (cdar vars))
         (if (variable? (caar vars))
             (deref (caar vars) (cdr vars))
             (caar vars)))
        (else
          (deref x (cdr vars)))))





(define local-stack 
  '((_r . _g0)
    (_z . _g0)
    (_n . 2)
    (_n1 . 1)
    (_z1 . _g1)
    (_z . _g1)
    (_n . 1)
    (_n1 . 0)
    (_z1 . _g2)
    (_g2 . 1)))


(define trail '())


(define (unify x y)
  (cond ((eqv? x y) #t)
        ((variable? x) (unifyv x y))
        ((variable? y) (unifyv y x))
        ((and (pair? x) (pair? y))
         (unify (cdr x) (cdr y))
         (unify (car x) (car y)))
        (else #f)))


(define (unifyv x y)
  (let ((r (deref x local-stack '())))
    (cond ((variable? r)
           (set! local-stack (cons (cons x y) local-stack)) #t)
          ((variable? y)
           (set! local-stack (cons (cons y x) local-stack)) #t)
          (else
            (equal? r y)))))

(define (unifyc x y)
  (if (variable? y)
      (begin (set! local-stack (cons (cons y x) local-stack)) #t)
      (equal? x y)))

(define-syntax push!
  (syntax-rules ()
    ((_ x y)
     (set! x (cons y x)))))

(define (variable? x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\_)))


