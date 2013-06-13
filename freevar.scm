
(import (normal system))

(define (scan-free-vars x)
  (let ((fv '()))
    (define (iter y)
      (cond ((null? y) #t)
            ((and (identifier-free? y) (not (memv y fv)))
             (identifier-bind! y (gensym))
             (set! fv (cons y fv)))
            ((atom? y) #t)
            ((vector? y) #t)
            (else (iter (car y))(iter (cdr y))))) 
    (iter x)
    fv))

(define (subst-free-vars x)
  (subst-free-vars1 x (scan-free-vars x)))

(define (subst-free-vars1 x fv)
  (cond ((null? x) '())
        ((and (identifier? x)(memv x fv))
         (identifier-bound (car (memv x fv))))
        ((atom? x) x)
        ((vector? x) x)
        (else
          (cons (subst-free-vars1 (car x) fv)
                (subst-free-vars1 (cdr x) fv)))))
            




(define foo (cons (symbol->identifier 'c) (symbol->identifier 'c)))
