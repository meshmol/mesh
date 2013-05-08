


(define-library (test)
  (export woo)
  (import (scheme base)
          (scheme inexact))
          
  (begin
    (define (woo x)
      (letrec ((a (lambda (x) (sin x))))
        (a x)))))


    







