;;r7rs ���C�u��������m�F

(define-library (scheme base)
  (export = + - * / < <= > >= (rename list fff)))

(define-library (test macro)
  (export foo fact)
  (import (scheme base)
          (normal user))
  (begin
    (define (foo x y) (fff x y))
    (define (fact n)
      (if (zero? n)
          1
          (* n (fact (- n 1)))))))








