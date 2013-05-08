;;;p555ƒ`ƒƒ[ƒ`”

(define zero (lambda (s) (lambda (z) z)))
(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s(s z)))))
(define three (lambda (s) (lambda (z) (s(s(s z))))))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))


(define repeated
  (lambda (f n)
    (if (zero? n)
        (lambda (x) x)
        (compose f (repeated f (-- n))))))

(define succ
  (lambda (v)
    (lambda (s)
      (lambda (z)
        (s ((v s) z))))))

(define succ#
  (lambda (v s z)
    (s ((v s) z))))


                            