
(define (foo x)
  (letrec ((bar (lambda (x) (+ x x))))
    (bar x)))



