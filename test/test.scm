
(define (foo x)
  (define (bar x)
    x)
  (define (woo x)
    x)
  (+ (bar x) (woo x)))
