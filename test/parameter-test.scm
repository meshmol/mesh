
(define radix
  (make-parameter
    10
    (lambda (x)
      (if (and (exact-integer? x) (<= 2 x 16))
          x
          (error "invalid radix")))))

(define (f n) (number->string n (radix)))
