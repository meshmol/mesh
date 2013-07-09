(import (scheme time))

(define (factorial n stop)
  (let loop ((n n) (o 1))
    (if (> n stop)
        (loop (- n 1) (* o n))
        o)))

(define (choose n k)
  (quotient (factorial n k) (factorial (- n k) 0)))

;(time (choose 50000 50))



