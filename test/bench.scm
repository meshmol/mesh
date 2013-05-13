;;ƒxƒ“ƒ`


(define tarai
  (lambda (x y z)
    (if (<= x y)
        y
        (tarai (tarai (- x 1) y z)
               (tarai (- y 1) z x)
               (tarai (- z 1) x y)))))

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))


(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1))))))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

