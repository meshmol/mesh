

(define (euclidean/ n1 n2)
  (when (zero? n2)
    (error "in ceiling/ devide by zero " n1 n2))
  (when (or (not (integer? n1)) (not (integer? n2)))
    (error "in ceiling/ not integer " n1 n2))
  (let ((q 0)(r 0))
    (if (> n2 0)
        (set! q (floor (/ n1 n2)))
        (set! q (ceiling (/ n1 n2))))
    (set! r (remainder n1 q))
    (values q r)))

      