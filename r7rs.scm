#|
Normal comment is adapted to R7RS
test1
asdf
|#

;;directive test
#!fold-case

(define aB 1)
(define AB 2)
(if (eq? aB AB)
    (display 'ok)
    (display 'not))

#!no-fold-case
(define Cd 3)
(define cD 4)
(if (not(eq? Cd cD))
    (display 'ok)
    (display 'not))

(define (floor/ n1 n2)
      (when (zero? n2)
        (error "in floor/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor/ not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))

(define (floor-quotient n1 n2)
      (when (zero? n2)
        (error "in floor-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor-quotient not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))

(define (floor-remainder n1 n2)
      (when (zero? n2)
        (error "in floor-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor-remainder not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))
    
    (define (round/ n1 n2)
      (when (zero? n2)
        (error "in round/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round/ not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))

(define (round-quotient n1 n2)
      (when (zero? n2)
        (error "in round-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round-quotient not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))

(define (round-remainder n1 n2)
      (when (zero? n2)
        (error "in round-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round-remainder not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))
    
    (define (truncate/ n1 n2)
      (when (zero? n2)
        (error "in truncate/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate/ not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))

(define (truncate-quotient n1 n2)
      (when (zero? n2)
        (error "in truncate-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate-quotient not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))

(define (truncate-remainder n1 n2)
      (when (zero? n2)
        (error "in truncate-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate-remainder not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))