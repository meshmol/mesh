#|
Normal comment is adapted to R7RS
test1
asdf
|#


 (define-syntax define-values 
   (syntax-rules () 
     ((define-values () exp) 
      (call-with-values (lambda () exp) (lambda () (undefined))))
     ((define-values (var . vars) exp) 
      (begin  
        (define var (call-with-values (lambda () exp) list)) 
        (define-values vars (apply values (cdr var))) 
        (define var (car var)))) 
     ((define-values var exp) 
      (define var (call-with-values (lambda () exp) list))))) 



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

(define (exact-integer-sqrt k)
  (when (negative? k) (error "in exact-integer-sqrt negative " k))
  (when (not (exact? k)) (error "in exact-integer-sqrt inexact " k))
  (let* ((s (exact (floor (sqrt k))))
         (r (- k (square s))))
    (values s r)))
  