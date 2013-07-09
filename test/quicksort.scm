
(define (quicksort ls)
  (if (null? ls)
      '()
      (let-values (((p s g) (partition ls)))
        (append (quicksort s) (cons p (quicksort g))))))

(define (partition ls)
  (define (iter x p s g)
    (cond ((null? x) (values p s g))
          ((< (car x) p) (iter (cdr x) p (cons (car x) s) g))
          (else (iter (cdr x) p s (cons (car x) g)))))
  (iter (cdr ls) (car ls) '() '()))

(define (radian x)
  (let ((2pi (* 2 (acos -1))))
    (- x (* (floor (/ x 2pi)) 2pi))))

