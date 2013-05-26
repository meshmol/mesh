(define (take-numbers ls)
  (define (num-or-none n acm)
    (if (number? n) (append acm (list n)) acm))
  (let loop ((ls (cdr ls)) (val (car ls)) (a '()))
    (if (null? ls)
      (num-or-none val a)
      (loop (cdr ls) (car ls) (num-or-none val a))
      )))

;; take-numbers ‚ÌƒeƒXƒg(2)
(define (test-take-num)
  (let ((ls '(1 2 3 "p" 4 5 "p" 6 7 "p" 8 9)))
    (print "arg : " ls)
    (take-numbers ls)
    ))

(define (print . x)
  (for-each display x))

