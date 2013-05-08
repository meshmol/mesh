(define (solve? num ls)
  (let loop ((ls ls)(n 1))
    (cond
      ((null? ls)#t)
      ((= (abs (- num (car ls))) n)
       #f)
      (else (loop (cdr ls)(+ n 1))))))

(define (check? lst)
  (let loop ((lst lst))
    (cond
      ((null? (cdr lst))#t)
      ((solve? (car lst) (cdr lst))
       (loop (cdr lst)))
      (else #f))))

(define (queen lst)
  (let loop ((lst lst)(result '()))
    (if (null? lst)
      (print (reverse result))
      (for-each (lambda(x)
        (if (check? (reverse (cons x result)))
                  (loop (delete x lst) (cons x result)))) lst))))

(define (delete x lst)
  (cond ((null? lst) '())
        ((eq? x (car lst)) (delete x (cdr lst)))
        (else (cons (car lst) (delete x (cdr lst))))))

(define (print x)
  (display x)
  (newline))

