
(define (string-map f . args)
  (list->string 
    (reverse 
      (string-map1 f (apply min (map string-length args)) args))))

(define (string-map1 f n args)
  (if (= n 0)
      '()
      (cons (apply f (string-nth (- n 1) args))
            (string-map1 f (- n 1) args))))


(define (string-nth n args)
  (if (null? args)
      '()
      (cons (string-ref (car args) n)
            (string-nth n (cdr args)))))
 
