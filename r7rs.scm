(define (string-map f . args)
  (let ((c (apply min (map string-length args))))
    (string-map1 f c args)))
    
    (define (string-map1 f n args)
      (if (= n 0)
          '()
          (string-nth (- n 1) args)))
    
    
    (define (string-nth n args)
      (if (null? args)
          '()
          (cons (string-ref (car args) n)
                (string-nth n (cdr args)))))


(define (map f ls . more)
  (if (null? more)
      (let map1 ((ls ls))
        (if (null? ls)
            '()
            (cons (f (car ls))
                  (map1 (cdr ls)))))
      (let map-more ((ls ls) (more more))
        (if (null? ls)
            '()
            (cons (apply f (car ls) (map car more))
                  (map-more (cdr ls)
                            (map cdr more)))))))

    

