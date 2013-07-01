;;マクロテスト

(define (fact-let n)
  (let tag ((n1 n) (p n))
    (if (= n1 1)                    
        p
        (tag (- n1 1) (* p (- n1 1))))))     

(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
                   (if (= n1 1)
                       p
                       (let ((m (- n1 1)))
                         (iter m (* p m)))))))
    (iter n n)))

(define (fact-do n)
  (do ((n1 n (- n1 1)) (p n (* p (- n1 1)))) ((= n1 1) p)))

(define (remove x ls)
  (let loop((ls0 ls) (ls1 ()))
    (if (null? ls0) 
        (reverse ls1)
        (loop
          (cdr ls0)
          (if (eqv? x (car ls0))
              ls1
              (cons (car ls0) ls1))))))

(define (my-reverse-let ls)
  (let loop((ls0 ls) (ls1 ()))
    (if (null? ls0)
        ls1
        (loop (cdr ls0) (cons (car ls0) ls1)))))
