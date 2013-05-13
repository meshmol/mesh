
(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

(define (iterative=? n)
  (= (length (iterative-div2 (create-n n))) (/ n 2)))
 
(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))

(define (recursive=? n)
  (= (length (recursive-div2 (create-n n))) (/ n 2)))



(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (error "No derivation method available" a))))

;;; DESTRUC -- Destructive operation benchmark.


(define (append-to-tail! x y)
  (if (null? x)
    y
    (let loop ((a x) (b (cdr x)))
      (if (null? b)
        (begin
          (set-cdr! a y)
          x)
        (loop b (cdr b))))))

(define (destructive n m)
  (let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
               ((= i 0) a))))
    (do ((i n (- i 1)))
        ((= i 0) l)
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               (if (null? (car l)) (set-car! l (cons '() '())))
               (append-to-tail! (car l)
                                (do ((j m (- j 1)) (a '() (cons '() a)))
                                  ((= j 0) a)))))
            (else
             (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               (set-cdr! (do ((j (div (length (car l2)) 2) (- j 1))
                              (a (car l2) (cdr a)))
                             ((zero? j) a)
                           (set-car! a i))
                         (let ((n (div (length (car l1)) 2)))
                           (cond ((= n 0)
                                  (set-car! l1 '())
                                  (car l1))
                                 (else
                                  (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                      ((= j 1)
                                       (let ((x (cdr a)))
                                         (set-cdr! a '())
                                         x))
                                    (set-car! a i))))))))))))


(define (sum n)
  (let loop ((i n) (result 0))
    (if (< i 0)
      result
      (loop (- i 1) (+ i result)))))

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  
  (tak x y z (lambda (a) a)))

(define (ctak x y z)
  (call-with-current-continuation
    (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
        (lambda (k)
          (ctak-aux
           k
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- x 1) y z)))
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- y 1) z x)))
           (call-with-current-continuation
             (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (succ n) (+ n 1))
(define (pred n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (zero? y)
    (k x)
    (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (zero? x)
    (c 0)
    (if (zero? (pred x))
      (c 1)
      (addc (call-with-current-continuation
             (lambda (c) (fibc (pred x) c)))
            (call-with-current-continuation
             (lambda (c) (fibc (pred (pred x)) c)))
            c))))

(define (fibfp n)
  (if (fl<? n 2.)
    n
    (fl+ (fibfp (fl- n 1.))
         (fibfp (fl- n 2.)))))


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


(define (foo n)
  (call/cc 
    (lambda (c)
      (if (= n 0)
          (c 0)
          (+ n (foo (- n 1)))))))

(define (nth ls n)
  (if (zero? n)
      (car ls)
      (nth (cdr ls) (- n 1))))
