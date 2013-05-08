;;「素数夜曲」よりテストコード用に拝借


;;p470
(define ** (lambda (a b) (expt a b)))
(define // (lambda (a b) (quotient a b)))
(define /@ (lambda (a b) (remainder a b)))
(define /: (lambda (a b) (modulo a b)))
(define ++ (lambda (i) (+ i 1)))
(define -- (lambda (i) (- i 1)))

(define make-even (lambda (n) (* 2 n)))
(define make-odd (lambda (n) (+ (* 2 n) 1)))

(define parity-of
  (lambda (p)
    (if (odd? p) -1 1)))

(define pi 3.141592653589793)

(define f_2x+3
  (lambda (x)
    (let ((a 2)(b 3))
      (+ (* a x) b))))

(define generator
  (lambda (initial)
    (let ((n initial))
      (lambda () (set! n (+ n 1))))))

(define count-1 (generator -1))

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(letrec ((fact
           (lambda (n)
             (if (zero? n)
                 1
                 (* n (fact (- n 1)))))))
  (fact 10))

;;p482
(define succ (lambda (list) (cons 1 list)))
(define pred (lambda (list) (cdr list))) 

(define plus
  (lambda (x y)
    (if (null? y)
        x
        (succ (plus x (pred y))))))

(define mult
  (lambda (x y)
    (if (null? y)
        '()
        (plus x (mult x (pred y))))))

(define pows
  (lambda (x y)
    (if (null? y)
        '(1)
        (mult x (pows x (pred y))))))

;;p494
(define fact-tailrec
  (lambda (n)
    (define fact-iter
      (lambda (y p)
        (if (zero? y)
            p
            (fact-iter (- y 1) (* y p)))))
    (fact-iter n 1)))


(define card-1
  (lambda (x)
    (if (= x 1234)
        'ok
        'oops!)))

(define my-abs
  (lambda (x)
    (cond ((positive? x) x)
          ((zero? x) 0)
          ((negative? x) (- x)))))

(define dom1-9
  (lambda (x) (and (< 1 x) (< x 9))))

(define or-less5
  (lambda (x) (or (< x 5) (= x 5))))

(define nonzero?
  (lambda (x) (not (zero? x))))

(letrec 
  ((even? (lambda (x) (or (zero? x)(odd? (- x 1)))))
   (odd? (lambda (x) (and (nonzero? x) (even? (- x 1))))))
   (even? 3))

;;p449
(define type-check
  (lambda (x)
    (define form
      (lambda (str)
        (display "This is ") (display str)))
    (cond ((procedure? x) (form "a procedure: ") x)
          ((number? x)    (form "a number: ") x))))


;;p462
(* 7 (call/cc (lambda (x) (x (+ 3 5)))))

(define opp/cc '())

(* 7 (call/cc (lambda (x) (set! opp/cc x) (x (+ 3 5)))))

;;p462
(define branch
  (lambda (s)
    (cond ((= s 1) (call/cc (lambda (x) (* (x 7) (+ 3 5)))))
          ((= s 2) (call/cc (lambda (x) (* 7 (x (+ 3 5))))))
          ((= s 3) (call/cc (lambda (x) (* 7 (+ (x 3) 5)))))
          (else 'again))))

;;p472
(define swap-test
  (lambda (a b)
    (let* ((dummy a) (a b)(b dummy))
      (** a b))))


;;p473
(define adjust-of
  (lambda (x)
    (let ((digit 1000)
          (slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))

;;p478
(define iota
  (lambda (max . opt)
    (let* ((min (if (null? opt) 1 (car opt)))
           (step (if (or (null? opt) (null? (cdr opt)))
                     1 (cadr opt)))
           (dummy max)
           (max (if (null? opt) max min))
           (min (if (null? opt) min dummy)))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)))))))


;;p502
(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst)
           (append (flatten (car lst))
                   (flatten (cdr lst))))
          (else (list lst)))))


;;p518
(define del-obj
  (lambda (lst obj)
    (call/cc
      (lambda (k)
        (cond ((null? lst) '())
              ((equal? (car lst) obj) (k (cdr lst)))
              (else (cons (car lst)
                          (del-obj (cdr lst) obj))))))))

(define del-obj1
  (lambda (lst obj)
    (cond ((null? lst) '())
          ((equal? (car lst) obj)(cdr lst))
          (else (cons (car lst)
                      (del-obj1 (cdr lst) obj))))))




;;p518
(define (permutation lst)
  (if (null? lst)
      (list '())
      (apply append
             (map (lambda (i)
                    (map (lambda (j) (cons i j))
                         (permutation (del-obj lst i))))
                  lst))))

;;p491
(define sq
  (lambda (x) (* x x)))

;;p615
(define (/@zero? x y)
  (zero? (/@ x y)))


;;p616
(define (minid-of n)
  (define (next k)
    (if (= k 2) 3 (+ k 2)))
  (let ((i 2))
    (let loop ((i i))
      (cond ((< n (sq i)) n)
            ((/@zero? n i) i)
            (else (loop (next i)))))))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (minid-of n))))

(define (factp-of n)
  (let loop ((i n) (tmp '()))
    (let ((div (minid-of i)))
      (if (= i div)
          (reverse (cons i tmp))
          (loop (/ i div) (cons div tmp))))))

(define (wall lst)
  (define pivot?
    (lambda (x) (lambda (y) (= x y))))
  (let loop ((lst1 '()) (lst2 lst))
    (if (null? lst2)
        (reverse lst1)
        (loop
          (cons (filter (pivot? (car lst2)) lst2) lst1)
          (remove (pivot? (car lst2)) lst2) ))))


(define (factorize-of n)
  (fact-exp (wall (factp-of n))))

;;p619
(define (tau-of n)
  (define (fact-list lst)
    (if (null? lst)
        '()
        (cons (+ (cadar lst) 1)
              (fact-list (cdr lst)))))
  (if (= n 1)
      1
      (apply * (fact-list (factorize-of n)))))

;;p594

(define (memoize proc)
  (let ((table '()))
    (lambda x (let ((stock (assoc x table)))
                (if stock
                    (cdr stock)
                    (let ((data (apply proc x)))
                      (set! table
                            (cons (cons x data) table)) data))))))


(define fib-memo
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib-memo (- n 1))
                            (fib-memo (- n 2))))))))

;;p581
(define quine
  (lambda (x) (list x (list (quote quote) x))))

;;p515
(define flatmap
  (lambda (proc lst)
    (apply append (map proc lst))))

;;; (flatmap (lambda (i) (map (lambda (j) (list i j)) '(1 2))) '(a b))
;;; ((a 1) (a 2) (b 1) (b 2))

;;p516
(define (double n)
  (apply append
         (map (lambda (i)
                (map (lambda (j) (list i j))
                     (iota (- i 1)) ))
              (iota n))))

(define (triple n)
  (apply append
         (map (lambda (i)
                (map (lambda (j)
                       (list (sq+ i j) i j))
                     (iota (- i 1)) ))
              (iota n))))

(define dismap
  (lambda (proc dlst)
    (map (lambda (x) (map proc x)) dlst)))


;;p511
(define map-unit
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
              (map-unit proc (cdr lst))))))

(define map-mult
  (lambda (proc rest)
    (if (null? (car rest))
        '()
        (cons (apply proc (map-unit car rest))
              (apply my-map proc (map-unit cdr rest))))))

(define my-map
  (lambda (proc . rest)
    (if (null? (cdr rest))
        (map-unit proc (car rest))
        (map-mult proc rest))))


;;p466
(define product
  (lambda (lst)
    (call/cc (lambda (k)
               (cond ((null? lst) 1)
                     ((= (car lst) 0) (k 0))
                     (else (* (car lst) (product (cdr lst)))))))))




(define sum
  (lambda (initial final body)
    (if (> initial final)
        0
        (+ (body initial)
           (sum (++ initial) final body)))))

(define product
  (lambda (initial final body)
    (if (> initial final)
        1
        (* (body initial)
           (product (++ initial) final body)))))


;;p785
(define-syntax s-cons
  (syntax-rules ()
    ((_ x y)(cons x (delay y)))))

(define (int-from n)
  (s-cons n (int-from (+ n 1))))

(define numbers* (int-from 1))

(define (s-car stm) (car stm))
(define (s-cdr stm) (force (cdr stm)))


