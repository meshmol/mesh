;;継続テストコード

;;紫藤さんの解説コードより,一部改編

(import (mesh test))

(define (find-leaf obj tree)
  (call/cc
    (lambda (cc)
      (letrec ((iter
                 (lambda (tree)
                   (cond
                     ((null?  tree) #f)
                     ((pair? tree)
                      (iter (car tree))
                      (iter (cdr tree)))
                     (else
                       (if (eqv? obj tree)
                           (cc obj)
                           #t))))))
        (iter tree)))))
  

   
(test* "cont1" 9 (* 3 (call/cc (lambda (k) (+ 1 2)))))
(test* "cont2" 6 (* 3 (call/cc (lambda (k)  (+ 1 (k 2))))))
(test* "cont3" 7 (find-leaf 7 '(1 (2 3) 4 (5 (6 7)))))
(test* "cont4" #f (find-leaf 8 '(1 (2 3) 4 (5 (6 7)))))

(define cont #f)
(test* "cont5" 6 (+ 5 (call/cc (lambda (c) (begin (set! cont c) 1)))))
(test* "cont5-1" 16 (cont 11))
(test* "cont5-2" 21 (cont 16))
(test* "cont6" 65 (+ 20 30 (call/cc (lambda (cont) (+ 5 10)))))

(define cc 0)
(* 3 (call/cc (lambda (k)
                 (set! cc k)
                 (+ 1 2))))


(define cc1 0)

(define (bar)
  (call/cc 
    (lambda (c)
      (display "foo ")
      (set! cc1 c)
      (display "bar "))))

(define (loop)
  (call/cc 
    (lambda (esc)
      (letrec ((iter (lambda (s)
                       (display s) 
                       (if (eq? s 'end)
                           (esc #t)
                           (iter (read))))))
        (iter (read))))))


(define cont #f)
(define a
  (lambda ()
    (if (call/cc (lambda (c) (set! cont c) #f))
        (display 1)
        (display 0))
    (display 9)))

(define foo
  (lambda ()
    (let ((a (call/cc (lambda (c) c))))
      (let ((b (call/cc (lambda (c) c))))
        (a not)))))
      

(define product
  (lambda (ls)
    (call/cc
      (lambda (break)
        (let f ((ls ls))
          (cond ((null? ls) 1)
                ((= (car ls) 0) (break 0))
                (else (* (car ls)(f (cdr ls))))))))))

(((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!")

(let ((x (call/cc (lambda (k) k))))
  (x (lambda (ignore) "hi")))

(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))

(define cc #f)

(define (foo)
  (display "a\n")
  (call/cc (lambda (c) (set! cc c)))
  (display "b\n")
  (display "c\n"))
