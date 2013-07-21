;;syntax-rules テストコード
;;;紫藤さんの「Scheme入門15構文の定義（マクロ）」より

(import (mesh test))


(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

(define-syntax unless
  (syntax-rules ()
    ((_ pred b1 ...)
     (if (not pred)
	 (begin
	   b1 ...)))))


(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (<= i to)
	  b1 ...
	  (loop (+ i 1)))))
    ((_ (i from to step) b1 ...)
     (let loop ((i from))
       (when (<= i to)
	  b1 ...
	  (loop (+ i step)))))))


(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))
    ((_ x i) (begin (set! x (+ x i)) x))))


(define-syntax decf
  (syntax-rules ()
    ((_ x) (begin (set! x (- x 1)) x))
    ((_ x i) (begin (set! x (- x i)) x))))



(define-syntax my-and
  (syntax-rules ()
    ((_) #t)
    ((_ e) e)
    ((_ e1 e2 ...)
     (if e1
	 (my-and e2 ...)
	 #f))))

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...)
     (let ((t e1))
       (if t t (my-or e2 ...))))))

(define-syntax my-cond
  (syntax-rules (else)
    ((_ (else e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...))
     (when e1 e2 ...))
    ((_ (e1 e2 ...) c1 ...)
     (if e1 
	 (begin e2 ...)
	 (cond c1 ...)))))


(define-syntax my-let*
  (syntax-rules ()
    ((_ ((p v)) b ...)
     (let ((p v)) b ...))
    ((_ ((p1 v1) (p2 v2) ...) b ...)
     (let ((p1 v1))
       (my-let* ((p2 v2) ...)
		b ...)))))

;;以下Wiliki、Shiroさんが示していたコード。

(define-syntax arithmetic-if 
  (syntax-rules ()
    ((arithmetic-if test neg-form zero-form pos-form)
     (let ((var test))
       (cond ((< var 0) neg-form)
             ((= var 0) zero-form)
             (else      pos-form))))
    ((arithmetic-if test neg-form zero-form)
     (arithmetic-if test neg-form zero-form #f))
    ((arithmetic-if test neg-form)
     (arithmetic-if test neg-form #f #f))))

(define-syntax foo1
  (syntax-rules ()
    ((foo1 x ...) (list x ...))))

(define-syntax foo2
  (syntax-rules ()
    ((foo2 . x) (list . x))))

(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((v a))
       (set! a b)
       (set! b v)))))



(define x 1)
(nil! x)
(test* "(nil! x)" '() x)
(test* "(when (null? x) 'true)" 'true (when (null? x) 'true))
(test* "(unless (not (null? x)) 'true)" 'true (unless (not (null? x)) 'true))
(test* "(let ((x 0)(y 0))(while (<= i 100) (set! x (+ x i))(incf i)) x)" 5050
        (let ((x 0)(i 0))(while (<= i 100) (set! x (+ x i))(incf i)) x))


(test* "(let ((x 0)) (for (j 1 10) (set! x (+ x j))) x)" 55
        (let ((x 0)) (for (j 1 10) (set! x (+ x j))) x))
(test* "(my-or #f #f #f)" #f (my-or #f #f #f))
(test* "(my-and (integer? 1)(real? 1)(complex? 1))" #t (my-and (integer? 1)(real? 1)(complex? 1)))
(define (fact n)
  (my-cond ((= n 0) 1)
           (else (* n (fact (- n 1))))))
(test* "(fact 10)" 3628800 (fact 10))
(test* "(let ((var 3)) (arithmetic-if -1 (list var)))" '(3) (let ((var 3)) (arithmetic-if -1 (list var))))
;;(test* "(my-let* ((x 1)(y (+ x 1))) (+ x y))" 3 (my-let* ((x 1)(y (+ x 1))) (+ x y)))
(define a 1)
(define v 2)
(test* "(let ()(swap! a v) (list a v))" '(2 1) (let ()(swap! a v) (list a v)))

(define-syntax setvar
  (syntax-rules ()
    ((_ var x) (set! var x))))

(define-syntax settest
  (syntax-rules ()
    ((_) (set! b 1))))

(define b 0)
(let ((b 10)) (settest))
(test* "(let ((b 10)) (settest))" 1 b)


(define-syntax hoge
  (syntax-rules()
    ((_ b ...)(list b ...))))

(test* "(hoge)" '() (hoge))


(define-syntax foo
  (syntax-rules()
    ((_ a)
     (let ((a #t)) a))))

(define-syntax bar
  (syntax-rules()
    ((_)(foo b))))

(test* "(bar)" #t (bar))


(define-syntax hoge-helper
  (syntax-rules ()
    ((_ (n ...) () (temp ...))
     (let ((temp n) ...)
       (list temp ...)))
    ((_ (n ...) (r0 r1 ...) (temp ...))
     (hoge-helper (n ...) (r1 ...) (newtemp temp ...)))))
 
(define-syntax hoge
  (syntax-rules ()
    ((_ num ...)
     (hoge-helper (num ...) (num ...) ()))))
  
(test* "(hoge 1 2 3)" '(1 2 3) (hoge 1 2 3))


(define-syntax dot-pattern
  (syntax-rules ()
    ((_ . x) 'x)))

(test* "(dot-pattern)" '() (dot-pattern))

(define-syntax vec-pat
  (syntax-rules ()
    ((_ #(x y z)) (list x y z))))

(define-syntax vector-pattern
  (syntax-rules ()
    ((_ #(x y z)) (list x y z))))

(test* "(vector-pattern #(1 2 3))"
       '(1 2 3) (vector-pattern #(1 2 3)))

(define-syntax define-definer
  (syntax-rules ()
    ((_ k f)
     (define-syntax k
       (syntax-rules ()
         ((_ v)
          (define v f)))))))
 
(define-definer define-nil '())
(define-nil nil)
 
(test* "nil" '() nil)


