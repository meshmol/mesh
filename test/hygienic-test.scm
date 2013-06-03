
(import (normal test)
        (normal compile)
        (normal debug))

(debug #t)
(trace comp)

(define-syntax foo
  (syntax-rules ()
    ((_ (x . y))
     (list x y))))

(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(define-syntax my-and
  (syntax-rules ()
    ((_) #t)
    ((_ e) e)
    ((_ e1 e2 ...)
     (if e1
	 (my-and e2 ...)
	 #f))))

(define-syntax arithmetic-if
  (syntax-rules ()
    ((_ test neg zero pos)
     (let ((var test))
       (cond ((< var 0) neg)
             ((= var 0) zero)
             (else pos))))))


(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
         b1 ...
         (loop (+ i 1)))))))

(define-syntax incf
  (syntax-rules ()
    ((_ x) (begin (set! x (+ x 1)) x))
    ((_ x i) (begin (set! x (+ x i)) x))))

(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((value a))
       (set! a b)
       (set! b value)))))

(define b 2)

(define-syntax settest
  (syntax-rules ()
    ((_) (set! b 1))))

(let ((b 100))
  (display b)
  (newline)
  (settest)
  (display b))


(define-syntax foo
  (syntax-rules (lit)
    ((_ (x . y)) (list x y))
    ((_ lit x) (list x x))))


(define-syntax my-if
  (syntax-rules (then else)
    ((_ test then e1 else e2) (if test e1 e2))
    ((_ test then e1) (if test e1 #f))
    ((_ test else e1) (if test #f e1))))

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

(define (fact n)
  (my-cond ((= n 0) 1)
           (else (* n (fact (- n 1))))))

(define-syntax dot-pattern
  (syntax-rules ()
    ((_ . x) 'x)))

(test* "(dot-pattern)" '() (dot-pattern))

(define-syntax vec-pat
  (syntax-rules ()
    ((_ #(x y z)) #(z y x))))

(test* "(vec-pat #(1 2 3))" #(3 2 1) (vec-pat #(1 2 3)))

(define-syntax vector-pat2
  (syntax-rules ()
    ((_ #(x y z)) (list x y z))))

(test* "(vector-pattern #(1 2 3))"
       '(1 2 3) (vector-pat2 #(1 2 3)))



;;(let ((a 1)) (define-syntax b (syntax-rules () ((_) a))) (b))

(define (foo)
  (let ((a 1)(c 3)) 
    (let-syntax ((b (syntax-rules () ((_) a))))
      (let ((a 2)(d 4))
        (let-syntax ((f (syntax-rules () ((_) d))))
          (+ (b) (f)))))))
