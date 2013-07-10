;;Normal test
;;R5RS
(import (mesh test))
(import (scheme inexact))

;;等価
(test* "(eqv? 'a 'a)" #t (eqv? 'a 'a))
(test* "(eqv? 'a 'b)" #f (eqv? 'a 'b))
(test* "(eqv? 2 2)" #t (eqv? 2 2))
(test* "(eqv? '() '())" #t (eqv? '() '()))
(test* "(eqv? 100000000 100000000)" #t (eqv? 100000000 100000000))
(test* "(eqv? (cons 1 2) (cons 1 2))" #f (eqv? (cons 1 2) (cons 1 2)))
(test* "(eqv? (lambda () 1) (lambda () 2))" #f (eqv? (lambda () 1) (lambda () 2)))
(test* "(eqv? #f 'nil)" #f (eqv? #f 'nil))
(test* "(eqv? #\c #\c)" #t (eqv? #\c #\c))
(test* "(equal? 'a 'b)" #f (equal? 'a 'b))
(test* "(equal? 'a 'a)" #t (equal? 'a 'a))
(test* "(equal? '(1 2) '(1 2))" #t (equal? '(1 2) '(1 2)))
(test* "(equal? '(1) '(1 2))" #f (equal? '(1) '(1 2)))

;;数値
(test* "(complex? 3+4i)" #t (complex? 3+4i))
(test* "(complex? 3)" #t (complex? 3))
(test* "(real? 3)" #t (real? 3))
(test* "(real? -2.5+0.0i)" #t (real? -2.5+0.0i))
(test* "(rational? 6/10)" #t (rational? 6/10))
(test* "(rational? 6/3)" #t (rational? 6/3))
(test* "(integer? 3+0i)" #t (integer? 3+0i))
(test* "(integer? 3.0)" #t (integer? 3.0))
(test* "(integer? 8/4)" #t (integer? 8/4))

(test* "(< 1 2 3)" #t (< 1 2 3))

(test* "(exact? 1)" #t (exact? 1))
(test* "(exact? 1234567890)" #t (exact? 1234567890))

(test* "(zero? 0)" #t (zero? 0))
(test* "(zero? 0.0)" #t (zero? 0))
(test* "(zero? 0+0i)" #t (zero? 0+0i))
(test* "(zero? 0+3i)" #f (zero? 0+3i))
(test* "(positive? 0)" #f (positive? 0))
(test* "(positive? 0.1)" #t (positive? 0.1))
(test* "(positive? 2/3)" #t (positive? 2/3))
(test* "(negative? 0)" #f (negative? 0))
(test* "(negative? -1)" #t (negative? -1))
(test* "(odd? -1)" #t (odd? -1))
(test* "(odd? -2)" #f (odd? -2))
(test* "(odd? 12345678901)" #t (odd? 12345678901))
(test* "(max 3 4)" 4 (max 3 4))
(test* "(max 3.9 4)" 4.0 (max 3.9 4))
(test* "(+ 3 4)" 7 (+ 3 4))
(test* "(+ 3)" 3 (+ 3))
(test* "(+)" 0 (+))
(test* "(* 4)" 4 (* 4))
(test* "(*)" 1 (*))
(test* "(modulo 13 4)" 1 (modulo 13 4))
(test* "(remainder 13 4)" 1 (remainder 13 4))
(test* "(modulo -13 4)" 3 (modulo -13 4))
(test* "(remainder -13 4)" -1 (remainder -13 4))
(test* "(modulo 13 -4)" -3 (modulo 13 -4))
(test* "(remainder 13 -4)" 1 (remainder 13 -4))
(test* "(modulo -13 -4)" -1 (modulo -13 -4))





;;真偽値
(test* "(not #t)" #f (not #t))
(test* "(not 3)" #f (not 3))


;;制御機能
(test* "(procedure? car)" #t (procedure? car))
(test* "(procedure? 'car)" #f (procedure? 'car))
(test* "(procedure? (lambda (x) (* x x)))" #t (procedure? (lambda (x) (* x x))))
(test* "(call/cc procedure?)" #t (call/cc procedure?))
(test* "(apply + (list 3 4))" 7 (apply + (list 3 4)))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(test* "((compose sqrt *) 12 75)" 30 ((compose sqrt *) 12 75))
(test* "(map cadr '((a b) (d e) (g h)))" '(b e h) (map cadr '((a b) (d e) (g h))))
(test* "(map (lambda (n) (expt n n)) '(1 2 3 4 5))" '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
(test* "(map + '(1 2 3) '(4 5 6))" '(5 7 9) (map + '(1 2 3) '(4 5 6)))
;;; 

;;リスト
;;(test* "list" '(a b c . d) '(a . (b . (c . d))))

(define x (list 'a 'b 'c))
(define y x)

(test* "y" '(a b c) y)
(test* "(list? y)" #t (list? y))
(set-cdr! x 4)
(test* "x" '(a . 4) x)
(test* "eqv? x y" #t (eqv? x y))


(test* "(pair? '(a . b))" #t (pair? '(a . b)))
(test* "(pair? '(a b c))" #t (pair? '(a b c)))
(test* "(pair? '())" #f (pair? '()))

(test* "(memq 'a '(a b c))" '(a b c) (memq 'a '(a b c)))
(test* "(memq 'b '(a b c))" '(b c) (memq 'b '(a b c)))
(test* "(memq 'a '(b c d))" #f (memq 'a '(b c d)))
(test* "(memq (list 'a) '(b (a) c))" #f (memq (list 'a) '(b (a) c)))
(test* "(member (list 'a) '(b (a) c))" '((a) c) (member (list 'a) '(b (a) c)))
(test* "(memq 101 '(100 101 102))" '(101 102) (memq 101 '(100 101 102)))
(test* "(memv 101 '(100 101 102))" '(101 102) (memv 101 '(100 101 102)))

(define e '((a 1)(b 2)(c 3)))
(test* "(assq 'a e)" '(a 1) (assq 'a e))

