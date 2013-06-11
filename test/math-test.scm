;;数学関数のテスト
(import (normal test)
        (scheme inexact)
        (scheme complex)
        (scheme time))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-tail n)
  (define (iter x n1 n2)
    (if (= x n)
        (+ n1 n2)
        (iter (+ x 1) (+ n1 n2) n1)))
  (iter 2 1 0))

(define (sigma n)
  (if (= n 1)
      1
      (+ n (sigma (- n 1)))))

(define (tarai x y z)
  (if (<= x y)
      y
      (tarai (tarai (- x 1) y z)
             (tarai (- y 1) z x)
             (tarai (- z 1) x y))))



(define (fact n) 
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact-tail n)
  (fact-rec n n))

(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
        (fact-rec m (* p m)))))

(define (collatz n)
  (display n)(display '->)
  (cond ((= n 1) #t)
        ((even? n) (collatz (quotient n 2)))
        ((odd? n) (collatz (+ (* n 3) 1)))
        (else #f)))

(define ack
  (lambda (m n)
    (cond ((= m 0) (+ n 1))
          ((= n 0) (ack (- m 1) 1))
          (else (ack (- m 1) (ack m (- n 1)))))))


(test* "(sin 1+2i)" 3.165778513216168+1.959601041421606i (sin 1+2i))
(test* "(sin 3+2.5i)" 0.8653887407955783-5.989657039127781i (sin 3+2.5i))
(test* "(cos 1+2i)" 2.0327230070196656-3.0518977991518002i (cos 1+2i))
(test* "(acos 1+2i)" 1.1437177404024206-1.5285709194809978i (acos 1+2i))
(test* "(tan 1+2i)" 0.033812826079896691+1.0147936161466335i (tan 1+2i))
(test* "(atan 1+2i)" 1.3389725222944935+0.40235947810852513i (atan 1+2i))

(test* "(real-part 3+4i)" 3.0 (real-part 3+4i))
(test* "(real-part -2.3+0.7i)" -2.3 (real-part -2.3+0.7i))
(test* "(real-part 17.2)" 17.2 (real-part 17.2))
(test* "(real-part -17/100)" -17/100 (real-part -17/100))
(test* "(make-rectangular -2 7)" -2+7i (make-rectangular -2 7))
(test* "(make-polar 1 (asin -1))" 0+1.0i (make-polar 1 (asin -1)))

(test* "(exp 0)" 1 (exp 0))
(test* "(exp 1)" 2.718281828459045 (exp 1))
(test* "(exp -0.5)" 0.6065306597126334 (exp -0.5))
(test* "(exp -1/2)" 0.6065306597126334 (exp -1/2))

(test* "(log (exp 1))" 1.0 (log (exp 1)))
(test* "(/ (log 100) (log 10))" 2.0 (/ (log 100) (log 10)))

(test* "(min 4 -7 2 0 -6)" -7 (min 4 -7 2 0 -6))
(test* "(min 1/2 3/4 4/5 5/6 6/7)" 1/2 (min 1/2 3/4 4/5 5/6 6/7))
(test* "(min 1.5 1.3 -0.3 0.4 2.0 1.8)" -0.3 (min 1.5 1.3 -0.3 0.4 2.0 1.8))
(test* "(min -5 2.0)" -5 (min -5 2.0))
(test* "(min -5 -2.0)" -5 (min -5 -2.0))

(test* "(gcd)" 0 (gcd))
(test* "(gcd 34)" 34 (gcd 34))
(test* "(gcd 33.0 15.0)" 3.0 (gcd 33.0 15.0))

(test* "(modulo 9876543210 8765432)" 6666778 (modulo 9876543210 8765432))
(test* "(modulo 9876543210 -8765432)" -2098654 (modulo 9876543210 -8765432))
(test* "(modulo -9876543210 8765432)" 2098654 (modulo -9876543210 8765432))
(test* "(modulo -9876543210 -8765432)" -6666778 (modulo -9876543210 -8765432))

(test* "(floor 3)" 3 (floor 3))
(test* "(floor 3.14)" 3.0 (floor 3.14))
(test* "(floor -2.72)" -3.0 (floor -2.72))
(test* "(ceiling 3)" 3 (ceiling 3))
(test* "(ceiling 3.14)" 4.0 (ceiling 3.14))
(test* "(ceiling -2.72)" -2.0  (ceiling -2.72))

(test* "(sqrt -1)" 0.0+1.0i (sqrt -1))


;;; 
(define (deterministic-prime? n)
  (define (iter x y n)
    (cond ((> x y) #t)
          ((divisible? n x) #f)
          (else (iter (+ x 2) y n))))
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (iter 3 (ceiling (sqrt n)) n))))

(define (divisible? m n)
  (= (modulo m n) 0))


(define (prime-factors n)
  (define (iter ls p n mult)
    (cond ((null? ls) (cons (list p n) mult))
          ((eq? (car ls) p) (iter (cdr ls) p (+ n 1) mult))
          (else (iter (cdr ls) (car ls) 1 (cons (list p n) mult)))))
  (let ((ls (prime-factors2 n)))
    (iter (cdr ls) (car ls) 1 '())))




(define (prime-factors2 n)
  (define (iter p x ls z)
    (cond ((= x 1) ls)
          ((> p z) (cons x ls))
          ((divisible? x p) 
           (iter1 p (quotient x p) (cons p ls)))
          ((= p 2) (iter 3 x ls z))
          (else (iter (+ p 2) x ls z))))
  (define (iter1 p x ls)
    (if (divisible? x p)
        (iter1 p (quotient x p) (cons p ls))
        (iter p x ls (sqrt x))))
  (iter 2 n '() (sqrt n)))


