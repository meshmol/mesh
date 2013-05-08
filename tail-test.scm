;;末尾再帰最適化テスト
;;CALLJ命令によりループと同様になっていないといけない。

(define lastl
  (lambda (l)
    (if (null? (cdr l))
        (car l)
        (lastl (cdr l)))))

(define (fact-tail n)
  (fact-tail2 n 1))

(define (fact-tail2 n m)
  (if (= n 0)
      m
      (fact-tail2 (- n 1) (* m n))))

(define (sigma-tail n)
  (sigma-tail2 n 0))

(define (sigma-tail2 n m)
  (if (= n 0)
      m
      (sigma-tail2 (- n 1) (+ m n))))

(define (sigma-rec n)
  (if (= n 0)
      0
      (+ n (sigma-rec (- n 1)))))

(define (foo x)
  (define (boo x)
    (+ x x))
  (define (uoo x)
    (* x x))
  (define (goo x)
    (/ x x))
  (/ (boo x) (uoo x)))

(define (fib-tail n)
  (fib-tail2 n 2 1 0))

(define (fib-tail2 n x n1 n2)
    (if (= x n)
        (+ n1 n2)
        (fib-tail2 n (+ x 1) (+ n1 n2) n1)))



(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))     ; *
    (iter n n)))

