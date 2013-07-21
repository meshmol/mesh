;;遅延評価のテスト

(import (scheme lazy))

(force (delay (+ 1 2)))

(let ((p (delay (+ 1 2))))
  (list (force p) (force p)))

(define integers
  (letrec ((next
             (lambda (n)
               (delay (cons n (next (+ n 1)))))))
    (next 0)))

(define head
  (lambda (stream) (car (force stream))))

(define tail
  (lambda (stream) (cdr (force stream))))

(head (tail (tail integers)))
          