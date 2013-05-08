;;モジュールのテスト
(define-library (scheme base)
  (export = + - * / <))

(define-library (normal user)
  (export let letrec let* cond and or do))

(define-library (test foo)
  (export fact woo my-or)
  (import (scheme base))
  (import (normal user))
  (begin
    (define (fact n)
      (if (= n 0)
          1
          (* n (fact (- n 1)))))
    (define (woo x)
      (cond ((= x 1) #t)
            ((= x 2) #f)
            (else 1)))
    (define (my-or x)
      (or x))))

(import (test foo))

(define-library (test macro)
  (export ttt nil! foo)
  (import (scheme base))
  (begin
    (define-macro ttt
      (lambda (expr)
        `(+ ,expr)))
    (define-syntax nil!
      (syntax-rules ()
        ((_ x)
         (set! x '()))))
    (define (foo x) (+ x x))))















