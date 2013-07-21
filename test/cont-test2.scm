;;continuation test
;; Scheme による記号処理入門 by 猪股俊光、益崎真治　共著

(import (mesh system))

(define cont1 '())

(define (cont)
  (display "foo ")
  (call/cc (lambda (exit) (set! cont1 exit)))
  (display "bar ")(display "baz "))

;;; mesh> (cont)
;;; foo bar baz #<undef>
;;; mesh> (cont1 #t)
;;; bar baz #<undef>

(define (read-eval-print)
  (call/cc
    (lambda (c)
      (letrec ((loop
                 (lambda (x)
                   (if (eq? x  'end)
                       (c 'bye)
                       (display x))
                   (newline)
                   (flush)
                   (loop (read)))))
        (loop (read))))))

;;オリジナルのc=exit だと動作不良

(define (node-expand n lst)
  (if (zero? n)
      '()
      (cons (cons n lst) (node-expand (- n 1) lst))))


(define n 8)

(define (safe? lst)
  (let ((new (car lst))
        (hlst (cdr lst)))
    (if (null? hlst)
        #t
        (safe-aux? new (+ new 1) (- new 1) hlst))))


(define (safe-aux? new up down hlst)
  (if (null? hlst)
      #t
      (let ((pos (car hlst)))
        (and (not (= pos new))
             (not (= pos up))
             (not (= pos down))
             (safe-aux? new (+ up 1) (- down 1) (cdr hlst))))))

(define (goal? x n) (= (length x) n))

(define resume '())


(define (queens-cont n)
  (call/cc (lambda (exit)
             (letrec
               ((lst (node-expand n '()))
                (x '())
                (pop (lambda () (let ((y (car lst)))
                                  (set! lst (cdr lst)) y)))
                (push (lambda (y) (set! lst (append y lst))))
                (search
                  (lambda ()
                    (set! x (pop))
                    (if (null? lst) '()
                        (begin
                          (if (safe? x)
                              (if (goal? x n)
                                  (call/cc (lambda (cont)
                                                    (set! resume (lambda () (cont #t)))
                                                    (exit x)))
                                  (push (node-expand n x)))
                              #f)
                          (search))))))
               (search)))))



;;; mesh> (queens-cont 8)
;;; (5 7 2 6 3 1 4 8)
;;; mesh> (resume)
;;; (4 7 5 2 6 1 3 8)
;;; mesh> (resume)
;;; (6 4 7 1 3 5 2 8)
;;; mesh> 

(define (increase n k)
  (if (> n 10)
      '()
      (begin (display " i:")(display n)
             (increase (+ n 1) (call/cc k)))))

(define (decrease n k)
  (if (< n 0)
      '()
      (begin (display " d:")(display n)
             (decrease (- n 1) (call/cc k)))))
