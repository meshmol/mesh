;;教育用LISPインタープリタ　Ver0.3 for Meiji-Scheme
;;「まったくはじめての人のためのScheme」の後編で作ったPure Lispを
;;Meshで動くように手直ししたものです。

(import (mesh system))

(define *env* '((T T)(NIL NIL)))


(define (lisp)
  (init)
  (let loop ((e (read)))
    (if (equal? e '(exit))
        'good-bye
        (begin (print (ed-eval e *env*))
               (prompt)
               (loop (read))))))

(define (init)
  (begin 
    (display "Pure LISP for education")(newline)
    (prompt)))

(define (print s)
  (display s)
  (newline))

(define (prompt)
  (display "L> ")
  (flush))


(define (pair x y)
  (cond ((and (null? x)(null? y)) '())
        ((and (not (atom? x))(not (atom? y))) 
         (cons (list (car x) (car y))
               (pair (cdr x) (cdr y))))))


(define (ed-assoc x y)
  (cond ((null? y) 'NIL) 
        ((eq? (caar y) x) (cadar y))
        (else (ed-assoc x (cdr y)))))


(define (ed-eval e a)
  (cond ((atom? e)
         (cond ((number? e) e)
               ((string? e) e)
               ((symbol? e) (ed-assoc e a))))
        ((atom? (car e))
         (cond ((special-form? (car e)) (special-form e a))
               ((function? (car e)) (apply (function (car e)) (evlis (cdr e) a)))
               (else (ed-eval (cons (ed-assoc (car e) a)
                                    (evlis (cdr e) a))
                              a))))
        ((eq? (caar e) 'lambda) (ed-eval (caddar e)
                                         (append (pair (cadar e)
                                                       (evlis (cdr e) a))
                                                       a))))
        )


(define (special-form? e)
  (member e '(quote set! if define cond)))

(define (special-form e a)
  (cond ((eq? (car e) 'quote) (cadr e))
        ((eq? (car e) 'cond) (evcon (cdr e) a))
        ((eq? (car e) 'set!) (ed-set! (cadr e) (ed-eval (caddr e) a)))
        ((eq? (car e) 'define) (ed-define! (cadr e) (caddr e)))
        ((eq? (car e) 'if) (evif (cdr e) a))))

(define (function? e)
  (assoc e func-symbol))

(define (function e)
  (eval (cdr (assoc e func-symbol)))) 

(define func-symbol
  '((+ . +)
    (- . -)
    (* . *)
    (/ . /)
    (sin . sin)
    (cos . cos)
    (tan . tan)
    (asin . asin)
    (acos . acos)
    (atan . atan)
    (car . car)
    (cdr . ed-cdr)
    (cons . cons)
    (atom? . ed-atom)
    (eq? . ed-eq)))


(define (evcon c a)
  (cond ((not (eq? (ed-eval (caar c) a) 'NIL)) (ed-eval (cadar c) a))
        (else (evcon (cdr c) a))))

(define (evif c a)
  (if (not (eq? (ed-eval (car c) a) 'NIL))
      (ed-eval (cadr c) a)
      (ed-eval (caddr c) a)))

(define (evlis m a)
  (cond ((null? m) '())
        (else (cons (ed-eval (car m) a)
                    (evlis (cdr m) a)))))


(define (ed-cdr m)
  (cond ((null? m) 'NIL)
        ((null? (cdr m))'NIL)
        (else (cdr m))))
 
(define (ed-atom x)
  (if (atom? x)
      'T
      'NIL))

(define (ed-eq x y)
  (if (eq? x y)
      'T
      'NIL))

(define (ed-set! x y)
  (set! *env* (cons (list x y) *env*))
  y)

(define (ed-define! x y)
  (cond ((symbol? x) (ed-set! x y))
        ((pair? x) (ed-set! (car x)
                           (list 'lambda
                                 (cdr x))))))


