;;アイディアスケッチ

(import (normal system)
        (normal debug))

;(debug #t)
;(trace match1 match-subpat combine)

(define (expand-subpat p n)
  (if (not (has-ellipsis? p))
      (error "template has no ellipsis" p)
      (expand-subpat1 p '() n)))

(define (expand-subpat1 p subp n)
  (let ((val (subst-from-identifier p n)))
    (if (not val)
        (reverse subp)
        (expand-subpat1 p (cons val subp) n)))) 


(define (has-ellipsis? p)
  (cond ((null? p) #f)
        ((identifier-ellipsis? p) #t)
        ((atom? p) #f)
        ((vector? p) #f)
        (else
          (or (has-ellipsis? (car p))
              (has-ellipsis? (cdr p))))))


(define (combine x y)
  (cond ((null? y) (map (lambda (z) (cons (identifier-ellipsis! (symbol->identifier (car z)))
                                          (list (cdr z)))) x))
        ((null? x) y)
        (else (cons (cons (caar x)(cons (cdar x)(cdar y)))
                    (combine (cdr x) (cdr y))))))


(define (subst-from-identifier p n)
  (cond ((null? p) '())
        ((and (identifier-variable? p) (> n 0))
         (error "syntax-rules illegal template" p))
        ((and (identifier-ellipsis? p) (null? (identifier-bound p)) (> n 0))
         #f)
        ((and (identifier-ellipsis? p) (> n 0))
         (let ((val (identifier-bound p)))
           (identifier-bind! p (cdr val))
           (car val)))
        ((identifier-free? p) (identifier->symbol p))
        ((identifier? p) (identifier-bound p))
        ((atom? p) p)
        ((vector? p)
         (list->vector (subst-from-identifier (vector->list p) n)))
        ;;(p ...)
        ((and (= (length p) 2)(ellipsis? (list-take p 2)))
         (identifier-bound (car p)))
        ;;(p ... n)
        ((and (> (length p) 2)(ellipsis? (list-take p 2)))
         (append (identifier-bound (car p))
                 (subst-from-identifier (cddr p) n)))
        ((ellipsises? p) (expand-subpat (car p) (+ n 1)))
        ((and (eqv? (car p) '...)(eqv? (cadr p) '...)(= (length p) 2))
         (cadr p))
        ((eqv? (car p) '...) (cdr p))
        (else (let ((r1 (subst-from-identifier (car p) n))
                    (r2 (subst-from-identifier (cdr p) n)))
                (if (and r1 r2)
                    (cons r1 r2)
                    #f)))))


(define (match-subpat p f lits vars)
  (if (null? f)
      vars
      (let ((val (match1 p (car f) lits '())))
        (if (not val)
            #f
            (match-subpat p (cdr f) lits (combine val vars))))))

(define (match p f lits)
  (match1 p f lits '()))

(define (match1 p f lits vars)
  ;(display p)(newline)(display f)(newline)
  (cond ((and (null? p) (null? f)) vars)
        ((and (symbol? p) (memv p lits) (not(eqv? p f))) #f)
        ((and (symbol? f) (memv f lits) (not(eqv? p f))) #f)
        ((symbol? p) (cons (cons (identifier-variable! (symbol->identifier p)) f) vars))
        ((ellipsis? p) (match1 (cddr p)
                               (list-take-right f (length (cddr p)))
                               lits
                               (cons (cons (identifier-ellipsis!(symbol->identifier (car p))) 
                                           (list-take f (- (length f) (length (cddr p))))) vars)))
        ((ellipsises? p) (match-subpat (car p) f lits '()))
        ((and (vector? p) (vector? f)) 
         (match1 (vector->list p) (vector->list f) lits vars))
        ((equal? p f) vars)
        ((and (pair? p)(pair? f))
         (let ((r1 (match1 (car p) (car f) lits vars))
               (r2 (match1 (cdr p) (cdr f) lits vars)))
                (if (and r1 r2)
                    (append r1 r2)
                    #f)))
        (else #f)))
        
        
;;例((1 2)(3 4)(5 6)) -> ((1 3 5)(2 4 6)) 
(define (transpose ls) 
  (define (iter m n) 
    (if (= m n) 
        '() 
        (cons (map (lambda (x) (list-ref x m)) ls) 
              (iter (+ m 1) n)))) 
  (iter 0 (length (car ls)))) 

;;省略子
(define (ellipsis? x)
  (and (list? x)
       (>= (length x) 2)
       (or (identifier? (car x)) (symbol? (car x)))
       (eqv? (cadr x) '...)))

;;複合省略子
(define (ellipsises? x)
  (and (list? x)
       (>= (length x) 2)
       (list? (car x))
       (eqv? (cadr x) '...)))

;;ベクタ省略子
(define (vec-ellipsis? x)
  (and (vector? x)
       (eqv? (vector-ref x (- (vector-length x) 1)) '...)))


(define (list-take ls n)
  (if (= n 0)
      '()
      (cons (car ls) (list-take (cdr ls) (- n 1)))))

(define (list-take-right ls n)
  (list-drop ls (- (length ls) n)))

(define (list-drop ls n)
  (if (= n 0)
      ls
      (list-drop (cdr ls) (- n 1))))


(define (fail? x)
  (eq? x 'fail))


(define (subst-pattern-vars x pat)
  (cond ((null? x) '())
        ((and (identifier? x)(assv x pat))
         (let ((val (assv x pat)))
           (if (identifier-variable? (car val))
               (identifier-variable! (identifier-bind! x (cdr val)))
               (identifier-ellipsis! (identifier-bind! x (cdr val))))))
        ((atom? x) x)
        ((vector? x)
         (list->vector (subst-pattern-vars (vector->list x) pat)))
        (else (cons (subst-pattern-vars (car x) pat)
                    (subst-pattern-vars (cdr x) pat)))))



(define (subst-let-vars x)
  (subst-let-vars1 x (scan-let-vars x '())))

(define (subst-let-vars1 x a-list)
  (cond ((null? x) '())
        ((and (symbol? x)(assv x a-list))
         (cdr (assv x a-list)))
        ((atom? x) x)
        ((vector? x) x)
        (else (cons (subst-let-vars1 (car x) a-list)
                    (subst-let-vars1 (cdr x) a-list)))))


(define (scan-let-vars x v-list)
  (cond ((null? x) (map (lambda (x) (cons x (gensym))) v-list))
        ((atom? x) '())
        ((vector? x) '())
        ((and (lambda? x)(atom? (cadr x)))
         (scan-let-vars (cddr x) (cons (cadr x) v-list)))
        ((and (lambda? x)(list? (cadr x))(not (null? (cadr x))))
         (scan-let-vars (cddr x) (append (cadr x) v-list)))
        ((and (lambda? x)(pair? (cadr x))(not (null? (cadr x))))
         (scan-let-vars (caddr x) (pair->list (cadr x))))
        ((and (let? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (let*? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (letrec? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (named-let? x)(not (null? (caddr x))))
         (scan-let-vars (cdddr x) 
                        (append (car (transpose (caddr x))) 
                                (cons (cadr x) v-list))))
        (else (scan-let-vars (cdr x) v-list))))

(define (lambda? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'lambda)))

(define (let? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'let) (not (identifier? (cadr x)))))

(define (named-let? x)
  (and (list? x) (>= (length x) 4) (eqv? (car x) 'let) (identifier? (cadr x))))

(define (letrec? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'letrec)))

(define (let*? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'let*)))





(define (expand-template x vars comp-env lits)
  (let ((a #f)(b #f)(c #f)(d #f))
    (set! a (subst-to-identifier x comp-env lits))
    ;(display a)(newline)
    (set! b (subst-pattern-vars a vars))
    ;(display b)(newline)
    (set! c (subst-from-identifier b 0))
    ;(display c)(newline)
    (set! d (subst-let-vars c))
    ;(display d)(newline)
    d))
    


(define (subst-to-identifier x env lits)
  (cond ((null? x) '())
        ((and (symbol? x)(local-bound? x env))
         (make-syntactic-closure env '() x))
        ((and (symbol? x)(global-bound? x))
         (identifier-bind! (symbol->identifier x) x))
        ((and (symbol? x)(memv x lits))
         (identifier-bind! (symbol->identifier x) x))
        ((symbol? x)
         (symbol->identifier x))
        ((atom? x) x)
        ((vector? x)
         (list->vector (subst-to-identifier (vector->list x) env lits)))
        (else (cons (subst-to-identifier (car x) env lits)
                    (subst-to-identifier (cdr x) env lits)))))

(define (local-bound? x env)
  (cond ((null? env) #f)
        ((member x (car env)) #t)
        (else (local-bound? x (cdr env)))))


(define (expand x y lits comp-env)
  (let* ((pat (caar x))
         (temp (cadar x))
         (vars (match pat y lits)))
    (cond (vars (expand-template temp vars comp-env lits))
          ((null? (cdr x)) (error "syntax-rules match fail " y))
          (else (expand (cdr x) y lits comp-env)))))

  ;test
;;(match '(_ (x y)...) '(_ (1 2)(3 4)) '())
(expand '(((_ x y)(list x y))) '(foo 1 2) '() '())
(expand '(((_ (x ...)(y ...))(let ((x y)...) (list x ... ) ))) '(foo (a b c)(1 2 3)) '() '())