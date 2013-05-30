;;Hygienic macro


(define (match x y lits)
  (match1 x y lits '()))

(define (match1 x y lits vars)
  (cond ((and (null? x) (null? y)) vars)
        ((ellipsis? x) (match1 (cddr x)
                               (list-take-right y (length (cddr x)))
                               lits
                               (cons (cons (car x) (list-take y (- (length y) (length (cddr x))))) vars)))
        ((and (null? x) (not (null? y))) #f)
        ((and (identifier? x) (null? y)) (list (cons x y)))
        ((and (not (null? x)) (null? y)) #f)
        ((and (atom? (car x)) (memq (car x) lits) (not (eq? (car x) (car y)))) #f)
        ((atom? (car x)) (match1 (cdr x) (cdr y) lits (cons (cons (car x)(car y)) vars)))
        ((and (vec-ellipsis? (car x))(vector? (car y))) 
         (match1 (cdr x) (cdr y) lits (cons (cons (vector-ref (car x) 0) (vector->list (car y))) vars)))
        ((and (ellipsises? x) (= (length (car x)) (length (transpose y))))
         (let ((r (transpose y))) (append (list (cons (caar x) (car r))
                                                (cons (cadar x) (cadr r))) vars)))
        (else (let ((r1 (match1 (car x) (car y) lits vars))
                    (r2 (match1 (cdr x) (cdr y) lits vars)))
                (if (and r1 r2)
                    (append r1 r2)
                    #f)))))



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
  (and (list x)
       (>= (length x) 2)
       (list? (car x))
       (eqv? (cadr x) '...)))

;;ベクタ省略子
(define (vec-ellipsis? x)
  (and (vector? x)
       (symbol? (vector-ref x 0))
       (eq? (vector-ref x 1) '...)))


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

(define (subst-to-identifier x env)
  (cond ((null? x) '())
        ((and (symbol? x)
              (or (local-bound? x env) (global-bound? x) (eq? x 'else)))
         (identifier-bind! (symbol->identifier x) x))
        ((symbol? x)
         (symbol->identifier x))
        ((atom? x) x)
        ((vector? x) x)
        (else (cons (subst-to-identifier (car x) env)
                    (subst-to-identifier (cdr x) env)))))

(define (local-bound? x env)
  (cond ((null? env) #f)
        ((member x (car env)) #t)
        (else (local-bound? x (cdr env)))))

(define (subst-pattern-vars x pat)
  (cond ((null? x) '())
        ((and (identifier? x)(assv x pat))
         (identifier-bind! x (cdr (assv x pat))))
        ((atom? x) x)
        ((vector? x) x)
        (else (cons (subst-pattern-vars (car x) pat)
                    (subst-pattern-vars (cdr x) pat)))))



(define (subst-let-vars x)
  (subst-let-vars1 x (scan-let-vars x '())))

(define (subst-let-vars1 x a-list)
  (cond ((null? x) '())
        ((and (identifier? x)(identifier-free? x)(assv x a-list))
         (identifier-bind! x (cdr (assv x a-list))))
        ((atom? x) x)
        ((vector? x) x)
        (else (cons (subst-let-vars1 (car x) a-list)
                    (subst-let-vars1 (cdr x) a-list)))))


(define (scan-let-vars x v-list)
  (cond ((null? x) (map (lambda (x) (cons x (gensym))) v-list))
        ((atom? x) '())
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


(define (subst-from-identifier x)
  (cond ((null? x) '())
        ((identifier-free? x) x)
        ((identifier? x) (identifier-bound x))
        ((atom? x) x)
        ((vector? x) x)
        ((and (= (length x) 2)(ellipsis? x))
         (identifier-bound (car x)))
        ;;(x ...)
        ((and (> (length x) 2)(ellipsis? (list-take x 2)))
         (append (identifier-bound (car x))
                 (subst-from-identifier (cddr x))))
        ;;ex(x ... n)
        (else (cons (subst-from-identifier (car x))
                    (subst-from-identifier (cdr x))))))




(define (expand-template x vars comp-env)
  (subst-from-identifier
    (subst-let-vars
      (subst-pattern-vars
        (subst-to-identifier x comp-env)
        vars))))


(define (expand x y lits comp-env)
  (let* ((pat (caar x))
         (temp (cadar x))
         (vars (match pat y lits)))
    (cond (vars (expand-template temp vars comp-env))
          ((null? (cdr x)) (error "syntax-rules fail " y))
          (else (expand (cdr x) y lits comp-env)))))

