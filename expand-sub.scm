;;アイディアスケッチ

(define (expand-subpat p n)
  (if (not (has-pat-var? p))
      (error "expand no pattern vairable " p)
      (expand-subpat1 p '() n)))

(define (expand-subpat1 p subp n)
  (let ((val (subst-from-identifier p n)))
    (if (not (val))
        (reverse subp)
        (expand-subpat1 p (cons val subp) n)))) 


(define (has-pat-var? p)
  (cond ((null? p) #f)
        ((identifier-variable? p) #t)
        ((atom? p) #f)
        ((vector? p) #f)
        (else
          (or (has-pat-var? (car p))
              (has-pat-var? (cdr p))))))


(define (combine x y)
  (if (null? x)
      '()
      (cons (cons (caar x)(list (cdar x)(cdar y)))
            (combine (cdr x) (cdr y)))))


(define (subst-from-identifier p n)
  (cond ((null? p) '())
        ((and (identifier-ellipsis? p) (> n 0))
         (error "syntax-rules illegal template" p))
        ((and (identifier-ellipsis? p) (null? (identifier-bound p)) (> n 0))
         #f)
        ((and (identifier-vairable? p) (> n 0))
         (let ((val (identifier-bound p)))
           (identifier-bind! p (cdr val))
           (car val)))
        ((identifier-free? p) (identifier->symbol p))
        ((identifier? p) (identifier-bound p))
        ((atom? p) p)
        ((vector? p)
         (list->vector (subst-from-identifier (vector->list p) n)))
        ;;ex(p ... n)
        ((and (> (length p) 2)(ellipsis? (list-take p 2)))
         (append (identifier-bound (car p))
                 (subst-from-identifier (cddr p) n)))
        ((ellipsises? p) (expand-subpat p (+ n 1)))
        ((and (eqv? (car p) '...)(eqv? (cadr p) '...)(= (length p) 2))
         (cadr p))
        ((eqv? (car p) '...) (cdr p))
        (else (cons (subst-from-identifier (car p) n)
                    (subst-from-identifier (cdr p) n)))))


(define (match-subpat p f lits vars)
  (if (null? f)
      vars
      (let ((val (match1 p (car f) lits vars)))
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
        ((symbol? p) (cons (cons p f) vars))
        ((ellipsis? p) (match1 (cddr p)
                               (list-take-right f (length (cddr p)))
                               lits
                               (cons (cons (car p) (list-take f (- (length f) (length (cddr p))))) vars)))
        ((ellipsises? p) (match-subpat p f lits '()))
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
