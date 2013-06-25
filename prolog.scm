
(assert (append () _l _l))
(assert (append (_x1 . _l1) _l2 (_x . _l3)) (append _l1 _l2 _l3))


(import (normal compile)
        (normal system))

(define (deref x vars back)
  (cond ((null? vars) x)
        ((eqv? x (caar vars))
         (if (variable? (cdar vars))
             (deref (cdar vars) (reverse back) '())
             (cdar vars)))
        (else
          (deref x (cdr vars) (cons (car vars) back)))))




(define local-stack '((_x . 1)(_y . _x)))

(define trail '())


(define (unify x y)
  (cond ((eqv? x y) #t)
        ((variable? x) (unifyv x y))
        ((variable? y) (unifyv y x))
        ((and (pair? x) (pair? y))
         (unify (cdr x) (cdr y))
         (unify (car x) (car y)))
        (else #f)))


(define (unifyv x y)
  (let ((r (deref x local-stack '())))
    (cond ((variable? r)
           (set! local-stack (cons (cons x y) local-stack)) #t)
          ((variable? y)
           (set! local-stack (cons (cons y x) local-stack)) #t)
          (else
            (equal? r y)))))

(define (unifyc x y)
  (if (variable? y)
      (begin (set! local-stack (cons (cons y x) local-stack)) #t)
      (equal? x y)))

(define-syntax push!
  (syntax-rules ()
    ((_ x y)
     (set! x (cons y x)))))

(define (variable? x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\_)))


(define (comp-pred x)
  (seq (comp-term (cdr x))
       (gen 'gvar (car x))
       (gen 'call (length (cdr x)))))

(define (comp-term x)
  (if (null? x)
      '()
      (if (variable? (car x))
          (seq (gen 'deref (car x))
               (comp-term (cdr x)))
          (seq (gen 'const (car x))
               (comp-term (cdr x))))))


(define (comp-clause-head x)
  (comp-clause-head1 0 (cdr x)))

(define (comp-clause-head1 n x)
  (cond ((null? x) '())
        ((variable? (car x)) (seq (gen 'unifyv 0 n (car x))
                                  (comp-clause-head1 (+ n 1) (cdr x))))
        ((atom? (car x)) (seq (gen 'unifyc 0 n (car x))
                              (comp-clause-head1 (+ n 1) (cdr x))))
        ((null? (car x)) (seq (gen 'unifyc 0 n (car x))
                              (comp-clause-head1 (+ n 1) (cdr x))))
        ((pair? (car x)) (seq (gen 'unify 0 n (car x))
                              (comp-clause-head1 (+ n 1) (cdr x))))
        (else
          (error "illegal predicate term" (car x)))))

(define (comp-clauses sym)
  (let* ((x (reverse(getprop sym)))
         (args-n (length (cdaar x))))
    (seq (gen 'fn args-n (seq (gen 'args args-n) (comp-clauses1 x)))
         (gen 'def sym))))

(define (comp-clauses1 x)
  (cond ((null? (cdr x)) (seq (gen 'try #f) (comp-a-clause (car x))))
        (else (let ((label (gen-label)))
                (seq (gen 'try label)
                     (comp-a-clause (car x))
                     (gen label)
                     (comp-clauses1 (cdr x)))))))

(define (comp-a-clause x)
  (seq (comp-clause-head (car x))
       (comp-clause-body (cdr x))))

(define (comp-clause-body x)
  (cond ((null? x) '())
        ((null? (cdr x)) (seq (comp-pred (car x))
                            (gen 'proceed)))
        (else
          (seq (comp-pred (car x))
               (gen 'pop)
               (comp-clause-body (cdr x))))))

