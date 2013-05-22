
(define (unify x y bindings no-bindings)
  (cond ((eq? bindings 'fail) 'fail)
        ((eq? x y) bindings)
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings))
        ((and (pair? x) (pair? y))
         (unify (cdr x) (cdr y) bindings no-bindings)
         (unify (car x) (car y) bindings no-bindings))
        (else 'fail)))

(define (unify-variable var x bindings no-bindings)
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings no-bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings no-bindings))
        (else (extend-bindings var x bindings))))

(define (variable? x)
  (if (not (symbol? x))
      #f
      (let ((x1 (string->list (symbol->string x))))
        (if (eqv? (car x1) #\?)
            #t
            #f))))
