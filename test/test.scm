;;inner-define -> letrec
(define (inner-transfer x)
  (cond ((define? x)
         (let ((e (formal-define x)))
           (list (car e) (cadr e) (inner-transfer1 (caddr e)))))
        ((define-macro? x)
         (formal-define x))
        (else (inner-transfer1 x))))

(define (inner-transfer1 x) 
  (cond ((null? x) '()) 
        ((atom? x) x)
        ((vector? x) x) 
        ((and (pair? x)(not (list? x))) x)   
        ((eqv? (car x) 'quote) x)  
        ((eqv? (car x) 'lambda)
         (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x)))))) 
        ((and (eqv? (car x) 'let)(not (atom? (cadr x)))) 
         (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x))))))  
        ((eqv? (car x) 'letrec) (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x)))))) 
        ((eqv? (car x) 'let*)   (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x))))))
        (else (cons (car x)(map inner-transfer1 (cdr x))))))

;;定義部と本体を分離してletrec/letrec-syntaxに変換する。
(define (inner-transfer2 x)
  (let* ((e (separate x))
         (def (car e))
         (defsyn (cadr e))
         (body (caddr e)))
    (if (null? def)
        (if (null? defsyn)
            body
            (list (cons 'letrec-syntax (cons (reverse defsyn) body))))
        (if (null? defsyn)
            (list (cons 'letrec (cons (reverse def) body)))
            (list (cons 'letrec (cons (reverse def)
                                      (list (cons 'letrec-syntax (cons (reverse defsyn) body))))))))))

;;定義部と本体に分離。
(define (separate x) 
  (separate1 x '() '()))

(define (separate1 x def defsyn)
  (cond ((and (pair? x)(define? (car x))) 
         (let ((e (formal-define (car x)))) 
           (separate1 (cdr x)    
                      (cons (list (cadr e) (inner-transfer1 (caddr e))) def)
                      defsyn)))
        ((and (pair? x)(define-syntax? (car x)))
         (separate1 (cdr x)
                    def
                    (cons (list (cadr (car x)) (caddr (car x))) defsyn)))
        (else (list def defsyn x))))

;;定義文か？
(define (define? x) 
  (and (list? x) (eqv? (car x) 'define)))

(define (define-macro? x)
  (and (list? x) (eqv? (car x) 'define-macro)))

(define (define-syntax? x)
  (and (list? x) (eqv? (car x) 'define-syntax)))

;;mitスタイルならlambdaへ変換。
(define (formal-define x)  
  (if (symbol? (cadr x))
      x   
      (list (car x) (caadr x) (cons 'lambda (cons (cdadr x) (cddr x))))))
;;-