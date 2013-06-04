;;inner-define -> letrec
(define (inner-transfer x)
  (if (define? x)
      (let ((e (formal-define x)))
        (list (car e) (cadr e) (inner-transfer1 (caddr e)))) 
      (inner-transfer1 x)))

(define (inner-transfer1 x) 
  (cond ((null? x) '()) 
        ((atom? x) x)
        ((vector? x) x) 
        ((and (pair? x)(not (list? x))) x)   
        ((eq? (car x) 'quote) x)  
        ((eq? (car x) 'lambda)
         (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x)))))) 
        ((and (eq? (car x) 'let)(not (atom? (cadr x)))) 
         (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x))))))  
        ((eq? (car x) 'letrec) (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x)))))) 
        ((eq? (car x) 'let*)   (cons (car x) (cons (cadr x) (map inner-transfer1 (inner-transfer2 (cddr x))))))
        (else (cons (car x)(map inner-transfer1 (cdr x))))))

;;定義部と本体を分離してletrecに変換する。
(define (inner-transfer2 x)
  (let ((e (separate x)))
    (if (null? (car e))
        (cdr e)
        (list (cons 'letrec (cons (reverse (car e)) (cdr e)))))))

;;定義部と本体に分離。
(define (separate x) 
  (separate1 x '()))

(define (separate1 x def)
  (if (and (pair? x)(define? (car x))) 
      (let ((e (formal-define (car x)))) 
        (separate1 (cdr x)    
                   (cons (list (cadr e) (inner-transfer1 (caddr e))) def)))
      (cons def x)))

;;定義文か？
(define (define? x) 
  (and (list? x) (eq? (car x) 'define)))

;;mitスタイルならlambdaへ変換。
(define (formal-define x)  
  (if (symbol? (cadr x))
      x   
      (list (car x) (caadr x) (cons 'lambda (cons (cdadr x) (cddr x))))))