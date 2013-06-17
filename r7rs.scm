;;末尾再帰最適化コンパイラ
;;has-lambda? 入れ子のlambda式をもつかどうか。
;;in-lambda?　lambda式の中の式かどうか？
;;tail?　末尾再帰最適化をするべきかどうか？
;;define-macroのときには#fにする。
;;--------------------------------- 
(define hygienic '()) ;局所マクロ

(define (*compile x)
  (set! hygienic '())
  (append (comp (inner-transfer x) '() #t #t #f #f #t #f) (list (list 'halt))))



;;quasi-quote transfer
(define (quasi-transfer x)
  (cond ((null? x) '())
        ((atom? x)
         (list 'quote x))
        ((and (pair? x)(eqv? (car x) 'unquote))
         (cadr x))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote))
         (list 'cons (cadar x) (quasi-transfer (cdr x))))
        ((and (pair? x)(pair? (car x))(eqv? (caar x) 'unquote-splicing))
         (list 'append (cadar x) (quasi-transfer (cdr x))))
        (else
          (list 'cons (quasi-transfer (car x)) (quasi-transfer (cdr x))))))