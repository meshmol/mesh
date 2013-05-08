;;λ計算インタープリタ ver0.8 k.sasagawa
;;私の勉強用に書いたもの。簡約は不完全。
;;Normalの動作確認用。

(define (subst old new f)
  (cond ((null? f) '())
        ((equal? (car f) old) (cons new (subst old new (cdr f))))
        ((atom? (car f)) (cons (car f) (subst old new (cdr f))))
        (else (cons (subst old new (car f))(subst old new (cdr f))))))



;;λ式のリスト表現
;; λx.M (lambda (x) M)
;; (λx.M)N ((lambda (x) M) N)
;; M (M1 M2) ((M1 M2) M3)
;; 多重抽象も許すこととする。
;; ((lambda (x) (lambda (y) (x y))) a b)
;; ((lambda (x y) (x y)) a b)

(define (lambda-term? f)
  (cond ((null? f) #t)
        ((symbol? f) #t) ;x0,x1...
        ((number? f) #t) ;1,2,3..数もλ式ということにする。
        ((and (eq? (car f) 'lambda)(list? (cadr f))(lambda-term? (caddr f))) #t);λx.M
        ((and (lambda-term? (car f))(lambda-term? (cdr f))) #t) ;(M N)
        (else #f)))



;;α変換 λ式の定義にしたがってパース。
(define (alpha-convert f)
  (genvar 'reset)
  (reset-var)
  (alpha-convert1 f))

(define (alpha-convert1 f)
  (cond ((null? f) '())
        ((and (symbol? f)(bound? f)) (bound? f))
        ((and (symbol? f)(not(bound? f))) f)
        ((number? f) f)
        ((and (eq? (car f) 'lambda)(list? (cadr f)))
         (let ((new '()) (l '()))
           (push-var)
           (set! new (var-convert! (cadr f)))
           (set! l (list 'lambda new (alpha-convert1 (caddr f))))
           (pop-var)
           l))
        (else (cons (alpha-convert1 (car f))(alpha-convert1 (cdr f))))))

(define (var-convert! var)
  (define (iter old new)
    (if (null? old)
        (reverse new)
        (let ((v '()))
          (set! v (genvar 'gen))
          (bound! (car old) v)
          (iter (cdr old) (cons v new)))))
  (iter var '()))
                 

;;変数リスト
;;((x . x1)(y . x2) ...)
(define var-assoc '())
(define stack '())

(define (bound? v)
  (let ((ans (assq v var-assoc)))
    (if ans
        (cdr ans)
        #f)))

(define (bound! old new)
  (set! var-assoc (cons (cons old new) var-assoc)))

(define (reset-var)
  (set! var-assoc '()))

(define (push-var)
  (set! stack (cons var-assoc stack)))

(define (pop-var)
  (set! var-assoc (car stack))
  (set! stack (cdr stack)))

;; 変数を生成する
(define genvar
  (let ((x -1))
    (lambda (msg)
      (cond ((eq? msg 'gen)
             (begin (set! x (+ x 1))
                    (string->symbol (string-append "x" (number->string x)))))
            ((eq? msg 'reset)(set! x -1))))))


;;β基の判定 (λx.M)N 計算論p66
(define (beta-redex? f)
  (cond ((atom? f) #f)
        ((atom? (car f)) #f)
        ((and (eq? (caar f) 'lambda)
              (symbol? (caadar f))
              (lambda-term? (caddar f))
              (lambda-term? (cadr f))) #t)
        (else #f)))

;;(λx.M)N
;;((lambda (x) M) N)
;;β正規形 β-normal-form
(define (beta-normal-form? f)
  (cond ((not (lambda-term? f)) #f) 
        ((symbol? f) #t) ; x0,x1...
        ((number? f) #t) ; 1,2,3...
        ((and (list? f)(eq? (car f) 'lambda)
              (beta-normal-form? (caddr f))) #t) ;λx.x
        ((and (list? f)
              (symbol? (car f))
              (beta-normal-form? (cadr f))) #t) ;(x λx.x)
        ((and (list? f)(symbol? (car f))(symbol? (cadr f))) #t) ;(x0 x1)
        (else #f)))


;;β変換
;;;; example ((lambda (x) (lambda (y) (x y))) a b)
(define (beta-reduce form)
  (cond ((= (length (cadar form)) 1)
         (beta-reduce1 (car form) (cdr form)))
        ((> (length (cadar form)) 1)
         (beta-reduce2 1 (car form) (cdr form)))))
  
(define (beta-reduce1 l m) ; l=(lambda (x) ...) m=(a b)
  (display (cons l m))(newline)
  (if (null? m)
      l
      (let ((var (caadr l))  
            (body (cddr l))) 
        (beta-reduce1 (car (subst var (car m) body)) (cdr m)))))

(define (beta-reduce2 n l m)
  (display (cons l m))(newline)
  (if (null? m)
      (caddr l)
      (let ((var (nth n (cadr l))))
        (beta-reduce2 (+ n 1) (subst var (car m) l) (cdr m)))))

(define (nth n l)
  (if (= n 1)
      (car l)
      (nth (- n 1) (cdr l))))

(define (reduce form)
  (if (lambda-term? form)
      (reduce1 (alpha-convert form))
      (error "Not lambda term" form)))

(define (reduce1 form)
  (display form)(newline)
  (cond ((beta-normal-form? form) form) ;β-normal-form
        ((and (symbol? (car form))
              (beta-redex? (cadr form))) ;(x0 M)
         (reduce1 (list (car form) (reduce1 (cadr form)))))
        ((eq? (car form) 'lambda) ;λx.M
         (reduce1 (list (car form)
                        (cadr form)
                        (reduce1 (caddr form)))))
        ((eq? (caar form) 'lambda)
         (reduce1 (beta-reduce form))) ;(λx.x)M
        (else (error "Reduction error" form))))
               


;;テスト用データ
(define foo '((lambda (x) x) (lambda (a) a)))
(define bar '((lambda (y) (y ((lambda (z) (x z)) (lambda (z) z)))) (lambda (a) a)))
(define baz '(x ((lambda (x) x) y)))
(define boo '((lambda (x) x) y))
(define uoo '((lambda (x) (lambda (y) x)) y))
(define uoo2 '((lambda (x) (lambda (z) x)) y))
(define woo '((lambda (x) (lambda (y) x)) z))
(define hoo '((lambda (x) (x x)) (lambda (x) (x x))))


(define t '((lambda (x y z w) (x y (z w))) a b c d))

;(define t '(lambda (x y) x))
(define f '(lambda (x y) y))