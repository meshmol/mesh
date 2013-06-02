
(import (normal compile)
        (normal system))

(define hygienic '())

(let ((a (vm1 (assemble 
                        (seq (comp '(syntax-rules () ((_ x) (list x x)))
                                    '() #t #t #f #f #t #f) (gen 'pause))))))
       (set! hygienic (cons (cons 'a a) hygienic)))

(define x '(let-syntax 
             ((foo (syntax-rules () ((_ x) (list x x))))
              (bar (syntax-rules (lit) ((_ lit x) (list x x)))))
             (foo 1)))
             

(define env '())
(define val? #t)
(define more? #t)
(define has-lambda? #f)
(define in-lambda? #f)
(define tail? #t)
(define if? #f)

(begin
(for-each (lambda (y) 
            (set! hygienic 
                  (cons (cons (car y)
                              (vm1 (assemble (seq (comp (cadr y) env val? more? has-lambda? in-lambda? tail? if?)
                                                  (gen 'pause)))))
                        hygienic)))
          (cadr x))
(comp-begin (cddr x) env val? more? has-lambda? in-lambda? tail? if?))

((assv (car x) hygienic)
 (comp ((get-car (cdr (assv (car x) hygienic))) x) env val? more? has-lambda? in-lambda? tail? if?) )

(define x '(foo 1))



