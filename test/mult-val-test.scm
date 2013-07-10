;;多値のテスト;; M.Hiroi さんのHPにある例題を使わせていただきました。
(import (mesh test))

(define (partition pred ls)
  (if (null? ls)    
      (values '() '())  
      (call-with-values    
        (lambda ()      
          (partition pred (cdr ls)))   
        (lambda (a b)    
          (if (pred (car ls))   
              (values (cons (car ls) a) b)   
              (values a (cons (car ls) b)))))))

;;Kent本
(define split
  (lambda (ls)
    (if (or (null? ls) (null? (cdr ls)))
        (values ls '())
        (call-with-values
          (lambda () (split (cddr ls)))
          (lambda (odds evens)
            (values (cons (car ls) odds)
                    (cons (cadr ls) evens)))))))

(test* "(call-with-values * -)" -1 (call-with-values * -))
(test* "(call-with-values (lambda () (values 4 5)) (lamba (a b) b))"
       5
       (call-with-values (lambda () (values 4 5)) (lambda (a b) b)))
