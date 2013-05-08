;;テストプログラム　ラマヌジャンのタクシーナンバーの検証

;;使い方は
;;; Norm> (filter taxi (combination 2 '(1 2 3 4 5 6 7 8 9 10 11 12)))
;;; ((1 12) (9 10))
;;; Norm> 

;;組み合わせはvallogさんの書いたものをコピペさせていただきました。
(define combination 
  (lambda (r l) 
    (cond 
      ((null? l) '()) 
      ((or (zero? r)(> r (length l))) '()) 
      ((= r 1)(map list l)) 
      ((= r (length l))(list l)) 
      (else (append (map (lambda (n)(cons (car l) n)) 
                         (combination (- r 1)(cdr l))) 
                    (combination r (cdr l)))))))

(define (filter f ls)
  (cond ((null? ls) '())
        ((f (car ls)) (cons (car ls) (filter f (cdr ls))))
        (else (filter f (cdr ls)))))

(define (taxi x)
  (= (+ (expt (car x) 3) (expt (cadr x) 3))
     1729))