(define yield #f)
 
;ジェネレータの実装
(define (make-generator thunk)
  (define state (cons thunk '()))
  (define (run-generator)
    (set! state (call/cc (lambda (return)
      (set! yield (lambda x (call/cc (lambda (c) (return (cons c x))))))
      (apply (car state) (cdr state))
      (return (cons values '(#f))))))
    (apply values (cdr state)))
  run-generator)
 
;ハノイの塔の円盤を、fromからtoへn個動かす。
;towersは塔のリストで、塔は上から順に並べた円盤のリストである。
(define (hanoi n from to tmp towers)
  (if (= n 0)
      towers
      (let* ((t1 (hanoi (- n 1) from tmp to towers));まずn-1個の円盤をfromからtmpに動かす。
             (t2 (yield (move t1 from to))))        ;n個目の円盤をfromからtoへ動かし、一旦中断する。
            (hanoi (- n 1) tmp to from t2))))       ;その後、n-1個の円盤をtmpからtoへ動かす。
 
;ハノイの塔の一番上の円盤をfromからtoに移動する。
(define (move lis from to)
  (define (moved-tower lis from to index)
    (cond
      ((= index from) (cdr  (list-ref lis from)))
      ((= index to)   (cons (car (list-ref lis from)) (list-ref lis to)))
      (else (list-ref lis index))))
  (let loop ((n 0))
    (if (= n (length lis)) '()
      (cons (moved-tower lis from to n) (loop (+ n 1))))))
 
(define hanoi-generator (make-generator (lambda () (hanoi 5 0 1 2 '((0 1 2 3 4) () ())))))

(define (fail) #f)
(define (amb-proc . x)
  (define former-fail fail)
  (if (null? x)
    (fail)
    (call/cc (lambda (return) ; 分岐点
      (set! fail (lambda () ; 選ばなかった選択肢を保存
        (set! fail former-fail)
        (return (apply amb-proc (cdr x)))))
      (return ((car x))))))) ; 一つの選択肢を返す
