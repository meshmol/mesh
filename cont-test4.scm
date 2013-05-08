(define yield #f)
 
;�W�F�l���[�^�̎���
(define (make-generator thunk)
  (define state (cons thunk '()))
  (define (run-generator)
    (set! state (call/cc (lambda (return)
      (set! yield (lambda x (call/cc (lambda (c) (return (cons c x))))))
      (apply (car state) (cdr state))
      (return (cons values '(#f))))))
    (apply values (cdr state)))
  run-generator)
 
;�n�m�C�̓��̉~�Ղ��Afrom����to��n�������B
;towers�͓��̃��X�g�ŁA���͏ォ�珇�ɕ��ׂ��~�Ղ̃��X�g�ł���B
(define (hanoi n from to tmp towers)
  (if (= n 0)
      towers
      (let* ((t1 (hanoi (- n 1) from tmp to towers));�܂�n-1�̉~�Ղ�from����tmp�ɓ������B
             (t2 (yield (move t1 from to))))        ;n�ڂ̉~�Ղ�from����to�֓������A��U���f����B
            (hanoi (- n 1) tmp to from t2))))       ;���̌�An-1�̉~�Ղ�tmp����to�֓������B
 
;�n�m�C�̓��̈�ԏ�̉~�Ղ�from����to�Ɉړ�����B
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
    (call/cc (lambda (return) ; ����_
      (set! fail (lambda () ; �I�΂Ȃ������I������ۑ�
        (set! fail former-fail)
        (return (apply amb-proc (cdr x)))))
      (return ((car x))))))) ; ��̑I������Ԃ�
