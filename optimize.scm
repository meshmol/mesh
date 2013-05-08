;;のぞき穴最適化
;;optimizer


(define (instr1 x)
  (car x))

(define (instr2 x)
  (cadr x))

(define (instr3 x)
  (caddr x))

(define (instr4 x)
  (cadddr x))

(define (opcode x)
  (if (list? x)
      (car x)
      #f))

(define (operand1 x)
  (if (list? x)
      (cadr x)
      #f))

(define (operand2 x)
  (if (list? x)
      (caddr x)
      #f))

(define (optimize code)
  (optimize1 code code))

(define (optimize1 code all-code)
  (cond ((null? code) '())
        ((eq? (opcode (instr1 code)) 'fn)
         (seq (gen 'fn
                   (operand1 (instr1 code))
                   (optimize1 (operand2 (instr1 code))
                              (operand2 (instr1 code))))
              (optimize1 (cdr code) all-code)))
        ((and (eq? (opcode (instr1 code)) 'gvar)
              (eq? (opcode (instr2 code)) 'call))
         ;;((gvar g)(call 1)) -> (gvcl g 1)
         (seq (gen 'gvcl 
                   (operand1 (instr1 code))
                   (operand1 (instr2 code)))
              (optimize1 (cddr code) all-code)))
        ((and (eq? (opcode (instr1 code)) 'gvar)
              (eq? (opcode (instr2 code)) 'callj))
         ;;((gvar g)(callj 1)) -> (gvcj g 1)
         (seq (gen 'gvcj 
                   (operand1 (instr1 code))
                   (operand1 (instr2 code)))
              (optimize1 (cddr code) all-code)))
        ((and (label? (instr1 code))
              (not (find-label (instr1 code) all-code)))
         ;;使用されていないラベルがある場合
         (optimize1 (cdr code) all-code))
        (else
          (cons (car code) (optimize1 (cdr code) all-code)))))

(define (find-label l code)
  (cond ((null? code) #f)
        ((eq? (operand1 (instr1 code)) l) #t)
        (else (find-label l (next-instr code)))))

(define (next-instr code)
  (cond ((null? code) '())
        ((label? (instr1 (cdr code))) (next-instr (cddr code)))
        (else (cdr code))))

;;前処理 効率のよい関数への置換、定数畳み込みを行う。
(define (preprocess x)
  (cond ((null? x) '())
        ((atom? x) x)
        ((vector? x) x)
        ((and (list? x)
              (memq (car x) '(+ - * / sin cos tan asin acos atan))
              (for-all number? (cdr x)))
         (eval x))
        ((and (list? x)
              (eq? (car x) '+)
              (= (length x) 3)
              (eq? (cadr x) 1))
         (list '+ (caddr x) 1))
        ((and (list? x)
              (eq? (car x) '+)
              (= (length x) 3)
              (eq? (cadr x) 2))
         (list '+ (caddr x) 2))
        (else (cons (preprocess (car x))
                    (preprocess (cdr x))))))

(define (for-all pred x)
  (cond ((null? x) #t)
        ((pred (car x)) (for-all pred (cdr x)))
        (else #f)))


       

                   
