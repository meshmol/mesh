;;アセンブラ

(define (*assemble ls)
  (*pass2 ls (*pass1 ls))) 


(define (*pass1 x)
  (pass1-iter x 1 '()))

(define (*pass1-iter ls pc labels)
  (cond ((null? ls) labels)
        ((label? (car ls))
         (*pass1-iter (cdr ls) pc (cons (cons (car ls) pc) labels)))
        (else
          (*pass1-iter (cdr ls) (+ pc (op-count (car ls))) labels))))

(define (*pass2 x labels)
  (*pass2-iter x 1 '() labels))

(define (*pass2-iter ls pc obj labels)
  (cond ((null? ls) (reverse obj))
        ((label? (car ls)) (*pass2-iter (cdr ls) pc obj labels))
        (else
          (*pass2-iter (cdr ls)
                      (+ pc (op-count (car ls)))
                      (append (reverse (*mnemonic->code (car ls) labels pc))
                              obj)
                      labels))))

(define (*mnemonic->code x labels pc)
  (cond ((eqv? (car x) 'halt) (list (op-code (car x))))
        ((eqv? (car x) 'const) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'lvar) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'gvar) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'lset) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'gset) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'pop) (list (op-code (car x))))
        ((eqv? (car x) 'jump) (list (op-code (car x)) (- (cdr (assq (cadr x) labels)) pc)))
        ((eqv? (car x) 'tjump) (list (op-code (car x)) (- (cdr (assq (cadr x) labels)) pc)))
        ((eqv? (car x) 'fjump) (list (op-code (car x)) (- (cdr (assq (cadr x) labels)) pc)))
        ((eqv? (car x) 'return) (list (op-code (car x))))
        ((eqv? (car x) 'args) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'call) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'callj) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'fn) (list (op-code (car x)) (cadr x) (*assemble (caddr x))))
        ((eqv? (car x) 'save) (list (op-code (car x))))
        ((eqv? (car x) 'prim) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'def) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'defm) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'defh) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'neqp) (list (op-code (car x))))
        ((eqv? (car x) 'smlp) (list (op-code (car x))))
        ((eqv? (car x) 'esmlp) (list (op-code (car x))))
        ((eqv? (car x) 'grtp) (list (op-code (car x))))
        ((eqv? (car x) 'egrtp) (list (op-code (car x))))
        ((eqv? (car x) 'zerop) (list (op-code (car x))))
        ((eqv? (car x) 'add1) (list (op-code (car x))))
        ((eqv? (car x) 'sub1) (list (op-code (car x))))
        ((eqv? (car x) 'add2) (list (op-code (car x))))
        ((eqv? (car x) 'sub2) (list (op-code (car x))))
        ((eqv? (car x) 'gref) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'catch) (list (op-code (car x))))
        ((eqv? (car x) 'pause) (list (op-code (car x))))
        ((eqv? (car x) 'car) (list (op-code (car x))))
        ((eqv? (car x) 'cdr) (list (op-code (car x))))
        ((eqv? (car x) 'cons) (list (op-code (car x))))
        ((eqv? (car x) 'adapt) (list (op-code (car x))))
        ((eqv? (car x) 'deflib) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'explib) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'implib) (list (op-code (car x)) (cadr x)))))

(define (op-count x) 
  (length x))


(define (op-code? x)
  (member x mnemonic))

(define (op-code x)
  (- (length mnemonic) (length (member x mnemonic))))


(define mnemonic 
  (list 'nop 'halt 'const 'lvar 'gvar 'lset 'gset 'pop 'jump 'tjump 'fjump
        'return 'args 'call 'callj 'fn 'save 'prim 'def 'defm 'defh
        'neqp 'smlp 'esmlp 'grtp 'egrtp 'zerop 
        'add1 'sub1 'add2 'sub2 'gref 'catch 'pause 'car 'cdr 'cons 'adapt
        'deflib 'explib 'implib))


;;コンパイル
(define (*compile-file x)
  (let* ((inf (string-append x ".scm"))
         (outf (string-append x ".o"))
         (inp (open-input-file inf))
         (outp (open-output-file outf)))
    (*compile-file1 (read inp) inp outp)
    (close-input-port inp)
    (close-output-port outp)
    (display "compiled!")(flush)))

(define (*compile-file1 sexp inp outp)
  (cond ((eof-object? sexp) #t)
        (else (write (*assemble (compile sexp)) outp)
              (newline outp)
              (*compile-file1 (read inp) inp outp))))


