;;末尾再帰最適化コンパイラ
;;has-lambda? 入れ子のlambda式をもつかどうか。
;;in-lambda?　lambda式の中の式かどうか？
;;tail?　末尾再帰最適化をするべきかどうか？
;;define-macroのときには#fにする。
;;--------------------------------- 
(define hygienic '()) ;局所マクロ

(define (compile x)
  (set! hygienic '())
  (append (comp (transfer x) '() #t #t #f #f #t #f) (list (list 'halt))))


(define (comp x env val? more? has-lambda? in-lambda? tail? if?)
  (cond ((null? x) (comp-const x val? more? in-lambda?))
        ((boolean? x) (comp-const x val? more? in-lambda?))
        ((symbol? x) (comp-var x env val? more? in-lambda?))
        ((syntactic-closure? x) (comp-var x env val? more? in-lambda?))
        ((atom? x) (comp-const x val? more? in-lambda?))
        ((vector? x) (comp-const x val? more? in-lambda?))
        ((bytevector? x) (comp-const x val? more? in-lambda?))
        ((macro-name? (car x))
         (comp (macroexpand-1 x) env val? more? has-lambda? in-lambda? tail? if?))
        ((hygienic-name? (car x))
         (comp (macroexpand-1 x) env val? more? has-lambda? in-lambda? tail? if?))
        ((eqv? (car x) 'quote)
         (args-count-check x 1 1)
         (comp-const (cadr x) val? more? in-lambda?))
        ((eqv? (car x) 'begin)
         (comp-begin (cdr x) env val? more? has-lambda? in-lambda? tail? if?))
        ((eqv? (car x) 'set!)
         (args-count-check x 2 2)
         (args-type-check x 1 symbol? "require symbol ")
         (seq (comp (caddr x) env #t #t has-lambda? in-lambda? tail? if?)
              (gen-set (cadr x) env)
              (if (not val?) (gen 'pop) '())
              (if (not more?) (gen 'return) '())))
        ((eqv? (car x) 'if)
         (if (= (length x) 3) (set! x (append x '((undefined)))) (undefined))
         (args-count-check x 3 3)
         (comp-if (cadr x) (caddr x) (cadddr x)
                  env val? more? has-lambda? in-lambda? tail? #t))
        ((eqv? (car x) 'lambda)
         (if val?
             (let ((f (comp-lambda (cadr x) (cddr x) env tail? if?)))
               (seq (gen 'fn (args-count (cadr x))f)
                    (if (not more?) (gen 'return) '())))
             #f))
        ((eqv? (car x) 'define)
         (args-type-check x 1 symbol? "require symbol ")
         (args-type-check x 1 (lambda (y) (not (primitive-name? y)))
                          "can't overwrite primitive name ")
         (seq (comp (caddr x) env #t #t has-lambda? in-lambda? tail? if?) 
              (gen 'def (cadr x))))
        ((eqv? (car x) 'define-macro)
         (args-type-check x 1 symbol? "require symbol ")
         (seq (comp (caddr x) env #t #t has-lambda? in-lambda? #f if?)
              (gen 'defm (cadr x))))
        ((eqv? (car x) 'define-syntax)
         (args-type-check x 1 symbol? "require symbol ")
         (seq (comp (caddr x) env #t #t has-lambda? in-lambda? #f if?)
              (gen 'def (cadr x))))
        ((eqv? (car x) 'define-library)
         (args-count-check x 2 'infinity)
         (args-type-check x 1 list? "require list for library name")
         (seq (gen 'deflib (cadr x) (cddr x))
              (gen 'const (cadr x))))
        ((eqv? (car x) 'let-syntax)
         (for-each 
           (lambda (y)
             (set! hygienic 
                   (cons (cons (car y)
                               (vm1 (assemble (seq (comp (cadr y) env #t #t #f #f #t #f)
                                                   (gen 'pause)))))
                         hygienic)))
          (cadr x))
         (comp-begin (cddr x) env val? more? has-lambda? in-lambda? tail? if?))
        ((eqv? (car x) 'letrec-syntax)
         (for-each 
           (lambda (y)
             (set! hygienic 
                   (cons (cons (car y)
                               (vm1 (assemble (seq (comp (cadr y) env #t #t #f #f #t #f)
                                                   (gen 'pause)))))
                         hygienic)))
           (cadr x))
         (comp-begin (cddr x) env val? more? has-lambda? in-lambda? tail? if?))
        ((assv (car x) hygienic) ;let-syntax/letrec-syntax body
         (comp ((get-car (cdr (assv (car x) hygienic))) x) env val? more? has-lambda? in-lambda? tail? if?) )
        ((eqv? (car x) 'export)
         (args-count-check x 1 'infinity)
         (seq (gen 'explib (cdr x))))
        ((eqv? (car x) 'import)
         (args-count-check x 1 'infinity)
         (seq (gen 'implib (cdr x))))
        ((eqv? (car x) 'math)
         (comp (infix->prefix (cdr x)) env val? more? has-lambda? in-lambda? tail? if?))
        ((eqv? (car x) 'syntax-rules)
         (seq (comp `(lambda (expr) (expand ',(cddr x) expr ',(cadr x) ',env))
                    env val? more? has-lambda? in-lambda? tail? if?)
              (gen 'defh)))
        (else
          (comp-funcall (car x) (cdr x) env val? more? has-lambda? in-lambda? tail? if?))))


(define (comp-const x val? more? in-lambda?)
  (if val? 
      (seq (gen 'const x)
           (if (and (not more?) in-lambda?)
               (gen 'return)
               '()))
      '()))

(define (comp-var x env val? more? in-lambda?)
  (if val?
      (seq (gen-var x env)
           (if (and (not more?) in-lambda?)
               (gen 'return)
               '()))
      '()))



(define (comp-begin exps env val? more? has-lambda? in-lambda? tail? if?)
  (cond ((null? exps) (comp-const '() val? more? in-lambda?))
        ((length=1? exps) (comp (transfer (car exps)) env val? #f has-lambda? in-lambda? tail? if?))
        (else (seq (comp (transfer (car exps)) env #f #t has-lambda? in-lambda? tail? if?)
                   (comp-begin  (cdr exps) env val? more? has-lambda? in-lambda? tail? if?)))))


(define (comp-list exps env has-lambda? in-lambda? tail? if?)
  (if (null? exps)
      '()
      (seq (comp (car exps) env #t #t has-lambda? in-lambda? tail? if?)
           (comp-list (cdr exps) env has-lambda? in-lambda? tail? if?))))

(define (comp-if pred then else env val? more? has-lambda? in-lambda? tail? if?)
  (cond ((not pred)
         (comp else env val? more? has-lambda? in-lambda? tail? if?))
        ((and (atom? pred) (not (symbol? pred)) (not (not pred)))
         (comp then env val? more? has-lambda? in-lambda? tail? if?))
        ((and (list? pred)
              (length=1? (cdr pred))
              (eq? (car pred) 'not))
         (comp-if (cadr pred) else then env val? more? has-lambda? in-lambda? tail? if?))
        (else 
          (let ((pcode (comp pred env #t #t has-lambda? in-lambda? tail? if?))
                (tcode (comp then env val? more? has-lambda? in-lambda? tail? if?))
                (ecode (comp else env val? more? has-lambda? in-lambda? tail? if?)))
            (cond ((equal? tcode ecode)
                   (seq (comp pred env #f #t has-lambda? in-lambda? tail? if?) ecode))
                  ((not tcode)
                   (let ((L2 (gen-label)))
                     (seq pcode (gen 'tjump L2) ecode (list L2)
                          (if (not more?) (gen 'return) '()))))
                  (else
                    (let ((L1 (gen-label))
                          (L2 (if more? (gen-label) #f)))
                      (seq pcode (gen 'fjump L1) tcode
                           (if more? (gen 'jump L2) '())
                           (list L1) ecode (if more? (list L2) '())))))))))

(define (comp-funcall f args env val? more? has-lambda? in-lambda? tail? if?)
  (if (and (not (symbol? f)) (not (list? f))) (error "illegal function call " f))
  (let ((prim (primitive? f env)))
    (cond (prim
            (args-count-check (cons f args) (prim-min prim) (prim-max prim))
            (cond ((and (not val?) (not (prim-side-effect? prim)))
                   (comp-begin args env #f more? has-lambda? in-lambda? tail? if?))
                  ((and (memv f '(= < <= > >=))(= (length args) 2));;2項比較演算子
                   (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                        (gen (cadr (assv f binomial-op)))
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((and (eqv? f 'zero?)(= (length args) 1));;(zero? n)->(zerop)
                   (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                        (gen 'zerop)
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((and (eqv? f '-)(= (length args) 2)
                        (or (eq? (cadr args) 1) (eq? (cadr args) 2))) ;;(- x 1or2)->(sub1or2)
                   (seq (comp-list (butlast args) env has-lambda? in-lambda? tail? if?)
                        (if (= (cadr args) 1) (gen 'sub1) (gen 'sub2))
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((and (eqv? f '+)(= (length args) 2)
                        (or (eq? (cadr args) 1) (eq? (cadr args) 2))) ;;(+ x 1or2)->(add1or2)
                   (seq (comp-list (butlast args) env has-lambda? in-lambda? tail? if?)
                        (if (= (cadr args) 1) (gen 'add1) (gen 'add2))
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((eqv? f 'car)
                   (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                        (gen 'car)
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((eqv? f 'cdr)
                   (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                        (gen 'cdr)
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  ((eqv? f 'cons)
                   (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                        (gen 'cons)
                        (if (not val?) (gen 'pop) '())
                        (if (and (not more?) in-lambda?) (gen 'return) '())))
                  (else (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                             (gen 'prim f (length args))
                             (if (not val?) (gen 'pop) '())
                             (if (and (not more?) in-lambda?) (gen 'return) '())))))
          ((and (list? f) (eqv? (car f) 'lambda) (null? (cadr f)))
           (if (not (null? args)) (error "too many arguments: " args) '())
           (comp-begin (cddr f) env val? more? has-lambda? #f tail? if?))
          ((and (not more?)(not has-lambda?) tail? if?)
           (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                (comp f env #t #t has-lambda? in-lambda? tail? if?)
                (gen 'callj (length args))))
          ((eqv? f 'call/cc)
           (seq (comp-list args env has-lambda? in-lambda? #f if?)
                (comp f env #t #t has-lambda? in-lambda? tail? if?)
                (gen 'call (length args))
                (if (not val?) (gen 'pop) '())
                (if (not more?) (gen 'return) '())))
          (else
            (seq (comp-list args env has-lambda? in-lambda? tail? if?)
                 (comp f env #t #t has-lambda? in-lambda? tail? if?)
                 (gen 'call (length args))
                 (if (not val?) (gen 'pop) '())
                 (if (not more?) (gen 'return) '()))))))


(define binomial-op
  '((= neqp) (< smlp) (<= esmlp)
    (> grtp) (>= egrtp)))


(define (prim-always? x)
  (cadddr x))

(define (prim-side-effect? x)
  (cadr (cdddr x)))

(define (prim-min x)
  (cadr x))

(define (prim-max x)
  (caddr x))


(define (primitive? x env)
  (and (not (in-env? x env))
       (assoc x *primitive*)))

;;macroではtail?=#fにして末尾再帰最適化はしない。
(define (comp-lambda args body env tail? if?)
  (if tail?
      (seq (gen 'args (args-count args))
           (comp-begin body
                       (cons args env) #t #t 
                       (inner-lambda? (macroexpand body)) #t tail? if?))
      (seq (gen 'args (args-count args))
           (comp-begin body
                       (cons args env) #t #t #f #t tail? if?))))


(define (args-count x)
  (cond ((symbol? x) -1)
        ((list? x)(length x))
        (else (- (pair-length x)))))

(define (length=1? x)
  (and (list? x)
       (= (length x) 1)))

(define (mappend proc x)
  (if (null? x)
      '()
      (append (proc (car x)) (mappend proc (cdr x)))))

(define (lambda-args? x)
  (cond ((null? x) #t)
        ((symbol? x) #t)
        ((atom? x) #f)
        ((not (symbol? (car x))) #f)
        (else (lambda-args? (cdr x)))))

(define (symbol-or-list? x)
  (or (symbol? x)
      (list? x)))

(define (args-count-check x min max)
  (let ((n (length (cdr x))))
    (cond ((and (>= n min) (eq? max 'infinity)) #t)
          ((and (>= n min) (<= n max)) #t)
          (else (error "incorrect argument count " x)))))

(define (args-type-check x n pred msg)
  (if (pred (list-ref x n))
      #t
      (error msg x)))

(define (gen opcode . args)
  (list (cons opcode args)))

;;マクロ定義時環境はsyntactic-closureに保存されている。
;;展開時には展開時環境との差分を調整して位置を計算している。
(define (gen-var var env)
  (let ((p (in-env? var env)))
    (cond (p (gen 'lvar (car p) (cadr p)))
          ((identifier-free? var) (gen 'gvar (identifier->symbol var)))
          ((syntactic-closure? var)
           (let* ((var1 (syntactic-closure-expr var))
                  (env1 (syntactic-closure-env var))
                  (p1 (in-env? var1 env1)))
             (if p1
                 (gen 'lvar (+ (car p1) (- (length env) (length env1))) (cadr p1) )
                 (gen 'gvar var1))))
          (else (gen 'gvar var)))))

(define (gen-set var env)
  (let ((p (in-env? var env)))
    (if p
        (gen 'lset (car p) (cadr p))
        (gen 'gset var))))

(define (seq . code)
  (apply append code))

(define (gen-label)
  (gensym "L"))

(define (label? x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 2) #\L)))


(define (in-env? symbol env)
  (in-env-iter symbol env 0))

(define (in-env-iter symbol env i)
  (cond ((null? env) #f)
        ((in-args? symbol (car env)) (list i (nth-args symbol (car env))))
        (else (in-env-iter symbol (cdr env) (+ i 1)))))

(define (in-args? symbol args)
  (cond ((null? args) #f)
        ((and (symbol? args) (eq? symbol args)) #t)
        ((symbol? args) #f)
        ((eq? symbol (car args)) #t)
        (else (in-args? symbol (cdr args)))))


(define (nth-args symbol args)
  (cond ((and (symbol? args) (eq? symbol args)) 0)
        ((eq? symbol (car args)) 0)
        (else (+ 1 (nth-args symbol (cdr args))))))


(define (inner-lambda? x)
  (cond ((null? x) #f)
        ((atom? x) #f)
        ((vector? x) #f)
        ((eq? (car x) 'lambda) #t)
        (else (or (inner-lambda? (car x))
                  (inner-lambda? (cdr x))))))


;;内挿表現から前置表現へ変換する。

(define (arg1 f)
  (cadr f))

(define (arg2 f)
  (caddr f))

(define (arg3 f)
  (cadddr f))

(define (op f)
  (car f))


(define (opcode op)
  (case op
        ((+) '+)((-) '-)((/) '/)((*) '*)((^) '^)
        ((sin) 'sin)((cos) 'cos)((exp) 'exp)((log) 'log)
        ((sinh) 'sinh)((cosh) 'cosh)
        (else (error "opecode else: " op))))

(define (weight op)
  (case op
        ((+) 1)((-) 1)((/) 2)((*) 3)((^) 4)
        ((sin) 6)((cos) 6)((exp) 6)((log) 6)
        ((cosh) 6)((sinh) 6)
        (else 9)))

(define (infix->prefix fmla)
  (infip fmla))

(define (infip fmla)
  (if (atom? fmla) fmla (inf1 fmla '() '())))

(define (inf1 fmla optr opln)
  (if (or (< (weight (op fmla)) 5)
          (> (weight (op fmla)) 7))
      (inf2 (cdr fmla) optr (cons (infip (car fmla)) opln))
      (inf3 (cddr fmla)
            optr
            (cons (list (op fmla) (infip (arg1 fmla))) opln))))

(define (inf2 fmla optr opln)
  (cond ((and (null? fmla) (null? optr))
         (car opln))
        ((and (not (null? fmla))
              (or (null? optr)
                  (> (weight (car fmla))
                     (weight (car optr)))))
         (inf1 (cdr fmla) (cons (car fmla) optr) opln))
        (else (inf2 fmla
                    (cdr optr)
                    (cons (list (opcode (car optr))
                                (cadr opln)
                                (car opln))
                          (cddr opln))))))

(define (inf3 fmla optr opln)
  (cond ((and (null? fmla) (null? opln))
         (car opln))
        ((and (not (null? fmla))
              (or (null? optr)
                  (> (weight (car fmla))
                     (weight (cadr fmla)))))
         (inf1 (cdr fmla) (cons (car fmla) optr) opln))
        (else (inf2 fmla optr opln)))) ;原著修正




;;アセンブラ

(define (assemble ls)
  (pass2 ls (pass1 ls))) 


(define (pass1 x)
  (pass1-iter x 1 '()))

(define (pass1-iter ls pc labels)
  (cond ((null? ls) labels)
        ((label? (car ls))
         (pass1-iter (cdr ls) pc (cons (cons (car ls) pc) labels)))
        (else
          (pass1-iter (cdr ls) (+ pc (op-count (car ls))) labels))))

(define (pass2 x labels)
  (pass2-iter x 1 '() labels))

(define (pass2-iter ls pc obj labels)
  (cond ((null? ls) (reverse obj))
        ((label? (car ls)) (pass2-iter (cdr ls) pc obj labels))
        (else
          (pass2-iter (cdr ls)
                      (+ pc (op-count (car ls)))
                      (append (reverse (mnemonic->code (car ls) labels pc))
                              obj)
                      labels))))

(define (mnemonic->code x labels pc)
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
        ((eqv? (car x) 'fn) (list (op-code (car x)) (cadr x) (assemble (caddr x))))
        ((eqv? (car x) 'save) (list (op-code (car x))))
        ((eqv? (car x) 'prim) (list (op-code (car x)) (cadr x) (caddr x)))
        ((eqv? (car x) 'def) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'defm) (list (op-code (car x)) (cadr x)))
        ((eqv? (car x) 'defh) (list (op-code (car x))))
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

;;(symbol-name args-min args-max always side-effect)
(define *primitive*
  '((car 1 1 #t #f)
    (cdr 1 1 #t #f)
    (cons 2 2 #t #f)
    (caar 1 1 #t #f)
    (cdar 1 1 #t #f)
    (cddr 1 1 #t #f)
    (cadr 1 1 #t #f)
    (caaar 1 1 #t #f)
    (cdaar 1 1 #t #f)
    (cadar 1 1 #t #f)
    (caadr 1 1 #t #f)
    (cddar 1 1 #t #f)
    (caddr 1 1 #t #f)
    (cdadr 1 1 #t #f)
    (cdddr 1 1 #t #f)
    (caaaar 1 1 #t #f)
    (cdaaar 1 1 #t #f)
    (cadaar 1 1 #t #f)
    (caadar 1 1 #t #f)
    (caaadr 1 1 #t #f)
    (cddaar 1 1 #t #f)
    (caddar 1 1 #t #f)
    (caaddr 1 1 #t #f)
    (cdaadr 1 1 #t #f)
    (cdadar 1 1 #t #f)
    (cadddr 1 1 #t #f)
    (cdaddr 1 1 #t #f)
    (cddadr 1 1 #t #f)
    (cdddar 1 1 #t #f)
    (cddddr 1 1 #t #f)
    (assq 2 2 #t #f)
    (assv 2 2 #t #f)
    (assoc 2 2 #t #f)
    (memq 2 2 #t #f)
    (memv 2 2 #t #f)
    (member 2 2 #t #f)
    (reverse 1 1 #t #f)
    (reverse! 1 1 #t #t)
    (list-tail 1 1 #t #f)
    (list-ref 2 2 #t #f)
    (list-set! 3 3 #t #t)
    (append 2 2 #t #f)
    (append! 2 infinity #t #t)
    (set-car! 2 2 #t #f)
    (set-cdr! 2 2 #t #f)
    (list 0 infinity #t #f)
    (length 1 1 #t #f)
    (pair-length 1 1 #t #f)
    (last 1 1 #t #f)
    (butlast 1 1 #t #f)
    (newline 0 1 #t #t)
    (display 1 2 #t #t)
    (write 1 2 #t #t)
    (write-char 1 1 #t #t)
    (null? 1 1 #t #f)
    (list? 1 1 #t #f)
    (pair? 1 1 #t #f)
    (atom? 1 1 #t #f)
    (eq? 1 infinity #t #f)
    (eqv? 1 infinity #t #f)
    (equal? 1 infinity #t #f)
    (boolean? 1 1 #t #f)
    (procedure? 1 1 #t #f)
    (number? 1 1 #t #f)
    (integer? 1 1 #t #f)
    (real? 1 1 #t #f)
    (rational? 1 1 #t #f)
    (complex? 1 1 #t #f)
    (exact? 1 1 #t #f)
    (inexact? 1 1 #t #f)
    (symbol? 1 1 #t #f)
    (string? 1 1 #t #f)
    (char? 1 1 #t #f)
    (bignum? 1 1 #t #f)
    (vector? 1 1 #t #f)
    (macro? 1 1 #t #f)
    (macro-name? 1 1 #t #f)
    (hygienic? 1 1 #t #f)
    (hygienic-name? 1 1 #t #f)
    (zero? 1 1 #t #f)
    (+ 0 infinity #t #f)
    (- 1 infinity #t #f)
    (* 0 infinity #t #f)
    (/ 1 infinity #t #f)
    (< 2 infinity #t #f)
    (<= 2 infinity #t #f)
    (> 2 infinity #t #f)
    (>= 2 infinity #t #f)
    (= 2 infinity #t #f)
    (sin 1 1 #t #f)
    (cos 1 1 #t #f)
    (tan 1 1 #t #f)
    (asin 1 1 #t #f)
    (acos 1 1 #t #f)
    (atan 1 2 #t #f)
    (log 1 1 #t #f)
    (exp 1 1 #t #f)
    (sqrt 1 1 #t #f)
    (expt 2 2 #t #f)
    (not 1 1 #t #f)
    (odd? 1 1 #t #f)
    (even? 1 1 #t #f)
    (floor 1 1 #t #f)
    (ceiling 1 1 #t #f)
    (truncate 1 1 #t #f)
    (round 1 1 #t #f)
    (numerator 1 1 #t #f)
    (denominator 1 1 #t #f)
    (positive? 1 1 #t #f)
    (negative? 1 1 #t #f)
    (abs 1 1 #t #f)
    (max 1 infinity #t #f)
    (min 1 infinity #t #f)
    (real-part 1 1 #t #f)
    (imag-part 1 1 #t #f)
    (magnitude 1 1 #t #f)
    (angle 1 1 #t #f)
    (make-rectangular 2 2 #t #f)
    (make-polar 2 2 #t #f)
    (exact->inexact 1 1 #t #f)
    (exact 1 1 #t #f)
    (inexact->exact 1 1 #t #f)
    (inexact 1 1 #t #f)
    (remainder 2 2 #t #f)
    (modulo 2 2 #t #f)
    (quotient 2 2 #t #f)
    (gcd 0 2 #t #f)
    (lcm 0 2 #t #f)
    (char=? 2 2 #t #f)
    (char>? 2 2 #t #f)
    (char>=? 2 2 #t #f)
    (char<? 2 2 #t #f)
    (char<=? 2 2 #t #f)
    (char-ci=? 2 2 #t #f)
    (char-ci>? 2 2 #t #f)
    (char-ci>=? 2 2 #t #f)
    (char-ci<? 2 2 #t #f)
    (char-ci<=? 2 2 #t #f)
    (char-alphabetic? 1 1 #t #f)
    (char-numeric? 1 1 #t #f)
    (char-whitespace? 1 1 #t #f)
    (char-upper-case? 1 1 #t #f)
    (char-lower-case? 1 1 #t #f)
    (char->integer 1 1 #t #f)
    (integer->char 1 1 #t #f)
    (char-upcase 1 1 #t #f)
    (char-downcase 1 1 #t #f)
    (string-append 1 infinity #t #f)
    (number->string 1 2 #t #f)
    (string->number 1 2 #t #f)
    (string=? 1 1 #t #f)
    (string>? 1 1 #t #f)
    (string>=? 1 1 #t #f)
    (string<? 1 1 #t #f)
    (string<=? 1 1 #t #f)
    (string-ci=? 1 1 #t #f)
    (string-ci>? 1 1 #t #f)
    (string-ci>=? 1 1 #t #f)
    (string-ci<? 1 1 #t #f)
    (string-ci<=? 1 1 #t #f)
    (string->symbol 1 1 #t #f)
    (symbol->string 1 1 #t #f)
    (string-length 1 1 #t #f)
    (make-string 1 1 #t #f)
    (string 1 1 #t #f)
    (string-ref 2 2 #t #f)
    (string-set! 3 3 #t #t)
    (substring 1 1 #t #f)
    (string->list 1 1 #t #f)
    (list->string 1 1 #t #f)
    (string-copy 1 1 #t #f)
    (string-fill! 1 1 #t #t)
    (make-vector 1 2 #t #t)
    (vector-set! 3 3 #t #t)
    (vector 0 infinity #t #t)
    (vector-ref 2 2 #t #f)
    (vector-length 1 1 #t #f)
    (vector-fill! 2 2 #t #t)
    (vector->list 1 1 #t #f)
    (list->vector 1 1 #t #f)
    (read 0 1 #t #f)
    (load 1 1 #t #f)
    (open-input-file 1 1 #t #t)
    (open-output-file 1 1 #t #t)
    (close-input-port 1 1 #t #t)
    (close-output-port 1 1 #t #t)
    (eof-object? 1 1 #t #f)
    (input-port? 1 1 #t #f)
    (output-port? 1 1 #t #f)
    (current-input-port 1 1 #t #f)
    (current-output-port 1 1 #t #f)
    (read-char 1 1 #t #f)
    (peek-char 1 1 #t #f)
    (read-line 0 0 #t #f)
    (read-string 1 1 #t #f)
    (char-ready? 1 1 #t #f)
    (exit 0 0 #t #t)
    (gensym 0 1 #t #f)
    (apply 2 infinity #t #t)
    (primitive-name? 1 1 #t #f)
    (macroexpand-1 1 1 #t #f)
    (macroexpand 1 1 #t #f)
    (addr 1 1 #t #f)
    (entity-addr 1 1 #t #f)
    (undefined 0 0 #t #f)
    (step 1 1 #t #t)
    (vm2-step 1 1 #t #t)
    (vm1 1 1 #t #t)
    (vm2 1 1 #t #t)
    (dump 1 2 #t #t)
    (addr-prt 1 1 #t #t)
    (gbc 0 1 #t #t)
    (room 0 0 #t #f)
    (vmcode 1 1 #t #f)
    (env 1 1 #t #f)
    (timer-set 0 0 #t #t)
    (timer-get 0 0 #t #t)
    (timer-gbc 0 0 #t #t)
    (current-second 0 0 #t #f)
    (current-jiffy 0 0 #t #f)
    (jiffies-per-second 0 0 #t #f)
    (eval 1 1 #t #f)
    (load 1 1 #t #t)
    (error 1 infinity #t #t)
    (flush 0 1 #t #t)
    (set-trace 0 infinity #t #t)
    (set-untrace 0 infinity #t #t)
    (current-module 0 0 #t #f)
    (transfer 1 1 #t #f)
    (debug 1 1 #t #t)
    (profiler 1 1 #t #t)
    (lambda/asm 2 2 #t #f)
    (values 0 infinity #t #t)
    (sys-cont-room 1 1 #t #t)
    (make-syntactic-closure 3 3 #t #t)
    (syntactic-closure-expr 1 1 #t #f)
    (syntactic-closure-env 1 1 #t #f)
    (syntactic-closure-freevar 1 1 #t #f)
    (symbol->identifier 1 1 #t #f)
    (identifier->symbol 1 1 #t #f)
    (syntactic-closure? 1 1 #t #f)
    (identifier? 1 1 #t #f)
    (identifier-bind! 2 2 #t #t)
    (identifier-free? 1 1 #t #f)
    (identifier-bound? 1 1 #t #f)
    (identifier-bound 1 1 #t #f)
    (global-bound? 1 1 #t #f)
    (inspect 0 0 #t #t)
    (exact-integer? 1 1 #t #f)
    (file-exists? 1 1 #t #f)
    (system 1 1 #t #t)
    (flush-output-port 0 1 #t #t)
    (infinity? 1 1 #t #f)
    (finity? 1 1 #t #f)
    (nan? 1 1 #t #f)
    (square 1 1 #t #f)
    (bytevector? 1 1 #t #f)
    (make-bytevector 1 2 #t #t)
    (bytevector 0 infinity #t #t)
    (bytevector-length 1 1 #t #f)
    (bytevector-u8-set! 3 3 #t #f)
    (bytevector-u8-ref 2 2 #t #f)
    (bytevector-copy 1 3 #t #t)
    (bytevector-copy! 3 5 #t #t)
    (bytevector-append 0 infinity #t #f)
    (command-line 0 0 #t #f)
    (get-environment-variable 1 1 #t #f)
    (get-environment-variables 0 0 #t #f)
    (get-car 1 1 #t #f)
    ))

;;コンパイル
(define (compile-file x)
  (let* ((inf (string-append x ".scm"))
         (outf (string-append x ".o"))
         (inp (open-input-file inf))
         (outp (open-output-file outf)))
    (compile-file1 (read inp) inp outp)
    (close-input-port inp)
    (close-output-port outp)
    #t))

(define (compile-file1 sexp inp outp) 
  (cond ((eof-object? sexp) #t)
        (else (write (assemble (compile sexp)) outp)
              (newline outp)
              (compile-file1 (read inp) inp outp))))



;;継続
(define ncall/cc 
  (lambda/asm 1 '((args 1) (catch) (lvar 0 0) (call 1) (return))))

;;多値
(define call-with-values
  (lambda/asm 2 '((args 2) (lvar 0 0) (call 0) (adapt) (lvar 0 1) (call 0) (return))))

;;dynamic-wind 
;;Kent dybvig p93 参照
(define winders '())

(define (dynamic-wind before body after)
  (before)
  (set! winders (cons (cons before after) winders))
  (let ((ans (body)))
    (set! winders (cdr winders))
    (after)
    ans))


(define (common-tail x y)
  (reverse (common-tail1 (reverse x) (reverse y))))

(define (common-tail1 x y)
  (cond ((null? x) '())
        ((null? y) '())
        ((not (eq? (car x)(car y))) '())
        (else (cons (car x) (common-tail1 (cdr x) (cdr y))))))

(define (do-wind new)
  (let ((tail (common-tail new winders)))
    (do-wind-after winders tail)
    (do-wind-before (reverse new) (reverse tail))
    (set! winders new)))

(define (do-wind-after old tail)
  (if (equal? old tail)
      #t
      (begin
        ((cdar old))
        (do-wind-after (cdr old) tail))))

(define (do-wind-before new tail)
  (cond ((and (null? new) (null? tail)) #t)
        ((null? tail) ((caar new))(do-wind-before (cdr new) tail))
        (else (do-wind-before (cdr new) (cdr tail)))))

(define (call/cc f)
  (ncall/cc
    (lambda (k)
      (f (let ((save winders))
           (lambda x
             (if (not (eq? save winders))
                 (do-wind save)
                 (undefined))
             (apply k x)))))))


(define call-with-current-continuation call/cc)

;;Hygienic macro


(define (match p f lits)
  (match1 p f lits '()))

(define (match1 p f lits vars)
  ;(display p)(newline)(display f)(newline)
  (cond ((and (null? p) (null? f)) vars)
        ((and (symbol? p) (memv p lits) (not(eqv? p f))) #f)
        ((and (symbol? f) (memv f lits) (not(eqv? p f))) #f)
        ((symbol? p) (cons (cons p f) vars))
        ((ellipsis? p) (match1 (cddr p)
                               (list-take-right f (length (cddr p)))
                               lits
                               (cons (cons (car p) (list-take f (- (length f) (length (cddr p))))) vars)))
        ((and (vector? p) (vector? f)) 
         (match1 (vector->list p) (vector->list f) lits vars))
        ((and (null? p) (not(null? f))) #f)
        ((and (not(null? p)) (null? f)) #f)
        ((and (atom? p) (not(equal? p f))) #f)
        (else
         (let ((r1 (match1 (car p) (car f) lits vars))
               (r2 (match1 (cdr p) (cdr f) lits vars)))
                (if (and r1 r2)
                    (append r1 r2)
                    #f)))))



;;例((1 2)(3 4)(5 6)) -> ((1 3 5)(2 4 6)) 
(define (transpose ls) 
  (define (iter m n) 
    (if (= m n) 
        '() 
        (cons (map (lambda (x) (list-ref x m)) ls) 
              (iter (+ m 1) n)))) 
  (iter 0 (length (car ls)))) 

;;省略子
(define (ellipsis? x)
  (and (list? x)
       (>= (length x) 2)
       (or (identifier? (car x)) (symbol? (car x)))
       (eqv? (cadr x) '...)))

;;複合省略子
(define (ellipsises? x)
  (and (list x)
       (>= (length x) 2)
       (list? (car x))
       (eqv? (cadr x) '...)))

;;ベクタ省略子
(define (vec-ellipsis? x)
  (and (vector? x)
       (eqv? (vector-ref x (- (vector-length x) 1)) '...)))


(define (list-take ls n)
  (if (= n 0)
      '()
      (cons (car ls) (list-take (cdr ls) (- n 1)))))

(define (list-take-right ls n)
  (list-drop ls (- (length ls) n)))

(define (list-drop ls n)
  (if (= n 0)
      ls
      (list-drop (cdr ls) (- n 1))))


(define (fail? x)
  (eq? x 'fail))

(define (subst-to-identifier x env lits)
  (cond ((null? x) '())
        ((and (symbol? x)(local-bound? x env))
         (make-syntactic-closure env '() x))
        ((and (symbol? x)(global-bound? x))
         (identifier-bind! (symbol->identifier x) x))
        ((and (symbol? x)(memv x lits))
         (identifier-bind! (symbol->identifier x) x))
        ((symbol? x)
         (symbol->identifier x))
        ((atom? x) x)
        ((vector? x)
         (list->vector (subst-to-identifier (vector->list x) env lits)))
        (else (cons (subst-to-identifier (car x) env lits)
                    (subst-to-identifier (cdr x) env lits)))))

(define (local-bound? x env)
  (cond ((null? env) #f)
        ((member x (car env)) #t)
        (else (local-bound? x (cdr env)))))

(define (subst-pattern-vars x pat)
  (cond ((null? x) '())
        ((and (identifier? x)(assv x pat))
         (identifier-bind! x (cdr (assv x pat))))
        ((atom? x) x)
        ((vector? x)
         (list->vector (subst-pattern-vars (vector->list x) pat)))
        (else (cons (subst-pattern-vars (car x) pat)
                    (subst-pattern-vars (cdr x) pat)))))



(define (subst-let-vars x)
  (subst-let-vars1 x (scan-let-vars x '())))

(define (subst-let-vars1 x a-list)
  (cond ((null? x) '())
        ((and (identifier? x)(identifier-free? x)(assv x a-list))
         (identifier-bind! x (cdr (assv x a-list))))
        ((atom? x) x)
        ((vector? x) x)
        (else (cons (subst-let-vars1 (car x) a-list)
                    (subst-let-vars1 (cdr x) a-list)))))


(define (scan-let-vars x v-list)
  (cond ((null? x) (map (lambda (x) (cons x (gensym))) v-list))
        ((atom? x) '())
        ((vector? x) '())
        ((and (lambda? x)(atom? (cadr x)))
         (scan-let-vars (cddr x) (cons (cadr x) v-list)))
        ((and (lambda? x)(list? (cadr x))(not (null? (cadr x))))
         (scan-let-vars (cddr x) (append (cadr x) v-list)))
        ((and (lambda? x)(pair? (cadr x))(not (null? (cadr x))))
         (scan-let-vars (caddr x) (pair->list (cadr x))))
        ((and (let? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (let*? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (letrec? x)(not (null? (cadr x)))) (scan-let-vars (cddr x) (car (transpose (cadr x)))))
        ((and (named-let? x)(not (null? (caddr x))))
         (scan-let-vars (cdddr x) 
                        (append (car (transpose (caddr x))) 
                                (cons (cadr x) v-list))))
        (else (scan-let-vars (cdr x) v-list))))

(define (lambda? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'lambda)))

(define (let? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'let) (not (identifier? (cadr x)))))

(define (named-let? x)
  (and (list? x) (>= (length x) 4) (eqv? (car x) 'let) (identifier? (cadr x))))

(define (letrec? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'letrec)))

(define (let*? x)
  (and (list? x) (>= (length x) 3) (eqv? (car x) 'let*)))


(define (subst-from-identifier x)
  (cond ((null? x) '())
        ((identifier-free? x) (identifier->symbol x))
        ((identifier? x) (identifier-bound x))
        ((atom? x) x)
        ((vector? x)
         (list->vector (subst-from-identifier (vector->list x))))
        ((and (= (length x) 2)(ellipsis? x))
         (identifier-bound (car x)))
        ;;(x ...)
        ((and (> (length x) 2)(ellipsis? (list-take x 2)))
         (append (identifier-bound (car x))
                 (subst-from-identifier (cddr x))))
        ;;ex(x ... n)
        (else (cons (subst-from-identifier (car x))
                    (subst-from-identifier (cdr x))))))




(define (expand-template x vars comp-env lits)
  (let ((a #f)(b #f)(c #f)(d #f))
    (set! a (subst-to-identifier x comp-env lits))
    ;(display a)(newline)
    (set! b (subst-pattern-vars a vars))
    ;(display b)(newline)
    (set! c (subst-let-vars b))
    ;(display c)(newline)
    (set! d (subst-from-identifier c))
    ;(display d)(newline)
    d))
    
  


(define (expand x y lits comp-env)
  (let* ((pat (caar x))
         (temp (cadar x))
         (vars (match pat y lits)))
    (cond (vars (expand-template temp vars comp-env lits))
          ((null? (cdr x)) (error "syntax-rules match fail " y))
          (else (expand (cdr x) y lits comp-env)))))


;;Normal macros
;;Scheme macros written by M.hiroi modified for Normal by k.sasagawa
;;condなどの制御構造はマクロで与えられる。
;;M.Hiroiさんのmicro Schemeのものを使わせていただいています。
;;Normalの起動時に読み込まれコンパイルされる。

(define map 
  (lambda (f ls . more)
    ((lambda (map1 map-more)
       (set! map1 (lambda (ls)
                    (if (null? ls) 
                        (quote ()) 
                        (cons (f (car ls))
                              (map1 (cdr ls))))))
       (set! map-more (lambda (ls more)
                        (if (null? ls)
                            (quote ()) 
                            (cons (apply f (car ls) (map car more))
                                  (map-more (cdr ls) (map cdr more))))))
       (if (null? more)
           (map1 ls)
           (map-more ls more)))
      (undefined) (undefined))))

(define-macro and
  (lambda args
    (if (null? args)
        #t
        (if (null? (cdr args))
            (car args)
            `(if ,(car args) (and ,@(cdr args)) #f)))))

(define-macro or
  (lambda args
    (if (null? args)
        #f
        (if (null? (cdr args))
            (car args)
            `(if ,(car args) ,(car args) (or ,@(cdr args)))))))


(define-macro let
  (lambda (args . body)
    (if (null? args)
        `((lambda () ,@body))
        (if (pair? args)
            `((lambda ,(map car args) ,@body) ,@(map cadr args))
            ; named-let
            `(letrec ((,args (lambda ,(map car (car body)) ,@(cdr body))))
               (,args ,@(map cadr (car body))))))))


(define-macro let*
  (lambda (args . body)
    (if (null? args)
        `((lambda () ,@body))
        (if (null? (cdr args))
            `(let (,(car args)) ,@body)
            `(let (,(car args)) (let* ,(cdr args) ,@body))))))

(define-macro letrec
  (lambda (args . body)
    (let ((vars (map car args))
          (vals (map cadr args)))
      `(let ,(map (lambda (x) `(,x '*undef*)) vars)
            ,@(map (lambda (x y) `(set! ,x ,y)) vars vals)
            ,@body))))

(define-macro cond
  (lambda args
    (if (null? args)
        `(undefined)
        (if (null? (cdar args))
            `(let ((*val* ,(caar args)))
               (if *val* *val* (cond ,@(cdr args))))
            (if (eq? (cadar args) '=>)
            `(let ((*val* ,(caar args)))
               (if *val* (,(caddar args) *val*)))
                (if (eqv? (caar args) 'else)
                    (if (length=1? (cdar args))
                        (cadar args)
                        `(begin ,@(cdar args)))
                    (if (length=1? (cdar args))
                        `(if ,(caar args)
                           ,@(cdar args)
                           (cond ,@(cdr args)))
                        `(if ,(caar args)
                           (begin ,@(cdar args))
                           (cond ,@(cdr args))))))))))

(define-macro case
  (lambda (key . args)
    (if (null? args)
        `(undefined)
        (if (eq? (caar args) 'else)
            `(begin ,@(cdar args))
            `(if (memv ,key ',(caar args))
                 (begin ,@(cdar args))
                 (case ,key ,@(cdr args)))))))


(define-macro do
  (lambda (var-form test-form . args)
    (let ((vars (map car var-form))
          (vals (map cadr var-form))
          (step (map cddr var-form)))
      `(letrec ((loop (lambda ,vars
                        (if ,(car test-form)
                            (begin ,@(cdr test-form))
                          (begin
                            ,@args
                            (loop ,@(map (lambda (x y)
                                             (if (null? x) y (car x)))
                                           step
                                           vars)))))))
        (loop ,@vals)))))


;;kent dyvig p89
(define for-each
  (lambda (f ls . more)
    (do ((ls ls (cdr ls)) (more more (map cdr more)))
        ((null? ls))
        (apply f (car ls) (map car more)))
    (undefined)))