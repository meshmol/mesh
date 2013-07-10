
(define-library (mesh system)
  (export
    addr addr-prt butlast current-module debug dump entity-addr flush gensym
    global-bound? hygienic? hygienic-name? identifier->symbol identifier-bind! identifier-bound
    identifier-bound? identifier-free? identifier? inspect lambda/asm last macro?
    macro-name? macroexpand macroexpand-1 make-syntactic-closure pair-length
    primitive-name? profiler room step symbol->identifier syntactic-closure?
    sys-code sys-cont-room sys-macro-room sys-env sys-set-trace sys-set-untrace sys-timer-gbc
    sys-timer-get sys-timer-set system undefined vm1 vm2 vm2-step freecell 
    syntactic-closure-expr syntactic-closure-env syntactic-closure-freevar get-car
    identifier-variable! identifier-variable? make-record record? record-set! record-ref
    identifier-ellipsis! identifier-ellipsis? sleep putprop getprop))
    
    
(define-library (mesh compile)
  (export compile comp assemble compile-file map for-each and or let let* letrec do case cond
          call/cc call-with-current-continuation dynamic-wind call-with-values winders do-wind
          macrotrace lambda if set! quote begin define define-syntax predicate))


(define-library (scheme inexact)
  (export
    sin cos tan asin acos atan log exp sqrt
    infinity? finity? nan?))

(define-library (scheme cxr)
  (export
    caaaar caaadr caaar caadar caaddr caadr caar cadaar cadar caddar cadddr caddr
    cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar
    cddddr cdddr cddr cdr))

(define-library (scheme base)
  (import (mesh system)
          (mesh compile)
          (only (scheme inexact) sqrt)
          (scheme cxr)
          (scheme char))
  (export
    * + - / < <= = > >= abs and append append! apply assoc assq assv atom? bignum?
    boolean? bytevector bytevector-append bytevector-copy bytevector-copy!
    bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector?
    call-with-current-continuation call-with-values call/cc case  ceiling char-ready?
    char? close-input-port close-output-port complex?
    cond cons current-input-port current-output-port denominator do
    dynamic-wind eof-object? eq? equal? eqv? error even? exact exact->inexact
    exact-integer-sqrt exact-integer? exact? expt floor flush-output-port for-each
    gbc gcd inexact inexact->exact inexact? input-port? integer? lcm length
    let let* letrec list list->string list->vector list-ref list-set! list-tail list?
    make-bytevector make-list make-string make-vector map max member memq memv
    min modulo negative? newline not null? number->string number? numerator odd?
    open-input-file open-output-file or output-port? pair? peek-char positive?
    procedure? quotient raise raise-continuable rational? read-char real? remainder
    reverse reverse! round set-car! set-cdr! square string string->list string->number
    string->symbol string-append string-ci<=? string-ci<? string-ci=? string-ci>=?
    string-ci>? string-copy string-fill! string-length string-map string-ref string-set!
    string<=? string<? string=? string>=? string>? string? substring symbol->string
    symbol? truncate unless values vector vector->list vector-fill! vector-length
    vector-map vector-ref vector-set! vector? when with-exception-handler
    write-char zero? read-line read-string guard define-values 
    make-parameter parameterize floor/ floor-quotient floor-remainder round/
    round-quotient round-remainder truncate/ truncate-quotient truncate-remainder
    let-values let*-values define-record-type)
  (begin
    
    (define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))
    
    (define-syntax let
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
         (letrec ((tag (lambda (name ...)
                         body1 body2 ...)))
           (tag val ...)))))

    (define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f))))

    
    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...))))))
    
    (define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))
    
    
    (define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))
    
    
    (define (string-map f . args)
      (list->string 
        (reverse 
          (string-map1 f (apply min (map string-length args)) args))))
    
    (define (string-map1 f n args)
      (if (= n 0)
          '()
          (cons (apply f (string-nth (- n 1) args))
                (string-map1 f (- n 1) args))))
    
    
    (define (string-nth n args)
      (if (null? args)
          '()
          (cons (string-ref (car args) n)
                (string-nth n (cdr args)))))
    
    (define (vector-map f . args)
      (list->vector 
        (reverse 
          (vector-map1 f (apply min (map vector-length args)) args))))
    
    
    (define (vector-map1 f n args)
      (if (= n 0)
          '()
          (cons (apply f (vector-nth (- n 1) args))
                (vector-map1 f (- n 1) args))))
    
    
    (define (vector-nth n args)
      (if (null? args)
          '()
          (cons (vector-ref (car args) n)
                (vector-nth n (cdr args)))))
    
    (define (exact-integer-sqrt k)
      (if (negative? k) (error "in exact-integer-sqrt negative " k))
      (if (not (exact? k)) (error "in exact-integer-sqrt inexact " k))
      (let* ((s (exact (floor (sqrt k))))
             (r (- k (square s))))
        (values s r)))
    
    (define (floor/ n1 n2)
      (when (zero? n2)
        (error "in floor/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor/ not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))
    
    (define (floor-quotient n1 n2)
      (when (zero? n2)
        (error "in floor-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor-quotient not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))
    
    (define (floor-remainder n1 n2)
      (when (zero? n2)
        (error "in floor-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in floor-remainder not integer " n1 n2))
      (let* ((q (floor (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))
    
    (define (round/ n1 n2)
      (when (zero? n2)
        (error "in round/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round/ not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))
    
    (define (round-quotient n1 n2)
      (when (zero? n2)
        (error "in round-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round-quotient not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))
    
    (define (round-remainder n1 n2)
      (when (zero? n2)
        (error "in round-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in round-remainder not integer " n1 n2))
      (let* ((q (round (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))
    
    (define (truncate/ n1 n2)
      (when (zero? n2)
        (error "in truncate/ devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate/ not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        (values q r)))
    
    (define (truncate-quotient n1 n2)
      (when (zero? n2)
        (error "in truncate-quotient devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate-quotient not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        q))
    
    (define (truncate-remainder n1 n2)
      (when (zero? n2)
        (error "in truncate-remainder devide by zero " n1 n2))
      (when (or (not (integer? n1)) (not (integer? n2)))
        (error "in truncate-remainder not integer " n1 n2))
      (let* ((q (truncate (/ n1 n2)))
             (r (- n1 (* n2 q))))
        r))
    
    #|
    srfi-34 Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved
    |#
    (define *current-exception-handlers*
      (list (lambda (condition)
              (error "unhandled exception" condition))))
    
    (define (with-exception-handler handler thunk)
      (with-exception-handlers (cons handler *current-exception-handlers*)
                               thunk))
    
    (define (with-exception-handlers new-handlers thunk)
      (let ((previous-handlers *current-exception-handlers*))
        (dynamic-wind
          (lambda ()
            (set! *current-exception-handlers* new-handlers))
          thunk
          (lambda ()
            (set! *current-exception-handlers* previous-handlers)))))
    
    (define (raise obj)
      (let ((handlers *current-exception-handlers*))
        (with-exception-handlers (cdr handlers)
                                 (lambda ()
                                   ((car handlers) obj)
                                   (error "handler returned"
                                          (car handlers)
                                          obj)))))
    
    (define (raise-continuable obj)
      (let ((handlers *current-exception-handlers*))
        (with-exception-handlers (cdr handlers)
                                 (lambda ()
                                   ((car handlers) obj)))))
    
    (define-syntax guard
      (syntax-rules ()
        ((guard (var clause ...) e1 e2 ...)
         ((call-with-current-continuation
            (lambda (guard-k)
              (with-exception-handler
                (lambda (condition)
                  ((call-with-current-continuation
                     (lambda (handler-k)
                       (guard-k
                         (lambda ()
                           (let ((var condition))      ; clauses may SET! var
                             (guard-aux (handler-k (lambda ()
                                                     (raise condition)))
                                        clause ...))))))))
                (lambda ()
                  (call-with-values
                    (lambda () e1 e2 ...)
                    (lambda args
                      (guard-k (lambda ()
                                 (apply values args)))))))))))))
    
    (define-syntax guard-aux
      (syntax-rules (else =>)
        ((guard-aux reraise (else result1 result2 ...))
         (begin result1 result2 ...))
        ((guard-aux reraise (test => result))
         (let ((temp test))
           (if temp 
               (result temp)
               reraise)))
        ((guard-aux reraise (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (guard-aux reraise clause1 clause2 ...))))
        ((guard-aux reraise (test))
         test)
        ((guard-aux reraise (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (guard-aux reraise clause1 clause2 ...))))
        ((guard-aux reraise (test result1 result2 ...))
         (if test
             (begin result1 result2 ...)
             reraise))
        ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (guard-aux reraise clause1 clause2 ...)))))
    
    (define-syntax define-values 
      (syntax-rules () 
        ((define-values () exp) 
         (call-with-values (lambda () exp) (lambda () 'unspecified))) 
        ((define-values (var . vars) exp) 
         (begin  
           (define var (call-with-values (lambda () exp) list)) 
           (define-values vars (apply values (cdr var))) 
           (define var (car var)))) 
        ((define-values var exp) 
         (define var (call-with-values (lambda () exp) list))))) 
    
    #|
    srfi-39 Copyright (C) Marc Feeley 2002. All Rights Reserved.
    |#
    (define make-parameter
      (lambda (init . conv)
        (let ((converter
                (if (null? conv) (lambda (x) x) (car conv))))
          (let ((global-cell
                  (cons #f (converter init))))
            (letrec ((parameter
                       (lambda new-val
                         (let ((cell (dynamic-lookup parameter global-cell)))
                           (cond ((null? new-val)
                                  (cdr cell))
                                 ((null? (cdr new-val))
                                  (set-cdr! cell (converter (car new-val))))
                                 (else ; this case is needed for parameterize
                                   (converter (car new-val))))))))
              (set-car! global-cell parameter)
              parameter)))))

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize ((expr1 expr2) ...) body ...)
         (dynamic-bind (list expr1 ...)
                       (list expr2 ...)
                       (lambda () body ...)))))
    
    (define dynamic-bind
      (lambda (parameters values body)
        (let* ((old-local
                 (dynamic-env-local-get))
               (new-cells
                 (map (lambda (parameter value)
                        (cons parameter (parameter value #f)))
                      parameters
                      values))
               (new-local
                 (append new-cells old-local)))
          (dynamic-wind
            (lambda () (dynamic-env-local-set! new-local))
            body
            (lambda () (dynamic-env-local-set! old-local))))))
    
    (define dynamic-lookup
      (lambda (parameter global-cell)
        (or (assq parameter (dynamic-env-local-get))
            global-cell)))
    
    (define dynamic-env-local '())
    
    (define dynamic-env-local-get
      (lambda () dynamic-env-local))
    
    (define dynamic-env-local-set!
      (lambda (new-env) (set! dynamic-env-local new-env)))
    
    #|
    Copyright (C) Lars T Hansen (1999). All Rights Reserved.
    |#

    (define-syntax let-values
      (syntax-rules ()
        ((let-values (?binding ...) ?body0 ?body1 ...)
         (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
        
        ((let-values "bind" () ?tmps ?body)
         (let ?tmps ?body))
        
        ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
         (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
        
        ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
         (call-with-values 
           (lambda () ?e0)
           (lambda ?args
             (let-values "bind" ?bindings ?tmps ?body))))
        
        ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
         (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
        
        
        ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
         (call-with-values
           (lambda () ?e0)
           (lambda (?arg ... . x)
             (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))
    
    (define-syntax let*-values
      (syntax-rules ()
        ((let*-values () ?body0 ?body1 ...)
         (begin ?body0 ?body1 ...))
        
        ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
         (let-values (?binding0)
                     (let*-values (?binding1 ...) ?body0 ?body1 ...)))))
    
    #|
    srfi-9
    Copyright (C) Richard Kelsey (1999). All Rights Reserved. 
    |#

    (define-syntax define-record-type
      (syntax-rules ()
        ((define-record-type type
                             (constructor constructor-tag ...)
                             predicate
                             (field-tag accessor . more) ...)
         (begin
           (define type
             (make-record-type 'type '(field-tag ...)))
           (define constructor
             (record-constructor type '(constructor-tag ...)))
           (define predicate
             (record-predicate type))
           (define-record-field type field-tag accessor . more)
           ...))))
    
    (define-syntax define-record-field
      (syntax-rules ()
        ((define-record-field type field-tag accessor)
         (define accessor (record-accessor type 'field-tag)))
        ((define-record-field type field-tag accessor modifier)
         (begin
           (define accessor (record-accessor type 'field-tag))
           (define modifier (record-modifier type 'field-tag))))))
    
    
    (define (record-type record)
      (record-ref record 0))
    
    
    (define :record-type (make-record 3))
    
    (record-set! :record-type 0 :record-type)	; Its type is itself.
    (record-set! :record-type 1 ':record-type)
    (record-set! :record-type 2 '(name field-tags))
    
    
    (define (make-record-type name field-tags)
      (let ((new (make-record 3)))
        (record-set! new 0 :record-type)
        (record-set! new 1 name)
        (record-set! new 2 field-tags)
        new))
    
    
    (define (record-type-name record-type)
      (record-ref record-type 1))
    
    (define (record-type-field-tags record-type)
      (record-ref record-type 2))
    
    
    (define (field-index type tag)
      (let loop ((i 1) (tags (record-type-field-tags type)))
        (cond ((null? tags)
               (error "record type has no such field" type tag))
              ((eq? tag (car tags))
               i)
              (else
                (loop (+ i 1) (cdr tags))))))
    
    
    (define (record-constructor type tags)
      (let ((size (length (record-type-field-tags type)))
            (arg-count (length tags))
            (indexes (map (lambda (tag)
                            (field-index type tag))
                          tags)))
        (lambda args
          (if (= (length args)
                 arg-count)
              (let ((new (make-record (+ size 1))))
                (record-set! new 0 type)
                (for-each (lambda (arg i)
                            (record-set! new i arg))
                          args
                          indexes)
                new)
              (error "wrong number of arguments to constructor" type args)))))
    
    (define (record-predicate type)
      (lambda (thing)
        (and (record? thing)
             (eq? (record-type thing)
                  type))))
    
    (define (record-accessor type tag)
      (let ((index (field-index type tag)))
        (lambda (thing)
          (if (and (record? thing)
                   (eq? (record-type thing)
                        type))
              (record-ref thing index)
              (error "accessor applied to bad value" type tag thing)))))
    
    (define (record-modifier type tag)
      (let ((index (field-index type tag)))
        (lambda (thing value)
          (if (and (record? thing)
                   (eq? (record-type thing)
                        type))
              (record-set! thing index value)
              (error "modifier applied to bad value" type tag thing)))))
    
    
    ))


(define-library (scheme complex)
  (export
    real-part imag-part magnitude angle make-rectangular make-polar))



#|
srfi-16
Copyright (C) Lars T Hansen (1999). All Rights Reserved.
|#
(define-library (scheme case-lambda)
  (import (scheme base)
          (mesh compile))
  (export case-lambda)
  (begin
    
    (define-syntax case-lambda
      (syntax-rules ()
        ((case-lambda
           (?a1 ?e1 ...)
           ?clause1 ...)
         (lambda args
           (let ((l (length args)))
             (case-lambda "CLAUSE" args l
                          (?a1 ?e1 ...)
                          ?clause1 ...))))
        ((case-lambda "CLAUSE" ?args ?l
                      ((?a1 ...) ?e1 ...)
                      ?clause1 ...)
         (if (= ?l (length '(?a1 ...)))
             (apply (lambda (?a1 ...) ?e1 ...) ?args)
             (case-lambda "CLAUSE" ?args ?l
                          ?clause1 ...)))
        ((case-lambda "CLAUSE" ?args ?l
                      ((?a1 . ?ar) ?e1 ...)
                      ?clause1 ...)
         (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...)
                      ?clause1 ...))
        ((case-lambda "CLAUSE" ?args ?l
                      (?a1 ?e1 ...)
                      ?clause1 ...)
         (let ((?a1 ?args))
           ?e1 ...))
        ((case-lambda "CLAUSE" ?args ?l)
         (error "Wrong number of arguments to CASE-LAMBDA."))
        ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
                      ?clause1 ...)
         (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...)
                      ?clause1 ...))
        ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...)
                      ?clause1 ...)
         (if (>= ?l ?k)
             (apply (lambda ?al ?e1 ...) ?args)
             (case-lambda "CLAUSE" ?args ?l
                          ?clause1 ...)))))
    ))


(define-library (scheme eval)
  (export eval))

(define-library (scheme load)
  (export load))

(define-library (scheme read)
  (export read))

(define-library (scheme write)
  (export display write))

(define-library (scheme char)
  (export
    char=? char>? char>=? char<? char<=? char-ci=? char-ci>? char-ci>=? char-ci<? char-ci<=?
    char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case?
    char->integer integer->char char-upcase char-downcase))


#|
from R7RS-Small draft9 p70
|#
(define-library (scheme lazy)
  (export delay-force delay force make-promise)
  (import (scheme base)
          (scheme cxr)
          (mesh compile))
  (begin
    (define-syntax delay-force
      (syntax-rules ()
        ((delay-force expression)
         (make-promise #f (lambda () expression)))))
    
    (define-syntax delay
      (syntax-rules ()
        ((delay expression)
         (delay-force (make-promise #t expression)))))
    
    (define (make-promise done? proc)
      (list (cons done? proc)))
    
    (define (force promise)
      (if (promise-done? promise)
          (promise-value promise)
          (let ((promise* ((promise-value promise))))
            (unless (promise-done? promise)
              (promise-update! promise* promise))
            (force promise))))
    
    
    (define (promise-done? x)
      (caar x))
    
    (define (promise-value x)
      (cdar x))
    
    (define (promise-update! new old)
      (set-car! (car old) (promise-done? new))
      (set-cdr! (car old) (promise-value new))
      (set-car! new (car old)))
    
    (define (promise? x)
      (eqv? (cadr x) 'promise))
))
    


(define-library (scheme process-context)
  (import (mesh compile))
  (export command-line get-environment-variable get-environment-variables exit emergency-exit)
  (begin
    (define (emergency-exit . obj)
      (do-wind winders)
      (exit))))

(define-library (scheme time)
  (import (mesh system)
          (scheme base)
          (scheme write))
  (export current-second current-jiffy jiffies-per-second time)
  (begin
    (define-macro time
      (lambda (expr)
        `(begin (gbc)
                (sys-timer-set)
                (display ,expr)
                (newline)
                (display "total ")
                (display (sys-timer-get))
                (display " second")
                (newline)
                (display "GC    ")
                (display (sys-timer-gbc))
                (display " second")
                (newline))))))

(define-library (scheme file)
  (export call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          file-exists?
          delete-file)
  (import (scheme base)
          (mesh system))
  (begin
    ;;ファイル
    (define (call-with-input-file filename proc)
      (let ((p (open-input-file filename)))
        (let ((v (proc p)))
          (close-input-port p)
          v)))
    
    (define (call-with-output-file filename proc)
      (let ((p (open-output-file filename)))
        (let ((v (proc p)))
          (close-output-port p)
          v)))
    
    (define (with-input-from-file filename proc)
      (let ((p (open-input-file filename)))
        (let ((v (proc)))
          (close-input-port p)
          v)))
    
    (define (with-output-to-file filename proc)
      (let ((p (open-output-file filename)))
        (let ((v (proc)))
          (close-output-port p)
          v)))
    (define (delete-file x)
      (when (not (string? x)) (error "in delete-file require string " x))
      (system (string-append "del " x)))
    ))

;;テスト用のマクロ
(define-library (mesh test)
  (import (scheme base)
          (scheme write))
  (export test*)
  (begin
    (define-macro test*
      (lambda (name expected expr)
        `(let ((result '()))
           (display "test ")
           (display ,name)
           (display ", expects ")
           (display ,expected)
           (display " ==> ")
           (set! result ,expr)
           (if (equal? result ,expected)
               (display "ok\n")
               (begin (display "ERROR: GOT ")
                      (display result)
                      (newline))))))))

;;デバッグ用のマクロ
(define-library (mesh debug)
  (import (mesh system)
          (scheme base))
  (export trace untrace debug step profiler)
  (begin
    
    (define-macro trace
      (lambda fn
        `(sys-set-trace ,@(map (lambda (x) (list 'quote x)) fn))))
    
    (define-macro untrace
      (lambda fn
        `(sys-set-untrace ,@(map (lambda (x) (list 'quote x)) fn))))))

;;Gaucheとの互換ライブラリ
;;よく使うものだけ
(define-library (mesh gauche)
  (import (only (scheme base) for-each)
          (only (scheme write) display))
  (export print)
  (begin
    (define (print . x)
      (for-each display x))))

(import (scheme base)
        (scheme cxr)
        (scheme load)
        (scheme write)
        (scheme read)
        (scheme eval)
        (scheme char)
        (scheme inexact)
        (scheme case-lambda)
        (only (mesh system) macroexpand macroexpand-1)
        (only (mesh compile) macrotrace)
        (only (scheme process-context) exit))


  