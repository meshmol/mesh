
(define-library (normal system)
  (export
    pair-length
    last
    butlast
    sys-code
    sys-env
    sys-timer-set
    sys-timer-get
    sys-timer-gbc
    primitive-name?
    macroexpand-1
    macroexpand
    addr
    entity-addr
    undefined
    step
    vm2-step
    vm1
    vm2
    dump
    addr-prt
    room
    macro-name?
    hygienic-name?
    gensym
    flush
    sys-set-trace
    sys-set-untrace
    transfer
    debug
    profiler
    current-module
    sys-cont-room
    make-syntactic-closure
    symbol->identifier
    identifier->symbol
    syntactic-closure?
    identifier?
    identifier-bind!
    identifier-free?
    identifier-bound?
    identifier-bound
    global-bound?
    inspect
    lambda/asm
    system))
  
(define-library (normal compile)
  (export compile assemble compile-file map for-each and or let let* cond letrec do case
          call/cc call-with-current-continuation dynamic-wind call-with-values))

(define-library (scheme base)
  (import (normal system)
          (normal compile)
          (scheme char))
  (export
    car cdr cons caar cdar cddr cadr caaar cdaar cadar caadr cddar caddr cdadr
    cdddr caaaar cdaaar cadaar caadar caaadr cddaar caddar caaddr cdaadr cdadar
    cadddr cdaddr cddadr cdddar cddddr assq assv assoc memq memv member reverse
    reverse! list-tail list-ref append append! set-car! set-cdr! list make-list length newline
    write-char null? list? pair? atom? eq? eqv? equal? boolean? procedure? number?
    integer? real? rational? complex? exact? inexact? symbol? string? char? bignum?
    vector? macro? zero? error
    + - * / < <= > >= =
    expt not odd? even? floor ceiling truncate round numerator denominator positive?
    negative? abs max min exact->inexact inexact->exact remainder modulo quotient
    gcd lcm string-append number->string string->number
    string=? string>? string>=? string<? string<=? string-ci=? string-ci>? string-ci>=?
    string-ci<? string-ci<=? string->symbol symbol->string string-length make-string
    string string-ref string-set! substring string->list list->string string-copy string-fill!
    make-vector vector-set! vector vector-ref vector-length vector-fill! vector->list list->vector
    open-input-file open-output-file close-input-port close-output-port
    eof-object? input-port? output-port? current-input-port current-output-port
    read-char peek-char char-ready? exit apply gbc values flush-output-port square
    map for-each and or let let* cond letrec do case call/cc call-with-current-continuation
    dynamic-wind call-with-values exact-integer? when unless with-exception-handler raise
    raise-continuable bytevector? make-bytevector bytevector bytevector-length string-map vector-map)
  (begin
    (define-macro when
      (lambda (pred . true)
        `(if ,pred (begin ,@true))))
    
    (define-macro unless
      (lambda (pred . else)
        `(if (not ,pred) (undefined) (begin ,@else))))
    
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
    
))



(define-library (scheme inexact)
  (export
    sin cos tan asin acos atan log exp sqrt
    infinity? finity? nan?))

(define-library (scheme complex)
  (export
    real-part imag-part magnitude angle make-rectangular make-polar))

(define-library (scheme division)
  (import (scheme base))
  (export floor/ floor-quotient floor-remainder round/ round-quotient round-remainder 
          truncate/ truncate-quotient truncate-remainder)
  (begin
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

(define-library (scheme lazy)
  (export delay force)
  (import (scheme base))
  (begin
    ;;; ;;遅延評価
    ;;; ;;Kent Dybvig p98
    (define-macro delay
      (lambda (expr)
        `(make-promise (lambda () ,expr))))
    
    (define make-promise
      (lambda (p)
        (let ((val #f) (set? #f))
          (lambda ()
            (if (not set?)
                (let ((x (p)))
                  (if (not set?)
                      (begin (set! val x)
                             (set! set? #t))
                      '()))
                '())
            val))))
    
    (define force
      (lambda (x)
        (if (procedure? x)
            (x)
            x)))))

(define-library (scheme time)
  (import (normal system)
          (scheme base)
          (scheme write))
  (export time)
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
          (normal system))
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
(define-library (normal test)
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
(define-library (normal debug)
  (import (normal system)
          (scheme base))
  (export trace untrace debug step profiler)
  (begin
    
    (define-macro trace
      (lambda fn
        `(sys-set-trace ,@(map (lambda (x) (list 'quote x)) fn))))
    
    (define-macro untrace
      (lambda fn
        `(sys-set-untrace ,@(map (lambda (x) (list 'quote x)) fn))))))
    

(import (scheme base)
        (scheme load)
        (scheme write)
        (scheme read)
        (scheme eval)
        (scheme char))


  