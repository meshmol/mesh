
(define-library (normal system)
  (import (normal compile)
          (scheme base)
          (scheme write))
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
    error
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
    lambda/asm))
  
(define-library (normal compile)
  (export compile assemble compile-file let let* cond letrec do case
          map))

(define-library (scheme base)
  (import (normal system)
          (normal compile))
  (export
    car cdr cons caar cdar cddr cadr caaar cdaar cadar
    caadr
    cddar
    caddr
    cdadr
    cdddr
    caaaar
    cdaaar
    cadaar
    caadar
    caaadr
    cddaar
    caddar
    caaddr
    cdaadr
    cdadar
    cadddr
    cdaddr
    cddadr
    cdddar
    cddddr
    assq
    assv
    assoc
    memq
    memv
    member
    reverse
    reverse!
    list-tail
    list-ref
    append
    append!
    set-car!
    set-cdr!
    list
    length
    newline
    write-char
    null?
    list?
    pair?
    atom?
    eq?
    eqv?
    equal?
    boolean?
    procedure?
    number?
    integer?
    real?
    rational?
    complex?
    exact?
    inexact?
    symbol?
    string?
    char?
    bignum?
    vector?
    macro?
    zero?
    +
    -
    *
    /
    <
    <=
    >
    >=
    =
    expt
    not
    odd?
    even?
    floor
    ceiling
    truncate
    round
    numerator
    denominator
    positive?
    negative?
    abs
    max
    min
    exact->inexact
    inexact->exact
    remainder
    modulo
    quotient
    gcd
    lcm
    string-append
    number->string
    string->number
    string=?
    string>?
    string>=?
    string<?
    string<=?
    string-ci=?
    string-ci>?
    string-ci>=?
    string-ci<?
    string-ci<=?
    string->symbol
    symbol->string
    string-length
    make-string
    string
    string-ref
    string-set!
    substring
    string->list
    list->string
    string-copy
    string-fill!
    make-vector
    vector-set!
    vector
    vector-ref
    vector-length
    vector-fill!
    vector->list
    list->vector
    open-input-file
    open-output-file
    close-input-port
    close-output-port
    eof-object?
    input-port?
    output-port?
    current-input-port
    current-output-port
    read-char
    peek-char
    char-ready?
    exit
	apply
    gbc
    eval
    values
    let let* cond letrec do case map for-each)
  (begin
    ;;kent dyvig p89
    (define for-each
      (lambda (f ls . more)
        (do ((ls ls (cdr ls)) (more more (map cdr more)))
            ((null? ls))
            (apply f (car ls) (map car more)))
        (undefined))))
)



(define-library (scheme inexact)
  (export
    sin
    cos
    tan
    asin
    acos
    atan
    log
    exp
    sqrt))

(define-library (scheme complex)
  (export
    real-part
    imag-part
    magnitude
    angle
    make-rectangular
    make-polar))

(define-library (scheme load)
  (export load))

(define-library (scheme read)
  (export read))

(define-library (scheme write)
  (export display write))

(define-library (scheme char)
  (export
    char=?
    char>?
    char>=?
    char<?
    char<=?
    char-ci=?
    char-ci>?
    char-ci>=?
    char-ci<?
    char-ci<=?
    char-alphabetic?
	char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char->integer
    integer->char
    char-upcase
    char-downcase))

(define-library (scheme lazy)
  (export delay force)
  (import (scheme base))
  (begin
    ;;; ;;’x‰„•]‰¿
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
          with-output-to-file)
  (begin
    ;;ƒtƒ@ƒCƒ‹
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
    ))


(import (scheme base))
(import (scheme load))
(import (scheme write))
(import (scheme read))


  