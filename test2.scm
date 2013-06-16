
(define (foo)
  (define p (current-input-port))
  (define (read-char*) (peek-char p))
  (define (bar)
    (define c (read-char*))
    c)
  (bar))

