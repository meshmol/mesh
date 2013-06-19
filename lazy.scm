
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

(define a
'(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (if (not (promise-done? promise))
            (promise-update! promise* promise))
        (force promise)))))


