
;; �჌�x���ȗ�O���J�j�Y���̊T�O�I�Ȏ���
;; %xh�͗�O�n���h���̃��X�g

(define (current-exception-handler) (car %xh))

(define (raise exn)
  (receive r ((car %xh) exn)
    (when (uncontinuable-exception? exn)
      (set! %xh (cdr %xh))
      (raise (make-error "returned from uncontinuable exception")))
    (apply values r)))

(define (with-exception-handler handler thunk)
  (let ((prev %xh))
    (dynamic-wind
      (lambda () (set! %xh (cons handler %xh)))
      thunk
      (lambda () (set! %xh prev)))))
