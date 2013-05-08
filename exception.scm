
;; 低レベルな例外メカニズムの概念的な実装
;; %xhは例外ハンドラのリスト

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
