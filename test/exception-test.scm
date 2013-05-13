

(with-exception-handler
  (lambda (x)
    (display "something went wrong\n")
    42)
  (lambda ()
    (+ 1 (raise-continuable 'an-error))))

(with-exception-handler
  (lambda (x)
    (display "something went wrong\n"))
  (lambda ()
    (+ 1 (raise 'an-error))))

(call-with-current-continuation
  (lambda (k)
    (with-exception-handler
      (lambda (x)
        (display "condition: ")
        (write x)
        (newline)
        (k 'exception))
      (lambda ()
        (+ 1 (raise 'an-error))))))
