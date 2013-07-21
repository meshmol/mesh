;;Shiroさんのテストコード。一部改編 動作不良

  
(let ((cc #f)
      (count #f))
  
  (call/cc
    (lambda (return)
      (dynamic-wind
        (lambda () (display "A"))
        (lambda () 
          (set! count (call/cc (lambda (c) (set! cc c) 0)))
          (display "B")
          (if (> count 3) (return #f)))
        (lambda () (display "C")))
      (dynamic-wind 
        (lambda () (display "D"))
        (lambda () (cc (+ count 1)))
        (lambda () (display "E"))))))
      
      



;;R5RS より
(let ((path '())
      (c #f))
  (let ((add (lambda (s)
               (set! path (cons s path)))))
    (dynamic-wind
      (lambda () (add 'connect))
      (lambda ()
        (add (call/cc
               (lambda (c0)
                 (set! c c0)
                 'talk1))))
      (lambda () (add 'disconnect)))
    (if (< (length path) 4)
        (c 'talk2)
        (reverse path))))

