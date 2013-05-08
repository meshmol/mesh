;;éáì°Ç≥ÇÒÇÃÉRÅ[Éh
;;; This function is re-assigned in `choose' and `fail' itself.
(define fail #f)

;;; function for nondeterminsm
(define (choose . ls)
  (if (null? ls)
      (fail)
      (let ((fail0 fail))
        (call/cc
          (lambda (cc)
            (set! fail
                  (lambda ()
                    (set! fail fail0)
                    (cc (apply choose (cdr ls)))))
            (cc (car ls)))))))

;;; write following at the end of file
;;; to initialize the value of the fail.
(call/cc
  (lambda (cc)
    (set! fail
          (lambda ()
            (cc 'no-choise)))))


