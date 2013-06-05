

(define (*compile-file x)
  (let* ((inf (string-append x ".scm"))
         (outf (string-append x ".o"))
         (inp (open-input-file inf))
         (outp (open-output-file outf)))
    (*compile-file1 (read inp) inp outp)
    (close-input-port inp)
    (close-output-port outp)
    (display "compiled!\n")
    #t))

(define (*compile-file1 sexp inp outp) 
  (cond ((eof-object? sexp) #t)
        (else (write (assemble (compile sexp)) outp)
              (newline outp)
              (gbc)
              (*compile-file1 (read inp) inp outp))))