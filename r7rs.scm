#|
Normal comment is adapted to R7RS
test1
asdf
|#

;;directive test
#!fold-case

(define aB 1)
(define AB 2)
(if (eq? aB AB)
    (display 'ok)
    (display 'not))

#!no-fold-case
(define Cd 3)
(define cD 4)
(if (not(eq? Cd cD))
    (display 'ok)
    (display 'not))

