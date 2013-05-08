;;’x‰„•]‰¿‚ÌƒeƒXƒg

(import (scheme lazy))

(define (tarai x y z)
  (if (<= (force x) (force y))
      (force y)
      (tarai (delay (tarai (- x 1) y z))
             (delay (tarai (- y 1) z x))
             (delay (tarai (- z 1) x y)))))


