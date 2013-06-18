;;record test

(import (normal test))

(define-record-type pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))


(test* "(pare? (kons 1 2))" #t (pare? (kons 1 2)))
(test* "(kar (kons 1 2))" 1 (kar (kons 1 2)))
(test* "(kdr (kons 1 2))" 2 (kdr (kons 1 2)))
(test* "(let ((k (kons 1 2))) (set-kar! k 3) (kar k))" 3 
       (let ((k (kons 1 2)))
         (set-kar! k 3)
         (kar k)))

