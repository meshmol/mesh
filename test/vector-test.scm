;;ベクタテスト
(import (mesh test))

(test* "(vector 1 2 3)" #(1 2 3) (vector 1 2 3))
(define a (vector 1 2 3))
(test* "(vector-ref a 1)"  1 (vector-ref a 0))
(vector-set! a 0 5)
(test* "(vector-set a 0)" 5 (vector-ref a 0))
(test* "(list->vector '(1 2 3))" #(1 2 3) (list->vector '(1 2 3)))
(test* "(vector->list #(1 2 3))" '(1 2 3) (vector->list #(1 2 3)))
(test* "(make-vector 3 1)" #(1 1 1) (make-vector 3 1))
(vector-fill! a 5)
(test* "vector-fill!" #(5 5 5) a)


(test* "(vector-map cadr '#((a b)(d e)(g h)))" #(b e h) (vector-map cadr '#((a b)(d e)(g h))))
(test* "(vector-map + '#(1 2 3) '#(4 5 6 7 ))" #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7 )))


