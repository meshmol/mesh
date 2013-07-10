

(import (mesh test))

(test* "1 / 0.0" +inf.0 (/ 1 0.0))
(test* "1 / -0.0" -inf.0 (/ 1 -0.0))
(test* "-1 / 0.0" -inf.0 (/ -1 0.0))
(test* "-1 / -0.0" +inf.0 (/ -1 -0.0))
(test* "2.2 / 0.0" +inf.0 (/ 2.2 0.0))
(test* "2.2 / -0.0" -inf.0 (/ 2.2 -0.0))
(test* "-2.2 / 0.0" -inf.0 (/ -2.2 0.0))
(test* "-2.2 / -0.0" +inf.0 (/ -2.2 -0.0))
(test* "0 / 0.0" +nan.0 (/ 0 0.0))
(test* "0 / -0.0" +nan.0 (/ 0 -0.0)) ;;???
(test* "-inf.0 + +inf.0" +nan.0 (+ -inf.0 +inf.0))
(test* "+inf.0 + -inf.0" +nan.0 (+ +inf.0 -inf.0))
(test* "+inf.0 + +inf.0" +inf.0 (+ +inf.0 +inf.0))
(test* "-inf.0 + -inf.0" -inf.0 (+ -inf.0 -inf.0))
(test* "0+0i / 0.0" +nan.0 (/ 0+0i 0.0))
(test* "1+2i / 0.0" +inf.0+inf.0i (/ 1+2i 0.0))

