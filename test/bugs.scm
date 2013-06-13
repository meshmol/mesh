;; bugs

;;; norm> (/ 29 3 7)
;;; 329/21
;;; norm> 

; definition of pi
(define pi (* 4 (atan 1.0)))

; degree -> radian
(define (radian deg)
  (* deg (/ pi 180.0)))

; free fall time
(define (ff-time vy)
  (/ (* 2.0 vy) 9.8))

; horizontal distance 
(define (dx vx t)
  (* vx t))

; distance
(define (distance v ang)
  (dx
   (* v (cos (radian ang)))                     ; vx
   (ff-time (* v (sin (radian ang))))))         ; t

;;; norm>  (distance 40 30)
;;; 0.0
;;; norm> 
;;; > (distance 40 30)
;;; 141.39190265868385

(define (throw v a)
  (let ((r (/ (* 4 a (atan 1.0)) 180)))
    (/ (* 2 v v (cos r) (sin r)) 9.8)))

;;; norm> (throw 40 30)
;;; +inf.0
;;; norm>
;;; gosh> (throw 40 30)
;;; 141.39190265868385
;;; gosh> 
