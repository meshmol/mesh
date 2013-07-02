;;;
;;; SICP}Œ`Œ¾Œê‚æ‚è   2009,12,21 by sasagawa
;;; İ’è
(import (normal kids))

(define height 400)
(define width 400)

(define (draw-line v1 v2)
  (begin (pen-up)
         (move (xcor-vect v1) (ycor-vect v1))
         (pen-down)
         (move (xcor-vect v2) (ycor-vect v2))))

(define (initial)
  (hide-turtle)
  (quadrant 1))

;;; ---------------------------------------------------------------------------
;;; g‚¢•û
;;; (initial)
;;; ((corner-split wave 3) full-frame)‚Ì‚æ‚¤‚É‚µ‚Ä•`‚«‚Ü‚·
;;; Á‚·‚Æ‚«‚É‚Í(cls)
;;

(define (demo)
  (initial)
  (cls)
  ((right-split wave 3) full-frame)
  (cls)
  ((corner-split wave 3) full-frame)
  (cls)
  ((square-limit wave 3) full-frame))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;;; –â‘è‚QD‚S‚U

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;;; –â‘è‚QD‚S‚V
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define full-frame (make-frame (make-vect 0 0)
                               (make-vect width 0)
                               (make-vect 0 height)))



(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line 
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;; –â‘è‚QD‚S‚X
(define wave
  (segments->painter
    (list (make-segment (make-vect 0.000 0.645) (make-vect 0.154 0.411))
          (make-segment (make-vect 0.154 0.411) (make-vect 0.302 0.588))
          (make-segment (make-vect 0.302 0.588) (make-vect 0.354 0.497))
          (make-segment (make-vect 0.354 0.497) (make-vect 0.245 0.000))
          (make-segment (make-vect 0.419 0.000) (make-vect 0.497 0.171))
          (make-segment (make-vect 0.497 0.171) (make-vect 0.575 0.000))
          (make-segment (make-vect 0.748 0.000) (make-vect 0.605 0.462))
          (make-segment (make-vect 0.605 0.462) (make-vect 1.000 0.142))
          (make-segment (make-vect 1.000 0.354) (make-vect 0.748 0.657))
          (make-segment (make-vect 0.748 0.657) (make-vect 0.582 0.657))
          (make-segment (make-vect 0.582 0.657) (make-vect 0.640 0.857))
          (make-segment (make-vect 0.640 0.857) (make-vect 0.575 1.000))
          (make-segment (make-vect 0.419 1.000) (make-vect 0.354 0.857))
          (make-segment (make-vect 0.354 0.857) (make-vect 0.411 0.657))
          (make-segment (make-vect 0.411 0.657) (make-vect 0.285 0.657))
          (make-segment (make-vect 0.285 0.657) (make-vect 0.154 0.605))
          (make-segment (make-vect 0.154 0.605) (make-vect 0.000 0.857)))))
;;; ‚¿‚á‚ñ‚Æ‚µ‚½}Œ`‚É‚·‚é‚Ì‚Í‚¯‚Á‚±‚¤‘å•Ï‚È‚Ì‚ÅAƒf[ƒ^‚ğ˜a“cæ¶‚Ì‰ğ“š‚©‚ç”qØ‚µ‚Ü‚µ‚½B

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;;; –â‘è‚QD‚T‚O
;;; 
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;;; –â‘è‚QD‚T‚P
;;; 
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
                               
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

