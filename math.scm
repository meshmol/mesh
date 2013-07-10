
(define-library (mesh math)
  (import (scheme base)
          (scheme inexact))
  (export pi e gamma
          make-matrix matrix-ident matrix-set! matrix-ref matrix->list matrix-row matrix-col
          matrix? matrix-square? matrix-ident? matrix-zero? m= m+ m- m* m-expt matrix-tr
          m/ matrix->list list->matrix vector-math-ref vector-math-set! sum product
          coprime? divisible? divisors eqmod? prime? prime-factors phi 
          primitive-root? primitive-root ind)

  (begin
    (define pi 3.141592653589793)
    (define e 2.718281828459045)
    (define gamma 0.57721566490153286060)
    

    (define (make-matrix m n)
      (define (iter v n1)
        (if (< n1 0)
            v
            (begin
              (vector-set! v n1 (make-vector n 0))
              (iter v (- n1 1)))))
      (iter (make-vector m) (- m 1)))
    

    (define (matrix-ident n)
      (let ((m (make-matrix n n)))
        (do ((i 1 (+ i 1)))
            ((> i n) m)
            (matrix-set! m i i 1))))
    
  

    (define (matrix-set! mat m n x)
      (vector-set! (vector-ref mat (- m 1)) (- n 1) x))
    
    
    (define (matrix-ref mat m n)
      (vector-ref (vector-ref mat (- m 1)) (- n 1)))
    

    (define (matrix->list mat)
      (define (map-vec m)
        (cond ((= m -1) '())
              (else (cons (vector->list (vector-ref mat m))
                          (map-vec (- m 1))))))
      (reverse (map-vec (- (vector-length mat) 1))))
    

    ;((1 2)(3 4)) -> #(#(1 2) #(3 4))
    (define (list->matrix ls)
      (unless (and (list? ls)(list? (car ls)))
        (error "list->matrix malformed list: " ls))
      (unless (if (> (length ls) 1) (apply = (map length ls)))
        (error "list->matrix malformed list: " ls))
      (let* ((r (length ls))
             (c (length (car ls)))
             (m (make-matrix r c)))
        (let loop ((i 1)(ls1 ls))
          (vector-set! m (- i 1) (list->vector (car ls1)))
          (if (>= i r) m (loop (+ i 1)(cdr ls1))))))
    

    (define (matrix-row mat)
      (vector-length mat))
    
    (define (matrix-col mat)
      (vector-length (vector-ref mat 0)))
    
    (define (matrix? m)
      (and (vector? m) (vector? (vector-ref m 0))))
    
    (define (matrix-square? m)
      (cond ((not (matrix? m)) #f)
            (else 
              (and (matrix? m) (= (matrix-row m) (matrix-col m))))))
    
    
    (define (matrix-ident? m)
      (cond ((not (matrix? m)) #f)
            ((not (matrix-square? m)) #f)
            (else (matrix-ident1? m))))
    
    (define (matrix-ident1? m)
      (let ((r (matrix-row m))(c (matrix-col m)))
        (call-with-current-continuation 
          (lambda (esc)
            (do ((i 1 (+ i 1)))
                ((> i r) #t)
                (do ((j 1 (+ j 1)))
                    ((> j c))
                    (if (or (and (= i j)(= (matrix-ref m i j) 1))
                            (and (not (= i j))(= (matrix-ref m i j) 0)))
                        #t
                        (esc #f))))))))
    

    (define (matrix-zero? m)
      (matrix-for-all? zero? m))
    
    
    (define (m= . ms)
      (unless (>= (length ms) 2)
        (error "m= require at least 2, got " (length ms)))
      (%m= (car ms) (cdr ms)))
    
    (define (%m= m ms)
      (cond ((null? ms) #t)
            ((%%m= m (car ms)) (%m= (car ms) (cdr ms)))
            (else #f)))
    
    (define (%%m= m1 m2)
      (unless (and (matrix? m1)(matrix? m2))
        (error "m= require matrix: " m1 m2))
      (let ((r (matrix-row m1)) (c (matrix-col m1)))
        (cond ((not (= (matrix-row m1) (matrix-row m2))) #f)
              ((not (= (matrix-col m1) (matrix-col m2))) #f)
              (else 
                (call-with-current-continuation
                  (lambda (esc)
                    (do ((i 1 (+ i 1)))
                        ((> i r) #t)
                        (do ((j 1 (+ j 1)))
                            ((> j c))
                            (when (not (= (matrix-ref m1 i j)(matrix-ref m2 i j)))
                              (esc #f))))))))))
    
    (define (m+ . ms)
      (when (null? ms)
        (error "m+ require at least 1,got 0: " ms))
      (%m+ (car ms) (cdr ms)))
    
    (define (%m+ m ms)
      (if (null? ms)
          m
          (%m+ (%%m+ m (car ms)) (cdr ms))))
    
    (define (%%m+ m1 m2)
      (unless (and (= (matrix-row m1)(matrix-row m2))
                   (= (matrix-col m1)(matrix-col m2)))
        (error "m+ size not match: " m1 m2))
      (let* ((row (matrix-row m1))
             (col (matrix-col m1))
             (mat (make-matrix row col)))
        (do ((i 1 (+ i 1)))
            ((> i row) mat)
            (do ((j 1 (+ j 1)))
                ((> j col))
                (matrix-set! mat i j 
                             (+ (matrix-ref m1 i j)(matrix-ref m2 i j)))))))
    

    (define (m- . ms)
      (when (null? ms)
        (error "m- require at least 1,got 0: " ms))
      (%m- (car ms) (cdr ms)))
    
    (define (%m- m ms)
      (if (null? ms)
          m
          (%%m- m (%m+ (car ms) (cdr ms)))))
    
    (define (%%m- m1 m2)
      (unless (and (= (matrix-row m1)(matrix-row m2))
                   (= (matrix-col m1)(matrix-col m2)))
        (error "m- size not match: " m1 m2))
      (let* ((row (matrix-row m1))
             (col (matrix-col m1))
             (mat (make-matrix row col)))
        (do ((i 1 (+ i 1)))
            ((> i row) mat)
            (do ((j 1 (+ j 1)))
                ((> j col))
                (matrix-set! mat i j 
                             (- (matrix-ref m1 i j)(matrix-ref m2 i j)))))))
    
    

    (define (m* . ms)
      (when (null? ms)
        (error "m* require at least 1,got 0: " ms))
      (%m* (car ms) (cdr ms)))
    
    (define (%m* m ms)
      (if (null? ms)
          m
          (%m* (%%m* m (car ms)) (cdr ms))))
    
    (define (%%m* m1 m2)
      (unless (= (matrix-col m1)(matrix-row m2))
        (error "m* size not match: "))
      (let* ((r1 (matrix-row m1))(r2 (matrix-row m2))
             (c1 (matrix-col m1))(c2 (matrix-col m2))
             (m (make-matrix r1 c2))
             (p 0))
        (do ((i 1 (+ i 1)))
            ((> i r1) m)
            (do ((j 1 (+ j 1)))
                ((> j c2))
                (do ((k 1 (+ k 1)))
                    ((> k c1) (matrix-set! m i j p) (set! p 0))
                    (set! p (+ p (* (matrix-ref m1 i k)(matrix-ref m2 k j)))))))))
    
    
    (define (m-expt mat n)
      (unless (matrix-square? mat)
        (error "m-expt require square-matrix: "))
      (when (matrix-zero? mat)
        (error "m-expt not defined for zero-matrix: "))
      (m-expt1 mat n))
    

    (define (m-expt1 mat n)
      (cond ((zero? n) (matrix-ident (matrix-row mat)))
            ((even? n) (m-square (m-expt mat (/ n 2))))
            (else (m* mat (m-expt1 mat (- n 1))))))
    
    (define (m-square mat)
      (m* mat mat))
    
    
    (define (matrix-tr m)
      (unless (matrix-square? m)
        (error "matrix-tr require square-matrix:"))
      (let ((r (matrix-row m))
            (t 0))
        (do ((i 1 (+ i 1)))
            ((> i r) t)
            (set! t (+ (matrix-ref m i i) t)))))
    
    
    (define (m/ mat s)
      (let* ((r (matrix-row mat))(c (matrix-col mat))
             (m (make-matrix r c)))
        (do ((i 1 (+ i 1)))
            ((> i r) m)
            (do ((j 1 (+ j 1)))
                ((> j c))
                (matrix-set! m i j (/ (matrix-ref mat i j) s))))))
    
    
    (define (vector-math-ref v n)
      (vector-ref v (- n 1)))
    
    (define (vector-math-set! v n x)
      (vector-set! v (- n 1) x))
    
  ;;���X�gls�Ɋ֐�f��K�p�����l�̑��a�����߂�B
  (define (sum f ls)
    (if (null? ls)
        0
        (+ (f (car ls)) (sum f (cdr ls)))))
  
  ;;ls�̊e�v�f�ɂ��Ċ֐�f��K�p���Ă��̐ς����߂�B
  (define (product f ls)
    (if (null? ls)
        1
        (* (f (car ls)) (product f (cdr ls)))))
  
  
  ;;���Ƃ����݂��ɑf�ł����#t �����łȂ����#f
  (define (coprime? m n)
    (= (gcd  m n) 1))
  
  ;;�������Ŋ���؂�邩�ǂ����B����؂���#t �����łȂ����#f
  ;; n|m ����
  (define (divisible? m n)
    (= (modulo m n) 0))
  
  ;;���Ƃ����@���ō������ǂ����B�����Ȃ�#t �����łȂ����#f
  (define (eqmod? m n a)
    (= (modulo m a) (modulo n a)))
  
  ;;�f���Ȃ��#t �����łȂ����#f
  (define (prime? n)
    (define (iter x y)
      (cond ((> x y) #t)
            ((divisible? n x) #f)
            ((= x 2) (iter 3 y))
            (else (iter (+ x 2) y))))
    (if (< n 2)
        #f
        (iter 2 (sqrt n))))
  
  ;;�񐔂����߂ă��X�g�ɂ��ĕԂ��B
  (define (divisors n)
    (define (iter m ls)
      (cond ((> m n) ls)
            ((divisible? n m) (iter (+ m 1) (cons m ls)))
            (else (iter (+ m 1) ls))))
    (iter 1 '()))
  
  
  ;;n��f������������B�w���`���ł͂Ȃ��P���ɑf������ׂ����X�g�ŕԂ��B
  ;;prime-factors�̉�����
  ;;n<0�̏ꍇ�ɂ�#f�An=0,n=1�̏ꍇ�ɂ�'(0),'(1)��Ԃ��B
  (define (prime-factors1 n)
    (define (iter p x ls z)
      (cond ((= x 1) ls)
            ((> p z) (cons x ls))
            ((divisible? x p) (iter1 p (quotient x p) (cons p ls)))
            ((= p 2) (iter 3 x ls z))
            (else (iter (+ p 2) x ls z))))
    (define (iter1 p x ls)
      (if (divisible? x p)
          (iter1 p (quotient x p) (cons p ls))
          (iter p x ls (sqrt x))))
    (cond ((< n 0) #f)
          ((= n 0) '(0))
          ((= n 1) '(1))
          ((prime? n) (list n))
          (else (iter 2 n '() (sqrt n)))))
  
  ;;n��f�����������ĕW���`���ɂ��ĕԂ��Bp^a + q^b + r^c ((p a)(q b)(r c))
  (define (prime-factors n)
    (define (iter ls p n mult)
      (cond ((null? ls) (cons (list p n) mult))
            ((eq? (car ls) p) (iter (cdr ls) p (+ n 1) mult))
            (else (iter (cdr ls) (car ls) 1 (cons (list p n) mult)))))
    (let ((ls (prime-factors1 n)))
      (iter (cdr ls) (car ls) 1 '())))
  
  ;;�I�C���[�̃ӊ֐�
  ;;���ȉ��̐��ł��ƌ݂��ɑf�ł�����̂̌���Ԃ��B
  ;;�f���������ɂ��v�Z���Ă���B ��(n=p^a q^b r^c) = n(1-1/p)(1-1/q)(1-1/r)
  (define (phi n)
    (if (= n 1)
        1
        (inexact->exact
          (exact->inexact
            (* n (product (lambda (ls) (- 1 (/ 1 (car ls)))) (prime-factors n)))))))
  
  ;;���n���̔���
  ;;�����f������@�Ƃ��Č��n���ł���Ȃ�#t 
  ;;�f���Ȃ�K�����݂��邪������������Ă��Ȃ���� #f���Ԃ�B
  (define (primitive-root? n p)
    (define (iter i)
      (cond ((>= i (- p 1)) #t)
            ((= (expmod n i p) 1) #f)
            (else (iter (+ i 1)))))
    (and (iter 1)
         (= (expmod n (- p 1) p) 1)))
  
  ;;sicp
  ;;�J��Ԃ����@�ɂ��mod�v�Z�B
  ;; a^n (mod m)���v�Z����BSICP���ؗp�B
  (define (expmod a n m)
    (cond ((= 0 n) 1)
          ((even? n)
           (remainder (square (expmod a (/ n 2) m)) m))
          (else
            (remainder (* a (expmod a (- n 1) m)) m))))
    
  ;;�f�����̍ŏ��̌��n����Ԃ��B
  ;;���̔C�ӂ̌��n���ɐ��藧�藝�������̂Ɉ�ԏ����Ȍ��n�����g�����ƂƂ����B
  ;;�v�Z���y�Ȃ̂ŁB
  (define (primitive-root p)
    (define (iter n)
      (cond ((> n p) #f)
            ((primitive-root? n p) n)
            (else (iter (+ n 1)))))
    (iter 2))
    
  ;;�w���̌v�Z
  ;;���n�������Ƃ��đf������@�Ƃ���a�ɑ΂���w�������߂�
  ;;�w���͕K�����݂��邪�^����ꂽ�l�������ɍ���Ȃ����#f���Ԃ�B
  (define (ind r a p)
    (define (iter i)
      (cond ((> i p) #f)
            ((= (expmod r i p) a) i)
            (else (iter (+ i 1)))))
    (iter 0))
    
    
  ))