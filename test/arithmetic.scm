;;�����֐����C�u����


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


;;���X�gls�̗v�f���ׂĂɂ��Ċ֐��������藧���H
(define (for-all f ls)
  (cond ((null? ls) #t)
        ((not (f (car ls))) #f)
        (else (for-all f (cdr ls)))))

;;���X�gls�̏��Ȃ��Ƃ�1�Ɋ֐��������藧���H
(define (at-least f ls)
  (cond ((null? ls) #f)
        ((f (car ls)) #t)
        (else (at-least f (cdr ls)))))

;;�K�E�X�̑f���藝�ɂ��w�ȉ��̑f���̊T����Ԃ��B
(define (gauss-primes x)
  (/ x (log x)))

;;���Ƃ����݂��ɑf�ł����#t �����łȂ����#f
(define (coprime? m n)
  (= (gcd  m n) 1))

;;�I�C���[�̃ӊ֐�
;;��`�ʂ�ɑf�p�Ɍv�Z���Ă���̂Œx���B
;;���Z�p
(define (phi2 n)
  (define (iter m x)
    (cond ((> m n) x)
          ((coprime? m n) (iter (+ m 1) (+ x 1)))
          (else (iter (+ m 1) x))))
  (iter 1 0))

;;�I�C���[�̃ӊ֐�
;;���ȉ��̐��ł��ƌ݂��ɑf�ł�����̂̌���Ԃ��B
;;�f���������ɂ��v�Z���Ă���B ��(n=p^a q^b r^c) = n(1-1/p)(1-1/q)(1-1/r)
(define (phi n)
  (if (= n 1)
      1
      (inexact->exact
        (exact->inexact
          (* n (product (lambda (ls) (- 1 (/ 1 (car ls)))) (prime-factors n)))))))

;;�������Ŋ���؂�邩�ǂ����B����؂���#t �����łȂ����#f
;; n|m ����
(define (divisible? m n)
  (= (modulo m n) 0))

;;�������Ŋ���؂�邩�ǂ����B����؂���#t �����łȂ����#f
;; n|m ����
(define (divisible? m n)
  (= (modulo m n) 0))

;;���Ƃ����@���ō������ǂ����B�����Ȃ�#t �����łȂ����#f
(define (eqmod? m n a)
  (= (modulo m a) (modulo n a)))

;;n��f������������B�w���`���ł͂Ȃ��P���ɑf������ׂ����X�g�ŕԂ��B
;;prime-factors�̉�����
;;n���傫�ȑf���������ꍇ�ɔ����A�f������i�~���[���r���j���ă��X�g�ɂ��鏈�������Ă���B
;;n<0�̏ꍇ�ɂ�#f�An=0,n=1�̏ꍇ�ɂ�'(0),'(1)��Ԃ��B
(define (prime-factors2 n)
  (define (iter p x ls z)
    (cond ((= x 1) ls)
          ((> p z) (cons x ls))
          ((divisible? x p) (iter1 p (/ x p) (cons p ls)))
          ((= p 2) (iter 3 x ls z))
          (else (iter (+ p 2) x ls z))))
  (define (iter1 p x ls)
    (if (divisible? x p)
        (iter1 p (/ x p) (cons p ls))
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
  (let ((ls (prime-factors2 n)))
    (iter (cdr ls) (car ls) 1 '())))

;;�f������ 10^11��菬������Ό���I����A���S���Y���A�傫����΃��r���~���[�@�Ŕ���
(define (prime? n)
  (if (< n 100000000000)
      (deterministic-prime? n)
      (rabin-miller? n)))


;;����I���@��n���f���ł��邩�ǂ�����Ԃ��B
;;�f���Ȃ��#t �����łȂ����#f
;;�傫�ȑf���ɑ΂��Ă͎��Ԃ�������B
(define (deterministic-prime? n)
  (define (iter x y)
    (cond ((> x y) #t)
          ((divisible? n x) #f)
          ((= x 2) (iter 3 y))
          (else (iter (+ x 2) y))))
  (if (< n 2)
      #f
      (iter 2 (sqrt n))))



;;�t�F���}�̏��藝�𗘗p���Ċm���I�ɑf�����������B
;;�f���Ȃ�#t �����łȂ����#f
;;���2�`n-1�܂ŗ�����������10�񎎍s����B
(define (fermat? n)
  (define (iter t)
    (cond ((< t 1) #t)
          ((not (= 1 (gaussmod (+ (random (- n 2)) 1) (- n 1) n))) #f)
          (else (iter (- t 1)))))
  (iter 10))

;;�����Z���k��(2^p-1)�ɑ΂��郋�J�X-�e�X�g
;;�f���Ȃ��#t �����łȂ����#f
(define (lucas? p)
  (define (iter n i m)
    (cond ((and (= i (- p 1)) (zero? (modulo n m))) #t)
          ((and (= i (- p 1)) (not (zero? (modulo n m)))) #f)
          (else (iter (modulo (- (expt n 2) 2) m) (+ i 1) m))))
  (cond ((< p 2) #f)
        ((= p 2) #t)
        (else (iter 4 1 (mersenne p)))))

;;�t�F���}�[��
(define (fermat-number n)
  (+ (expt 2 (expt 2 n)) 1))

;;���r���~���[�e�X�g
;;�e�X�g�Ώۂ̂��� n^k * q �ɕ�������B
(define (rm1 n)
  (define (iter k q)
    (if (not (= (modulo q 2) 0))
        (list k q)
        (iter (+ k 1) (/ q 2))))
  (iter 0 (- n 1)))

;;���r���~���[�e�X�g�����P
;;�������Ȃ�#t �f���Ȃ�#f
(define (rm2 a q n)
  (not (= (gaussmod a q n) 1)))

;;���r���~���[�e�X�g�����Q
;;�������Ȃ�#t �f���Ȃ�#f
(define (rm3 a k q n)
  (define (iter i)
    (cond ((>= i k) #t)
          ((= (gaussmod a (* (expt 2 i) q) n) -1) #f)
          (else (iter (+ i 1)))))
  (iter 0))


;;���r���~���[�e�X�g
;;n�ɂ��Ē�a�ŏ����P�C�Q���e�X�g
;;�������Ȃ�#t �f���Ȃ�#f
(define (rm4 n a)
  (let* ((ls (rm1 n))
         (k (car ls))
         (q (cadr ls)))
    (and (rm2 a q n)
         (rm3 a k q n))))

;;���r���~���[�e�X�g
;;�֐��{��
;;���2�`n-1�܂ŗ����Ŕ�������10�񎎍s����B
;;�f���Ȃ�#t �������Ȃ�#f
;;�[�f���ł���m���� 0.25^10 ���悻0.000095% 
(define (rabin-miller? n)
  (define (iter t)
    (cond ((< t 1) #f)
          ((rm4 n (+ (random (- n 2)) 1)) #t)
          (else (iter (- t 1)))))
  (if (= n 2)
      #t
      (not (iter 10))))

(define (random n)
  (* (random-integer (quotient n 100000000000))
     100000000000))

;;sicp
;;�J��Ԃ����@�ɂ��mod�v�Z�B
;; a^n (mod m)���v�Z����BSICP���ؗp�B
(define (square n) (* n n))
(define (expmod a n m)
  (cond ((= 0 n) 1)
        ((even? n)
         (remainder (square (expmod a (/ n 2) m)) m))
        (else
         (remainder (* a (expmod a (- n 1) m)) m))))

;;�J��Ԃ����@�ɂ��mod�v�Z�Ō��ʂ�@���Ƃ����ꍇ -m/2�`m/2 �ŕ\���B
(define (gaussmod a k m)
  (let ((k1 (expmod a k m)))
    (cond ((and (> k1 0) (> k1 (/ m 2)) (< k1 m)) (- k1 m))
          ((and (< k1 0) (< k1 (- (/ m 2))) (> k1 (- m))) (+ k1 m))
          (else k1))))

