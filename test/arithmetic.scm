;;初等関数ライブラリ


;;リストlsに関数fを適用した値の総和を求める。
(define (sum f ls)
  (if (null? ls)
      0
      (+ (f (car ls)) (sum f (cdr ls)))))

;;lsの各要素について関数fを適用してその積を求める。
(define (product f ls)
  (if (null? ls)
      1
      (* (f (car ls)) (product f (cdr ls)))))


;;リストlsの要素すべてについて関数ｆが成り立つか？
(define (for-all f ls)
  (cond ((null? ls) #t)
        ((not (f (car ls))) #f)
        (else (for-all f (cdr ls)))))

;;リストlsの少なくとも1つに関数ｆが成り立つか？
(define (at-least f ls)
  (cond ((null? ls) #f)
        ((f (car ls)) #t)
        (else (at-least f (cdr ls)))))

;;ガウスの素数定理によりＸ以下の素数の概数を返す。
(define (gauss-primes x)
  (/ x (log x)))

;;ｍとｎが互いに素であれば#t そうでなければ#f
(define (coprime? m n)
  (= (gcd  m n) 1))

;;オイラーのφ関数
;;定義通りに素朴に計算しているので遅い。
;;検算用
(define (phi2 n)
  (define (iter m x)
    (cond ((> m n) x)
          ((coprime? m n) (iter (+ m 1) (+ x 1)))
          (else (iter (+ m 1) x))))
  (iter 1 0))

;;オイラーのφ関数
;;ｎ以下の数でｎと互いに素であるものの個数を返す。
;;素因数分解により計算している。 φ(n=p^a q^b r^c) = n(1-1/p)(1-1/q)(1-1/r)
(define (phi n)
  (if (= n 1)
      1
      (inexact->exact
        (exact->inexact
          (* n (product (lambda (ls) (- 1 (/ 1 (car ls)))) (prime-factors n)))))))

;;ｍがｎで割り切れるかどうか。割り切れれば#t そうでなければ#f
;; n|m 相当
(define (divisible? m n)
  (= (modulo m n) 0))

;;ｍがｎで割り切れるかどうか。割り切れれば#t そうでなければ#f
;; n|m 相当
(define (divisible? m n)
  (= (modulo m n) 0))

;;ｍとｎが法ａで合同かどうか。合同なら#t そうでなければ#f
(define (eqmod? m n a)
  (= (modulo m a) (modulo n a)))

;;nを素因数分解する。指数形式ではなく単純に素数を並べたリストで返す。
;;prime-factorsの下請け
;;nが大きな素数だった場合に備え、素数判定（ミラーラビン）してリストにする処理をしている。
;;n<0の場合には#f、n=0,n=1の場合には'(0),'(1)を返す。
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

;;nを素因数分解して標準形式にして返す。p^a + q^b + r^c ((p a)(q b)(r c))
(define (prime-factors n)
  (define (iter ls p n mult)
    (cond ((null? ls) (cons (list p n) mult))
          ((eq? (car ls) p) (iter (cdr ls) p (+ n 1) mult))
          (else (iter (cdr ls) (car ls) 1 (cons (list p n) mult)))))
  (let ((ls (prime-factors2 n)))
    (iter (cdr ls) (car ls) 1 '())))

;;素数判定 10^11より小さければ決定的判定アルゴリズム、大きければラビンミラー法で判定
(define (prime? n)
  (if (< n 100000000000)
      (deterministic-prime? n)
      (rabin-miller? n)))


;;決定的方法でnが素数であるかどうかを返す。
;;素数ならば#t そうでなければ#f
;;大きな素数に対しては時間がかかる。
(define (deterministic-prime? n)
  (define (iter x y)
    (cond ((> x y) #t)
          ((divisible? n x) #f)
          ((= x 2) (iter 3 y))
          (else (iter (+ x 2) y))))
  (if (< n 2)
      #f
      (iter 2 (sqrt n))))



;;フェルマの小定理を利用して確率的に素数判定をする。
;;素数なら#t そうでなければ#f
;;底を2～n-1まで乱数生成して10回試行する。
(define (fermat? n)
  (define (iter t)
    (cond ((< t 1) #t)
          ((not (= 1 (gaussmod (+ (random (- n 2)) 1) (- n 1) n))) #f)
          (else (iter (- t 1)))))
  (iter 10))

;;メルセンヌ数(2^p-1)に対するルカス-テスト
;;素数ならば#t そうでなければ#f
(define (lucas? p)
  (define (iter n i m)
    (cond ((and (= i (- p 1)) (zero? (modulo n m))) #t)
          ((and (= i (- p 1)) (not (zero? (modulo n m)))) #f)
          (else (iter (modulo (- (expt n 2) 2) m) (+ i 1) m))))
  (cond ((< p 2) #f)
        ((= p 2) #t)
        (else (iter 4 1 (mersenne p)))))

;;フェルマー数
(define (fermat-number n)
  (+ (expt 2 (expt 2 n)) 1))

;;ラビンミラーテスト
;;テスト対象のｎを n^k * q に分解する。
(define (rm1 n)
  (define (iter k q)
    (if (not (= (modulo q 2) 0))
        (list k q)
        (iter (+ k 1) (/ q 2))))
  (iter 0 (- n 1)))

;;ラビンミラーテスト条件１
;;合成数なら#t 素数なら#f
(define (rm2 a q n)
  (not (= (gaussmod a q n) 1)))

;;ラビンミラーテスト条件２
;;合成数なら#t 素数なら#f
(define (rm3 a k q n)
  (define (iter i)
    (cond ((>= i k) #t)
          ((= (gaussmod a (* (expt 2 i) q) n) -1) #f)
          (else (iter (+ i 1)))))
  (iter 0))


;;ラビンミラーテスト
;;nについて底aで条件１，２をテスト
;;合成数なら#t 素数なら#f
(define (rm4 n a)
  (let* ((ls (rm1 n))
         (k (car ls))
         (q (cadr ls)))
    (and (rm2 a q n)
         (rm3 a k q n))))

;;ラビンミラーテスト
;;関数本体
;;底を2～n-1まで乱数で発生させ10回試行する。
;;素数なら#t 合成数なら#f
;;擬素数である確率は 0.25^10 およそ0.000095% 
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
;;繰り返し二乗法によるmod計算。
;; a^n (mod m)を計算する。SICPより借用。
(define (square n) (* n n))
(define (expmod a n m)
  (cond ((= 0 n) 1)
        ((even? n)
         (remainder (square (expmod a (/ n 2) m)) m))
        (else
         (remainder (* a (expmod a (- n 1) m)) m))))

;;繰り返し二乗法によるmod計算で結果を法ｍとした場合 -m/2～m/2 で表す。
(define (gaussmod a k m)
  (let ((k1 (expmod a k m)))
    (cond ((and (> k1 0) (> k1 (/ m 2)) (< k1 m)) (- k1 m))
          ((and (< k1 0) (< k1 (- (/ m 2))) (> k1 (- m))) (+ k1 m))
          (else k1))))

