;;WAM の理解のためにWAMをGaucheで書く試み。
;; written by kenichi sasagawa version=2011/11/13

(use srfi-1)
(use srfi-13)

(define *memory* (make-vector #xfff 0))

(define *code* 0)

(define *heap* 100)

(define *stack*  200)

(define *bottom-of-stack* 299)

(define *trail* 300)

(define *PDL* 400)

(define *mode* '())

(define nil '())

(define *number-of-args* 0)

;;トップレベルへの継続
(define *fail-and-exit-program* '())


;;レジスタ
(define P *code*)
(define CP 0)
(define E *stack*)
(define B 0)
(define H *heap*)
(define HB 0)
(define TR *trail*)
(define A *PDL*)
(define S 0)
(define B0 0) ;;cut pointer
(define A1 0)
(define A2 0)
(define A3 0)
(define A4 0)
(define A5 0)
(define A6 0)
(define A7 0)
(define X1 0)
(define X2 0)
(define X3 0)
(define X4 0)
(define X5 0)
(define X6 0)
(define X7 0)

;;メイン関数
;;WAMソースリストをアセンブルし
;;マシン語をcode領域にセットし実行する。
(define (run l)
  (set-code (asm l))
  (go))

(define (go)
  (set! P *code*)
  (set! E *stack*)
  (set! B *stack*)
  (set! H *heap*)
  (set! TR *trail*)
  (set! A *PDL*)
  (call/cc 
    (lambda (c)
      (set! *fail-and-exit-program* c)
      (let loop ()
        (steper)
        (eval `(,(vector-ref *memory* P)) interaction-environment)
        (loop)))))

(define (steper)
  (print "A1  A2  A3  A4  A5  A6  A7")
  (format #t " ~d   ~d   ~d   ~d   ~d   ~d   ~d \n" A1 A2 A3 A4 A5 A6 A7)
  (print "X1  X2  X3  X4  X5  X6  X7")
  (format #t " ~d   ~d   ~d   ~d   ~d   ~d   ~d \n" X1 X2 X3 X4 X5 X6 X7)
  (print "P   CP   E   B   H   HB  TR   A   S   B0")
  (format #t " ~d  ~d  ~d  ~d  ~d  ~d  ~d  ~d  ~d  ~d \n" P CP E B H HB TR A S B0)
  (print (mem-read P))
  (print "continue? yes= ; no= .")
  (flush)
  (if (continue?) #t (*fail-and-exit-program*)))

(define (continue?)
  (case (read-char)
    ((#\;) #t)
    ((#\.) #f)
    (else (continue?))))


;;停止命令
(define (halt)
  (*fail-and-exit-program*))

(define (m-halt)
  (*fail-and-exit-program*))

(define (set-code x)
  (let loop ((i 0)(y x))
    (cond ((not (null? y))
           (vector-set! *memory* i (car y))
           (loop (+ i 1) (cdr y))))))

;;マシン語はm-call のように定義する。
;;マシン語を対話関数名に変換
(define (machine->interaction x)
  (string->symbol (string-drop (symbol->string x) 2)))

;;WAM アセンブラ-------------------------------
(define *label* (make-hash-table))

(define (label? x)
  (and (symbol? x)
       (string=? (string-take-right (symbol->string x) 1) ":")))

(define (label->name x)
  (string->symbol (string-drop-right (symbol->string x) 1)))

(define (interaction->machine x)
  (string->symbol (string-append "m-" (symbol->string x))))

(define (pass1 l)
  (let loop ((p 0)(s l))
    (cond ((null? s) #t)
          ((label? (first (car s))) 
           (hash-table-put! *label* 
                            (label->name (first(car s)))
                            p)
           (loop (+ p (instruction-size (second (car s))))
                      (cdr s)))
          (else (loop (+ p (instruction-size (first (car s))))
                      (cdr s))))))

(define (pass2 l)
  (let loop ((o '())(s l))
    (if (null? s)
        (reverse! o)
        (loop (append (reverse (map-to-object (car s)) o))
              (cdr s)))))

(define (map-to-object x)
  (if (label? (car x))
      (map-to-object (cdr x))
      (map (lambda (y) (cond ((hash-table-exists? *label* y)
                              (hash-table-get *label* y))
                             ((instruction-size y)
                              (interaction->machine y))
                             (else
                               y)))
           x)))

(define (asm l)
  (pass1 l)
  (pass2 l))


;;下位関数群------------------------------------
;;メモリダンプ　指定したアドレスの前後10メモリを取り出す。
(define (dump addr)
  (vector-copy *memory* addr (+ addr 20)))


(define (mem-write addr val)
  (vector-set! *memory* addr val))

(define (mem-read addr)
  (vector-ref *memory* addr))

(define (reg-set reg x)
  (case reg
    ((A) (set! A x))
    ((A1) (set! A1 x))
    ((A2) (set! A2 x))
    ((A3) (set! A3 x))
    ((A4) (set! A4 x))
    ((A5) (set! A5 x))
    ((A6) (set! A6 x))
    ((A7) (set! A7 x))
    ((X1) (set! X1 x))
    ((X2) (set! X2 x))
    ((X3) (set! X3 x))
    ((X4) (set! X4 x))
    ((X5) (set! X5 x))
    ((X6) (set! X6 x))
    ((X7) (set! X7 x))
    ((P) (set! P x))
    ((CP) (set! CP x))
    ((E) (set! E x))
    ((B) (set! B x))
    ((H) (set! H x))
    ((HB) (set! HB x))
    ((E) (set! E x))
    ((S) (set! S x))
    ((TR) (set! TR x))
    ((B0) (set! B0 x))))

(define (copy x)
  (cond ((not (pair? x)) x)
        ((null? x) '())
        (else (cons (car x) (copy (cdr x))))))



(define (atom? x)
  (not (pair? x)))

(define (tag x)
  (first x))

(define (data x)
  (second x))



;;xにはレジスタあるいはアドレスが与えられる。
(define (deref x)
  (cond ((list? x) (deref (data x)))
        (else
          (let ((t (tag (mem-read x)))
                (d (data (mem-read x))))
            (if (and (eq? t 'ref) (not (= d x))) ;;bound-ref
                (deref d)
                x))))) ;;non-ref or unbound-ref



;;トレイル
(define (trail a)
  (cond ((or (< a HB)
             (and (< H a)(< a B))) ;;Bより小さいアドレスのスタック上の変数の場合
         (mem-write TR a)
         (inc! TR))))

;;変数束縛
(define (bind a1 a2)
  (let ((t1 (tag (mem-read a1)))
        (t2 (tag (mem-read a2))))
    (cond ((and (eq? t1 'ref)
                (or (not (eq? t2 'ref))(< a2 a1)));;a1が変数でa2が非変数あるいはa2の方が古い
           (mem-write a1 `(ref ,a2))
           (trail a1))
          (else (mem-write a2 `(ref ,a1))
                (trail a2)))))



(define (push x kind)
  (case kind
     ((PDL) (mem-write A x) (inc! A))
     ((heap) (mem-write H x) (inc! H))
     ((stack) (mem-write E x) (inc! E))
     ((trail) (mem-write TR x) (inc! TR))))

(define (pop kind)
  (case kind
     ((PDL) (dec! A) (mem-read A))
     ((heap) (dec! H) (mem-read H))
     ((stack) (dec! E) (mem-read E))
     ((trail) (dec! TR) (mem-read TR))))



(define (empty? kind)
  (case kind
    ((PDL) (= A *PDL*))
    ((heap) (= H *heap*))
    ((stack) (= E *stack*))
    ((trail) (= TR *trail*))))

;;構造体の述語名シンボルを取り出す。
(define (functor f/n)
  (string->symbol (first (string-split (symbol->string f/n) "/"))))

;;構造体のアリティーを取り出す。
(define (arity f/n)
  (string->symbol (second (string-split (symbol->string f/n) "/"))))

;;uniyしようとするもののアドレスが引数として与えられる。
(define (unify a1 a2)
  (push a1 'PDL)(push a2 'PDL);;初期値として引数のアドレスをPDLへ
  (let ((fail #f));;述語の失敗
    (while (not (or (empty? 'PDL) fail));;PDLが空か失敗でない限り繰り返す
      (let ((d1 (deref (pop 'PDL)));;比較しようとするもののアドレス
            (d2 (deref (pop 'PDL))));;からデリファレンス
        (if (not (= d1 d2));;比較するもののアドレスが異なれば
            (let ((t1 (tag (mem-read d1)));;それぞれのtag、dataをとりだす
                  (v1 (data (mem-read d1)))
                  (t2 (tag (mem-read d2)))
                  (v2 (data (mem-read d2))))
              (if (or (eq? t1 'ref) (eq? t2 'ref));;いずれか一方が変数ならば
                  (bind d1 d2);;束縛する
                  (let ((f1 (functor (mem-read v1)));;構造体の場合には
                        (n1 (arity (mem-read v1)));;構造体の述語名及び
                        (f2 (functor (mem-read v2)));;アリティを取り出す
                        (n2 (arity (mem-read v2))))
                    (if (and (= f1 f2)(= n1 n2));;同じ構造体であるならば
                        (let ((i 1))
                          (while (<= i n1)
                            (push (+ v1 i) 'PDL);;それぞれの構造体の項のアドレスを
                            (push (+ v2 i) 'PDL);;PDLスタックへ
                            (set! i (+ i 1))))
                        (set! fail #t))))))));;アドレスが異なり変数でも構造体でもなければ失敗
    fail))

;;バックトラック
(define (backtrack)
  (cond ((= B *bottom-of-stack*)
         (*fail-and-exit-program*)) ;;top-levelへの継続を起動
        (else
          (reg-set B0 (mem-read (+ B (mem-read B) 7)))
          (reg-set P (mem-read (+ B (mem-read B) 4))))))

;;オペコード、オペランドのバイト数
;;アドレスpにあるオペコードからオペコード＋オペランドのバイト数を返す。
(define (m-instruction-size p)
  (instruction-size (machine->interaction (mem-read p))))

(define (instruction-size op)
  (let ((type1 '(allocate deallocate proceed trust-me neck-cut halt))
        (type2 '(put-list set-variable set-value set-local-value 
                          set-constant set-void execute get-list
                          unify-variable unify-value unify-local-value
                          unify-constant unify-void
                          try-me-else retry-me-else try retry trust
                          get-level cut))
        (type3 '(put-variable put-value put-unsafe-value put-structure
                              put-constant call switch-on-constant
                              switch-on-structure get-variable
                              get-value get-structure get-constant
                              ))
        (type5 '(switch-on-term)))
    (cond ((member op type1) 1)
          ((member op type2) 2)
          ((member op type3) 3)
          ((member op type5) 5)
          (else #f))))

;;トレイルの整理
(define (tidy-trail)
  (let ((i (mem-read (+ B (mem-read B) 5))));;前のトレイルのアドレス
    (while (< i TR)
      (cond ((or (< (mem-read i) HB);;ヒープの低い番地
                 (and (< H (mem-read i))(< (mem-read i) B)));;スタックの低い番地
             (inc! i));;にあるならばi=i+1
            (else
              (mem-write i (mem-read (- TR 1)));;そうでなければトレイル領域にあるポインタを
              (dec! TR))))));;低い番地に移す
;;結果として古い番地を参照しているトレイルポインタ残し、新しいトレイルは廃棄されるはず。
;;p84ページからカットの説明。


;;前TRから今のTRまで未定義変数とする。
(define (unwind-trail a1 a2)
  (let loop ((i a1))
    (cond ((< i a2)
           (mem-write (mem-read i) `(ref ,(mem-read i)))
           (loop (+ i 1))))))

;;永久変数、一時変数の区別に使う
(define (variable-type x)
  (string->symbol (string-take (symbol->string x) 1)))

(define (variable-nth x)
  (string->number (string-take-right (symbol->string x) 1)))

;;--------------------------
;;WAM命令
;;--------------------------
;;Put命令
(define (put-variable x a)
  (case (variable-type x)
    ((X)
     (put-variable-x x a))
    ((Y)
     (put-variable-y x a))))

(define (put-variable-x x a)
  (mem-write H `(ref ,H))
  (reg-set x (mem-read H))
  (reg-set a (mem-read H))
  (inc! H)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (put-variable-y y a)
  (let* ((n (variable-nth y))
         (addr (+ E n 1)))
    (mem-write addr `(ref ,addr))
    (reg-set a (mem-read addr))
    (reg-set 'P (+ P (m-instruction-size P)))))

(define (m-put-variable)
  (put-variable (mem-read (+ P 1)) (mem-read (+ P 2))))

(define (put-value v a)
  (reg-set a v)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-put-value)
  (put-value (mem-read (+ P 1)) (mem-read (+ P 2))))


(define (put-structure f/n x)
  (mem-write H f/n)
  (reg-set x `(str ,H))
  (inc! H)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-put-structure)
  (put-structure (mem-read (+ P 1)) (mem-read (+ P 2))))

(define (put-constant c x)
  (reg-set x `(con ,H))
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-put-constant)
  (put-constant (mem-read (+ P 1)) (mem-read (+ P 2))))

(define (put-list x)
  (reg-set x `(lis ,H))
  (reg-set 'P (+ P (m-instruction-size P))))


(define (m-put-list)
  (put-list (mem-read (+ P 1))))

;;Get命令---------------------------------
(define (get-structure f/n x)
  (let* ((addr (deref x))
         (t (tag (mem-read addr)))
         (d (data (mem-read addr)))
         (fail #f))
     (case t
       ((ref) 
        (mem-write H `(str ,(+ H 1)))
        (mem-write (+ H 1) f/n)
        (bind addr H)
        (inc! H)(inc! H)
        (set! *mode* 'write))
       ((str)
        (cond ((eq? (mem-read d) f/n)
               (set! S (+ d 1))
               (set! *mode* 'read))
              (else (set! fail #t))))
       (else (set! fail #t)))
    (if fail
        (backtrack)
        (reg-set P (+ P (m-instruction-size P))))))

(define (m-get-structure)
  (get-structure (mem-read (+ P 1))
                 (mem-read (+ P 2))))



(define (get-constant c x)
  (let ((t (tag (eval x interaction-environment)))
        (d (data (eval x interaction-environment)))
        (fail #f))
    (case t
      ((ref)
       (mem-write d `(con ,c))
       (trail addr))
      ((con) (if (not (eq? c d)) (set! fail #t)))
      (else (set! fail #t)))
    (if fail
        (backtrack)
        (reg-set 'P (+ P (m-instruction-size P))))))

(define (m-get-constant)
  (get-constant (mem-read (+ P 1))
                (mem-read (+ P 2))))


(define (get-list x)
  (let* ((v (eval x interaction-environment))
         (t (tag v))
         (d (data v))
         (fail #f))
     (case t
       ((ref)
        (mem-write H `(lis ,(+ H 1)))
        (bind d H)
        (inc! H)
        (set! *mode* 'write))
       ((lis)
        (reg-set 'S d)
        (set! *mode* 'read))
       (else (set! fail #t)))
    (if fail
        (backtrack)
        (reg-set 'P (+ P (m-instruction-size P))))))

(define (m-get-list)
  (get-list (mem-read (+ P 1))))


(define (get-variable v a)
  (reg-set v a)
  (reg-set `P (+ P (m-instruction-size P))))

(define (m-get-variable)
  (get-variable (mem-read (+ P 1))
                (mem-read (+ P 2))))

(define (get-value v a)
  (let ((a1 (data (eval v interaction-environment)))
        (a2 (data (eval a interaction-environment)))
        (fail #f))
    (set! fail (unify a1 a2))
    (if (eq? fail #t)
        (backtrack)
        (reg-set 'P (+ P (m-instruction-size P))))))

(define (m-get-value)
  (get-value (mem-read (+ P 1))
             (mem-read (+ P 2))))

;;Set命令--------------------------------
(define (set-variable x)
  (mem-write H `(ref ,H))
  (reg-set x H)
  (inc! H)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-set-variable)
  (set-variable (mem-read (+ P 1))))

(define (set-value x)
  (mem-write H (copy (mem-read x)))
  (inc! H)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-set-value)
  (set-value (eval (mem-read (+ P 1)) interaction-environment)))

(define (set-constant c)
  (mem-write H `(con ,c))
  (inc! H)
  (reg-set 'P (+ P (m-instruction-size P))))

(define (m-set-constant)
  (set-constant (mem-read (+ P 1))))

;;unify命令--------------------------------------------
(define (unify-variable x)
  (let* ((v (eval x interaction-environment))
         (t (if (list? v) (tag v) '()))
         (d (if (list? v) (data v) '())))
  (case *mode*
    ((read)
     (if (and (eq? t 'lis) (not (equal? (mem-read S) '(con nil))))
         (reg-set x `(lis ,S))
         (reg-set x (mem-read S))))
    ((write)
     (mem-write H `(ref ,H))
     (reg-set x (mem-read H))
     (inc! H)))
    (inc! S)
    (reg-set 'P (+ P (m-instruction-size P)))))

(define (m-unify-variable)
  (unify-variable (mem-read (+ P 1))))

(define (unify-value x)
  (let* ((v (eval x interaction-environment))
         (d (data v))
         (fail #f))
    (case *mode*
      ((read)
       (set! fail (unify d S)))
      ((write)
       (mem-write H (copy v))
       (inc! H)))
    (inc! S)
    (if fail
        (trackback)
        (reg-set 'P (+ P (m-instruction-size P))))))



(define (m-unify-value)
  (unify-value (mem-read (+ P 1))))

(define (unify-constant c)
  (case *mode*
    ((read)
     (let* ((addr (deref S))
            (t (tag addr))
            (d (data addr)))
       (case t
         ((ref)
          (mem-write addr `(con ,c))
          (trail addr))
         ((con)
          (if (not (eq? c d))
              #f))
         (else #f))))
    ((write)
     (mem-write H `(con ,c))
     (inc! H))))

(define (m-unify-constant)
  (m-unify-constant (mem-read (+ P 1))))


;;制御-------------------------------------------------
;;call命令
(define (call addr n)
    (cond (addr (reg-set 'CP (+ P (m-instruction-size P)))
                (reg-set 'P addr))
          (else (backtrack))))

(define (m-call)
  (call (mem-read (+ P 1)) (mem-read (+ P 2))))

(define (execute addr)
  (cond (addr (reg-set 'B0 B)
              (reg-set 'P addr))
        (else (backtrack))))

(define (m-execute)
  (execute (mem-read (+ P 1))))


;;proceed命令
(define (proceed)
  (reg-set P CP))

(define m-proceed proceed)

;;フレーム
;;; E   CE previous environment
;;; E+1 CP continutation point
;;; E+2 n number of permanent variables
;;; E+3 Y1 permanent variable 1
;;; ...
;;; E+n+2 Yn permanent variable n

(define (allocate n)
  (let ((newE (+ E (mem-read (+ E 2) 3)))) ;;E+2スタックは未定義では？
    (mem-write newE E)
    (mem-write (+ newE 1) CP)
    (mem-write (+ newE 2) n)
    (reg-set 'E newE)
    (reg-set 'P (+ P 2)))) ;;instruction-size=2

(define (m-allocate)
  (allocate (mem-read (+ P 1))))

(define (deallocate)
  (reg-set 'P (mem-read (+ E 1)))
  (reg-set 'E (mem-read E)))

(define m-deallocate deallocate)


;;Choice命令---------------------------------
;;チョイスポイント（選択点）
;;; B n number-of arguments
;;; B+1 A1 argument register1
;;; ...
;;; B+n An argument register n
;;; B+n+1 CE continutation environment
;;; B+n+2 CP continutation pointer
;;; B+n+3 B previous choice point
;;; B+n+4 BP next clause
;;; B+n+5 TR trail pointer
;;; B+n+6 H heap pointer
;;; B+n+7 B0 cut pointer

(define (try-me-else L)
  (let ((newB 0) (n 0))
    (if (> E B)
        (set! newB (+ E (mem-read (+ E 1)) 2))
        (set! newB (+ B (mem-read B) 8)))
    (mem-write newB *number-of-args*) ;;callのときに引数の個数を記憶
    (set! n (mem-read newB))
    (for-each (lambda (i a) (mem-write (+ newB i) (eval a interaction-environment)))
              (take (iota 7 1) n) (take '(A1 A2 A3 A4 A5 A6 A7) n))
    (mem-write (+ newB n 1) E)
    (mem-write (+ newB n 2) CP)
    (mem-write (+ newB n 3) B)
    (mem-write (+ newB n 4) L)
    (mem-write (+ newB n 5) TR)
    (mem-write (+ newB n 6) H)
    (mem-write (+ newB n 7) B0) ;;cut-pointer
    (reg-set 'B newB)
    (reg-set 'HB H)
    (reg-set 'P (+ P 2)) ;; instruction size =2;
    ))

(define (m-try-me-else)
  (try-me-else (mem-read (+ P 1))))

(define (trust-me)
  (let ((n (mem-read B)))
    (for-each (lambda (i a) (reg-set a (mem-read (+ B i))))
              (take (iota 7 1) n) (take '(A1 A2 A3 A4 A5 A6 A7) n))
    (reg-set 'E (mem-read (+ B n 1)))
    (reg-set 'CP (mem-read (+ B n 2)))
    (unwind-trail (mem-read (+ B n 5)) TR)
    (reg-set 'TR (mem-read (+ B n 5)))
    (reg-set 'H (mem-read (+ B n 6)))
    (reg-set 'B (mem-read (+ B n 3)))
    (reg-set 'HB (mem-read (+ B n 6)))
    (reg-set 'P (+ p (instruction-size P)))))

(define m-trust-me trust-me)



(define (switch-on-term v c l s)
  (case (tag A1)
    ((ref) (reg-set 'P v))
    ((con) (reg-set 'P c))
    ((lis) (reg-set 'P l))
    ((str) (reg-set 'P s))))

(define (m-switch-on-term)
  (switch-on-term (mem-read (+ P 1))
                  (mem-read (+ P 2))
                  (mem-read (+ P 3))
                  (mem-read (+ P 4))))


