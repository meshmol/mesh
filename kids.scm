;;Babbage用の音楽、画像ライブラリ。Normal用に書き直した。
;;written by K.Sasagawa
;;------MIDI関係----------------

(define-library (normal kids)
  (import (normal system)
          (scheme base)
          (scheme char)
          (scheme write))
  (export midi-open midi-close midi-msg note-on note-off all-note-off all-sound-off
          reset-all-controller pitch-bend logand pitch-bend-sensitivity voice volume
          tempo channel note make-regular-pitch note1 ceiling->exact bend-n chord
          check-arpeggio every max-list min-list chord1 chord2 check-pitch pitch-name
          char->symbol char-append pitch-nth pitch? pitch->number number->pitch check-number
          check-integer check-symbol close move set-pos set-x set-y home forward fd back bk
          north pset dot fill circle set-zoom quadrant left lt right rt cls fore pen-up
          pu pen-down pd show-turtle st hide-turtle ht letter pen-color red blue aqua
          black cream dkgray fuchsia gray yellow purple maroon white skyblue green
          navy silver pen-width from-top from-left)
  (begin
    
    (define *volume* 127)
    
    (define *tempo* 240)
    
    (define *channel* 0)
    
    (define (midi-open)
      (display "@MIDIOpen;")
      (flush))
    
    (define (midi-close)
      (display "@MIDIClose;")
      (flush))
    
    (define (midi-msg x1 x2 x3 x4)
      (check-integer x1)
      (check-integer x2)
      (check-integer x3)
      (check-integer x4)
      (display "@MIDIMsg ")
      (display x1)
      (display " ")
      (display x2)
      (display " ")
      (display x3)
      (display " ")
      (display x4)
      (display ";")
      (flush))
    
    
    (define (note-on n)
      (midi-msg (+ #x90 *channel*) n *volume* 0))
    
    (define (note-off n)
      (midi-msg (+ #x80 *channel*) n *volume* 0))
    
    (define (all-note-off)
      (midi-msg (+ #xb0 *channel*) #x7b 0 0))
    
    (define (all-sound-off)
      (midi-msg (+ #xb0 *channel*) #x78 0 0))
    
    (define (reset-all-controller)
      (midi-msg (+ #xb0 *channel*) #x79 0 0))
    
    (define (pitch-bend n)
      (check-integer n)
      (let* ((m (+ n 8192))
             (msb (/ (logand #b11111110000000 m) (expt 2 7)))
             (lsb (logand #b00000001111111 m)))
        (midi-msg (+ #xe0 *channel*) lsb msb 0)))
    
    (define (logand m n)
      (if (or (= m 0) (= n 0))
          0
          (+ (* (remainder m 2) (remainder n 2))
             (* 2 (logand (quotient m 2) (quotient n 2)))))) 
    
    (define (pitch-bend-sensitivity n)
      (check-integer n)
      (midi-msg (+ #xb0 *channel*) 101 0 0)
      (midi-msg (+ #xb0 *channel*) 100 0 0)
      (midi-msg (+ #xb0 *channel*) 6 n 0))
    
    (define (voice n)
      (check-integer n)
      (midi-msg (+ #xc0 *channel*) n 0 0))
    
    (define (volume n)
      (check-integer n)
      (cond ((< n 0) (set! *volume* 0))
            ((> n 127) (set! *volume* 127))
            (else (set! *volume* n))))
    
    (define (tempo n)
      (check-integer n)
      (cond ((< n 10) (set! *tempo* 10))
            ((> n 240) (set! *tempo* 240))
            (else (set! *tempo* n))))
    
    (define (channel n)
      (check-integer n)
      (if (and (>= n 0) (<= n 16))
          (set! *channel* n)
          (error "channel must be  0<= n <= 16" n)))
    
    (define (note p . options)
      (let ((l 1/4)
            (b #f))
        (cond ((assq 'b options) (set! b (cadr (assq 'b options))))
              ((assq 'l options) (set! l (cadr (assq 'l options)))))
        (note1 (make-regular-pitch p) l b)))
    
    
    (define (make-regular-pitch p)
      (if (member p '(A A# B B# C C# D D# E F F# G G#))
          (string->symbol (string-append (symbol->string p) "4"))
          p))
    
    (define (note1 p l b)
      (check-pitch p)
      (check-number l)
      (let* ((n (pitch->number p))
             (milli 1000)
             (time1 (* (/ (* 60 4) *tempo*) l milli))
             (time2 (ceiling->exact time1)))
        (note-on n)
        (cond (b (note-on n)
                 (pitch-bend-sensitivity 12)
                 (bend-n 10 (* b 683) (quotient (* b 683) 10) (quotient time2 20))
                 (sleep (quotient time2 20))
                 (note-off n))
              (else 
                (note-on n)
                (sleep time2)
                (note-off n)))))
    
    (define (ceiling->exact x)
      (inexact->exact (ceiling x)))
    
    
    (define (bend-n n sensitivity diff time1)
      (cond ((zero? n) #t)
            (else (pitch-bend sensitivity)
                  (sleep time1)
                  (bend-n (- n 1) (- sensitivity diff) diff time1))))
    
    
    (define (chord ps . options)
      (let ((l 1)
            (a #f))
        (cond ((assq 'l options) (set! l (cadr (assq 'l options))))
              ((assq 'a options) (set! a (cadr (assq 'a options)))))
        (let ((ps1 (map make-regular-pitch ps)))
          (for-each check-pitch ps1)
          (check-number l)
          (cond (a
                  (check-arpeggio a)
                  (chord1 ps1 a))
                (else
                  (chord2 ps1 l))))))
    
    (define (check-arpeggio pat)
      (if (and (list? pat)
               (every integer? pat)
               (<= (max-list pat) (length pat))
               (>= (min-list pat) 1))
          #t
          (error "malformed arpegio pattern: " pat)))
    
    (define (every f ls)
      (cond ((null? ls) #t)
            ((not (f (car ls))) #f)
            (else (every f (cdr ls)))))
    
    
    (define (max-list ls)
      (if (null? (cdr ls))
          (car ls)
          (max (car ls) (max-list (cdr ls)))))
    
    (define (min-list ls)
      (if (null? (cdr ls))
          (car ls)
          (max (car ls) (min-list (cdr ls)))))
    
    
    (define (chord1 ps a)
      (define (iter ps l a1)
        (cond ((null? a1) #t)
              (else (note (list-ref ps (- (car a1) 1)) `(l ,l))
                    (iter ps l (cdr a1)))))
      (let ((l (/ 1 (length a))))
        (iter ps l a)))
    
    
    (define (chord2 ps l)
      (let* ((ps1 (map pitch->number ps))
             (milli 1000)
             (time1 (* (/ (* 60 4) *tempo*) l milli))
             (time2 (ceiling->exact time1)))
        (for-each note-on ps1)
        (sleep time2)
        (all-note-off)))
    
    
    (define (check-pitch p)
      (if (not (pitch? p))
          (error "pitch required but got: " p)))
    
    
    
    (define (pitch-name p)
      (let ((p1 (symbol->string p)))
        (cond ((= (string-length p1) 2)
               (char->symbol(string-ref (symbol->string p) 0)))
              ((= (string-length p1) 3)
               (string->symbol (char-append (string-ref (symbol->string p) 0)
                                            (string-ref (symbol->string p) 1))))
              (else #f))))
    
    
    (define (char->symbol x)
      (string->symbol (list->string (list x))))
    
    (define (char-append x y)
      (list->string (list x y)))
    
    (define (pitch-nth p)
      (let* ((p1 (symbol->string p))
             (n (string-length p1)))
        (- (char->integer (string-ref p1 (- n 1))) (char->integer #\0))))
    
    (define (pitch? p)
      (cond ((symbol? p)
             (let ((name (pitch-name p))
                   (nth (pitch-nth p)))
               (if (and (member name '(A A# B B# C C# D D# E F F# G G#))
                        (member nth '(0 1 2 3 4 5 6 7 8 9)))
                   #t
                   #f)))
            (else #f)))
    
    (define (pitch->number p)
      (let ((name (pitch-name p))
            (nth (pitch-nth p)))
        (case name
              ((C)  (* 12 nth))
              ((C#) (+ (* 12 nth) 1))
              ((D)  (+ (* 12 nth) 2))
              ((D#) (+ (* 12 nth) 3))
              ((E)  (+ (* 12 nth) 4))
              ((F)  (+ (* 12 nth) 5))
              ((F#) (+ (* 12 nth) 6))
              ((G)  (+ (* 12 nth) 7))
              ((G#) (+ (* 12 nth) 8))
              ((A)  (+ (* 12 nth) 9))
              ((A#) (+ (* 12 nth) 10))
              ((B)  (+ (* 12 nth) 11)))))
    
    
    (define (number->pitch n)
      (let* ((nth (- (quotient n 12) 1))
             (r (remainder n 12))
             (name (list-ref '(C C# D D# E F F# G G# A A# B) r)))
        (string->symbol (string-append (symbol->string name) (number->string nth)))))
    
    ;;------描画関係----------------
    (define (check-number x)
      (if (not (number? x))
          (error "number required but got: " x)))
    
    (define (check-integer n)
      (if (not(integer? n))
          (error "integer required but got: " n)))
    
    (define (check-symbol s)
      (if (not(symbol? s))
          (error "symbol required but got: " s)))
    
    (define (close)
      (display "@Close;")
      (flush))
    
    (define (move x y)
      (check-number x)
      (check-number y)
      (if (and (number? x)(number? y))
          (begin (display "@Move ")
                 (display x)
                 (display " ")
                 (display y)
                 (display ";")
                 (flush))
          (error "expect arguments of type <number>" x)))
    
    (define (set-pos x y)
      (check-number x)
      (check-number y)
      (pen-up)
      (move x y)
      (pen-down))
    
    (define (set-x x)
      (check-number x)
      (display "@Setx ")
      (display x)
      (display ";")
      (flush))
    
    (define (set-y y)
      (check-number y)
      (display "@Sety ")
      (display y)
      (display ";")
      (flush))
    
    
    (define (home)
      (display "@Home;")
      (flush))
    
    
    (define (forward n)
      (check-number n)
      (display "@FD ")
      (display n)
      (display ";")
      (flush))
    
    
    (define (fd n)
      (forward n))
    
    
    (define (back n)
      (check-number n)
      (display "@BK ")
      (display n)
      (display ";")
      (flush))
    
    
    (define (bk n)
      (back n))
    
    
    (define (north)
      (display "@North;")
      (flush))
    
    
    (define (pset x y)
      (check-number x)
      (check-number y)
      (display "@PS ")
      (display x)
      (display " ")
      (display y)
      (display ";")
      (flush))
    
    (define (dot x y)
      (check-number x)
      (check-number y)
      (pset x y))
    
    (define (fill x y)
      (check-number x)
      (check-number y)
      (display "@Fill ")
      (display x)
      (display " ")
      (display y)
      (display ";")
      (flush))
    
    (define (circle x y r)
      (check-number x)
      (check-number y)
      (check-number r)
      (display "@Circle ")
      (display x)
      (display " ")
      (display y)
      (display " ")
      (display r)
      (display ";")
      (flush))
    
    
    (define (set-zoom x)
      (check-number x)
      (display "@SetZoom ")
      (display x)
      (display ";")
      (flush))
    
    
    (define (quadrant n)
      (check-integer n)
      (display "@Quadrant ")
      (display n)
      (display ";")
      (flush))
    
    
    (define (left n)
      (check-integer n)
      (display "@LT ")
      (display n)
      (display ";")
      (flush))
    
    
    (define (lt n)
      (left n))
    
    
    (define (right n)
      (check-integer n)
      (display "@RT ")
      (display n)
      (display ";")
      (flush))
    
    
    (define (rt n)
      (right n))
    
    
    (define (cls)
      (display "@Cls;")
      (flush))
    
    
    (define (fore)
      (display "@Fore;")
      (flush))
    
    
    (define (pen-up)
      (display "@PenUp;")
      (flush))
    
    
    (define (pu)
      (pen-up))
    
    
    (define (pen-down)
      (display "@PenDown;")
      (flush))
    
    
    (define (pd)
      (pen-down))
    
    
    (define (show-turtle)
      (display "@ShowTurtle;")
      (flush))
    
    (define (st)
      (show-turtle))
    
    
    (define (hide-turtle)
      (display "@HideTurtle;")
      (flush))
    
    (define (ht)
      (hide-turtle))
    
    (define (letter x)
      (check-number x)
      (display "@Letter ")
      (display x)
      (display ";")
      (flush))
    
    (define (pen-color c)
      (check-symbol c)
      (cond ((eqv? c 'blue) (display "@PenBlue;"))
            ((eqv? c 'aqua) (display "@PenAqua;"))
            ((eqv? c 'red) (display "@PenRed;"))
            ((eqv? c 'black) (display "@PenBlack;"))
            ((eqv? c 'cream) (display "@PenCream;"))
            ((eqv? c 'dkgray) (display "@PenDkGray;"))
            ((eqv? c 'fuchsia) (display "@PenFuchsia;"))
            ((eqv? c 'gray) (display "@PenGray;"))
            ((eqv? c 'yellow) (display "@PenYellow;"))
            ((eqv? c 'purple) (display "@PenPurple;"))
            ((eqv? c 'maroon) (display "@PenMaroon;"))
            ((eqv? c 'white) (display "@PenWhite;"))
            ((eqv? c 'skyblue) (display "@PenSkyBlue;"))
            ((eqv? c 'green) (display "@PenMoneyGreen;"))
            ((eqv? c 'navy) (display "@PenNavy;"))
            ((eqv? c 'silver) (display "@PenSilver;"))
            (else (error "undefined color: " c))))
    
    (define (red)
      (pen-color 'red))
    
    (define (blue)
      (pen-color 'blue))
    
    (define (aqua)
      (pen-color 'aqua))
    
    (define (black)
      (pen-color 'black))
    
    (define (cream)
      (pen-color 'cream))
    
    (define (dkgray)
      (pen-color 'dkgray))
    
    (define (fuchsia)
      (pen-color 'fuchsia))
    
    (define (gray)
      (pen-color 'gray))
    
    (define (yellow)
      (pen-color 'yellow))
    
    (define (purple)
      (pen-color 'purple))
    
    (define (maroon)
      (pen-color 'maroon))
    
    (define (white)
      (pen-color 'white))
    
    (define (skyblue)
      (pen-color 'skyblue))
    
    (define (green)
      (pen-color 'green))
    
    (define (navy)
      (pen-color 'navy))
    
    (define (silver)
      (pen-color 'silver))
    
    (define (pen-width i)
      (check-integer i)
      (display "@PenWidth ")
      (display i)
      (display ";"))
    
    
    
    (define (from-top n)
      (check-integer n)
      (display "@FromTop ")
      (display n)
      (display ";"))
    
    
    (define (from-left n)
      (check-integer n)
      (display "@FromLeft ")
      (display n)
      (display ";"))
    
))