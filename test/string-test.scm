;;•¶Žš—ñ‚ÌƒeƒXƒgR7RS

(string-map 
  (lambda (c)
    (integer->char (+ 1 (char->integer c))))
  "HAL")

(string-map
  (lambda (c k)
    ((if (eqv? k #\u) char-upcase char-downcase)
     c))
  "sutudycaps xxx"
  "ululululul")
