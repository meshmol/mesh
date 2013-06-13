;;test values


(let ((a 'a)(b 'b)(x 'x)(y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
               (list a b x y)))
