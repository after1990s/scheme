(define a (cons 4 (cons 1  (cons 2 3)) ))
(display (cadr a))
(cond ((< 5 4)
  (display 3))
  ((< 1 4)
    ((display 4) (display 5))))

