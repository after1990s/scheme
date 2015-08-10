(define a (cons 4 (cons 1  (cons 2 3)) ))
(define (unuse) (
(display (cadr a))
(cond ((< 5 4)
  (display 3))
  ((< 1 4)
    ((display 4) (display 5))))))

(define (oncefunc para)
(define (oncefunc-internal para)
(display "the line should appear only one time" ))
  (let ((var (oncefunc-internal para)))
    (lambda (x)((display "the line should appear mutilple times")
                (display x)
                (newline))
    )))


((oncefunc 1) 2)

((oncefunc 1) 3)
