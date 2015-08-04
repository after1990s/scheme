#lang racket
(define (even? k)
  (= (remainder k 2) 0))
(define (f base power result)
  (cond ((= power 0)
         result)
        ((even? power)
         (f (* base base) (/ power 2) result))
        (else 
         (f base (- power 1) (* base result)))))
(f 2 4 1)