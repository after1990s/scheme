#lang racket
(define (even? k)
  (= (remainder k 2) 0))
(define (mul a b result)
  (cond ((= b 0)
         result)
        ((even? b)
         (mul (+ a a) (/ b 2) result ))
        (else
         (mul a (- b 1) (+ result a)))))
(mul 2 3 0)