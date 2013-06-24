#lang racket
(define (f x)
  (if (< x 3)
      x
      (+ (f (- x 1)) (* (f (- x 2)) 3) (* (f (- x 3)) 3))
      ))
(f 20)