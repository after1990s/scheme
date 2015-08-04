#lang scheme
(define (f a b)
(if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (f (+ a 4) b))
))

(* 8 (f 1 2000))