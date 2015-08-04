#lang racket
(define (cube_root guess x)
  (if (good_enough? guess x)
      guess
      (cube_root (more_precese guess x) x)))
(define (good_enough? guess x)
  (if (< (abs (- (cube guess) x)) 0.00001)
      #t
      #f))
(define (abs x)
  (cond ((< x 0) (- 0 x))
                 (else x)))
(define (cube x)
  (* x x x))
(define (more_precese x y) 
  (/ (+ (/ x (* y y)) (* 2 y)) 3))
(cube_root 1.9 8)