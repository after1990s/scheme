#lang racket
;R = 1/(1/r1 + 1/r2),由于r1,r2有误差，因此R也有误差，求出R的区间范围。

(define (print-interval x)
  (display "lower-bound:")
  (display (car x))
  (newline)
  (display "upper-bound:")
  (display (cdr x)))
;加法区间范围
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;乘法区间范围, 在全正数的情况下，最小的数相乘结果最小，最大的数相乘结果最大
(define (mul-interval x y)
    (make-interval (* (lower-bound x) (lower-bound y))
                   (* (upper-bound x) (upper-bound y))))
;减法区间范围
(define (sub-interval x y)
  (let ((p1 (abs (- (lower-bound x) (lower-bound y))))
        (p2 (abs (- (lower-bound x) (upper-bound y))))
        (p3 (abs (- (upper-bound x) (lower-bound y))))
        (p4 (abs (- (upper-bound x) (upper-bound y)))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
  
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
;电阻不能负数    
(define (make-interval x y) 
  (if (< x 0) 
      (display "error")
      0)
  (if (< y 0)
      (display "error")
      0)
  (cons x y))
;以中心点和百分比计算区间
(define (make-center-interval center radio) 
  (make-interval (* center (- 1 radio))
                 (* center (+ 1 radio))))
(print-interval (make-center-interval 1 0.2))