(define (make-stack)
 (let ((s '())
       (number-pushes 0)
       (max-depth 0)
       (current-depth 0))
  (define (push x)
   (set! s (cons x s))
   (set! number-pushes (+ number-pushes 1))
   (set! current-depth (+ current-depth 1))
   (set! max-depth (max current-depth max-depth)))
  (define (pop)
   (if (null? s)
    (error "empty stack")
    (let ((top (car s)))
     (set! s (cdr s))
     (set! current-depth (- current-depth 1))
     top)))
  (define (initialize)
   (set! s '())
   (set! number-pushes 0)
   (set! current-depth 0)
   (set! max-depth 0)
   'done)
  (define (print-stack)
   (newline)
   (display (list 'total-pushes '= number-pushes
                  'maxmun-depth '= max-depth)))
  (define (dispatch m)
   (cond ((eq? m 'push) push)
         ((eq? m 'pop) (pop))
         ((eq? m 'initialize) (initialize))
         (else (error "unknown request -- STACK" m))))
  dispatch))

(define (pop stack) (stack 'pop))
(define (push stack val) ((stack 'push) val))