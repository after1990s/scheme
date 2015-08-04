(define (display-stream stream)
 (cond ((null? stream) (display "end of stream")) 
  (else (begin 
		 (display (stream-car stream))
		 (newline)
		 (display-stream (stream-cdr stream))))))

(define one 
 (cons-stream 1 one))
(define integers 
 (cons-stream 1
  (add-streams one integers)))

(define (add-streams stream1 stream2)
 (stream-map + stream1 stream2))

(define (mul-streams stream1 stream2)
 (stream-map * stream1 stream2))
(define (fac n) (cons-stream n (fac (* n(+ n 1)))))
(define fac (cons-stream 1
			 (mul-streams fac integers)))
(define (partial-sums integers)
 (cons-stream 
  (stream-car integers) 
  (add-streams 
   (partial-sums integers) 
   (stream-cdr integers))))
;(display-stream integers)
(display-stream (partial-sums integers))
;(display (stream-ref fac 3))
