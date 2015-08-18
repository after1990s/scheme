(load "stack.scm")
 (define (make-machine register-names ops controller-text)
 (let ((machine (make-new-machine)))
  (for-each (lambda (register-name)
             ((machine 'allocate-register) register-name))
            register-names)
  ((machine 'install-operations) ops)
  ((machine 'install-instruction-sequence)
   (assemble controller-text machine))
  machine))

(define (make-register name)
 (let ((contents '*unassigned*))
  (define (dispatch m)
   (cond ((eq? m 'get) contents)
         ((eq? m 'set)
            (lambda (value) (set! contents value)))
         (else 
          (error "unknown requeset -- REGISTER" m))))
  dispatch))

(define (get-contents register)
 (register 'get))

(define (set-contents! register value)
 ((register 'set) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
   (let ((the-ops
          (list (list 'initialize-stack 
                 (lambda () (stack 'initialize)))))
        (register-table
         (list (list 'pc pc) (list 'flag flag))))
    (define (allocate-register name)
     (if (assoc name register-table)
      (error "Mutiply defined register: "name)
      (set! register-table (cons (list name (make-register name)) register-table)))
     'register-allocated)
    (define (lookup-register name)
     (let ((val (assoc name register-table)))
      (if val
       (cadr val)
       (error "unknown register" name))))
    (define (execute)
     (let ((insts (get-contents pc)))
      (if (null? insts)
       'done
       (begin 
        ((instuction-execution-proc (car insts)))
        (execute)))))
   (define (dispatch m)
    (cond ((eq? m 'start) 
           (set-content! pc the-instruction-sequence)
           (execute))
          ((eq? m 'install-instruction-sequence)
           (lambda(seq) (set! the-instruction-sequence seq)))
          ((eq? m 'allocte-register) allocate-register)
          ((eq? m 'get-register) lookup-register)
          ((eq? m 'install-operations) 
           (lambda(ops) (set! the-ops (append the-ops ops))))
          ((eq? m 'stack) stack)
          ((eq? m 'operations) the-ops)
          (else (error "unknown request -- MACHINE" m))))
  dispatch)))
     
(define (assemble controller-text machine)
 (extract-labels controller-text
  (lambda (insts labels)
   (update-insts! insts labels machine)
   insts)))

(define (exstract-labels text receive)
 (if (null? text)
  (receive '() '())
  (extract-labels (cdr text)
   (lambda( insts labels)
    (let ((next-inst (car text)))
     (if (symbol? next-inst)
      (receive insts (cons (make-label-entry next-inst insts) labels))
      (receive (cons (make-instruction next-inst) insts) labels)))))))

(define (update-insts! insts labels machine)
 (let ((pc (get-register machine 'pc))
       (flag (get-register machine 'flag))
       (stack (machine 'stack))
       (ops (machine 'operations)))
  (for-each 
   (lambda(inst)
    (set-instruction-execution-proc!
     inst
     (make-execution-procedure
      (instruction-text inst) labels machine pc flag stack ops)))
   insts)))

(define (make-instruction text)
 (cons text '()))

(define (instruction-text inst)
 (car inst))

(define (instruction-execution-proc inst)
 (cdr inst))

(define (set-instruction-execution-proc! inst proc)
 (set-cdr! inst proc))

(define (make-label-entry label-name insts)
 (cons label-name insts))

(define (lookup-label labels label-name)
 (let ((val (assoc label-name labels)))
  (if val
   (cdr val)
   (error "undefine label --ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
 :wq

