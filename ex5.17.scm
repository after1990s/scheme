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
            (lambda (value) 
         ;    (newline)
         ;    (display (list 'set name '= value))
             (set! contents value)))
         (else 
          (error "unknown requeset -- REGISTER" m))))
  dispatch))

(define (get-contents register)
 (register 'get))

(define (set-contents! register value)
 ((register 'set) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (ic (make-register 'ic));instruction counter 
        (sr (make-register 'sr));show instruction register
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
   (set-contents! ic 0)
   (set-contents! sr #f)
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
        ((instruction-execution-proc (car insts)))
        (execute)))))

   (define (print-instruction-counter)
    (newline)
    (display (list 'ic '= (get-contents ic))))

   (define (reset-instruction-counter)
    (set-contents! ic 0))
   (define (advance-pc)
    (set-contents! ic (+ (get-contents ic) 1))
    (if (get-contents sr)
      (begin (newline)
          (display (list 'last-instruction '= (caar (get-contents pc))))
          (newline)
          (display (list 'next-instruction '= (caadr (get-contents pc))))))
    (set-contents! pc (cdr (get-contents pc))))
   (define (make-trace-on)
    (set-contents! sr #t))
   (define (make-trace-off)
    (set-contents! sr #f))
   (define (dispatch m)
    (cond ((eq? m 'start) 
           (set-contents! pc the-instruction-sequence)
           (execute))
          ((eq? m 'install-instruction-sequence)
           (lambda(seq) (set! the-instruction-sequence seq)))
          ((eq? m 'allocate-register) allocate-register)
          ((eq? m 'get-register) lookup-register)
          ((eq? m 'install-operations) 
           (lambda(ops) (set! the-ops (append the-ops ops))))
          ((eq? m 'stack) stack)
          ((eq? m 'operations) the-ops)
          ((eq? m 'print-instruction-counter) (print-instruction-counter))
          ((eq? m 'reset-instruction-counter) (reset-instruction-counter))
          ((eq? m 'advance-pc) (advance-pc))
          ((eq? m 'trace-on) (make-trace-on))
          ((eq? m 'trace-off) (make-trace-off))
          (else (error "unknown request -- MACHINE" m))))
  dispatch)))
(define (print-ic machine)
 (machine 'print-instruction-counter))
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))
(define (start machine)
  (machine 'start))

(define (set-register-contents! machine register-name val)
  (set-contents! (get-register machine register-name) val))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (assemble controller-text machine)
 (extract-labels controller-text
  (lambda (insts labels)
   (update-insts! insts labels machine)
   insts)))

(define (extract-labels text receive)
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
  (cond ((eq? (car inst) 'assign)
    (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test)
    (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch)
    (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto)
    (make-goto inst machine labels pc))
    ((eq? (car inst) 'save)
    (make-save inst machine stack pc))
    ((eq? (car inst) 'restore)
    (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform)
    (make-perform inst machine labels ops pc))
    (else (error "unkonwn instruction type --ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
        (let ((value-proc 
          (if (operation-exp? value-exp)
              (make-operation-exp value-exp 
              machine labels operations)
              (make-primitive-exp (car value-exp) 
              machine labels ))))
          (lambda() (set-contents! target (value-proc))
                    (advance-pc pc machine)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc machine)
; (set-contents! pc (cdr (get-contents pc))))
(machine 'advance-pc))

(define (make-test inst machine labels operations flag pc)
  (let ((test-condition (test-condition inst)))
          (if (operation-exp? test-condition)
              (let ((test-proc 
                (make-operation-exp test-condition machine labels
                operations)))
                (lambda() 
                  (set-contents! flag (test-proc))
                  (advance-pc pc machine)))
              (error "unkonwn test operation " ))))

(define (test-condition inst)
  (cdr inst))

(define (make-branch inst machine labels flag pc)
  (let ((label (branch-dest inst)))
    (if (label-exp? label)
      (let ((dst-insts (lookup-label labels (label-exp-label label))))
        (lambda()
          (if (get-contents flag)
              (set-contents! pc dst-insts)
              (advance-pc pc machine))))
    (error "unknown branch label "))))

(define (branch-dest inst)
  (cadr inst))
    
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dst inst)))
    (cond ((label-exp? dest)
        (let ((dst-insts (lookup-label labels (label-exp-label dest))))
          (lambda()
            (set-contents! pc dst-insts)
            )))
          ((register-exp? dest)
            (let ((reg (get-register machine (register-exp-reg dest))))
              (lambda()
                (set-contents! pc (get-contents reg)))))

        (else (error "unkonwn goto label")))))
(define (goto-dst inst)
  (cadr inst))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (make-stack-reg-name inst))))
    (lambda() (push stack (get-contents reg))
              (advance-pc pc machine))))

(define (make-stack-reg-name inst)
  (cadr inst))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (make-stack-reg-name inst))))
    (lambda() (set-contents! reg (pop stack))
              (advance-pc pc machine))))
(define (make-perform inst machine labels operations pc)
  (let ((op (perform-operation)))
    (if (operation-exp? op)
        (let ((op-proc (make-operation-exp op machine labels operations)))
          (lambda() (op-proc) (advance-pc pc machine)))
        (error "unknown perform op"))))
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp) 
        (let ((c (constant-exp-value exp)))
          (lambda() c)))
        ((label-exp? exp)
        (let ((insts (lookup-label labels (label-exp-label exp))))
          (lambda() insts)))
        ((register-exp? exp)
        (let ((r (get-register machine (register-exp-reg exp))))
          (lambda() (get-contents r))))
        (else (error "unknown expression type" exp))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda(e)
                (make-primitive-exp e machine labels))
                (operation-exp-operands exp))))
       (lambda()
        (apply op (map (lambda (p) (p)) aprocs)))))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "unknown operation -- ASSEMBLE" symbol))))

(define (register-exp? e) (tagged-list? e 'reg))
(define (register-exp-reg e) (cadr e))
(define (constant-exp? e) (tagged-list? e 'const))
(define (constant-exp-value e) (cadr e))
(define (label-exp? e) (tagged-list? e 'label))
(define (label-exp-label e) (cadr e))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op e)
  (cadr (car e)))
(define (operation-exp-operands e)
  (cdr e))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
