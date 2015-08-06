;chapter 4.1 
(define (eval exp env)
 (cond ((self-evaluating? exp))
     ((bool? exp) (eval-bool exp env));exercise 4.4 P259
     ((variable? exp) (lookup-variable-value exp env))
     ((quoted? exp) (text-of-quotation exp))
     ((assignment? exp) (eval-assignment exp env))
     ((definition? exp) (eval-definition exp env))
     ((if? exp) (eval-if exp env))
     ((lambda? exp);lambda statment
    (make-procedure (lambda-parameters exp) (lambda-body exp) env))
     ((begin? exp) (eval-sequence (begin-actions exp) env));begin statment
     ((cond? exp) (eval (cond->if exp) env))
     ((application? exp) (apply (eval (operator exp) env);call function
       (list-of-values (operands exp) env)));list of arguments.
     (else (error "unkonuwn expression type-EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure) 
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence 
            (procedure-body procedure)
            (extend-enviroment 
              (procedure-paraments procedure)
              arguments
              (procedure-enviroments procedure))))
        (else (error "unknown procedure type-APPLY" procedure))))

(define (list-of-values exps env);calc all exps' value and return the list.
  (if ((no-operands? exps) '())
      (cons (eval (first-operands exps env)) 
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exp env)
  (if (last-exp? exp) 
      (eval (first-exp exp) env)
      (begin 
        (eval (first-exp exp) env)
        (eval-sequence (rest-exp exp) env))))

(define (eval-assigment exp env)
  (set-varialbe-value (assignment-variable exp)
                      (eval (assignment-value exp) env)
                      env))

(define (eval-definition exp env)
 (define-variable! (definition-variable exp)
                   (eval (definition-value exp) env)
                   env))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (quoted? exp)
 (tagged-list? exp 'quote))

(define (text-of-quotation exp) 
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-value exp)
  (if (pair? exp)
      (cadr exp)
      #f))

(define (assignment-variable exp)
  (if (pair? exp)
      (car exp)
      #f))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))))

(define (make-bambda  parameters body)
  (cons ('lambda (cons parameters body))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alterlative exp) 
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))

(define (begin-action exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (rest-exps exp) (cdr exp))

(define (first-exp exp) (car exp))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-being seq))))
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operand ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond->if exp) (expand-clauses (cdr exp)))

(define (expand-causes exp)
  (cond ((null? exp) 'false)
        ((pair? exp) 'false)
        ((make-if (caar exp) (cadar exp) (expand-causes (cdr exp))))))
;exercise 4.4 P259
(define (bool? exp)
  (or (tagged-list? exp 'or) (tagged-list? exp 'and)))

(define (eval-bool exp env)
  (cond ((tagged-list? exp 'or) (eval-bool-or (cdr exp) env))
        ((tagged-list? exp 'and) (eval-bool-and (cdr exp) env))
        (else 'false)))

(define (eval-bool-or exp env)
 (cond ((null? exp) #f)
       ((eval (car exp env)) #t)
       (else (eval-bool-or (cdr exp) env))))

(define (eval-bool-and exp env)
 (cond ((null? exp) #t)
       ((not (eval (car exp env))) #f)
       (else (eval-bool-and (cdr exp) env))))
;end of exercise 4.4

(define (make-frame var val)
 (cons var val))

(define (frame-values frame) (cdr frame))

(define (frame-variables frame) (car frame))

(define (add-binding-to-frame! var val frame)
 (set-car! frame (cons var (car frame)))
 (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
 (if (= (length vars) (length vals))
     (cons (make-frame vars vals) base-env)
     (error "arguments and parameters length unaccepted")))

(define (enclosing-enviroment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (lookup-variable-value var env)
 (define (env-loop env)
  (define (scan vars vals)
   (cond ((null? vars)
          (env-loop (enclosing-environment env)))
         ((eq? var (car vars)) (car vals))
         (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
     (error "unbound variable" var)
     (let ((frame (first-frame env)))
      (scan (frame-variables frame) (frame-values frame)))))
 (env-loop env))

(define (set-variable-value! var val env)
 (define (env-loop env)
  (define (scan vars vals)
   (cond ((null? vars) (env-loop (enclosing-environment env)))
         ((eq? var (car vars)) 
          (set-car! vals val))
         (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let ((frame (first-frame env)))
       (scan (frame-varialbes frame) 
             (frame-values frame)))))
 (env-loop env))

(define (define-variable! var val env)
 (let ((frame (first-frame env)))
  (define (scan vars vals)
   (cond ((null? vars)
          (add-binding-to-frame! var val frame))
         ((eq? var (car vars))
          (set-car! vals val))
         (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame))))
;exercise 4.13 
(define (undef-variable! var env)
 (let ((frame (first-frame env)))
  (define (scan vars)
   (cond ((null? vars)
          (error "symbol not found" var))
         ((eq? var (car vars))
          (make-unbound! var frame))
         (else (scan (cdr vars) ))))
  (scan (frame-variables frame)
        )))
;end of exercise 4.13

(define (primitive-procedure? proc)
 (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures (list (list 'car car)
                                   (list 'cdr cdr)
                                   (list 'cons cons)
                                   (list 'null? null?)
                             ))
(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
 (map (lambda (proc) (list 'primitive (cadr proc)))
      primitive-procedures))

(define (setup-environment)
 (let ((initial-env
        (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
  (define-variable! 'true true initial-env)
  (define-variable! 'false false initial-env)
 initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; input:")
(define output-prompt ";;; value:")


(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
  (let ((output (eval input the-global-environment)))
   (announce-output output-prompt)
   (user-print output)))
 (driver-loop))

(define (prompt-for-input str)
 (newline) (newline) (display str) (newline))

(define (announce-output str)
 (newline) (newline) (display str) (newline))

(define (user-print object)
 (if (compound-procedure? object)
     (display (list 'compound-procedure
               (procedure-parameters object)
               (procedure-body object)
               '<procedure-env>))
     (display object)))
(driver-loop)
