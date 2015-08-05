;chapter 4.1 
(define (eval exp env)
 (cond ((self-evaluation? exp))
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
  (tagged-list exp 'begin))

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
