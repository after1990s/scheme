(load "machine.scm")

(eval-dispatch
 (test (op self-evaluation?) (reg exp))
 (branch (label ev-self-eval))
 (test (op variable?) (reg exp))
 (branch (label ev-quoted))
 (test (op assignment?) (reg exp))
 (branch (op ev-assignment))
 (test (op definition?) (reg exp))
 (branch (op ev-definition))
 (test (op if?) (reg exp))
 (branch (label ev-if))
 (test (op lambda?) (reg exp))
 (branch (label ev-lambda))
 (test (op begin?) (reg exp))
 (branch (label ev-begin))
 (test (op application?) (reg exp))
 (branch (label ev-application))
 (goto (label unknown-epxression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
  
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label-dispatch))
  


