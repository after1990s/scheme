(load "ex5.18.scm")
(define fac-machine
 (make-machine 
  '(n val continue)
  (list (list '= =) (list '- -) (list '* *))
  '((assign continue (label fact-done))
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
    base-case
    (assign val (const 1))
    (goto (reg continue))
    fact-done)))


(set-register-contents! fac-machine 'n 10)
;(fac-machine 'trace-on)
(define fun (fac-machine 'trace-reg))
(fun 'val)
(fun 'n)
;(set-trace-reg fac-machine 'n)
(start fac-machine)
(get-register-contents fac-machine 'val)
