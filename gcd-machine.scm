(load "ex5.15.scm")
(define gcd-machine
  (make-machine '(t a b)
    (list (list 'rem remainder) (list '= =) (list 'display display))
    '(test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
      gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 7)
(start gcd-machine)
(display (get-register-contents gcd-machine 'a))

