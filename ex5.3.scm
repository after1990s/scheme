;unexpand form
(controller
  itr
  (test (op good-enough?) (rem g))
  (branch (label itr-end))
  (assign g (op improve) (reg g))
  (goto (label itr))
  itr-end)

;expand form
(controller
  itr
  ;expand good-enough?
  (assign t (op square) (rem g))
  (assign t (op -) (rem t) (rem x))
  (test (op >) (rem t) (constant 0))
  (branch jump-abs)
  (assign t (op -) (constant 0) (rem t))
  jump-abs
  (test (op <) (rem t) (constant 0))
  ;end of expan good-enough?
  (branch (label itr-end))
  ;expand improve
  (assign t (op /) (rem x) (rem g))
  (assign t (op +) (rem t) (rem g))
  (assign g (op /) (rem g) (constant 2))
  ;end of expand improve
  (goto (label itr))
  itr-end)
