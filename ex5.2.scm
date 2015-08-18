(controller
  factorial
  (test (op >) (reg c) (reg p))
  (branch fac-done)
  (assign p (op *) (reg p) (reg c))
  (assign c (op +) (reg c) (const 1))
  (goto (label factorial))
  fac-done);out put (reg p)
