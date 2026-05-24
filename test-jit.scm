(import (rnrs) (sagittarius vm) (time))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(disable-jit!)
(print (jit-compiled? tak))
(time (print (tak 30 20 10)))
(print (jit-compiled? tak))

(enable-jit!)
(jit-compile! tak)
(print (jit-compiled? tak))
(time (print (tak 30 20 10)))

;;(jit-disassemble tak)
