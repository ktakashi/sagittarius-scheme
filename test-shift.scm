(import (rnrs)
	(sagittarius continuations))

(let ((k #f)
      (count 0))
  (reset
    (print "before")
	(shift k0 (set! k k0))    ;; shift escapes from the reset
	(set! count (+ count 1))  ;; only these 2 lines are the
	(print "count " count)    ;; captured continuation
  )
  (print "calling continuation")
  (k 1) ;; argument is ignores
  (k 2)
  (k 3)
  (print "done"))
