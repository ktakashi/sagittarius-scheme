(import (rnrs)
	(clos user)
	(sagittarius mop allocation)
	(srfi :64 testing))

(test-begin "Sagittarius MOP")
;; simple test not completed

(define-class <allocation-test> (<allocation-mixin>)
  ((foo :allocation :class :init-value 0)))
(let ((t (make <allocation-test>))
      (t2 (make <allocation-test>)))
  (test-equal "initial value" 0 (slot-ref t 'foo))
  (test-assert "set" (slot-set! t 'foo 5))
  (test-equal "value t"  5 (slot-ref t 'foo))
  (test-equal "value t2" 5 (slot-ref t2 'foo)))

(define-class <allocation-test2> (<allocation-mixin>)
  ((foo :allocation :instance :init-value 0)))
(let ((t (make <allocation-test2>))
      (t2 (make <allocation-test2>)))
  (test-equal "initial value" 0 (slot-ref t 'foo))
  (test-assert "set" (slot-set! t 'foo 5))
  (test-equal "value t"  5 (slot-ref t 'foo))
  (test-equal "value t2" 0 (slot-ref t2 'foo)))

(test-end)
