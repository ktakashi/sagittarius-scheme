;; -*- scheme -*-
#!compatible

(import (rnrs)
	(sagittarius)
	(srfi :1)
	(srfi :64 testing))

(test-begin "Weak objects")

;; (define (scrub-stack . args)
;;   (unless (null? args)
;;     ;; we need to scrub c stack as well
;;     (do ((i 0 (+ i 1)))
;; 	((= i 10000))
;;       (let ((v (apply vector (iota 10))))
;; 	(vector-set! v 0 1)))
;;     (apply scrub-stack (cdr args)))
;;   args)

;; TODO involve GC
(test-assert "weak-box?" (make-weak-box (cons 1 2)))
(test-assert "weak-box-empty? (1)" 
	     ;; stack holds this
	     (let* ((o (cons 1 2))
		    (wb (make-weak-box o)))
	       (not (weak-box-empty? wb))))

(test-assert "weak-box-empty? (2)" 
	     ;; non pointer
	     (let ((wb (make-weak-box 0)))
	       (not (weak-box-empty? wb))))
;; we can't gurantee this
;;(test-assert "weak-box-empty? (3)" 
;;	     ;; pointer
;;	     (let ((wb (make-weak-box (cons 1 2))))
;;	       (scrub-stack 1 2 3 4 5)
;;	       (gc)
;;	       (weak-box-empty? wb)))

(test-equal "weak-box-ref (1)" '(1 . 2)
	     (let* ((o (cons 1 2))
		    (wb (make-weak-box o)))
	       (weak-box-ref wb)))
(test-equal "weak-box-ref (2)" 1
	     (let ((wb (make-weak-box 1)))
	       (weak-box-ref wb)))

(test-equal "weak-box-set! (1)" '(1 . 2)
	     (let ((wb (make-weak-box 0)))
	       (weak-box-set! wb (cons 1 2))
	       (weak-box-ref wb)))
(test-assert "weak-box-set! (2)" 
	     (let ((wb (make-weak-box (cons 1 2))))
	       ;; non pointer
	       (weak-box-set! wb 1)
	       (not (weak-box-empty? wb))))

;; TODO weak-vector
;; TODO weak-hashtable

(test-end)
