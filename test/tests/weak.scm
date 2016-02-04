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
(define (weak-hashtable-tests ht weakness)
  (define wbox (make-weak-box (string->symbol "value10")))
  (test-equal weakness (hashtable-weakness ht))
  ;; can be the the same interface as hashtable
  (test-assert (hashtable-set! ht (string->symbol "key1") 
			       (string->symbol "value1")))
  (test-assert (hashtable-set! ht (string->symbol "key2") 
			       (string->symbol "value2")))
  
  (test-assert (hashtable-update! ht (string->symbol "key3") values 
				  (string->symbol "value3")))
  
  (test-assert (hashtable-set! ht (string->symbol "key4") wbox))

  (test-assert (lset= eq? '(key1 key2 key3 key4) (hashtable-keys-list ht)))
  (test-assert (lset= eq? `(value1 value2 value3 ,wbox)
		      (hashtable-values-list ht)))

  (let ((ht2 (hashtable-copy ht)))
    (test-assert (lset= eq? '(key1 key2 key3 key4) (hashtable-keys-list ht2)))
    (test-assert (lset= eq? `(value1 value2 value3 ,wbox)
			(hashtable-values-list ht2)))
    
    (hashtable-set! ht (string->symbol "key4") (string->symbol "value4-a"))
    (test-assert (lset= eq? `(value1 value2 value3 ,wbox)
			(hashtable-values-list ht2)))
    (test-error (hashtable-set! ht2 (string->symbol "key3") 
				(string->symbol "v")))))

(weak-hashtable-tests (make-weak-eq-hashtable :weakness 'key) 'key)
(weak-hashtable-tests (make-weak-eq-hashtable :weakness 'value) 'value)
(weak-hashtable-tests (make-weak-eq-hashtable :weakness 'both) 'both)

(weak-hashtable-tests (make-weak-eqv-hashtable :weakness 'key) 'key)
(weak-hashtable-tests (make-weak-eqv-hashtable :weakness 'value) 'value)
(weak-hashtable-tests (make-weak-eqv-hashtable :weakness 'both) 'both)

(weak-hashtable-tests (make-weak-equal-hashtable :weakness 'key) 'key)
(weak-hashtable-tests (make-weak-equal-hashtable :weakness 'value) 'value)
(weak-hashtable-tests (make-weak-equal-hashtable :weakness 'both) 'both)

(define r1 (lambda (v) 1))
(weak-hashtable-tests (make-weak-hashtable r1 eq? :weakness 'key) 'key)
(weak-hashtable-tests (make-weak-hashtable r1 eq? :weakness 'value) 'value)
(weak-hashtable-tests (make-weak-hashtable r1 eq? :weakness 'both) 'both)

;; SRFI-126 interface
(test-assert (weak-hashtable? (make-eq-hashtable 10 'weak-key)))
(test-assert (weak-hashtable? (make-eq-hashtable 10 'weak-value)))
(test-assert (weak-hashtable? (make-eq-hashtable 10 'weak-key-and-value)))
;; weak-hashtable is now subclass of hashtable
(test-assert (hashtable? (make-eq-hashtable 10 'weak-key)))
(test-error  (make-eq-hashtable 10 'unknown))

(test-assert (weak-hashtable? (make-eqv-hashtable 10 'weak-key)))
(test-assert (weak-hashtable? (make-eqv-hashtable 10 'weak-value)))
(test-assert (weak-hashtable? (make-eqv-hashtable 10 'weak-key-and-value)))

(test-assert (weak-hashtable? (make-hashtable r1 eq? 10 'weak-key)))
(test-assert (weak-hashtable? (make-hashtable r1 eq? 10 'weak-value)))
(test-assert (weak-hashtable? (make-hashtable r1 eq? 10 'weak-key-and-value)))

;; extra
(test-assert (weak-hashtable? (make-equal-hashtable 10 'weak-key)))
(test-assert (weak-hashtable? (make-equal-hashtable 10 'weak-value)))
(test-assert (weak-hashtable? (make-equal-hashtable 10 'weak-key-and-value)))

(test-assert (weak-hashtable? (make-string-hashtable 10 'weak-key)))
(test-assert (weak-hashtable? (make-string-hashtable 10 'weak-value)))
(test-assert (weak-hashtable? (make-string-hashtable 10 'weak-key-and-value)))

(test-end)
