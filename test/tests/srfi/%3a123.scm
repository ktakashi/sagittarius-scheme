(import (except (rnrs) define-record-type)
	(only (scheme base) bytevector)
	(srfi :4)
	(srfi :9)
	(srfi :123)
	(srfi :64))

;; tests are extracted from sample implementation

(define-record-type <foo> (make-foo a b) foo?
  (a foo-a set-foo-a!)
  (b foo-b))

(test-begin "SRFI-123")

(test-begin "ref")

(test-assert "bytevector" (= 1 (ref (bytevector 0 1 2) 1)))
(test-assert "hashtable" (let ((table (make-eqv-hashtable)))
			   (hashtable-set! table 'foo 0)
			   (= 0 (ref table 'foo))))
(test-assert "hashtable default" (let ((table (make-eqv-hashtable)))
				   (= 1 (ref table 0 1))))
(test-assert "pair" (= 1 (ref (cons 0 1) 'cdr)))
(test-assert "list" (= 1 (ref (list 0 1 2) 1)))
(test-assert "string" (char=? #\b (ref "abc" 1)))
(test-assert "vector" (= 1 (ref (vector 0 1 2) 1)))
(test-assert "record" (= 1 (ref (make-foo 0 1) 'b)))

(test-assert "srfi-4" (= 1 (ref (s16vector 0 1 2) 1)))
(test-end "ref")

(test-assert "ref*" (= 1 (ref* '(_ #(_ (0 . 1) _) _) 1 1 'cdr)))

(test-begin "ref setter")
(let ((bv (bytevector 0 1 2)))
			    (set! (ref bv 1) 3)
			    (= 3 (ref bv 1)))
(test-assert "bytevector" (let ((bv (bytevector 0 1 2)))
			    (set! (ref bv 1) 3)
			    (= 3 (ref bv 1))))
(test-assert "hashtable" (let ((ht (make-eqv-hashtable)))
			   (set! (ref ht 'foo) 0)
			   (= 0 (ref ht 'foo))))
(test-assert "pair" (let ((p (cons 0 1)))
		      (set! (ref p 'cdr) 2)
		      (= 2 (ref p 'cdr))))
(test-assert "list" (let ((l (list 0 1 2)))
		      (set! (ref l 1) 3)
		      (= 3 (ref l 1))))
(test-assert "string" (let ((s (string #\a #\b #\c)))
			(set! (ref s 1) #\d)
			(char=? #\d (ref s 1))))
(test-assert "vector" (let ((v (vector 0 1 2)))
			(set! (ref v 1) 3)
			(= 3 (ref v 1))))
(test-assert "record" (let ((r (make-foo 0 1)))
			(set! (ref r 'a) 2)
			(= 2 (ref r 'a))))

(test-assert "bad record assignment"
	     (not (guard (err (else #f)) (set! (ref (make-foo 0 1) 'b) 2) #t)))

(test-assert "srfi-4" (let ((s16v (s16vector 0 1 2)))
			(set! (ref s16v 1) 3)
			(= 3 (ref s16v 1))))
(test-end "ref setter")

(test-assert "ref* setter"
	     (let ((obj (list '_ (vector '_ (cons 0 1) '_) '_)))
	       (set! (ref* obj 1 1 'cdr) 2)
	       (= 2 (ref* obj 1 1 'cdr))))

(test-end "SRFI-123")
