(import (rnrs)
	(srfi :86 mu-and-nu)
	(srfi :64 testing))

(test-begin "SRFI-86 tests")

;; from examples
(test-equal "alet (1.)"
	    '((1 2) 3 4) (alet (a (mu 1 2) ((b c) (mu 3 4))) (list a b c)))

(test-equal "alet (2.)"
	    (list "1st2nd3rd4th" '(1 2 3 4 5 (6)))
	    (let-values (((out getter) (open-string-output-port)))
	      (alet ((a (begin (display "1st" out) 1))
		     (b c (mu (begin (display "2nd" out) 2) 3))
		     (d (begin (display "3rd" out) 4))
		     ((e . f) (mu (begin (display "4th" out) 5) 6)))
		 (list (getter) (list a b c d e f)))))


(test-equal "alet* (3.)"
	    '(1 2 1 4 8 5 (6 7) 8 9 10 (8 9 10))
	    (alet* (((a b) (mu 1 2))
		    ((c d e) a (+ a b c) (+ a b c d))
		    ((f . g) (mu 5 6 7))
		    ((h i j . k) e 9 10 h i j))
	      (list a b c d e f g h i j k)))

(test-equal "alet* (4.)"
	    '(10 6 6 5 5)
	    (alet* tag ((a 1)
			(a b b c (mu (+ a 2) 4 5 6))
			((d e e) b 5 (+ a b c)))
	      (if (< a 10) (tag a 10 b c c d e d) (list a b c d e))))

(test-equal "alet* (5.)"
	    '(1 11 12 10 3 6)
	    (alet* ((a 1)
		    ((b 2) (b c c (mu 3 4 5))
		     ((d e d (mu a b c)) . intag) . tag)
		    (f 6))
		   (if (< d 10)
		       (intag d e 10)
		       (if (< c 10)
			   (tag b 11 c 12 a b d intag)
			   (list a b c d e f)))))

(test-equal "alet (6.)"
	    '("1st2nd3rd(1 2 3)" 10)
	    (let-values (((out getter) (open-string-output-port)))
	      (let ((r (alet ((exit)
			      (a (begin (display "1st" out) 1))
			      (b c (mu (begin (display "2nd" out) 2)
				       (begin (display "3rd" out) 3))))
			     (display (list a b c) out)
			     (exit 10)
			     (display "end"))))
		(list (getter) r))))

(test-equal "alet (7.)"
	    '("1st2ndfalse" #f)
	    (let-values (((out getter) (open-string-output-port)))
	      (let ((r (alet ((and (a (begin (display "1st" out) 1))
				   (b (begin (display "2nd" out) 2))
				   (c (begin (display "false" out) #f))
				   (d (begin (display "3nd" out) 3))))
			     (list a b c d))))
		(list (getter) r))))

(test-equal "alet* (8.1)"
	    "bcdefg"
	    ((lambda (str . rest)
	       (alet* ((len (string-length str))
		       (opt rest
			    (start 0 (integer? start)
				   (if (< start 0)
				       0 
				       (if (< len start) len start)))	 ;true
			    (end len (integer? end)
				 (if (< end start)
				     start
				     (if (< len end) len end)))));true
		      (substring str start end))) "abcdefg" 1 20))

(test-equal "alet* (8. 2)"
	     "bcdefg"
	     ((lambda (str . rest)
		(alet* ((len (string-length str))
			(min (apply min rest))
			(cat rest
			     (start 0 (= start min)
				    (if (< start 0)
					0 
					(if (< len start) len start)))	 ;true
			     (end len (integer? end)
				  (if (< end start)
				      start
				      (if (< len end) len end)))));true
		       (substring str start end))) "abcdefg" 20 1))

(test-equal "alet (8. 3)"
	    "bcdef"
	    ((lambda (str . rest)
	       (alet ((cat rest
			   (start 0
				  (and (list? start)
				       (= 2 (length start))
				       (eq? 'start (car start)))
				  (cadr start))	; true
			   (end (string-length str)
				(and (list? end)
				     (= 2 (length end))
				     (eq? 'end (car end)))
				(cadr end))))	; true
		     (substring str start end))) "abcdefg" '(end 6) '(start 1)))

(let ()
  (define rest-list '(a 10 cc 30 40 b 20))
  (test-equal "alet (9.1)"
	      '(10 2 30 (40 b 20))
	      (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) . d))
		(list a b c d)))

  (test-equal "alet (9.2)"
	      '(10 2 30 (40 b 20))
	      (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) #f . d))
		    (list a b c d)))

  (test-equal "alet (9.3)"
	      '(10 20 30 (40))
	      (alet ((key rest-list (a 1) (b 2) ((c 'cc) 3) #t . d))
		    (list a b c d))))

(let ()
  (define rest (list 'a 10 'd 40 "c" 30 50 'b 20))
  (test-equal "alet (9.4)"
	      '(10 2 30 (d 40 50 b 20))
	      (alet ((key rest (a 1) (b 2) ((c "c") 3) . d)) (list a b c d)))

  (test-equal "alet (9.5)"
	      '(10 2 3 (d 40 "c" 30 50 b 20))
	      (alet ((key rest (a 1) (b 2) ((c "c") 3) #f . d)) (list a b c d)))

  (test-equal "alet (9.6)"
	      '(10 20 30 (d 40 50))
	      (alet ((key rest (a 1) (b 2) ((c "c") 3) #t . d)) (list a b c d)))

  (test-equal "alet* (9.7)"
	      '(0 30 2 3 10 20)
	      ((lambda (m . n)
		 (alet* ((opt n (a 10) (b 20) (c 30) . d)
			 (key d (x 100) (y 200) (a 300)))
			(list m a b c x y)))
	       0 1 2 3 'a 30 'y 20 'x 10))

  (test-equal "alet* (9.8)"
	      '(0 1 2 3 10 20)
	      ((lambda (m . n)
		 (alet* ((key n (x 100) (y 200) (a 300) . d)
			 (opt d (a 10) (b 20) (c 30)))
			(list m a b c x y)))
	       0 'a 30 'y 20 'x 10 1 2 3)))

(test-equal "alet* (10.)"
	    '(2 2 2 50)
	    (alet* ((a 1)
		    (rec (a 2) (b 3) (b (lambda () c)) (c a))
		    (d 50))
		   (list a (b) c d)))

(test-equal "alet* (11.)"
	    '(1 2 3 4 5 6 7 8 9 10 (11 12) 13 14 (15 16) (17 18) (19 20))
	    (alet ((a b (mu 1 2))
		   (values c d (values 3 4))	;This is different from SRFI 71.
		   ((e f) (mu 5 6))
		   ((values g h) (values 7 8))
		   ((i j . k) (nu 9 '(10 11 12)))
		   ((values l m . n) (apply values 13 '(14 15 16)))
		   o (mu 17 18)
		   ((values . p) (values 19 20)))
		  (list a b c d e f g h i j k l m n o p)))

(test-equal "alet* (12.1)"
	    '(1 10)
	    (alet ((a 1)
		   (() (define a 10) (define b 100))
		   (b a))
		  (list a b)))
(test-equal "alet* (12.2)"
	    '(10 10)
	    (alet* ((a 1)
		    (() (define a 10) (define b 100))
		    (b a))
		   (list a b)))


(test-equal "Example 1"
	    '("1st2nd3rd4th5th6th" #f)
	    (let-values (((out getter) (open-string-output-port)))
	      (let ((r (alet ((a (begin (display "1st" out) 1))
			      ((b c) 2 (begin (display "2nd" out) 3))
			      (() (define m #f) (define n (list 8)))
			      ((d (begin (display "3rd" out) 4))
			       (key '(e 5 tmp 6) (e 0) ((f 'tmp) 55)) . p)
			      g (nu (begin (display "4th" out) 7) n)
			      ((values . h) 
			       (apply values 7
				      (begin (display "5th" out) n)))
			      ((m 11) (n n) . q)
			      (rec (i (lambda () (- (j) 1)))
				   (j (lambda ()  10)))
			      (and (k (begin (display "6th" out) m))
				   (l (begin (display "end" out) (newline) 12)))
			      (o))
			 (if (< d 10)
			     (p 40 50 60)
			     (if (< m 100)
				 (q 111 n)
				 (begin (display 
					 (list a b c d e f g h (i) (j) k l m n)
					 out)
					(newline out))))
			 (o (list o p q))
			 (display "This is not displayed"))))
		(list (getter) r))))
	    
(test-end)
