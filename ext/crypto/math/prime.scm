;;
#!compatible
(library (math prime)
    (export is-prime?
	    random-prime)
    (import (rnrs)
	    (sagittarius control)
	    (math)
	    (math helper)
	    (math random))

  (define *small-primes*
    '(3 5 7 11 13 17 19 23 29 31 37 41 43 
      47 53 59 61 67 71 73 79 83 89 97 101 
      103 107 109 113
      127 131 137 139 149 151 157 163 167 173
      179 181 191 193 197 199 211 223 227 229
      233 239 241 251 257 263 269 271 277 281
      283 293 307 311 313 317 331 337 347 349
      353 359 367 373 379 383 389 397 401 409
      419 421 431 433 439 443 449 457 461 463
      467 479 487 491 499 503 509 521 523 541
      547 557 563 569 571 577 587 593 599 601
      607 613 617 619 631 641 643 647 653 659
      661 673 677 683 691 701 709 719 727 733
      739 743 751 757 761 769 773 787 797 809
      811 821 823 827 829 839 853 857 859 863
      877 881 883 887 907 911 919 929 937 941
      947 953 967 971 977 983 991 997))

  ;; Miller Rabin primality test
  (define-optional (is-prime? q (optional (k 50)
					  (rand (pseudo-random RC4))))
    (define (check-small-prime q)
      (let loop ((p *small-primes*))
	(cond ((null? p) #f)
	      ((zero? (mod q (car p))) #t)
	      (else
	       (loop (cdr p))))))

    (let ((q (abs q)))
      (cond ((even? q) #f) ;; obvious
	    ((= q 1) #f)   ;; 1 is not prime
	    ((memv q *small-primes*) #t)
	    ((check-small-prime q) #f) ;; multiple of small-primes
	    (else
	     ;; Miller Rabin test
	     (let* ((t (- q 1))
		    (d (if (zero? (bitwise-and t 1))
			   (do ((d (bitwise-arithmetic-shift-right t 1)
				   (bitwise-arithmetic-shift-right d 1)))
			       ((not (zero? (bitwise-and d 1))) d))
			   t)))
	       (let loop ((i 0))
		 (if (= i k)
		     #t
		     (let* ((a (+ (random rand (- q 2)) 1))
			    (t d)
			    (y (mod-expt a t q)))
		       ;; check 0, ..., q - 1
		       (let loop2 ()
			 (when (and (not (= t (- q 1)))
				    (not (= y 1))
				    (not (= y (- q 1))))
			   (set! y (mod (* y y) q))
			   (set! t (bitwise-arithmetic-shift-left t 1))
			   (loop2)))
		       (if (and (not (= y (- q 1)))
				(zero? (bitwise-and t 1)))
			   #f
			   (loop (+ i 1)))))))))))

  (define-with-key (random-prime size :key (prng (pseudo-random RC4)))
    (let loop ()
      (let* ((bv (read-random-bytes prng size))
	     (len (bytevector-length bv)))
	(bytevector-u8-set! bv 0 (bitwise-ior (bytevector-u8-ref bv 0) #x80 #x40))
	(bytevector-u8-set! bv (- len 1) (bitwise-ior (bytevector-u8-ref bv (- len 1)) #x01))
	(let ((ret (bytevector->integer bv)))
	(if (is-prime? ret)
	    ret
	    (loop))))))
  )