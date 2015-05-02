;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; ilists.scm - SRFI-116 immutable list library
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; original copyright

;;; Copyright (C) John Cowan 2014. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files 
;;; (the "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish, 
;;; distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the 
;;; following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;;; DEALINGS IN THE SOFTWARE.

(library (srfi :116 ilists)
    (export 
     iq
     ipair ilist xipair ipair* make-ilist ilist-tabulate iiota
     ipair?
     proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist=
     icar icdr ilist-ref
     ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth
     icaar icadr icdar icddr
     icaaar icaadr icadar icaddr icdaar icdadr icddar icdddr
     icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr
     icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr
     icar+icdr itake idrop ilist-tail
     itake-right idrop-right isplit-at ilast last-ipair
     ilength iappend iconcatenate ireverse iappend-reverse
     izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5
     icount imap ifor-each ifold iunfold ipair-fold ireduce
     ifold-right iunfold-right ipair-fold-right ireduce-right
     iappend-map ipair-for-each ifilter-map imap-in-order
     ifilter ipartition iremove imember imemq imemv
     ifind ifind-tail iany ievery
     ilist-index itake-while idrop-while ispan ibreak
     idelete idelete-duplicates
     iassoc iassq iassv ialist-cons ialist-delete
     replace-icar replace-icdr
     pair->ipair ipair->pair list->ilist ilist->list
     tree->itree itree->tree gtree->itree gtree->tree
     iapply)
    (import (rnrs)
	    (core base) ;; wrong-type-argument-message
	    (sagittarius)
	    (sagittarius control) ;; for check-arg
	    (srfi :1 lists)
	    (core inline)
	    (clos user))

  (define-record-type (<ilist> ipair ipair?)
    (fields (immutable icar icar)
	    (immutable icdr icdr)))

;;; From reference implementation
  (define-syntax iq
    (syntax-rules ()
      ((iq . tree) (gtree->itree 'tree))))

  (define (replace-icar old new)
    (ipair new (icdr old)))

  (define (replace-icdr old new)
    (ipair (icar old) new))

;;; Conversion between lists and ilists

  (define (pair->ipair pair)
    (ipair (car pair) (cdr pair)))

  (define (ipair->pair ipair)
    (cons (icar ipair) (icdr ipair)))

  (define (list->ilist list)
    (let lp ((list list))
      (if (pair? list)
	  (ipair (car list) (lp (cdr list)))
	  list)))

  (define (ilist . objs)
    (list->ilist objs))

  (define (ilist->list ilist)
    (let lp ((ilist ilist))
      (if (ipair? ilist)
	  (cons (icar ilist) (lp (icdr ilist)))
	  ilist)))

  (define (tree->itree obj)
    (if (pair? obj)
	(ipair (tree->itree (car obj)) (tree->itree (cdr obj)))
	obj))

  (define (itree->tree obj)
    (if (ipair? obj)
	(cons (itree->tree (icar obj)) (itree->tree (icdr obj)))
	obj))

  (define (gtree->itree obj)
    (cond
     ((pair? obj)
      (ipair (gtree->itree (car obj)) (gtree->itree (cdr obj))))
     ((ipair? obj)
      (ipair (gtree->itree (icar obj)) (gtree->itree (icdr obj))))
     (else
      obj)))

  (define (gtree->tree obj)
    (cond
     ((pair? obj)
      (cons (gtree->tree (car obj)) (gtree->tree (cdr obj))))
     ((ipair? obj)
      (cons (gtree->tree (icar obj)) (gtree->tree (icdr obj))))
     (else
      obj)))

  ;; Apply a function to (arguments and) an ilist
  ;; If ilists are built in, optimize this!
  ;; TODO should we?
  (define (iapply proc . ilists)
    (cond
     ((null? ilists)
      (apply proc '()))
     ((null? (cdr ilists))
      (apply proc (ilist->list (car ilists))))
     (else
      (let ((final (ilist->list (last ilists))))
        (apply proc (append (drop-right! ilists 1) final))))))

;;; Printer for debugging

  (define (write-ipair ipair port)
    (write (gtree->tree ipair) port))

  (define-method write-object ((o <ilist>) out) (write-ipair o out))
  
  ;;; Analogues of R5RS list routines

  (define (iassq x lis)
    (ifind (lambda (entry) (eq? x (icar entry))) lis))

  (define (iassv x lis)
    (ifind (lambda (entry) (eqv? x (icar entry))) lis))

;;  (define (imap proc lis1 . lists)
;;    (apply imap-in-order proc lis1 lists))

  (define (ifor-each proc lis1 . lists)
    (check-arg procedure? proc ipair-for-each)
    (if (pair? lists)

	(let lp ((lists (cons lis1 lists)))
	  (let ((tails (%cdrs lists)))
	    (if (pair? tails)
		(begin (apply proc (map icar lists))
		       (lp tails)))))

	;; Fast path.
	(let lp ((lis lis1))
	  (if (not (null-ilist? lis))
	      (let ((tail (icdr lis)))    ; Grab the icdr now,
		(proc (icar lis))                ; even though it's unnecessary
		(lp tail))))))

;;; from ilist-impl.scm
  (define (xipair d a) (ipair a d))

;;;; Recursively copy every ipair.

;;; Make an ilist of length LEN.

  (define (make-ilist len :optional (elt #f))
    (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-ilist)
    (do ((i len (- i 1))
	 (ans '() (ipair elt ans)))
	((<= i 0) ans)))


;;; Make an ilist of ilength LEN. Elt i is (PROC i) for 0 <= i < LEN.

  (define (ilist-tabulate len proc)
    (check-arg (lambda (n) (and (integer? n) (>= n 0))) len ilist-tabulate)
    (check-arg procedure? proc ilist-tabulate)
    (do ((i (- len 1) (- i 1))
	 (ans '() (ipair (proc i) ans)))
	((< i 0) ans)))

;;; (ipair* a1 a2 ... an) = (ipair a1 (ipair a2 (ipair ... an)))
;;; (ipair* a1) = a1	(ipair* a1 a2 ...) = (ipair a1 (ipair* a2 ...))
;;;
;;; (ipair ifirst (iunfold not-ipair? icar icdr rest values))

  (define (ipair* ifirst . rest)
    (let recur ((x ifirst) (rest rest))
      (if (pair? rest)
	  (ipair x (recur (car rest) (cdr rest)))
	  x)))

  (define (ilist-copy lis)				
    (let recur ((lis lis))			
      (if (ipair? lis)				
	  (ipair (icar lis) (recur (icdr lis)))	
	  lis)))					


  (define (iiota count :optional (start 0) (step 1))
    (check-arg integer? count iiota)
    (if (< count 0) (error 'iiota "Negative step count" count))
    (check-arg number? start iiota)
    (check-arg number? step iiota)
    (let loop ((n 0) (r '()))
      (if (= n count)
	  (ireverse r)
	  (loop (+ 1 n)
		(ipair (+ start (* n step)) r)))))

  (define (proper-ilist? x)
    (let lp ((x x) (lag x))
      (if (ipair? x)
	  (let ((x (icdr x)))
	    (if (ipair? x)
		(let ((x   (icdr x))
		      (lag (icdr lag)))
		  (and (not (eq? x lag)) (lp x lag)))
		(null? x)))
	  (null? x))))
  (define (ilist? x) (proper-ilist? x))

  (define (dotted-ilist? x)
    (let lp ((x x) (lag x))
      (if (ipair? x)
	  (let ((x (icdr x)))
	    (if (ipair? x)
		(let ((x   (icdr x))
		      (lag (icdr lag)))
		  (and (not (eq? x lag)) (lp x lag)))
		(not (null? x))))
	  (not (null? x)))))

  (define-inline (not-ipair? x) (not (ipair? x)))	; Inline me.

  (define (null-ilist? l)
    (cond ((ipair? l) #f)
	  ((null? l) #t)
	  (else (error 'null-ilist? "argument out of domain" l))))
  
  (define (ilist= = . ilists)
    (or (null? ilists) ; special case

	(let lp1 ((ilist-a (car ilists)) (others (cdr ilists)))
	  (or (null? others)
	      (let ((ilist-b (car others))
		    (others (cdr others)))
		(if (eq? ilist-a ilist-b)	; EQ? => LIST=
		    (lp1 ilist-b others)
		    (let lp2 ((ilist-a ilist-a) (ilist-b ilist-b))
		      (if (null-ilist? ilist-a)
			  (and (null-ilist? ilist-b)
			       (lp1 ilist-b others))
			  (and (not (null-ilist? ilist-b))
			       (= (icar ilist-a) (icar ilist-b))
			       (lp2 (icdr ilist-a) (icdr ilist-b)))))))))))

  (define (ilength x)			; ILENGTH may diverge or
    (let lp ((x x) (len 0))		; raise an error if X is
      (if (ipair? x)			; a circular ilist. This version
	  (lp (icdr x) (+ len 1))		; diverges.
	  len)))

  (define (izip ilist1 . more-lists) (apply imap ilist ilist1 more-lists))

;;; selectors
  (define (icaar   x) (icar (icar x)))
  (define (icadr   x) (icar (icdr x)))
  (define (icdar   x) (icdr (icar x)))
  (define (icddr   x) (icdr (icdr x)))

  (define (icaaar  x) (icaar (icar x)))
  (define (icaadr  x) (icaar (icdr x)))
  (define (icadar  x) (icadr (icar x)))
  (define (icaddr  x) (icadr (icdr x)))
  (define (icdaar  x) (icdar (icar x)))
  (define (icdadr  x) (icdar (icdr x)))
  (define (icddar  x) (icddr (icar x)))
  (define (icdddr  x) (icddr (icdr x)))

  (define (icaaaar x) (icaaar (icar x)))
  (define (icaaadr x) (icaaar (icdr x)))
  (define (icaadar x) (icaadr (icar x)))
  (define (icaaddr x) (icaadr (icdr x)))
  (define (icadaar x) (icadar (icar x)))
  (define (icadadr x) (icadar (icdr x)))
  (define (icaddar x) (icaddr (icar x)))
  (define (icadddr x) (icaddr (icdr x)))
  (define (icdaaar x) (icdaar (icar x)))
  (define (icdaadr x) (icdaar (icdr x)))
  (define (icdadar x) (icdadr (icar x)))
  (define (icdaddr x) (icdadr (icdr x)))
  (define (icddaar x) (icddar (icar x)))
  (define (icddadr x) (icddar (icdr x)))
  (define (icdddar x) (icdddr (icar x)))
  (define (icddddr x) (icdddr (icdr x)))


  (define ifirst  icar)
  (define isecond icadr)
  (define ithird  icaddr)
  (define ifourth icadddr)
  (define (ififth   x) (icar    (icddddr x)))
  (define (isixth   x) (icadr   (icddddr x)))
  (define (iseventh x) (icaddr  (icddddr x)))
  (define (ieighth  x) (icadddr (icddddr x)))
  (define (ininth   x) (icar  (icddddr (icddddr x))))
  (define (itenth   x) (icadr (icddddr (icddddr x))))

  (define (icar+icdr ipair) (values (icar ipair) (icdr ipair)))

;;; itake & idrop

  (define (itake lis k)
    (check-arg integer? k itake)
    (let recur ((lis lis) (k k))
      (if (zero? k) '()
	  (ipair (icar lis)
		 (recur (icdr lis) (- k 1))))))

  (define (ilist-tail lis k) (idrop lis k))
  (define (idrop lis k)
    (check-arg integer? k idrop)
    (let iter ((lis lis) (k k))
      (if (zero? k) lis (iter (icdr lis) (- k 1)))))

  (define (itake-right lis k)
    (check-arg integer? k itake-right)
    (let lp ((lag lis)  (lead (idrop lis k)))
      (if (ipair? lead)
	  (lp (icdr lag) (icdr lead))
	  lag)))

  (define (idrop-right lis k)
    (check-arg integer? k idrop-right)
    (let recur ((lag lis) (lead (idrop lis k)))
      (if (ipair? lead)
	  (ipair (icar lag) (recur (icdr lag) (icdr lead)))
	  '())))

  (define (ilist-ref lis i) (icar (idrop lis i)))

  (define (isplit-at x k)
    (check-arg integer? k isplit-at)
    (let recur ((lis x) (k k))
      (if (zero? k) (values '() lis)
	  (receive (prefix suffix) (recur (icdr lis) (- k 1))
	    (values (ipair (icar lis) prefix) suffix)))))

  (define (ilast lis) (icar (last-ipair lis)))

  (define (last-ipair lis)
    (check-arg ipair? lis last-ipair)
    (let lp ((lis lis))
      (let ((tail (icdr lis)))
	(if (ipair? tail) (lp tail) lis))))


  (define (iunzip1 lis) (imap icar lis))

  (define (iunzip2 lis)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values lis lis)	; Use NOT-IPAIR? to handle
	  (let ((elt (icar lis)))			; dotted lists.
	    (receive (a b) (recur (icdr lis))
	      (values (ipair (icar  elt) a)
		      (ipair (icadr elt) b)))))))

  (define (iunzip3 lis)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values lis lis lis)
	  (let ((elt (icar lis)))
	    (receive (a b c) (recur (icdr lis))
	      (values (ipair (icar   elt) a)
		      (ipair (icadr  elt) b)
		      (ipair (icaddr elt) c)))))))

  (define (iunzip4 lis)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values lis lis lis lis)
	  (let ((elt (icar lis)))
	    (receive (a b c d) (recur (icdr lis))
	      (values (ipair (icar    elt) a)
		      (ipair (icadr   elt) b)
		      (ipair (icaddr  elt) c)
		      (ipair (icadddr elt) d)))))))

  (define (iunzip5 lis)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values lis lis lis lis lis)
	  (let ((elt (icar lis)))
	    (receive (a b c d e) (recur (icdr lis))
	      (values (ipair (icar     elt) a)
		      (ipair (icadr    elt) b)
		      (ipair (icaddr   elt) c)
		      (ipair (icadddr  elt) d)
		      (ipair (icar (icddddr  elt)) e)))))))

  (define (iappend . lists)
    (if (pair? lists)
	(let recur ((list1 (car lists)) (lists (cdr lists)))
	  (if (pair? lists)
	      (let ((tail (recur (car lists) (cdr lists))))
		(ifold-right ipair tail list1)) ; Append LIST1 & TAIL.
	      list1))
	'()))

  (define (iappend-reverse rev-head tail) (ifold ipair tail rev-head))

  (define (iappend-reverse rev-head tail)
    (let lp ((rev-head rev-head) (tail tail))
      (if (null-ilist? rev-head) tail
	  (lp (icdr rev-head) (ipair (icar rev-head) tail)))))

  (define (iconcatenate  lists) (ireduce-right iappend  '() lists))


;;; I actually don't like to use call/cc for these procedures but for now...
  (define (%cdrs lists)
    (call-with-current-continuation
     (lambda (abort)
       (let recur ((lists lists))
	 (if (pair? lists)
	     (let ((lis (car lists)))
	       (if (null? lis) (abort '())
		   (cons (icdr lis) (recur (cdr lists)))))
	     '())))))

  (define (%cars+ lists last-elt) ;; (append (map icar lists) (list last-elt))
    (let recur ((lists lists))
      (if (pair? lists)
	  (cons (icar (car lists)) (recur (cdr lists)))
	  (list last-elt))))

;;; LISTS is a (not very long) non-empty list of ilists.
;;; Return two lists: the icars & the icdrs of the ilists.
;;; However, if any of the ilists is empty, just abort and return [() ()].

  (define (%cars+cdrs ilists)
    (call-with-current-continuation
     (lambda (abort)
       (let recur ((ilists ilists))
	 (if (pair? ilists)
	     (let ((ilist (car ilists))
		   (other-ilists (cdr ilists)))
	       (if (null? ilist) (abort '() '()) ; LIST is empty -- bail out
		   (let ((a (icar ilist))
			 (d (icdr ilist)))
		     (receive (icars icdrs) (recur other-ilists)
		       (values (cons a icars) (cons d icdrs))))))
	     (values '() '()))))))

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars ilist. What a hack.
  (define (%cars+cdrs+ ilists cars-final)
    (call-with-current-continuation
     (lambda (abort)
       (let recur ((ilists ilists))
	 (if (pair? ilists)
	     (let ((ilist (car ilists))
		   (other-ilists (cdr ilists)))
	       (if (null? ilist) (abort '() '()) ; LIST is empty -- bail out
		   (receive (a d) (icar+icdr ilist)
		     (receive (cars cdrs) (recur other-ilists)
		       (values (cons a cars) (cons d cdrs))))))
	     (values (list cars-final) '()))))))

;;; Like %CARS+CDRS, but blow up if any ilist is empty.
  (define (%cars+cdrs/no-test ilists)
    (let recur ((ilists ilists))
      (if (pair? ilists)
	  (let ((ilist (car ilists))
		(other-ilists (cdr ilists)))
	    (let ((a (icar ilist))
		  (d (icdr ilist)))
	      (receive (cars cdrs) (recur other-ilists)
		(values (cons a cars) (cons d cdrs)))))
	  (values '() '()))))

  (define (icount pred ilist1 . ilists)
    (check-arg procedure? pred icount)
    (if (pair? ilists)

	;; N-ary case
	(let lp ((ilist1 ilist1) (ilists ilists) (i 0))
	  (if (null-ilist? ilist1) i
	      (receive (as ds) (%cars+cdrs ilists)
		(if (null? as) i
		    (lp (icdr ilist1) ds
			(if (apply pred (icar ilist1) as) (+ i 1) i))))))

	;; Fast path
	(let lp ((lis ilist1) (i 0))
	  (if (null-ilist? lis) i
	      (lp (icdr lis) (if (pred (icar lis)) (+ i 1) i))))))

;;; ifold/iunfold
;;;;;;;;;;;;;;;

  (define (iunfold-right p f g seed :optional (tail '()))
    (check-arg procedure? p iunfold-right)
    (check-arg procedure? f iunfold-right)
    (check-arg procedure? g iunfold-right)
    (let lp ((seed seed) (ans tail))
      (if (p seed) ans
	  (lp (g seed)
	      (ipair (f seed) ans)))))

  (define (iunfold p f g seed . maybe-tail-gen)
    (check-arg procedure? p iunfold)
    (check-arg procedure? f iunfold)
    (check-arg procedure? g iunfold)
    (if (pair? maybe-tail-gen)

	(let ((tail-gen (car maybe-tail-gen)))
	  (if (pair? (cdr maybe-tail-gen))
	      (apply error 'iunfold "Too many arguments" 
		     iunfold p f g seed maybe-tail-gen)

	      (let recur ((seed seed))
		(if (p seed) (tail-gen seed)
		    (ipair (f seed) (recur (g seed)))))))

	(let recur ((seed seed))
	  (if (p seed) '()
	      (ipair (f seed) (recur (g seed)))))))
  

  (define (ifold kons knil ilis1 . ilists)
    (check-arg procedure? kons ifold)
    (if (pair? ilists)
	(let lp ((ilists (cons ilis1 ilists)) (ans knil))	; N-ary case
	  (receive (cars+ans cdrs) (%cars+cdrs+ ilists ans)
	    (if (null? cars+ans) ans ; Done.
		(lp cdrs (apply kons cars+ans)))))
	
	(let lp ((ilis ilis1) (ans knil))			; Fast path
	  (if (null-ilist? ilis) ans
	      (lp (icdr ilis) (kons (icar ilis) ans))))))


  (define (ifold-right kons knil ilis1 . ilists)
    (check-arg procedure? kons ifold-right)
    (if (pair? ilists)
	(let recur ((ilists (cons ilis1 ilists)))		; N-ary case
	  (let ((cdrs (%cdrs ilists)))
	    (if (null? cdrs) knil
		(apply kons (%cars+ ilists (recur cdrs))))))

	(let recur ((ilis ilis1))				; Fast path
	  (if (null? ilis) knil
	      (let ((head (icar ilis)))
		(kons head (recur (icdr ilis))))))))


  (define (ipair-fold-right f zero ilis1 . ilists)
    (check-arg procedure? f ipair-fold-right)
    (if (pair? ilists)
	(let recur ((ilists (cons ilis1 ilists)))		; N-ary case
	  (let ((cdrs (%cdrs ilists)))
	    (if (null? cdrs) zero
		(apply f (append ilists (list (recur cdrs)))))))

	(let recur ((ilis ilis1))				; Fast path
	  (if (null-ilist? ilis) zero (f ilis (recur (icdr ilis)))))))

  (define (ipair-fold f zero ilis1 . ilists)
    (check-arg procedure? f ipair-fold)
    (if (pair? ilists)
	(let lp ((ilists (cons ilis1 ilists)) (ans zero))	; N-ary case
	  (let ((tails (%cdrs ilists)))
	    (if (null? tails) ans
		(lp tails (apply f (append ilists (list ans)))))))

	(let lp ((ilis ilis1) (ans zero))
	  (if (null-ilist? ilis) ans
	      (let ((tail (icdr ilis)))		; Grab the icdr now,
		(lp tail (f ilis ans)))))))	; in case F SET-CDR!s LIS.

;;; IREDUCE and IREDUCE-RIGHT only use RIDENTITY in the empty-ilist case.
;;; These cannot meaningfully be n-ary.

  (define (ireduce f ridentity ilis)
    (check-arg procedure? f ireduce)
    (if (null-ilist? ilis) ridentity
	(ifold f (icar ilis) (icdr ilis))))

  (define (ireduce-right f ridentity ilis)
    (check-arg procedure? f ireduce-right)
    (if (null-ilist? ilis) ridentity
	(let recur ((head (icar ilis)) (ilis (icdr ilis)))
	  (if (ipair? ilis)
	      (f head (recur (icar ilis) (icdr ilis)))
	      head))))


;;; Mappers: iappend-map ipair-for-each ifilter-map imap-in-order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (iappend-map f ilis1 . ilists)
    (really-iappend-map iappend-map  iappend  f ilis1 ilists))

  (define (really-iappend-map who appender f ilis1 ilists)
    (check-arg procedure? f who)
    (if (pair? ilists)
	(receive (cars cdrs) (%cars+cdrs (cons ilis1 ilists))
	  (if (null? cars) '()
	      (let recur ((cars cars) (cdrs cdrs))
		(let ((vals (apply f cars)))
		  (receive (cars2 cdrs2) (%cars+cdrs cdrs)
		    (if (null? cars2) vals
			(appender vals (recur cars2 cdrs2))))))))

	;; Fast path
	(if (null-ilist? ilis1) '()
	    (let recur ((elt (icar ilis1)) (rest (icdr ilis1)))
	      (let ((vals (f elt)))
		(if (null-ilist? rest) vals
		    (appender vals (recur (icar rest) (icdr rest)))))))))


  (define (ipair-for-each proc ilis1 . ilists)
    (check-arg procedure? proc ipair-for-each)
    (if (pair? ilists)

	(let lp ((ilists (cons ilis1 ilists)))
	  (let ((itails (%cdrs ilists)))
	    (if (pair? itails)
		(begin (apply proc ilists)
		       (lp itails)))))

	;; Fast path.
	(let lp ((ilis ilis1))
	  (if (not (null-ilist? ilis))
	      (let ((tail (icdr ilis)))	; Grab the icdr now,
		(proc ilis)		; even though nothing can happen
		(lp tail))))))

  (define (ifilter-map f ilis1 . ilists)
    (check-arg procedure? f ifilter-map)
    (if (pair? ilists)
	(let recur ((ilists (cons ilis1 ilists)))
	  (receive (cars cdrs) (%cars+cdrs ilists)
	    (if (pair? cars)
		(cond ((apply f cars) => (lambda (x) (ipair x (recur cdrs))))
		      (else (recur cdrs))) ; Tail call in this arm.
		'())))
	
	;; Fast path.
	(let recur ((ilis ilis1))
	  (if (null-ilist? ilis) ilis
	      (let ((tail (recur (icdr ilis))))
		(cond ((f (icar ilis)) => (lambda (x) (ipair x tail)))
		      (else tail)))))))

;;; Map F across lists, guaranteeing to go left-to-right.

  (define (imap-in-order f lis1 . lists)
    (check-arg procedure? f imap-in-order)
    (if (pair? lists)
	(let recur ((lists (cons lis1 lists)))
	  (receive (cars cdrs) (%cars+cdrs lists)
	    (if (pair? cars)
		(let ((x (apply f cars)))		; Do head first,
		  (ipair x (recur cdrs)))		; then tail.
		'())))
	
	;; Fast path.
	(let recur ((lis lis1))
	  (if (null-ilist? lis) lis
	      (let ((tail (icdr lis))
		    (x (f (icar lis))))		; Do head ifirst,
		(ipair x (recur tail)))))))	; then tail.

  (define imap imap-in-order)

  (define (ifilter pred lis)
    (check-arg procedure? pred ifilter)
    (let recur ((lis lis))
      (if (null-ilist? lis) lis
	  (let ((head (icar lis))
		(tail (icdr lis)))
	    (if (pred head)
		(let ((new-tail (recur tail)))
		  (if (eq? tail new-tail) lis
		      (ipair head new-tail)))
		(recur tail))))))

  (define (ipartition pred lis)
    (check-arg procedure? pred ipartition)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values lis lis)
	  (let ((elt (icar lis))
		(tail (icdr lis)))
	    (receive (in out) (recur tail)
	      (if (pred elt)
		  (values (if (ipair? out) (ipair elt in) lis) out)
		  (values in (if (ipair? in) (ipair elt out) lis))))))))

;;; Inline us, please.
  (define-inline (iremove pred l) (ifilter  (lambda (x) (not (pred x))) l))

  (define-inline (idelete x lis :optional (= equal?)) 
    (ifilter (lambda (y) (not (= x y))) lis))

;;; Extended from R4RS to take an optional comparison argument.
  (define-inline (imember x lis :optional (= equal?))
    (ifind-tail (lambda (y) (= x y)) lis))

;;; The IMEMBER and then IFIND-TAIL call should definitely
;;; be inlined for IMEMQ & IMEMV.
  (define-inline (imemq    x lis) (imember x lis eq?))
  (define-inline (imemv    x lis) (imember x lis eqv?))

  (define (idelete-duplicates lis :optional (elt= equal?))
    (check-arg procedure? elt= idelete-duplicates)
    (let recur ((lis lis))
      (if (null-ilist? lis) lis
	  (let* ((x (icar lis))
		 (tail (icdr lis))
		 (new-tail (recur (idelete x tail elt=))))
	    (if (eq? tail new-tail) lis (ipair x new-tail))))))

  (define (iassoc x lis :optional (= equal?))
    (ifind (lambda (entry) (= x (icar entry))) lis))

  (define (ialist-cons key datum alist) (ipair (ipair key datum) alist))

  ;; what is this?
  ;;   (define (alist-copy alist)
  ;;     (imap (lambda (elt) (ipair (icar elt) (icdr elt)))
  ;; 	  alist))

  (define (ialist-delete key alist :optional (= equal?))
    (ifilter (lambda (elt) (not (= key (icar elt)))) alist))


  (define (ifind pred ilist)
    (cond ((ifind-tail pred ilist) => icar)
	  (else #f)))

  (define (ifind-tail pred ilist)
    (check-arg procedure? pred ifind-tail)
    (let lp ((ilist ilist))
      (and (not (null-ilist? ilist))
	   (if (pred (icar ilist)) ilist
	       (lp (icdr ilist))))))

  (define (itake-while pred lis)
    (check-arg procedure? pred itake-while)
    (let recur ((lis lis))
      (if (null-ilist? lis) '()
	  (let ((x (icar lis)))
	    (if (pred x)
		(ipair x (recur (icdr lis)))
		'())))))

  (define (idrop-while pred lis)
    (check-arg procedure? pred idrop-while)
    (let lp ((lis lis))
      (if (null-ilist? lis) '()
	  (if (pred (icar lis))
	      (lp (icdr lis))
	      lis))))

  (define (ispan pred lis)
    (check-arg procedure? pred ispan)
    (let recur ((lis lis))
      (if (null-ilist? lis) (values '() '())
	  (let ((x (icar lis)))
	    (if (pred x)
		(receive (prefix suffix) (recur (icdr lis))
		  (values (ipair x prefix) suffix))
		(values '() lis))))))

  (define (ibreak  pred lis) (ispan  (lambda (x) (not (pred x))) lis))
  (define (ievery pred lis1 . lists)
    (check-arg procedure? pred ievery)
    (if (pair? lists)

	;; N-ary case
	(receive (heads tails) (%cars+cdrs (ipair lis1 lists))
	  (or (not (ipair? heads))
	      (let lp ((heads heads) (tails tails))
		(receive (next-heads next-tails) (%cars+cdrs tails)
		  (if (ipair? next-heads)
		      (and (apply pred heads) (lp next-heads next-tails))
		      (apply pred heads)))))) ; Last PRED app is tail call.

	;; Fast path
	(or (null-ilist? lis1)
	    (let lp ((head (icar lis1))  (tail (icdr lis1)))
	      (if (null-ilist? tail)
		  (pred head)	; Last PRED app is tail call.
		  (and (pred head) (lp (icar tail) (icdr tail))))))))

  (define (iany pred ilis1 . ilists)
    (check-arg procedure? pred iany)
    (if (pair? ilists)

	;; N-ary case
	(receive (heads tails) (%cars+cdrs (cons ilis1 ilists))
	  (and (pair? heads)
	       (let lp ((heads heads) (tails tails))
		 (receive (next-heads next-tails) (%cars+cdrs tails)
		   (if (pair? next-heads)
		       (or (apply pred heads) (lp next-heads next-tails))
		       (apply pred heads)))))) ; Last PRED app is tail call.

	;; Fast path
	(and (not (null-ilist? ilis1))
	     (let lp ((head (icar ilis1)) (tail (icdr ilis1)))
	       (if (null-ilist? tail)
		   (pred head)            ; Last PRED app is tail call.
		   (or (pred head) (lp (icar tail) (icdr tail))))))))

  (define (ilist-index pred lis1 . lists)
    (check-arg procedure? pred ilist-index)
    (if (pair? lists)

	;; N-ary case
	(let lp ((lists (cons lis1 lists)) (n 0))
	  (receive (heads tails) (%cars+cdrs lists)
	    (and (pair? heads)
		 (if (apply pred heads) n
		     (lp tails (+ n 1))))))

	;; Fast path
	(let lp ((lis lis1) (n 0))
	  (and (not (null-ilist? lis))
	       (if (pred (icar lis)) n (lp (icdr lis) (+ n 1)))))))

;;; Reverse
;;;;;;;;;;;

  (define (ireverse lis) (ifold ipair '() lis))

  )
