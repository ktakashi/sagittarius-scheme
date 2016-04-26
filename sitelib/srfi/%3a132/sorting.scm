;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a132/sorting.scm - Sort Libraries
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :132 sorting)
    (export list-sorted?               vector-sorted?
	    list-sort                  vector-sort
	    list-stable-sort           vector-stable-sort
	    list-sort!                 vector-sort!
	    list-stable-sort!          vector-stable-sort!
	    list-merge                 vector-merge
	    list-merge!                vector-merge!
	    list-delete-neighbor-dups  vector-delete-neighbor-dups
	    list-delete-neighbor-dups! vector-delete-neighbor-dups!
	    vector-find-median         vector-find-median!
	    vector-select!             vector-separate!)
    (import (rename (rnrs) (vector-sort r6rs:vector-sort))
	    (rnrs mutable-pairs)
	    (rnrs r5rs)
	    (sagittarius) ;; for vector-copy
	    (rename (only (scheme base) 
			  vector-copy! vector-fill! vector-copy
			  exact-integer?)
		    (vector-copy! r7rs-vector-copy!)
		    (vector-fill! r7rs-vector-fill!)
		    (vector-copy  r7rs-vector-copy))
	    (srfi :27 random-bits))

(define (list-sorted? < lis)
  (let loop ((lis lis))
    (cond ((null? lis))
	  ((null? (cdr lis)))
	  ((< (car lis) (cadr lis)) (loop (cdr lis)))
	  (else #f))))

(define (vector-sorted? elt< vec :optional (start 0) (end (vector-length vec)))
  (when (> start end) (assertion-violation 'vector-sorted? "start > end"))
  (when (or (< start 0) (< end 0))
    (assertion-violation 'vector-sorted? "negative start or end"))
  (let loop ((i start))
    (cond ((= i end))
	  ((= (+ i 1) end))
	  ((elt< (vector-ref vec i) (vector-ref vec (+ i 1))) (loop (+ i 1)))
	  (else #f))))

(define list-stable-sort list-sort)
(define (vector-sort elt< v :optional (start 0) (end (vector-length v)))
  (let ((r (if (and (zero? start) (= end (vector-length v)))
	       v
	       (vector-copy v start end))))
    (r6rs:vector-sort elt< r)))

(define vector-stable-sort vector-sort)

;; For now.
(define list-sort! list-sort)
(define list-stable-sort! list-sort)

;;; Copyright of the following portion of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah. Don't you think source files should contain more lines
;;; of code than copyright notice?

(define (vector-stable-sort! elt< v :optional (start 0) (end (vector-length v)))
  (%vector-merge-sort! elt< v start end (vector-copy v)))
;;; %VECTOR-MERGE-SORT! is not exported.
;;; Preconditions:
;;;   V TEMP vectors
;;;   START END fixnums
;;;   START END legal indices for V and TEMP
;;; If these preconditions are ensured by the cover functions, you
;;; can safely change this code to use unsafe fixnum arithmetic and vector
;;; indexing ops, for *huge* speedup.

;;; This merge sort is "opportunistic" -- the leaves of the merge tree are
;;; contiguous runs of already sorted elements in the vector. In the best
;;; case -- an already sorted vector -- it runs in linear time. Worst case
;;; is still O(n lg n) time.

(define (%vector-merge-sort! elt< v0 l r temp0)
  (define (xor a b) (not (eq? a b)))

  ;; Merge v1[l,l+len1) and v2[l+len1,l+len1+len2) into target[l,l+len1+len2)
  ;; Merge left-to-right, so that TEMP may be either V1 or V2
  ;; (that this is OK takes a little bit of thought).
  ;; V2=TARGET? is true if V2 and TARGET are the same, which allows
  ;; merge to punt the final blit half of the time.
  
  (define (merge target v1 v2 l len1 len2 v2=target?)
    (letrec ((vblit (lambda (fromv j i end)    ; Blit FROMV[J,END) to TARGET[I,?]
		      (let lp ((j j) (i i))    ; J < END. The final copy.
			(vector-set! target i (vector-ref fromv j))
			(let ((j (+ j 1)))
			  (if (< j end) (lp j (+ i 1))))))))
      
      (let* ((r1 (+ l  len1))
	     (r2 (+ r1 len2)))
					; Invariants:
	(let lp ((n l); N is next index of 
		 (j l)   (x (vector-ref v1 l));   TARGET to write.   
		 (k r1)  (y (vector-ref v2 r1))); X = V1[J]          
	  (let ((n+1 (+ n 1))); Y = V2[K]          
	    (if (elt< y x)
		(let ((k (+ k 1)))
		  (vector-set! target n y)
		  (if (< k r2)
		      (lp n+1 j x k (vector-ref v2 k))
		      (vblit v1 j n+1 r1)))
		(let ((j (+ j 1)))
		  (vector-set! target n x)
		  (if (< j r1)
		      (lp n+1 j (vector-ref v1 j) k y)
		      (if (not v2=target?) (vblit v2 k n+1 r2))))))))))
  

  ;; Might hack GETRUN so that if the run is short it pads it out to length
  ;; 10 with insert sort...
  
  ;; Precondition: l < r.
  (define (getrun v l r)
    (let lp ((i (+ l 1))  (x (vector-ref v l)))
      (if (>= i r)
	  (- i l)
	  (let ((y (vector-ref v i)))
	    (if (elt< y x)
		(- i l)
		(lp (+ i 1) y))))))
  
  ;; RECUR: Sort V0[L,L+LEN) for some LEN where 0 < WANT <= LEN <= (R-L).
  ;;   That is, sort *at least* WANT elements in V0 starting at index L.
  ;;   May put the result into either V0[L,L+LEN) or TEMP0[L,L+LEN).
  ;;   Must not alter either vector outside this range.
  ;;   Return:
  ;;     - LEN -- the number of values we sorted
  ;;     - ANSVEC -- the vector holding the value
  ;;     - ANS=V0? -- tells if ANSVEC is V0 or TEMP
  ;;
  ;; LP: V[L,L+PFXLEN) holds a sorted prefix of V0.
  ;;     TEMP = if V = V0 then TEMP0 else V0. (I.e., TEMP is the other vec.)
  ;;     PFXLEN2 is a power of 2 <= PFXLEN.
  ;;     Solve RECUR's problem.
  (if (< l r) ; Don't try to sort an empty range.
      (let-values (((ignored-len ignored-ansvec ansvec=v0?)
		    (let recur ((l l) (want (- r l)))
		      (let ((len (- r l)))
			(let lp ((pfxlen (getrun v0 l r)) (pfxlen2 1)
				 (v v0) (temp temp0)
				 (v=v0? #t))
			  (if (or (>= pfxlen want) (= pfxlen len))
			      (values pfxlen v v=v0?)
			      (let ((pfxlen2 
				     (let lp ((j pfxlen2))
				       (let ((j*2 (+ j j)))
					 (if (<= j pfxlen) (lp j*2) j))))
				    (tail-len (- len pfxlen)))
				;; PFXLEN2 is now the largest power of 2 <= PFXLEN.
				;; (Just think of it as being roughly PFXLEN.)
				(let-values (((nr-len nr-vec nrvec=v0?)
					      (recur (+ pfxlen l) pfxlen2)))
				  (merge temp v nr-vec l pfxlen nr-len
					 (xor nrvec=v0? v=v0?))
				  (lp (+ pfxlen nr-len) (+ pfxlen2 pfxlen2)
				      temp v (not v=v0?))))))))))
	(if (not ansvec=v0?)
	    (r7rs-vector-copy! v0 l temp0 l r)))))

(define (list-merge < a b)
  (cond ((not (pair? a)) b)
	((not (pair? b)) a)
	(else (let recur ((x (car a)) (a a); A is a pair; X = (CAR A).
			  (y (car b)) (b b)); B is a pair; Y = (CAR B).
		(if (< y x)

		    (let ((b (cdr b)))
		      (if (pair? b)
			  (cons y (recur x a (car b) b))
			  (cons y a)))

		    (let ((a (cdr a)))
		      (if (pair? a)
			  (cons x (recur (car a) a y b))
			  (cons x b))))))))

;;; This destructive merge does as few SET-CDR!s as it can -- for example, if
;;; the list is already sorted, it does no SET-CDR!s at all. It is also
;;; iterative, running in constant stack.

(define (list-merge! < a b)
  ;; The logic of these two loops is completely driven by these invariants:
  ;;   SCAN-A: (CDR PREV) = A. X = (CAR A). Y = (CAR B).
  ;;   SCAN-B: (CDR PREV) = B. X = (CAR A). Y = (CAR B).
  (letrec ((scan-a (lambda (prev a x b y); Zip down A doing
		     (if (< y x); no SET-CDR!s until
			 (let ((next-b (cdr b))); we hit a B elt that
			   (set-cdr! prev b); has to be inserted.
			   (if (pair? next-b)
			       (scan-b b a x next-b (car next-b))
			       (set-cdr! b a)))

			 (let ((next-a (cdr a)))
			   (if (pair? next-a)
			       (scan-a a next-a (car next-a) b y)
			       (set-cdr! a b))))))

	   (scan-b (lambda (prev a x b y); Zip down B doing
		     (if (< y x); no SET-CDR!s until 
			 (let ((next-b (cdr b))); we hit an A elt that
			   (if (pair? next-b)  ; has to be
			       (scan-b b a x next-b (car next-b)) ; inserted.
			       (set-cdr! b a))) 

			 (let ((next-a (cdr a)))
			   (set-cdr! prev a)
			   (if (pair? next-a)
			       (scan-a a next-a (car next-a) b y)
			       (set-cdr! a b)))))))

    (cond ((not (pair? a)) b)
	  ((not (pair? b)) a)

	  ;; B starts the answer list.
	  ((< (car b) (car a))
	   (let ((next-b (cdr b)))
	     (if (null? next-b)
		 (set-cdr! b a)
		 (scan-b b a (car a) next-b (car next-b))))
	   b)

	  ;; A starts the answer list.
	  (else (let ((next-a (cdr a)))
		  (if (null? next-a)
		      (set-cdr! a b)
		      (scan-a a next-a (car next-a) b (car b))))
		a))))

;;; Merge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (vector-merge < v1 v2 [start1 end1 start2 end2]) -> vector
;;; (vector-merge! < v v1 v2 [start start1 end1 start2 end2]) -> unspecific
;;;
;;; Stable vector merge -- V1's elements come out ahead of equal V2 elements.

(define (vector-merge < v1 v2 :optional (start1 0) (end1 (vector-length v1))
		      (start2 0) (end2 (vector-length v2)))
  (let ((ans (make-vector (+ (- end1 start1) (- end2 start2)))))
    (%vector-merge! < ans v1 v2 0 start1 end1 start2 end2)
    ans))

(define (vector-merge! < v v1 v2 :optional (start 0) 
		       (start1 0) (end1 (vector-length v1))
		       (start2 0) (end2 (vector-length v2)))
  (%vector-merge! < v v1 v2 start start1 end1 start2 end2))


;;; This routine is not exported. The code is tightly bummed.
;;;
;;; If these preconditions hold, the routine can be bummed to run with 
;;; unsafe vector-indexing and fixnum arithmetic ops:
;;;   - V V1 V2 are vectors.
;;;   - START START1 END1 START2 END2 are fixnums.
;;;   - (<= 0 START END0 (vector-length V),
;;;     where end0 = start + (end1 - start1) + (end2 - start2)
;;;   - (<= 0 START1 END1 (vector-length V1))
;;;   - (<= 0 START2 END2 (vector-length V2))
;;; If you put these error checks in the two client procedures above, you can
;;; safely convert this procedure to use unsafe ops -- which is why it isn't
;;; exported. This will provide *huge* speedup.

(define (%vector-merge! elt< v v1 v2 start start1 end1 start2 end2)
  (letrec ((vblit (lambda (fromv j i end) ; Blit FROMV[J,END) to V[I,?].
		    (let lp ((j j) (i i))
		      (vector-set! v i (vector-ref fromv j))
		      (let ((j (+ j 1)))
			(if (< j end) (lp j (+ i 1))))))))

    (cond ((<= end1 start1) (if (< start2 end2) (vblit v2 start2 start end2)))
          ((<= end2 start2) (vblit v1 start1 start end1))

	  ;; Invariants: I is next index of V to write; X = V1[J]; Y = V2[K].
	  (else (let lp ((i start)
			 (j start1)  (x (vector-ref v1 start1))
			 (k start2)  (y (vector-ref v2 start2)))
		  (let ((i1 (+ i 1))); "i+1" is a complex number in R4RS!
		    (if (elt< y x)
			(let ((k (+ k 1)))
			  (vector-set! v i y)
			  (if (< k end2)
			      (lp i1 j x k (vector-ref v2 k))
			      (vblit v1 j i1 end1)))
			(let ((j (+ j 1)))
			  (vector-set! v i x)
			  (if (< j end1)
			      (lp i1 j (vector-ref v1 j) k y)
			      (vblit v2 k i1 end2))))))))))

;;; This version tries to use cons cells from input by sharing longest
;;; common tail between input & output. Still needs N stack frames, for ans
;;; of length N.
(define (list-delete-neighbor-dups = lis)
  (if (pair? lis)
      (let* ((x0 (car lis))
	     (xs (cdr lis))
	     (ans (let recur ((x0 x0) (xs xs))
		    (if (pair? xs)
			(let ((x1  (car xs))
			      (x2+ (cdr xs)))
			  (if (= x0 x1)
			      (recur x0 x2+)
			      (let ((ans-tail (recur x1 x2+)))
				(if (eq? ans-tail x2+) xs
				    (cons x1 ans-tail)))))
			xs))))
	(if (eq? ans xs) lis (cons x0 ans)))

      lis))

;;; LIST-DELETE-NEIGHBOR-DUPS!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code runs in constant list space, constant stack, and also
;;; does only the minimum SET-CDR!'s necessary.

(define (list-delete-neighbor-dups! = lis)
  (if (pair? lis)
      (let lp1 ((prev lis) (prev-elt (car lis)) (lis (cdr lis)))
	(if (pair? lis)
	    (let ((lis-elt (car lis))
		  (next (cdr lis)))
	      (if (= prev-elt lis-elt)

		  ;; We found the first elts of a run of dups, so we know
		  ;; we're going to have to do a SET-CDR!. Scan to the end of
		  ;; the run, do the SET-CDR!, and loop on LP1.
		  (let lp2 ((lis next))
		    (if (pair? lis)
			(let ((lis-elt (car lis))
			      (next (cdr lis)))
			  (if (= prev-elt lis-elt)
			      (lp2 next)
			      (begin (set-cdr! prev lis)
				     (lp1 lis lis-elt next))))
			(set-cdr! prev lis))); Ran off end => quit.

		  (lp1 lis lis-elt next))))))
  lis)


(define (vector-delete-neighbor-dups elt= v :optional (start 0) 
				     (end (vector-length v)))
  (if (< start end)
      (let* ((x (vector-ref v start))
	     (ans (let recur ((x x) (i start) (j 1))
		    (if (< i end)
			(let ((y (vector-ref v i))
			      (nexti (+ i 1)))
			  (if (elt= x y)
			      (recur x nexti j)
			      (let ((ansvec (recur y nexti (+ j 1))))
				(vector-set! ansvec j y)
				ansvec)))
			(make-vector j)))))
	(vector-set! ans 0 x)
	ans)
      '#()))


;;; Packs the surviving elements to the left, in range [start,end'),
;;; and returns END'.
(define (vector-delete-neighbor-dups! elt= v :optional (start 0)
				      (end (vector-length v)))
  (if (>= start end)
      end
      ;; To eliminate unnecessary copying (read elt i then write the value 
      ;; back at index i), we scan until we find the first dup.
      (let skip ((j start) (vj (vector-ref v start)))
	(let ((j+1 (+ j 1)))
	  (if (>= j+1 end)
	      end
	      (let ((vj+1 (vector-ref v j+1)))
		(if (not (elt= vj vj+1))
		    (skip j+1 vj+1)

		    ;; OK -- j & j+1 are dups, so we're committed to moving
		    ;; data around. In lp2, v[start,j] is what we've done;
		    ;; v[k,end) is what we have yet to handle.
		    (let lp2 ((j j) (vj vj) (k (+ j 2)))
		      (let lp3 ((k k))
			(if (>= k end)
			    (+ j 1) ; Done.
			    (let ((vk (vector-ref v k))
				  (k+1 (+ k 1)))
			      (if (elt= vj vk)
				  (lp3 k+1)
				  (let ((j+1 (+ j 1)))
				    (vector-set! v j+1 vk)
				    (lp2 j+1 vk k+1))))))))))))))
;;; End of source code portion from Olin Shivers


;;; Linear-time (average case) algorithms for:
;;;
;;; Selecting the kth smallest element from an unsorted vector.
;;; Selecting the kth and (k+1)st smallest elements from an unsorted vector.
;;; Selecting the median from an unsorted vector.

;;; These procedures are part of SRFI 132 but are missing from
;;; its reference implementation as of 10 March 2016.

;;; SRFI 132 says this procedure runs in O(n) time.
;;; As implemented, however, the worst-case time is O(n^2) because
;;; vector-select is implemented using randomized quickselect.
;;; The average time is O(n), and you'd have to be unlucky
;;; to approach the worst case.

(define (vector-find-median < v knil . rest)
  (let* ((mean (if (null? rest)
                   (lambda (a b) (/ (+ a b) 2))
                   (car rest)))
         (n (vector-length v)))
    (cond ((zero? n)
           knil)
          ((odd? n)
           (%vector-select < v (quotient n 2) 0 n))
          (else
           (let-values (((a b) (%vector-select2 < v (- (quotient n 2) 1) 0 n)))
	     (mean a b))))))

;;; For this procedure, the SRFI 132 specification
;;; demands the vector be sorted (by side effect).

(define (vector-find-median! < v knil . rest)
  (let* ((mean (if (null? rest)
                   (lambda (a b) (/ (+ a b) 2))
                   (car rest)))
         (n (vector-length v)))
    (vector-sort! < v)
    (cond ((zero? n)
           knil)
          ((odd? n)
           (vector-ref v (quotient n 2)))
          (else
           (mean (vector-ref v (- (quotient n 2) 1))
                 (vector-ref v (quotient n 2)))))))

;;; SRFI 132 says this procedure runs in O(n) time.
;;; As implemented, however, the worst-case time is O(n^2).
;;; The average time is O(n), and you'd have to be unlucky
;;; to approach the worst case.
;;;
;;; After rest argument processing, calls the private version defined below.

(define (vector-select < v k . rest)
  (let* ((start (if (null? rest)
                    0
                    (car rest)))
         (end (if (and (pair? rest)
                       (pair? (cdr rest)))
                  (car (cdr rest))
                  (vector-length v))))
    (%vector-select < v k start end)))

;;; The vector-select procedure is needed internally to implement
;;; vector-find-median, but SRFI 132 has been changed (for no good
;;; reason) to export vector-select! instead of vector-select.
;;; Fortunately, vector-select! is not required to have side effects.

(define vector-select! vector-select)

;;; This could be made slightly more efficient, but who cares?

(define (vector-separate! < v k . rest)
  (let* ((start (if (null? rest)
                    0
                    (car rest)))
         (end (if (and (pair? rest)
                       (pair? (cdr rest)))
                  (car (cdr rest))
                  (vector-length v))))
    (if (and (> k 0)
             (> end start))
        (let ((pivot (vector-select < v (- k 1) start end)))
          (call-with-values
           (lambda () (count-smaller < pivot v start end 0 0))
           (lambda (count count2)
             (let* ((v2 (make-vector count))
                    (v3 (make-vector (- end start count count2))))
               (copy-smaller! < pivot v2 0 v start end)
               (copy-bigger! < pivot v3 0 v start end)
               (r7rs-vector-copy! v start v2 0)
               (r7rs-vector-fill! v pivot (+ start count) (+ start count count2))
               (r7rs-vector-copy! v (+ start count count2) v3))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For small ranges, sorting may be the fastest way to find the kth element.
;;; This threshold is not at all critical, and may not even be worthwhile.

(define just-sort-it-threshold 50)

;;; Given
;;;     an irreflexive total order <?
;;;     a vector v
;;;     an index k
;;;     an index start
;;;     an index end
;;; with
;;;     0 <= k < (- end start)
;;;     0 <= start < end <= (vector-length v)
;;; returns
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;; but is usually faster than that.

(define (%vector-select <? v k start end)
  (assert (and 'vector-select
               (procedure? <?)
               (vector? v)
               (exact-integer? k)
               (exact-integer? start)
               (exact-integer? end)
               (<= 0 k (- end start 1))
               (<= 0 start end (vector-length v))))
  (%%vector-select <? v k start end))

;;; Given
;;;     an irreflexive total order <?
;;;     a vector v
;;;     an index k
;;;     an index start
;;;     an index end
;;; with
;;;     0 <= k < (- end start 1)
;;;     0 <= start < end <= (vector-length v)
;;; returns two values:
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k 1))
;;; but is usually faster than that.

(define (%vector-select2 <? v k start end)
  (assert (and 'vector-select
               (procedure? <?)
               (vector? v)
               (exact-integer? k)
               (exact-integer? start)
               (exact-integer? end)
               (<= 0 k (- end start 1 1))
               (<= 0 start end (vector-length v))))
  (%%vector-select2 <? v k start end))

;;; Like %vector-select, but its preconditions have been checked.

(define (%%vector-select <? v k start end)
  (let ((size (- end start)))
    (cond ((= 1 size)
           (vector-ref v (+ k start)))
          ((= 2 size)
           (cond ((<? (vector-ref v start)
                      (vector-ref v (+ start 1)))
                  (vector-ref v (+ k start)))
                 (else
                  (vector-ref v (+ (- 1 k) start)))))
          ((< size just-sort-it-threshold)
           (vector-ref (vector-sort <? (r7rs-vector-copy v start end)) k))
          (else
           (let* ((ip (random-integer size))
                  (pivot (vector-ref v (+ start ip))))
             (call-with-values
		 (lambda () (count-smaller <? pivot v start end 0 0))
	       (lambda (count count2)
		 (cond ((< k count)
			(let* ((n count)
			       (v2 (make-vector n)))
			  (copy-smaller! <? pivot v2 0 v start end)
			  (%%vector-select <? v2 k 0 n)))
		       ((< k (+ count count2))
			pivot)
		       (else
			(let* ((n (- size count count2))
			       (v2 (make-vector n))
			       (k2 (- k count count2)))
			  (copy-bigger! <? pivot v2 0 v start end)
			  (%%vector-select <? v2 k2 0 n)))))))))))

;;; Like %%vector-select, but returns two values:
;;;
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k))
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) (+ start k 1))
;;;
;;; Returning two values is useful when finding the median of an even
;;; number of things.

(define (%%vector-select2 <? v k start end)
  (let ((size (- end start)))
    (cond ((= 2 size)
           (let ((a (vector-ref v start))
                 (b (vector-ref v (+ start 1))))
             (cond ((<? a b)
                    (values a b))
                   (else
                    (values b a)))))
          ((< size just-sort-it-threshold)
           (let ((v2 (vector-sort <? (r7rs-vector-copy v start end))))
             (values (vector-ref v2 k)
                     (vector-ref v2 (+ k 1)))))
          (else
           (let* ((ip (random-integer size))
                  (pivot (vector-ref v (+ start ip))))
             (call-with-values
		 (lambda () (count-smaller <? pivot v start end 0 0))
	       (lambda (count count2)
		 (cond ((= (+ k 1) count)
			(values (%%vector-select <? v k start end)
				pivot))
		       ((< k count)
			(let* ((n count)
			       (v2 (make-vector n)))
			  (copy-smaller! <? pivot v2 0 v start end)
			  (%%vector-select2 <? v2 k 0 n)))
		       ((< k (+ count count2))
			(values pivot
				(if (< (+ k 1) (+ count count2))
				    pivot
				    (%%vector-select <? v (+ k 1) start end))))
		       (else
			(let* ((n (- size count count2))
			       (v2 (make-vector n))
			       (k2 (- k count count2)))
			  (copy-bigger! <? pivot v2 0 v start end)
			  (%%vector-select2 <? v2 k2 0 n)))))))))))

;;; Counts how many elements within the range are less than the pivot
;;; and how many are equal to the pivot, returning both of those counts.

(define (count-smaller <? pivot v i end count count2)
  (cond ((= i end)
         (values count count2))
        ((<? (vector-ref v i) pivot)
         (count-smaller <? pivot v (+ i 1) end (+ count 1) count2))
        ((<? pivot (vector-ref v i))
         (count-smaller <? pivot v (+ i 1) end count count2))
        (else
         (count-smaller <? pivot v (+ i 1) end count (+ count2 1)))))

;;; Like vector-copy! but copies an element only if it is less than the pivot.
;;; The destination vector must be large enough.

(define (copy-smaller! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? (vector-ref src start) pivot)
         (vector-set! dst at (vector-ref src start))
         (copy-smaller! <? pivot dst (+ at 1) src (+ start 1) end))
        (else
         (copy-smaller! <? pivot dst at src (+ start 1) end))))

;;; Like copy-smaller! but copies only elements that are greater than the pivot.

(define (copy-bigger! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? pivot (vector-ref src start))
         (vector-set! dst at (vector-ref src start))
         (copy-bigger! <? pivot dst (+ at 1) src (+ start 1) end))
        (else
         (copy-bigger! <? pivot dst at src (+ start 1) end))))

)
