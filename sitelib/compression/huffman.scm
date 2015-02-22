;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; compression/huffman.scm - Huffman coding
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; This is needed to implement HPACK. So may not work in a lot of
;; cases (at least EOS issue)
(library (compression huffman)
    (export huffman-code-table->encode-table
	    huffman-code-table->dfs-table
	    make-huffman-encoder
	    make-huffman-decoder
	    huffman-encode
	    huffman-decode)
    (import (rnrs)
	    (clos user)
	    (core errors) ;; for implementation-restriction-violation
	    (srfi :1 lists)
	    (sagittarius)
	    (sagittarius control))

;; TODO create table APIs (to implement HPACK we don't need it)
;; so for now those aren't provided (yet).

;; We keep dfs-table and encode-table to be a read/write invariance
;; form (vector for now) so that users can dump it to file or something.

;; table  ::= #( coding ... )
;; coding ::= #(symbol codeword)
;; symbol ::= u8
;; codeword ::= (bit ...)
;; bit ::= 0 | 1
(define (huffman-code-table->encode-table table)
  ;; sort vector just in cese
  (define sorted-table (vector-sort (lambda (a b)
				      (let ((as (vector-ref a 0))
					    (bs (vector-ref b 0)))
					(< as bs)))
				    table))
  (define table-size (vector-length table))
  (define (bit-list->number bits)
    ;; TODO better implementation
    ;; NB: bit operation is expensive on Sagittarius
    (string->number
     (list->string (map (lambda (bit) (if (zero? bit) #\0 #\1)) bits))
     2))
  ;; make 257 size vector with following structure
  ;; #(bit-length code)
  ;; if there is no entry then bit-length is -1 and code is #f
  ;; NB: we need bit-length so that we can store code in number.
  ;; NB: the last one is EOS. it's handled kinda specially
  ;;     this is needed because we don't do bit by bit
  ;;     FIXME: above assumpiton is not required by pure huffman coding.
  ;;            but how? as long as we don't know input bit length, then
  ;;            there is no way to stop. filler is kinda required thing.
  (let1 encode-table (make-vector 257 #(-1 #f))
    (let loop ((i 0) (j 0))
      (if (or (= i 257) (= j table-size))
	  encode-table
	  (let1 e (vector-ref sorted-table j)
	    (if (= (vector-ref e 0) i)
		(let1 codeword (vector-ref e 1)
		  (when (> (length codeword) 32)
		    (implementation-restriction-violation 
		     'huffman-code-table->encode-table
		     "Codeword is more than 32 bits long" codeword))
		  (vector-set! encode-table i
			       (vector (length codeword)
				       (bit-list->number codeword)))
		  (loop (+ i 1) (+ j 1)))
		(loop (+ i 1) j)))))))

(define (make-huffman-encoder table)
  (let1 encode-table (huffman-code-table->encode-table table)
    (lambda (in out) (huffman-encode in out encode-table))))

;; FIXME this emitting too much bits at EOS.
(define (huffman-encode in out encode-table)
  (define carry #f)
  (define carry-size #f)
  (define (carry-shift-size bits)
    (let1 n (+ carry-size bits)
      (if (> n 8)
	  (- 8 carry-size)
	  bits)))
  (define (mask n) (- (bitwise-arithmetic-shift 1 n) 1))
  (let loop ()
    (let1 u8 (get-u8 in)
      (let* ((end? (eof-object? u8))
	     (e  (vector-ref encode-table (if end? 256 u8)))
	     (bits (vector-ref e 0))
	     (code (vector-ref e 1)))
	;;(print bits ":" code "(" end? ")")
	(if (and end? (not code))
	    (when carry (put-u8 out carry))
	    ;; ex) a:110 x 3
	    ;;     8 bits of above is 11011011 0
	    ;;     so first iteration makes carry 110 and size 3
	    ;;     second iteration, 110110, 6
	    (let loop2 ((bits bits) (code code))
	      ;;(print "  - " bits ":" code "[" carry "," carry-size "]")
	      (cond (carry-size
		     (let* ((carry-shift (carry-shift-size bits))
			    (ccarry (bitwise-arithmetic-shift-left 
				     carry carry-shift))
			    (code-shift (- bits carry-shift))
			    (ccode (bitwise-arithmetic-shift-right
				    code code-shift)))
		       ;; (print "  +" ccarry "," ccode ","
		       ;;       carry-shift "," code-shift)
		       (cond ((= 8 (+ carry-shift carry-size))
			      (put-u8 out (bitwise-ior ccarry ccode))
			      (set! carry #f)
			      (set! carry-size #f)
			      (if (zero? code-shift)
				  (unless end? (loop))
				  (loop2 code-shift
					 (bitwise-and code (mask code-shift)))))
			     (else
			      (set! carry (bitwise-ior ccarry ccode))
			      ;;(print "   [new carry] " ccarry)
			      (set! carry-size (+ carry-shift carry-size))
			      (unless end? (loop))))))
		    ;; easy :)
		    ((= bits 8) (put-u8 out code) (unless end? (loop)))
		    (else 
		     ;; first put until carry
		     (do ((bits bits (- bits 8))
			  (code code (bitwise-arithmetic-shift-right
				      code 8)))
			 ((<= bits 8)
			  (set! carry-size bits)
			  (set! carry code))
		       (put-u8 out (bitwise-arithmetic-shift-right
				    code (- bits 8))))
		     ;; (print "  *" carry "," carry-size)
		     (unless end? (loop))))))))))

(define (huffman-code-table->dfs-table table)
  (let1 root (huffman-table->binary-tree table)
    (dfs-setid! root '())
    (dfs! root root)
    (node->dfs-table root)))

(define (make-huffman-decoder table)
  (let1 dfs-table (huffman-code-table->dfs-table table)
    (lambda (in out) (huffman-decode in out dfs-table))))

;; Algorithm from
;; http://graphics.ics.uci.edu/pub/Prefix.pdf
;; Word size = 4
(define (huffman-decode in out dfs-table)
  (define (decode out in state eos? table)
    ;; e = #(id symbol? fail? accepted? syms)
    (let1 e (vector-ref (vector-ref table state) in)
      (when (vector-ref e 2)
	(error 'huffman-decode "Failed to decode Huffman coding input"))
      (when (vector-ref e 1)
	(for-each (lambda (c) (put-u8 out c)) (vector-ref e 4)))
      (values (vector-ref e 0) (vector-ref e 3))))
  (let loop ((state 0) (eos? #t))
    (let1 u8 (get-u8 in)
      (unless (eof-object? u8)
	(let ((hi (bitwise-arithmetic-shift-right u8 4))
	      (lo (bitwise-and u8 #x0f)))
	  (let*-values (((state eos?) (decode out hi state eos? dfs-table))
			((state eos?) (decode out lo state eos? dfs-table)))
	    (loop state eos?)))))))

;;; Internal procedures

;;; DFS convertion is taken from khufftbl.py. Below is the original Copyright

;; Copyright (c) 2012, 2014 Tatsuhiro Tsujikawa
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; binary tree... built-in treemap can't be used since we don't expose
;; entry structure
(define-class <node> ()
  ((term :init-keyword :term :init-value #f)
   (left :init-value #f)
   (right :init-value #f)
   (trans :init-value '())
   (id :init-value #f)
   (accept :init-value #f)))

;; for debugging
(define (dump-node node)
  (define (rec node depth)
    (let ((right-s (if (slot-ref node 'right)
		       (rec (slot-ref node 'right) (+ depth 1))
		       ""))
	  (own (string-append "\n" 
			      (make-string (* depth 4) #\space)
			      (format "~a[~a]" (slot-ref node 'term)
				      (slot-ref node 'id))))
	  (left-s  (if (slot-ref node 'left)
		       (rec (slot-ref node 'left) (+ depth 1))
		       "")))
      (string-append right-s own left-s)))
  (print (rec node 0)))

(define (insert! node char bits)
  (define (rec node char bits)
    (if (null? bits)
	(slot-set! node 'term char)
	(let1 child (if (zero? (car bits))
			(begin
			  (unless (slot-ref node 'left)
			    (slot-set! node 'left (make <node>)))
			  (slot-ref node 'left))
			(begin
			  (unless (slot-ref node 'right)
			    (slot-set! node 'right (make <node>)))
			  (slot-ref node 'right)))
	  (rec child char (cdr bits)))))
  (rec node char bits))

;; given table is a vector of vectors
;; table  ::= #( coding ... )
;; coding ::= #(symbol codeword)
;; symbol ::= u8
;; codeword ::= (bit ...)
;; bit ::= 0 | 1
(define (huffman-table->binary-tree table)
  (rlet1 root (make <node> :term #f)
    (dotimes (i (vector-length table))
      (let* ((coding (vector-ref table i))
	     (char (vector-ref coding 0))
	     (codeword (vector-ref coding 1)))
	(when (> (length codeword) 32)
	  (implementation-restriction-violation 'huffman-table->binary-tree
	    "Codeword is more than 32 bits long" codeword))
	(insert! root char codeword)))))

(define (dfs-setid! node prefix)
  (define idseed 0)
  (define (rec node prefix)
    (unless (slot-ref node 'term)
      (let ((len (length prefix)))
	(when (and (<= len 7) 
		   (equal? (list-tabulate len (lambda (n) 1)) prefix))
	  (slot-set! node 'accept #t))
	(slot-set! node 'id idseed)
	(set! idseed (+ idseed 1))
	(rec (slot-ref node 'left) (cons 0 prefix))
	(rec (slot-ref node 'right) (cons 1 prefix)))))
  (rec node prefix))

(define (dfs! node root)
  (define (traverse! node bits syms start-node root depth)
    (define (go node bit)
      (let ((nbits (cons bit bits))
	    (nsyms (cond ((slot-ref node 'term) =>
			  (lambda (t) (cons t syms)))
			 (else syms))))
	(traverse! node nbits nsyms start-node root (+ depth 1))))
    ;; word size = 4bit
    (if (= depth 4)
	(let ((trans (slot-ref start-node 'trans))
	      (e (if (memv 256 syms)
		     (vector #f bits '())
		     (vector node bits syms))))
	  (slot-set! start-node 'trans (append! trans (list e))))
	(let1 node (if (slot-ref node 'term)
		       root
		       node)
	  (go (slot-ref node 'left) 0)
	  (go (slot-ref node 'right) 1))))
  (when node
    (traverse! node '() '() node root 0)
    (dfs! (slot-ref node 'left) root)
    (dfs! (slot-ref node 'right) root)))

(define (node->dfs-table node)
  (define (rec node acc root?)
    (if (slot-ref node 'term)
	acc
	(let loop ((es (slot-ref node 'trans)) (r '()))
	  (if (null? es)
	      (if root?
		  (list (list->vector (reverse! r)))
		  (append (rec (slot-ref node 'left) 
			       (append acc (list (list->vector (reverse! r))))
			       #f)
			  (rec (slot-ref node 'right) '() #f)))
	      (let* ((e    (car es))
		     (nd   (vector-ref e 0))
		     (bits (vector-ref e 1))
		     ;; assume this will be u8 bytes...
		     ;; TODO what if?
		     (syms (reverse (vector-ref e 2))))
		(define (analyse nd syms)
		  (let-values (((id fail? accepted?)
				(if nd
				    (let ((id (slot-ref nd 'id)))
				      (cond ((not id) (values 0 #f #t))
					    ((slot-ref nd 'accept)
					     (values id #f #t))
					    (else (values id #f #f))))
				    (values 0 #t #f))))
		    (values id (not (null? syms)) fail? accepted?)))
		(let-values (((id symbol? fail? accepted?)
			      (analyse nd syms)))
		  (loop (cdr es)
			(cons (vector id symbol? fail? accepted? syms) r))))))))
  (list->vector (append (rec node '() #t)
			(rec (slot-ref node 'left) '() #f)
			(rec (slot-ref node 'right) '() #f))))

)    
