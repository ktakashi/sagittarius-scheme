;; for a bit of better performance
#!nounbound
(library (core base)
    (export hashtable-for-each hashtable-map hashtable-fold hashtable->alist
	    print wrong-type-argument-message
	    string-for-each string-join
	    null-list?
	    find find-tail
	    assp assoc
	    member
	    delete delete!
	    lset-union lset-intersection lset-difference
	    take drop list-head
	    char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
	    bytevector-uint-ref bytevector-sint-ref
	    bytevector-uint-set! bytevector-sint-set!
	    bytevector->uint-list bytevector->sint-list
	    uint-list->bytevector sint-list->bytevector
	    for-all exists
	    split-at filter filter! partition
	    map for-each filter-map
	    reduce fold fold-left fold-right
	    remp remove remv remq memp
	    list-sort
	    vector-map vector-map! vector-for-each vector-sort vector-sort!
	    call-with-values call-with-port
	    open-bytevector-output-port call-with-bytevector-output-port
	    open-string-output-port call-with-string-output-port
	    hashtable-equivalence-function hashtable-hash-function)
    (import (core)
	    (sagittarius)
	    (sagittarius vm))

(define (hashtable-for-each (proc procedure?) (ht hashtable?))
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ()
      (let-values (((k v) (itr eof)))
	(unless (eq? k eof)
	  (proc k v) (loop))))))

(define (hashtable-map (proc procedure?) (ht hashtable?))
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ((r '()))
      (let-values (((k v) (itr eof)))
	(if (eq? k eof)
	    r
	    (loop (cons (proc k v) r)))))))

(define (hashtable-fold (kons procedure?) (ht hashtable?) knil)
  (let ((itr (%hashtable-iter ht))
	(eof (cons #t #t)))
    (let loop ((r knil))
      (let-values (((k v) (itr eof)))
	(if (eq? k eof)
	    r
	    (loop (kons k v r)))))))

(define (hashtable->alist ht) (hashtable-map cons ht))

#;(define (any pred ls)
  (if (pair? ls) (if (pred (car ls)) (car ls) (any pred (cdr ls))) #f))

(define (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;; print
(define (print . args)
  (for-each display args)
  (newline))

(define (fold proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis lst1) (knil seed))
	(if (null? lis)
	    knil
	    (loop (cdr lis) (proc (car lis) knil))))
      (let loop ((lis (apply list-transpose* lst1 lst2)) (knil seed))
	(if (null? lis)
	    knil 
	    (loop (cdr lis) (apply proc (append (car lis) (list knil))))))))

;; from Ypsilon
(define (wrong-type-argument-message expect got . nth)
  (if (null? nth)
      (format "expected ~a, but got ~a" expect got)
      (format "expected ~a, but got ~a, as argument ~a" expect got (car nth))))

;; From Gauche
;; we don't define SRFI-43 vector-map/vector-for-each here
;; so make those tabulate/update! internal define for better performance.
(define (vector-map proc vec . more)
  (define (vector-tabulate len proc)
    (let loop ((i 0) (r '()))
      (if (= i len)
	  (list->vector (reverse! r))
	  (loop (+ i 1) (cons (proc i) r)))))
  (if (null? more)
      (vector-tabulate (vector-length vec)
		       (lambda (i) (proc (vector-ref vec i))))
      (let ((vecs (cons vec more)))
	(vector-tabulate (apply min (map vector-length vecs))
			 (lambda (i)
			   (apply proc (map (lambda (v) (vector-ref v i))
					    vecs)))))))
(define (vector-map! proc vec . more)
  (define (vector-update! vec len proc)
    (let loop ((i 0))
      (if (= i len)
	  vec
	  (begin
	    (vector-set! vec i (proc i))
	    (loop (+ i 1))))))
  (if (null? more)
      (vector-update! vec (vector-length vec)
		      (lambda (i) (proc (vector-ref vec i))))
      (let ((vecs (cons vec more)))
	(vector-update! vec (apply min (map vector-length vecs))
			(lambda (i)
			  (apply proc (map (lambda (v) (vector-ref v i))
					   vecs)))))))

(define (vector-for-each proc vec . more)
  (if (null? more)
      (let ((len (vector-length vec)))
	(let loop ((i 0))
	  (unless (= i len)
	    (proc (vector-ref vec i))
	    (loop (+ i 1)))))
      (let* ((vecs (cons vec more))
	     (len  (apply min (map vector-length vecs))))
	(let loop ((i 0))
	  (unless (= i len)
	    (apply proc (map (lambda (v) (vector-ref v i)) vecs))
	    (loop (+ i 1)))))))

;; same as vector-for-each
(define (string-for-each proc str . more)
  (if (null? more)
      (let ((len (string-length str)))
	(let loop ((i 0))
	  (unless (= i len)
	    (proc (string-ref str i))
	    (loop (+ i 1)))))
      (let* ((strs (cons str more))
	     (len  (apply min (map string-length strs))))
	(let loop ((i 0))
	  (unless (= i len)
	    (apply proc (map (lambda (s) (string-ref s i)) strs))
	    (loop (+ i 1)))))))

;;;;
;; from SRFI-13

;;; (string-join string-list [delimiter grammar]) => string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paste strings together using the delimiter string.
;;;
;;; (join-strings '("foo" "bar" "baz") ":") => "foo:bar:baz"
;;;
;;; DELIMITER defaults to a single space " "
;;; GRAMMAR is one of the symbols {prefix, infix, strict-infix, suffix} 
;;; and defaults to 'infix.
;;;
;;; I could rewrite this more efficiently -- precompute the length of the
;;; answer string, then allocate & fill it in iteratively. Using 
;;; STRING-CONCATENATE is less efficient.

(define (string-join strings :optional (delim " ") (grammar 'infix))
  (define (buildit lis final)
    (let recur ((lis lis))
      (if (pair? lis)
	  (cons delim (cons (car lis) (recur (cdr lis))))
	  final)))
  (unless (string? delim)
    (error 'string-join "Delimiter must be a string" delim))
  (cond ((pair? strings)
	 (string-concatenate
	  (case grammar
	    ((infix strict-infix)
	     (cons (car strings) (buildit (cdr strings) '())))
	    ((prefix) (buildit strings '()))
	    ((suffix)
	     (cons (car strings) (buildit (cdr strings) (list delim))))
	    (else (error 'string-join "Illegal join grammar"
			 grammar string-join)))))
	((not (null? strings))
	 (error 'string-join "STRINGS parameter not list."
		strings string-join))
	((eq? grammar 'strict-infix)
	 (error 'string-join 
		"Empty list cannot be joined with STRICT-INFIX grammar."
		string-join))
	(else "")))		; Special-cased for infix grammar.

;;;;
;; from Ypsilon

;; from srfi-1 start
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (assertion-violation 'null-list? "argument out of domain" l))))

(define (split-at x (k integer?))
  (let recur ((lis x) (k k) (r '()))
    (cond ((zero? k) (values (reverse! r) lis))
	  ((null? lis) (error 'split-at "given list it too short"))
	  (else (recur (cdr lis) (- k 1) (cons (car lis) r))))))

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

(define (find-tail (pred procedure?) list)
  (let lp ((list list))
    (and (not (null? list))
     (if (pred (car list)) list
         (lp (cdr list))))))

(define (assoc x (lis list?) . =)
  (if (null? =)
      (assoc x lis equal?)
      (find (lambda (entry) ((car =) x (car entry))) lis)))

(define (member x lis . =)
  (if (null? =)
      (member x lis equal?)
      (find-tail (lambda (y) ((car =) x y)) lis)))

(define (delete x lis . =)
  (if (null? =)
      (delete x lis equal?)
      (filter (lambda (y) (not ((car =) x y))) lis)))

(define (delete! x lis . =)
  (if (null? =)
      (delete x lis equal?)
      (filter! (lambda (y) (not ((car =) x y))) lis)))

(define (reduce (f procedure?) ridentity lis)
  (if (null? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (lset-union (= procedure?) . lists)
  (reduce (lambda (lis ans)     ; Compute ANS + LIS.
	    (cond ((null? lis) ans) ; Don't copy any lists
		  ((null? ans) lis)     ; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans)
			   (if (exists (lambda (x) (= x elt)) ans)
			       ans
			       (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (lset-intersection (= procedure?) lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((exists null? lists) '())      ; Short cut
      ((null? lists)          lis1)     ; Short cut
      (else (filter (lambda (x)
              (for-all (lambda (lis) (member x lis =)) lists))
            lis1)))))

(define (lset-difference (= procedure?) lis1 . lists)
  (let ((lists (filter pair? lists)))   ; Throw out empty lists.
    (cond ((null? lists)     lis1)  ; Short cut
      ((memq lis1 lists) '())   ; Short cut
      (else (filter (lambda (x)
              (for-all (lambda (lis) (not (member x lis =)))
                 lists))
            lis1)))))

(define (take lis (k integer?))
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
    (cons (car lis)
          (recur (cdr lis) (- k 1))))))

(define (drop lis (k integer?))
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))


(define list-head take)

;;;;
;; standard libraries

;; 1 Unicode
;; 1.1 characters
(define (make-ci-comparison = foldcase)
  (lambda (e1 e2 . rest)
    (let loop ((e1 (foldcase e1)) (e2 (foldcase e2)) (e* rest))
      (and (= e1 e2)
	   (or (null? e*)
	       (loop e2 (foldcase (car e*)) (cdr e*)))))))

(define char-ci=? (make-ci-comparison char=? char-foldcase))
(define char-ci<? (make-ci-comparison char<? char-foldcase))
(define char-ci>? (make-ci-comparison char>? char-foldcase))
(define char-ci<=? (make-ci-comparison char<=? char-foldcase))
(define char-ci>=? (make-ci-comparison char>=? char-foldcase))

;; 1.2 strings
(define string-ci=? (make-ci-comparison string=? string-foldcase))
(define string-ci<? (make-ci-comparison string<? string-foldcase))
(define string-ci>? (make-ci-comparison string>? string-foldcase))
(define string-ci<=? (make-ci-comparison string<=? string-foldcase))
(define string-ci>=? (make-ci-comparison string>=? string-foldcase))

;; 2 Bytevectors
;; 2.4 operations on integers of arbitary size
;; from Ypsilon
;; we can't use macro in this file so expand by hand!!
;;(define-syntax div256
;;  (syntax-rules ()
;;    ((_ x) (bitwise-arithmetic-shift x -8))))
;;
;;(define-syntax mod256
;;  (syntax-rules ()
;;    ((_ x) (bitwise-and x 255))))
;;
;; This moved to (rnrs bytevectors)
;;(define-syntax endianness
;;  (syntax-rules (big little native)
;;    ((_ big) 'big)
;;    ((_ little) 'little)
;;    ((_ native) (native-endianness))))

(define (bytevector-uint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (let ((end (+ index size)))
           (let loop ((i index) (acc 0))
             (if (>= i end)
                 acc
                 (loop (+ i 1) (+ (* 256 acc) (bytevector-u8-ref bv i)))))))
        ((eq? endien 'little)
         (let loop ((i (+ index size -1)) (acc 0))
           (if (< i index)
               acc
               (loop (- i 1) (+ (* 256 acc) (bytevector-u8-ref bv i))))))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~s, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-sint-ref bv index endien size)
  (cond ((eq? endien 'big)
         (if (> (bytevector-u8-ref bv index) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        ((eq? endien 'little)
         (if (> (bytevector-u8-ref bv (+ index size -1)) 127)
             (- (bytevector-uint-ref bv index endien size) (expt 256 size))
             (bytevector-uint-ref bv index endien size)))
        (else
         (assertion-violation 'bytevector-uint-ref
                              (format "expected endianness, but got ~s, as argument 3" endien)
                              (list bv index endien size)))))

(define (bytevector-uint-set! bv index val endien size)
  (cond ((= val 0)
         (let ((end (+ index size)))
           (let loop ((i index))
             (cond ((>= i end) (undefined))
                   (else
                    (bytevector-u8-set! bv i 0)
                    (loop (+ i 1)))))))
        ((< 0 val (expt 256 size))
         (cond ((eq? endien 'big)
                (let ((start (- (+ index size) 1)))
                  (let loop ((i start) (acc val))
                    (cond ((< i index) (undefined))
                          (else
                           ;; mod256 -> bitwise-and
                           (bytevector-u8-set! bv i (bitwise-and acc 255))
                           ;; div256 -> bitwise-arithmetic-shift
                           (loop (- i 1) (bitwise-arithmetic-shift acc -8)))))))
               ((eq? endien 'little)
                (let ((end (+ index size)))
                  (let loop ((i index) (acc val))
                    (cond ((>= i end) (undefined))
                          (else
                           ;; mod256 -> bitwise-and
                           (bytevector-u8-set! bv i (bitwise-and acc 255))
                           ;; div256 -> bitwise-arithmetic-shift
                           (loop (+ i 1) (bitwise-arithmetic-shift acc -8)))))))))
        (else
         (assertion-violation 'bytevector-uint-set!
                              (format "value out of range, ~s as argument 3" val)
                              (list bv index val endien size))))
  (undefined))

(define (bytevector-sint-set! bv index val endien size)
  (let* ((p-bound (expt 2 (- (* size 8) 1)))
         (n-bound (- (+ p-bound 1))))
    (if (< n-bound val p-bound)
        (if (>= val 0)
            (bytevector-uint-set! bv index val endien size)
            (bytevector-uint-set! bv index (+ val (expt 256 size)) endien size))
        (assertion-violation 'bytevector-sint-set!
                             (format "value out of range, ~s as argument 3" val)
                             (list bv index val endien size))))
  (undefined))

(define (bytevector->uint-list bv endien size)
  (let loop ((i (- (bytevector-length bv) size)) (acc '()))
    (if (> i -1)
        (loop (- i size) (cons (bytevector-uint-ref bv i endien size) acc))
        (if (= i (- size))
            acc
            (assertion-violation 'bytevector->uint-list
                                 (format "expected appropriate element size as argument 3, but got ~s" size)
                                 (list bv endien size))))))

(define (bytevector->sint-list bv endien size)
  (let loop ((i (- (bytevector-length bv) size)) (acc '()))
    (if (> i -1)
        (loop (- i size) (cons (bytevector-sint-ref bv i endien size) acc))
        (if (= i (- size))
            acc
            (assertion-violation 'bytevector->sint-list
                                 (format "expected appropriate element size as argument 3, but got ~s" size)
                                 (list bv endien size))))))

(define (uint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-uint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))

(define (sint-list->bytevector lst endien size)
  (let ((bv (make-bytevector (* size (length lst)))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
            (else
             (bytevector-sint-set! bv i (car lst) endien size)
             (loop (+ i size) (cdr lst)))))))

;; 3 list utilities

(define (for-all pred lst1 . lst2)
  (define (for-all-n pred list-of-lists)
    (let ((argc (length list-of-lists)))
      (define (collect-cdr lst)
        (let loop ((lst lst))
          (cond ((null? lst) '())
                ((null? (cdar lst)) (loop (cdr lst)))
                (else (cons (cdar lst) (loop (cdr lst)))))))
      (define (collect-car lst)
        (let loop ((lst lst))
          (cond ((null? lst) '())
                ((pair? (car lst))
                 (cons (caar lst) (loop (cdr lst))))
                (else
                 (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" (car lst)) list-of-lists)))))

      (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
        (or (= (length head) argc)
            (assertion-violation 'for-all "expected same length chains of pairs" list-of-lists))
        (if (null? rest)
            (apply pred head)
            (and (apply pred head)
                 (loop (collect-car rest) (collect-cdr rest)))))))

  (define (for-all-n-quick pred lst)
    (or (null? lst)
        (let loop ((head (car lst)) (rest (cdr lst)))
          (if (null? rest)
              (apply pred head)
              (and (apply pred head)
                   (loop (car rest) (cdr rest)))))))

  (define (for-all-1 pred lst)
    (cond ((null? lst) #t)
          ((pair? lst)
           (let loop ((head (car lst)) (rest (cdr lst)))
             (cond ((null? rest) (pred head))
                   ((pair? rest)
                    (and (pred head)
                         (loop (car rest) (cdr rest))))
                   (else
                    (and (pred head)
                         (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" rest) (list pred lst)))))))
          (else
           (assertion-violation 'for-all (format "expected chain of pairs, but got ~a, as argument 2" lst) (list pred lst)))))

  (cond ((null? lst2)
         (for-all-1 pred lst1))
        ((apply list-transpose+ lst1 lst2)
         => (lambda (lst) (for-all-n-quick pred lst)))
        (else
         (for-all-n pred (cons lst1 lst2)))))

(define (exists pred lst1 . lst2)
  (define (exists-1 pred lst)
    (cond ((null? lst) #f)
          ((pair? lst)
           (let loop ((head (car lst)) (rest (cdr lst)))
             (cond ((null? rest) (pred head))
                   ((pred head))
                   ((pair? rest) (loop (car rest) (cdr rest)))
                   (else
                    (assertion-violation 'exists (format "traversal reached to non-pair element ~s" rest) (list pred lst))))))
          (else
           (assertion-violation 'exists (format "expected chain of pairs, but got ~a, as argument 2" lst) (list pred lst)))))
  (define (exists-n-quick pred lst)
    (and (pair? lst)
         (let loop ((head (car lst)) (rest (cdr lst)))
           (if (null? rest)
               (apply pred head)
               (or (apply pred head)
                   (loop (car rest) (cdr rest)))))))
  (define (exists-n pred list-of-lists)
    (let ((argc (length list-of-lists)))
      (define (collect-cdr lst)
        (let loop ((lst lst))
          (cond ((null? lst) '())
                ((null? (cdar lst)) (loop (cdr lst)))
                (else (cons (cdar lst) (loop (cdr lst)))))))
      (define (collect-car lst)
        (let loop ((lst lst))
          (cond ((null? lst) '())
                ((pair? (car lst))
                 (cons (caar lst) (loop (cdr lst))))
                (else
                 (assertion-violation 'exists (format "traversal reached to non-pair element ~s" (car lst)) list-of-lists)))))

      (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
        (or (= (length head) argc)
            (assertion-violation 'exists "expected same length chains of pairs" list-of-lists))
        (if (null? rest)
            (apply pred head)
            (or (apply pred head)
                (loop (collect-car rest) (collect-cdr rest)))))))
  (cond ((null? lst2)
         (exists-1 pred lst1))
        ((apply list-transpose+ lst1 lst2)
         => (lambda (lst) (exists-n-quick pred lst)))
        (else
         (exists-n pred (cons lst1 lst2)))))

(define (filter pred lst)
  (let loop ((lst lst) (acc '()))
    (cond ((null? lst) (reverse! acc))
	  ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc)))
	  (else (loop (cdr lst) acc)))))

;; from SRFI-1, reference implementation
(define (filter! pred lis)
  (let lp ((ans lis))
    (cond ((null? ans)            ans)		  ; Scan looking for
	  ((not (pred (car ans))) (lp (cdr ans))) ; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (if (pair? lis)
					(if (pred (car lis))
					    (scan-in lis (cdr lis))
					    (scan-out prev (cdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (pair? lis)
					   (if (pred (car lis))
					       (begin (set-cdr! prev lis)
						      (scan-in lis (cdr lis)))
					       (lp (cdr lis)))
					   (set-cdr! prev lis))))))
		  (scan-in ans (cdr ans))
		  ans)))))

(define (partition pred lst)
  (let loop ((lst lst) (acc1 '()) (acc2 '()))
    (cond ((null? lst) (values (reverse! acc1) (reverse! acc2)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
          (else (loop (cdr lst) acc1 (cons (car lst) acc2))))))

(define (map proc lst1 . lst2)
  (if (null? lst2)
      (let loop ((xs lst1) (r '()))
	(cond ((pair? xs) (loop (cdr xs) (cons (proc (car xs)) r)))
	      ((null? xs) (reverse! r))
	      (else
	       (assertion-violation 'map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))
      (let loop ((xs (apply list-transpose* lst1 lst2)) (r '()))
	(cond ((pair? xs) (loop (cdr xs) (cons (apply proc (car xs)) r)))
	      ((null? xs) (reverse! r))
	      (else
	       (assertion-violation 'map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))))

(define (for-each proc lst1 . lst2)
  (if (null? lst2)
      (let loop ((xs lst1))
	(cond ((pair? xs) (proc (car xs)) (loop (cdr xs)))
	      ((null? xs) (undefined))
	      (else
	       (assertion-violation 'for-each 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))
      (let loop ((xs (apply list-transpose* lst1 lst2)))
	(cond ((pair? xs) (apply proc (car xs)) (loop (cdr xs)))
	      ((null? xs) (undefined))
	      (else
	       (assertion-violation 'for-each
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))))

;; it's used very often in the boot code so put it here
(define (filter-map (proc procedure?) lst1 . lst2)
  (if (null? lst2)
      (let loop ((lst lst1) (r '()))
	(cond ((null? lst) (reverse! r))
	      ((pair? lst)
	       (cond ((proc (car lst)) =>
		      (lambda (x) (loop (cdr lst) (cons x r))))
		     (else (loop (cdr lst) r))))
	      (else
	       (assertion-violation 'filter-map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))
      (let loop ((xs (apply list-transpose* lst1 lst2)) (r '()))
	(cond ((null? xs) (reverse! r))
	      ((pair? xs) 
	       (cond ((apply proc (car xs)) =>
		      (lambda (x) (loop (cdr xs) (cons x r))))
		     (else (loop (cdr xs) r))))
	      (else
	       (assertion-violation 'map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))))


(define (fold-left proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis lst1) (knil seed))
	(if (null? lis) knil (loop (cdr lis) (proc knil (car lis)))))
      (let loop ((lis (apply list-transpose* lst1 lst2)) (knil seed))
	(if (null? lis)
	    knil 
	    (loop (cdr lis) (apply proc knil (car lis)))))))

;; tail recursive version
(define (fold-right proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis (reverse lst1))
		 (result seed))
	(if (null? lis)
	    result
	    (loop (cdr lis)
		  (proc (car lis) result))))
      (let loop ((lis (reverse! (apply list-transpose* lst1 lst2))) (knil seed))
	(if (null? lis)
	    knil
	    (loop (cdr lis)
		  (apply proc (append! (car lis) (list knil))))))))

;;(define (fold-right proc seed lst1 . lst2)
;;  (if (null? lst2)
;;      (let loop ((lis lst1))
;;	(if (null-list? lis)
;;	    seed 
;;	    (proc (car lis) (loop (cdr lis)))))
;;      (let loop ((lis (apply list-transpose* lst1 lst2)) (knil seed))
;;	(if (null-list? lis)
;;	    knil 
;;	    (apply proc (append! (car lis) (list (loop (cdr lis) knil))))))))

(define (remp pred lst)
  (let loop ((lst lst) (r '()))
    (cond ((null? lst) (reverse! r))
          ((pred (car lst)) (loop (cdr lst) r))
          (else (loop (cdr lst) (cons (car lst) r))))))

(define (remove obj lst)
  (let loop ((lst lst) (r '()))
    (cond ((null? lst) (reverse! r))
          ((equal? (car lst) obj) (loop (cdr lst) r))
          (else (loop (cdr lst) (cons (car lst) r))))))

(define (remv obj lst)
  (let loop ((lst lst) (r '()))
    (cond ((null? lst) (reverse! r))
          ((eqv? (car lst) obj) (loop (cdr lst) r))
          (else (loop (cdr lst) (cons (car lst) r))))))

(define (remq obj lst)
  (let loop ((lst lst) (r '()))
    (cond ((null? lst) (reverse! r))
          ((eq? (car lst) obj) (loop (cdr lst) r))
          (else (loop (cdr lst) (cons (car lst) r))))))

(define (memp proc lst)
  (cond ((null? lst) #f)
	((proc (car lst)) lst)
	(else (memp proc (cdr lst)))))

(define (assp proc lst)
  (cond ((null? lst) #f)
	((proc (caar lst)) (car lst))
	(else (assp proc (cdr lst)))))

;;;;
;; 4 Sorting
;; The algorithm is from SBCL
(define (list-sort (proc procedure?) lst)
  (define (merge-list! proc head lst1 lst2 tail)
    (let loop ()
      (cond ((proc (car lst2) (car lst1))
	     ;; we can't use macro so duplicate it!
	     (set-cdr! tail lst2)
	     (set! tail lst2)
	     (let ((rest (cdr lst2)))
	       (cond ((null? rest)
		      (set-cdr! lst2 lst1)
		      (cdr head))
		     (else
		      (set! lst2 rest)
		      (loop)))))
	    (else
	     (set-cdr! tail lst1)
	     (set! tail lst1)
	     (let ((rest (cdr lst1)))
	       (cond ((null? rest)
		      (set-cdr! lst1 lst2)
		      (cdr head))
		     (else
		      (set! lst1 rest)
		      (loop))))))))
  (define (fast-merge-list! proc try? head lst1 tail1 lst2 tail2 rest)
    (if try?
	(cond ((not (proc (car lst2) (car tail1)))
	       (set-cdr! tail1 lst2)
	       (values lst1 tail2 rest))
	      ((proc (car tail2) (car lst1))
	       (set-cdr! tail2 lst1)
	       (values lst2 tail1 rest))
	      (else 
	       (values (merge-list! proc head lst1 lst2 head)
		       (if (null? (cdr tail1))
			   tail1
			   tail2)
		       rest)))
	(values (merge-list! proc head lst1 lst2 head)
		(if (null? (cdr tail1))
		    tail1
		    tail2)
		rest)))
  (define (do-sort lst size head)
    (define (recur lst size)
      (cond ((= size 1)
	     (let ((h (list (car lst))))
	       (values h h (cdr lst))))
	    ((= size 2)
	     (let* ((a (car lst))
		    (ad (cadr lst))
		    (h (if (proc ad a)
			   (list ad a)
			   (list a ad))))
	       (values h (cdr h) (cddr lst))))
	    (else
	     (let ((half (div size 2)))
	       (receive (lst1 tail1 rest) (recur lst half)
		 (receive (lst2 tail2 rest) (recur rest (- size half))
		   (fast-merge-list! proc (>= size 8) head
				     lst1 tail1
				     lst2 tail2
				     rest)))))))
    (receive (lst tail size) (recur lst size)
      lst))
  (define (divide lst)
    (let loop ((acc 1) (lst lst))
      (cond ((null? (cdr lst)) (values acc '()))
            (else
	     (if (proc (car lst) (cadr lst))
		 (loop (+ acc 1) (cdr lst))
		 (values acc (cdr lst)))))))
  (if (null? lst)
      lst
      (receive (n lst2) (divide lst)
	(if (null? lst2)
	    lst
	    (let* ((head (cons '() '()))
		   (r (do-sort lst2 (length lst2) head)))
	      (merge-list! proc head (list-head lst n) r head))))))

(define (vector-sort proc vect :optional (start 0) (maybe-end #f))
  (define len (vector-length vect))
  (define end (or maybe-end len))
  ;; TODO should we expose this?
  (define (vector-copy! src src-from dst dst-from size)
    (if (<= dst-from src-from)
	(do ((i 0 (+ i 1)) (s src-from (+ s 1)) (d dst-from (+ d 1)))
	    ((= i size) dst)
	  (vector-set! dst d (vector-ref src s)))
	(do ((i 0 (+ i 1)) 
	     (s (+ src-from size) (- s 1)) 
	     (d (+ dst-from size) (- d 1)))
	    ((= i size) dst)
	  (vector-set! dst d (vector-ref src s)))))

  (when (or (negative? start) (negative? end))
    (assertion-violation 'vector-sort! "start and end must be positive" start
			 vect))
  (when (or (> start len) (> end len))
    (assertion-violation 'vector-sort! "out of range" 
			 (list (list start end) len)
			 vect))
  (when (> start end)
    (assertion-violation 'vector-sort! "start is greater than end" 
			 (list start end)
			 vect))
  
  (let* ((lst (vector->list vect start end))
	 (lst2 (list-sort proc lst)))
    (cond ((eq? lst lst2) vect)
	  ((= (- end start) len)
	   (list->vector lst2))
	  (else
	   (let ((v (make-vector len)))
	     (vector-copy! vect 0 v 0 start)
	     (do ((i start (+ i 1)) (l lst2 (cdr l)))
		 ((null? l))
	       (vector-set! v i (car l)))
	     (vector-copy! vect end v end (- len end)))))))

(define (vector-sort! proc vect :optional (start 0) (maybe-end #f))
  (define len (vector-length vect))
  (define end (or maybe-end len))

  (when (or (negative? start) (negative? end))
    (assertion-violation 'vector-sort! "start and end must be positive" start
			 vect))
  (when (or (> start len) (> end len))
    (assertion-violation 'vector-sort! "out of range" 
			 (list (list start end) len)
			 vect))
  (when (> start end)
    (assertion-violation 'vector-sort! "start is greater than end" 
			 (list start end)
			 vect))
  
  (let* ((n (- end start)) 
	 (work (make-vector (+ (div n 2) 1))))

    (define (simple-sort! first last)
      (let loop1 ((i first))
        (cond ((< i last)
               (let ((m (vector-ref vect i)) (k i))
                 (let loop2 ((j (+ i 1)))
                   (cond ((<= j last)
                          (if (proc (vector-ref vect j) m)
                              (begin
                                (set! m (vector-ref vect j))
                                (set! k j)))
                          (loop2 (+ j 1)))
                         (else
                          (vector-set! vect k (vector-ref vect i))
                          (vector-set! vect i m)
                          (loop1 (+ i 1))))))))))

    (define (sort! first last)
      (cond ((> (- last first) 10)
             (let ((middle (div (+ first last) 2)))
               (sort! first middle)
               (sort! (+ middle 1) last)
               (let loop ((i first) (p2size 0))
                 (cond ((> i middle)
                        (let loop ((p1 (+ middle 1)) (p2 0) (p3 first))
                          (cond ((and (<= p1 last) (< p2 p2size))
                                 (cond ((proc (vector-ref work p2) (vector-ref vect p1))
                                        (vector-set! vect p3 (vector-ref work p2))
                                        (loop p1 (+ p2 1) (+ p3 1)))
                                       (else
                                        (vector-set! vect p3 (vector-ref vect p1))
                                        (loop (+ p1 1) p2 (+ p3 1)))))
                                (else
                                 (let loop ((s2 p2)(d3 p3))
                                   (cond ((< s2 p2size)
                                          (vector-set! vect d3 (vector-ref work s2))
                                          (loop (+ s2 1) (+ d3 1)))))))))
                       (else
                        (vector-set! work p2size (vector-ref vect i))
                        (loop (+ i 1) (+ p2size 1)))))))
            (else
             (simple-sort! first last))))
    ;; the end is exclusive
    (sort! start (- end 1))))

;;;;
;; 8 I/O
;; 8.2.6 input port and output port
;; from Ypsilon
(define (call-with-port port proc)
  (receive args (proc port)
    (close-port port)
    (apply values args)))


;; 8.2.10 output port
(define (open-bytevector-output-port :optional (transcoder #f))
  (let* ((port (open-output-bytevector transcoder))
         (proc (lambda () (extract-output-bytevector port))))
    (values port proc)))

(define (open-string-output-port)
  (let* ((port (open-output-string))
         (proc (lambda () (extract-output-string port))))
    (values port proc)))

(define (call-with-bytevector-output-port proc . maybe-transcoder)
  (receive (port extractor) (apply open-bytevector-output-port maybe-transcoder)
    (proc port) (extractor)))

(define (call-with-string-output-port proc)
  (receive (port extractor) (open-string-output-port)
    (proc port) (extractor)))

;;;;;
;; 13 hashtable
;; 13.3 inspection
(define (hashtable-equivalence-function (ht hashtable?))
  (case (hashtable-type ht)
    ((eq)     eq?)
    ((eqv)    eqv?)
    ((equal)  equal?)
    ((string) string=?)
    ((general) (hashtable-compare ht))))

(define (hashtable-hash-function (ht hashtable?))
  (case (hashtable-type ht)
    ((eq)     #f)
    ((eqv)    #f)
    ((equal)  equal-hash)
    ((string) string-hash)
    ((general) (hashtable-hasher ht))))
)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
