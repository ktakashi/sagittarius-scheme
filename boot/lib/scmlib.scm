(define (hashtable-for-each proc ht)
  (for-each proc (hashtable-keys-list ht) (hashtable-values-list ht)))

(define (hashtable-map proc ht)
  (map proc (hashtable-keys-list ht) (hashtable-values-list ht)))

(define (hashtable->alist ht)
  (hashtable-map cons ht))

(define (unique-id-list? lst)
  (and (list? lst)
       (not (let loop ((lst lst))
              (and (pair? lst)
                   (or (not (variable? (car lst)))
                       (id-memq (car lst) (cdr lst))
                       (loop (cdr lst))))))))

#;(define (any pred ls)
  (if (pair? ls) (if (pred (car ls)) (car ls) (any pred (cdr ls))) #f))

(define (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;; er-macro-transformer
(define (er-macro-transformer f)
  (lambda (expr)
    (let ((dict (make-eq-hashtable))
	  (use-env (current-usage-env))
	  (mac-env (current-macro-env)))
      (define (rename s) (er-rename s mac-env dict))
      (define (compare a b)
	;;(identifier=? use-env a mac-env b))
	(or (and (identifier? a)
		 (identifier? b)
		 (free-identifier=? a b))
	    (identifier=? use-env a mac-env b)))
      (f expr rename compare))))

(define (safe-length lst)
  (let loop ((lst lst) (n 0))
    (if (pair? lst)
        (loop (cdr lst) (+ n 1))
        (or (and (null? lst) n) -1))))

;; print
(define (print . args)
  (for-each display args)
  (newline))

(define (fold proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis lst1) (knil seed))
	(if (null-list? lis)
	    knil
	    (loop (cdr lis) (proc (car lis) knil))))
      (let loop ((lis (apply list-transpose* lst1 lst2)) (knil seed))
	(if (null-list? lis)
	    knil 
	    (loop (cdr lis) (apply proc (append (car lis) (list knil))))))))

;; from Ypsilon
(define (wrong-type-argument-message expect got . nth)
  (if (null? nth)
      (format "expected ~a, but got ~a" expect got)
      (format "expected ~a, but got ~a, as argument ~a" expect got (car nth))))


(define (vector-map proc vec1 . vec2)
  (list->vector
   (apply map proc (vector->list vec1)
	  (map vector->list vec2))))

(define (vector-for-each proc vec1 . vec2)
  (apply for-each proc (vector->list vec1)
	 (map vector->list vec2)))

(define (string-for-each proc str1 . str2)
  (apply for-each proc (string->list str1)
	 (map string->list str2)))

;;;; record
;; NB: this functions is because of my lazyness.
;;     it's kinda hard to implement in C. so we just lookup this in C.
(define (record-printer inst . port)
  (let ((p (if (null? port)
               (current-output-port)
               (car port)))
        (rtd (tuple-ref inst 0)))
    (format p "#<record ~s ~a~a ~a>"
            (record-type-name rtd)
            (if (record-type-opaque? rtd) "opaque " "")
            (if (record-type-sealed? rtd) "sealed " "")
            (let ((len (tuple-size inst)))
              (let loop ((i 1)
                         (r '()))
                (if (= i len)
                    (reverse! r)
                    (loop (+ i 1) (cons (tuple-ref inst i) r))))))))


;; from Ypsilon
(define (make-nested-conser desc rtd argc)
  ((rcd-protocol desc)
   ((let loop ((desc desc))
      (cond ((rcd-parent desc)
             => (lambda (parent)
                  (lambda extra-field-values
                    (lambda protocol-args
                      (lambda this-field-values
                        (apply ((rcd-protocol parent)
                                (apply (loop parent)
                                       (append this-field-values extra-field-values)))
                               protocol-args))))))
            (else
             (lambda extra-field-values
               (lambda this-field-values
                 (let ((field-values (append this-field-values extra-field-values)))
                   (if (= (length field-values) argc)
                       (let ((tuple (make-tuple (+ (length field-values) 1) record-printer))
                             (all-valeus (append (list rtd) field-values)))
                         (tuple-list-set! tuple all-valeus)
                         tuple)
                       (assertion-violation "record constructor" "wrong number of arguments" field-values)))))))))))

(define (make-simple-conser desc rtd argc)
  ((rcd-protocol desc)
   (lambda field-values
     (if (= (length field-values) argc)
         (let ((tuple (make-tuple (+ (length field-values) 1) record-printer))
               (all-valeus (append (list rtd) field-values)))
           (tuple-list-set! tuple all-valeus)
           tuple)
         (assertion-violation "record constructor" "wrong number of arguments" field-values)))))

(define (default-protocol rtd)
  (let ((parent (record-type-parent rtd)))
    (if parent
        (let ((parent-field-count (rtd-total-field-count parent)))
          (lambda (p)
            (lambda field-values
              (receive (parent-field-values this-field-values) (split-at field-values parent-field-count)
                (apply (apply p parent-field-values) this-field-values)))))
        (lambda (p)
          (lambda field-values
            (apply p field-values))))))

;;;;
;; from Ypsilon

;; from srfi-1 start
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (assertion-violation 'null-list? "argument out of domain" l))))

(define (split-at x k)
  (or (integer? k)
      (assertion-violation 'split-at
                           (wrong-type-argument-message "integer" k 2)))
  (let recur ((lis x) (k k))
    (if (zero? k)
        (values '() lis)
        (receive (prefix suffix)
            (recur (cdr lis) (- k 1))
          (values (cons (car lis) prefix) suffix)))))

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

(define (find-tail pred list)
  (or (procedure? pred)
      (assertion-violation 'find-tail
			   (wrong-type-argument-message "procedure" pred 2)))
  (let lp ((list list))
    (and (not (null-list? list))
     (if (pred (car list)) list
         (lp (cdr list))))))

(define (assoc x lis . =)
  (or (list? lis)
      (assertion-violation 'assoc
			   (wrong-type-argument-message "list" lis 2)))
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

(define (reduce f ridentity lis)
  (or (procedure? f)
      (assertion-violation 'reduce (wrong-type-argument-message "procedure" = 1)))
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (lset-union = . lists)
  (or (procedure? =)
      (assertion-violation 'lset-union
			   (wrong-type-argument-message "procedure" = 1)))
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

(define (lset-intersection = lis1 . lists)
  (or (procedure? =)
      (assertion-violation 'lset-intersection
			   (wrong-type-argument-message "procedure" = 1)))
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((exists null-list? lists) '())      ; Short cut
      ((null? lists)          lis1)     ; Short cut
      (else (filter (lambda (x)
              (for-all (lambda (lis) (member x lis =)) lists))
            lis1)))))

(define (lset-difference = lis1 . lists)
  (or (procedure? =)
      (assertion-violation 'lset-difference
			   (wrong-type-argument-message "procedure" = 1)))
  (let ((lists (filter pair? lists)))   ; Throw out empty lists.
    (cond ((null? lists)     lis1)  ; Short cut
      ((memq lis1 lists) '())   ; Short cut
      (else (filter (lambda (x)
              (for-all (lambda (lis) (not (member x lis =)))
                 lists))
            lis1)))))

(define (take lis k)
  (or (integer? k)
      (assertion-violation 'take
			   (wrong-type-argument-message "integer" k 2)))
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
    (cons (car lis)
          (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (or (integer? k)
      (assertion-violation 'drop
			   (wrong-type-argument-message "integer" k 2)))
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))


(define list-head take)

;;;;
;; standard libraries

;; 1 Unicode
;; 1.1 characters
;; from Ypsilon
(define char-ci=? (lambda lst (apply char=? (map char-foldcase lst))))
(define char-ci<? (lambda lst (apply char<? (map char-foldcase lst))))
(define char-ci>? (lambda lst (apply char>? (map char-foldcase lst))))
(define char-ci<=? (lambda lst (apply char<=? (map char-foldcase lst))))
(define char-ci>=? (lambda lst (apply char>=? (map char-foldcase lst))))

;; 1.2 strings
;; from Ypsilon
(define string-ci=?
  (lambda strings
    (apply string=? (map string-foldcase strings))))

(define string-ci<?
  (lambda strings
    (apply string<? (map string-foldcase strings))))

(define string-ci>?
  (lambda strings
    (apply string>? (map string-foldcase strings))))

(define string-ci<=?
  (lambda strings
    (apply string<=? (map string-foldcase strings))))

(define string-ci>=?
  (lambda strings
    (apply string>=? (map string-foldcase strings))))

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
                              (format "expected endianness, but got ~r, as argument 3" endien)
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
                              (format "expected endianness, but got ~r, as argument 3" endien)
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
        (if (> val 0)
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
                                 (format "expected appropriate element size as argument 3, but got ~r" size)
                                 (list bv endien size))))))

(define (bytevector->sint-list bv endien size)
  (let loop ((i (- (bytevector-length bv) size)) (acc '()))
    (if (> i -1)
        (loop (- i size) (cons (bytevector-sint-ref bv i endien size) acc))
        (if (= i (- size))
            acc
            (assertion-violation 'bytevector->sint-list
                                 (format "expected appropriate element size as argument 3, but got ~r" size)
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
;; from Ypsilon
;; Using SRFI-1 implementation
;;(define find
;;  (lambda (pred lst)
;;    (cond ((null? lst) #f)
;;	  ((pair? lst)
;;	   (let loop ((head (car lst)) (rest (cdr lst)))
;;	     (cond ((pred head) head)
;;		   ((null? rest) #f)
;;		   ((pair? rest) (loop (car rest) (cdr rest)))
;;		   (else
;;		    (assertion-violation 'find (format "traversal reached to non-pair element ~s" rest) (list pred lst))))))
;;	  (else
;;	   (assertion-violation 'find (format "expected chain of pairs, but got ~s, as argument 2" lst) (list pred lst))))))

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
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (loop (cdr lst))))
          (else (loop (cdr lst))))))

(define (partition pred lst)
  (let loop ((lst lst) (acc1 '()) (acc2 '()))
    (cond ((null? lst) (values (reverse acc1) (reverse acc2)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
          (else (loop (cdr lst) acc1 (cons (car lst) acc2))))))

(define (map proc lst1 . lst2)
  (if (null? lst2)
      (let loop ((xs lst1) (r '()))
	(cond ((pair? xs) (loop (cdr xs) (cons (proc (car xs)) r)))
	      ((null? xs) (reverse! r))
	      (else
	       (assertion-violation 
		'map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))
      (let loop ((xs (apply list-transpose* lst1 lst2)) (r '()))
	(cond ((pair? xs) (loop (cdr xs) (cons (apply proc (car xs)) r)))
	      ((null? xs) (reverse! r))
	      (else
	       (assertion-violation 
		'map 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))))

(define (for-each proc lst1 . lst2)
  (if (null? lst2)
      (let loop ((xs lst1))
	(cond ((pair? xs) (proc (car xs)) (loop (cdr xs)))
	      ((null? xs) (undefined))
	      (else
	       (assertion-violation 
		'for-each 
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))
      (let loop ((xs (apply list-transpose* lst1 lst2)))
	(cond ((pair? xs) (apply proc (car xs)) (loop (cdr xs)))
	      ((null? xs) (undefined))
	      (else
	       (assertion-violation 
		'for-each
		(wrong-type-argument-message "proper list" lst1 2)
		(list proc lst1 lst2)))))))

(define (fold-left proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis lst1) (knil seed))
	(if (null-list? lis) knil (loop (cdr lis) (proc knil (car lis)))))
      (let loop ((lis (apply list-transpose* lst1 lst2)) (knil seed))
	(if (null-list? lis)
	    knil 
	    (loop (cdr lis) (apply proc knil (car lis)))))))

;; tail recursive version
(define (fold-right proc seed lst1 . lst2)
  (if (null? lst2)
      (let loop ((lis (reverse lst1))
		 (result seed))
	(if (null-list? lis)
	    result
	    (loop (cdr lis)
		  (proc (car lis) result))))
      (let loop ((lis (reverse! (apply list-transpose* lst1 lst2))) (knil seed))
	(if (null-list? lis)
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
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((pred (car lst))
           (loop (cdr lst)))
          (else
           (cons (car lst)
                 (loop (cdr lst)))))))

(define (remove obj lst)
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((equal? (car lst) obj)
           (loop (cdr lst)))
          (else
           (cons (car lst)
                 (loop (cdr lst)))))))

(define (remv obj lst)
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((eqv? (car lst) obj)
           (loop (cdr lst)))
          (else
           (cons (car lst)
                 (loop (cdr lst)))))))

(define (remq obj lst)
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((eq? (car lst) obj)
           (loop (cdr lst)))
          (else
           (cons (car lst)
                 (loop (cdr lst)))))))

(define (memp proc lst)
  (cond
   ((null? lst) #f)
   ((proc (car lst)) lst)
   (else
    (memp proc (cdr lst)))))

(define (assp proc lst)
  (cond
   ((null? lst) #f)
   ((proc (caar lst)) (car lst))
   (else
    (assp proc (cdr lst)))))

;;;;
;; 4 Sorting
;; The algorithm is from SBCL
(define (list-sort proc lst)
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
  (receive (n lst2) (divide lst)
    (if (null? lst2)
	lst
	(let* ((head (cons '() '()))
	       (r (do-sort lst2 (length lst2) head)))
	  (merge-list! proc head (list-head lst n) r head)))))
#|
;; from Ypsilon
(define (list-sort proc lst)

  (define (merge lst1 lst2)
    (cond
     ((null? lst1) lst2)
     ((null? lst2) lst1)
     (else
      (let ((a1 (car lst1)) (a2 (car lst2)))
	(if (proc a2 a1)
	    (cons a2 (merge lst1 (cdr lst2)))
	    (cons a1 (merge (cdr lst1) lst2)))))))

  (define (sort lst n)
    (cond ((= n 1)
           (list (car lst)))
          ((= n 2)
	   (let ((ad (cadr lst)) (a (car lst)))
	     (if (proc ad a)
		 (list ad a)
		 (list a ad))))
          (else
           (let ((n/2 (div n 2)))
             (merge (sort lst n/2)
                    (sort (list-tail lst n/2) (- n n/2)))))))

  (define (divide lst)
    (let loop ((acc 1) (lst lst))
      (cond ((null? (cdr lst)) (values acc '()))
            (else
	     (if (proc (car lst) (cadr lst))
		 (loop (+ acc 1) (cdr lst))
		 (values acc (cdr lst)))))))

  (cond ((null? lst) '())
        (else
         (let ((len (length lst)))
           (receive (n rest) (divide lst)
             (cond ((null? rest) lst)
                   (else
                    (merge (list-head lst n)
                           (sort rest (- len n))))))))))
|#

(define (vector-sort proc vect)
  (let ((lst (vector->list vect)))
    (let ((lst2 (list-sort proc lst)))
      (cond ((eq? lst lst2) vect)
            (else
             (list->vector lst2))))))

(define (vector-sort! proc vect)
  (let* ((n (vector-length vect)) (work (make-vector (+ (div n 2) 1))))

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

    (sort! 0 (- n 1))))

;;;;
;; 8 I/O
;; 8.2.6 input port and output port
;; from Ypsilon
(define (call-with-port port proc)
  (receive args (proc port)
    (close-port port)
    (apply values args)))


;; 8.2.10 output port
(define (open-bytevector-output-port . maybe-transcoder)
  (when (> (length maybe-transcoder) 1)
      (assertion-violation
       'open-bytevector-output-port
       (format 
        "wrong number of argument: expected between 0 and 1, but got ~a"
        (length maybe-transcoder))
       maybe-transcoder))
  (let ((transcoder (if (null? maybe-transcoder)
                        #f
                        (car maybe-transcoder))))
    (let* ((port (open-output-bytevector transcoder))
           (proc (lambda () (get-output-bytevector port))))
      (values port proc))))

(define (open-string-output-port)
  (let* ((port (open-output-string))
         (proc (lambda () (get-output-string port))))
    (values port proc)))

(define (call-with-bytevector-output-port proc . maybe-transcoder)
  (receive (port extractor) (apply open-bytevector-output-port maybe-transcoder)
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc port) (extractor))
      (lambda () (close-port port)))))

(define (call-with-string-output-port proc)
  (receive (port extractor) (open-string-output-port)
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc port) (extractor))
      (lambda () (close-port port)))))

;;;;;
;; 13 hashtable
;; 13.2 procedures
(define (hashtable-update! ht key proc default)
  (or (and (hashtable? ht)
	   (hashtable-mutable? ht))
      (assertion-violation
       'hashtable-update!
       (wrong-type-argument-message "mutable hashtable" ht 1)))
  (hashtable-set! ht key (proc (hashtable-ref ht key default))))

(define (hashtable-entries ht)
  (or (hashtable? ht)
      (assertion-violation 'hashtable-entries
			   (wrong-type-argument-message "hashtable" ht)))
  (values (hashtable-keys ht) (hashtable-values ht)))

;; 13.3 inspection
(define (hashtable-equivalence-function ht)
  (or (hashtable? ht)
      (assertion-violation 'hashtable-equivalence-function
			   (wrong-type-argument-message "hashtable" ht)))
  (case (hashtable-type ht)
    ((eq)     eq?)
    ((eqv)    eqv?)
    ((equal)  equal?)
    ((string) string=?)
    ((general) (hashtable-compare ht))))

(define (hashtable-hash-function ht)
  (or (hashtable? ht)
      (assertion-violation 'hashtable-hash-function
			   (wrong-type-argument-message "hashtable" ht)))
  (case (hashtable-type ht)
    ((eq)     #f)
    ((eqv)    #f)
    ((equal)  equal-hash)
    ((string) string-hash)
    ((general) (hashtable-hasher ht))))

;; parameter
;; From Ypsilon

(define (make-parameter init . maybe-filter)
  (let ((parameter (if (null? maybe-filter)
                       (parameter-proc-0 (gensym))
                       (parameter-proc-1 (gensym) (car maybe-filter)))))
    (begin (parameter init) parameter)))

(define (parameter-proc-0 key)
  (lambda value
    (if (null? value)
        (hashtable-ref  (current-dynamic-environment) key #f)
        (hashtable-set! (current-dynamic-environment) key (car value)))))

(define (parameter-proc-1 key proc)
  (lambda value
    (if (null? value)
        (hashtable-ref  (current-dynamic-environment) key #f)
        (hashtable-set! (current-dynamic-environment)
                        key (proc (car value))))))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
