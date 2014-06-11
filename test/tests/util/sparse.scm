(import (rnrs)
	(util sparse)
	(util hashtables)
	(sagittarius control)
	(srfi :27)
	(srfi :64))

(test-begin "Sparse collection")

(test-assert "predict" (sparse-vector? (make-sparse-vector)))

(let ((spvec (make-sparse-vector)))
  (sparse-vector-set! spvec 0 'a)
  (test-equal "ref" 'a (sparse-vector-ref spvec 0))
  (test-equal "fallback" #f (sparse-vector-ref spvec 1 #f))
  (test-equal "size" 1 (sparse-vector-size spvec)))

;; from Gauche
(define (simple-test name obj %ref %set! %exists? key1 key2
                     :optional (val1 'ok) (val2 'okok) (val3 'okokok))
  (test-equal (format "~a basic set!/ref" name) val1
	      (begin (%set! obj (key1) val1)
		     (%ref obj (key1))))
  #;
  (test-error (format "~a referencing nokey" name) condition?
	      (%ref obj (key2)))
  (test-equal (format "~a referencing nokey fallback" name) 'huh?
	      (%ref obj (key2) 'huh?))
  (test-equal (format "~a exists?" name) #t (%exists? obj (key1)))
  (test-equal (format "~a exists?" name) #f (%exists? obj (key2)))
  (test-equal (format "~a replace" name) val2
	      (begin (%set! obj (key1) val2)
		     (%ref obj (key1))))
  (test-equal (format "~a add" name) val3
	      (begin (%set! obj (key2) val3)
		     (%ref obj (key2))))
  (test-equal (format "~a ref" name) val2 (%ref obj (key1)))
  )

(define (const x) (^[] x))

;; To test sparse table seriously, set *data-set-size* larger number
;; like 20000.  The default size is chosen not to take too long for the tests.
(define *data-set-size* 5000)

(define *data-set*
  (rlet1 ht (make-eqv-hashtable)
    (let loop ([i 0])
      (unless (= i *data-set-size*)
        (let1 k (random-integer (expt 2 32))
          (cond [(hashtable-contains? ht k) (loop i)]
                [else (hashtable-set! ht k (* k k)) (loop (+ i 1))]))))))


(define-syntax let/cc
  (syntax-rules ()
    ((_ c body ...)
     (call/cc (lambda (c) body ...)))))

(define (heavy-test name obj %ref %set! %cnt %clr %del %copy
                    %check keygen valgen)
  (test-equal (format "~a many set!" name) *data-set-size*
	      (let/cc return
		(hashtable-fold 
		 (^[k v cnt]
		   (let ([kk (keygen k)]
			 [vv (valgen v)])
		     (if (and (not (%ref obj kk #f))
			      (begin (%set! obj kk vv)
				     (equal? vv (%ref obj kk #f))))
			 (+ cnt 1)
			 (return
			  `(error ,cnt ,kk ,vv ,(%ref obj kk #f))))))
		 *data-set*
		 0)))

  (test-equal (format "~a many numelements" name) *data-set-size* (%cnt obj))

  (when %check 
    (test-equal (format "~a many check" name) #t (begin (%check obj) #t)))

  (test-equal (format "~a many ref" name) *data-set-size*
	      (let/cc return
		(hashtable-fold 
		 (^[k v cnt]
		   (let ([kk (keygen k)]
			 [vv (valgen v)])
		     (if (equal? (%ref obj kk) vv)
			 (+ cnt 1)
			 (return `(error ,cnt ,kk ,vv ,(%ref obj kk))))))
		 *data-set*
		 0)))
#|
  (test-equal (format "~a keys" name) *data-set-size*
	      (let/cc return
		(let1 tt (make-sparse-table 'equal?)
		  (hashtable-for-each (^[k v]
					(sparse-table-set! tt (keygen k) #t))
				      *data-set*)
		  (fold (^[k cnt] (if (sparse-table-ref tt k #f)
				      (+ cnt 1)
				      (return `(error ,cnt ,k))))
			0 (%keys obj)))))
  (test-equal (format "~a values" name) *data-set-size*
	      (let/cc return
		(let1 tt (make-sparse-table 'equal?)
		  (hashtable-for-each *data-set*
				       (^[k v]
					 (sparse-table-set! tt (valgen v) #t)))
		  (fold (^[v cnt] (if (sparse-table-ref tt v #f)
				      (+ cnt 1)
				      (return `(error ,cnt ,v))))
			0 (%vals obj)))))
  (test-equal (format "~a many copy" name) (list *data-set-size* #t #t)
	      (let* ([new (%copy obj)]
		     [keys (%keys new)])
		(list (length keys)
		      (if %check (begin (%check new) #t) #t)
		      (every (^k (equal? (%ref new k) (%ref obj k))) keys))))
|#
  (test-equal (format "~a many clear!" name) 0 (begin (%clr obj) (%cnt obj)))
  (test-equal (format "~a many ref2" name) *data-set-size*
	      (let/cc return
		(hashtable-fold 
		 (^[k v cnt]
		   (let ([kk (keygen k)]
			 [vv (valgen v)])
		     (if (%ref obj kk #f)
			 (return `(error ,cnt ,kk ,vv ,(%ref obj kk)))
			 (+ cnt 1))))
		 *data-set*
		 0)))
  (test-equal (format "~a many delete!" name) '(#t 0)
	      (begin
		(hashtable-for-each (^[k v] (%set! obj (keygen k) (valgen v)))
				    *data-set*)
		(when %check (%check obj))
		(let1 r
		    (hashtable-fold (^[k v s]
				      (when %check (%check obj))
				      (and s (%del obj (keygen k)) #t))
				    *data-set*
				    #t)
		  (list r (%cnt obj)))))
  )

(define (spvec-simple)
  (apply simple-test "sparse-vector" (make-sparse-vector)
         sparse-vector-ref sparse-vector-set! sparse-vector-exists?
         (const 0) (const 1)
         '(3 6 9)))

(spvec-simple)

(define (spvec-heavy valgen)
  (heavy-test "sparse-vector"
              (make-sparse-vector)
              sparse-vector-ref sparse-vector-set!
              sparse-vector-size sparse-vector-clear!
	      sparse-vector-delete!
              sparse-vector-copy #f
              values valgen))
(spvec-heavy values)

(test-end)