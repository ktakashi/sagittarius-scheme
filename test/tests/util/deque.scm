(import (rnrs)
	(util deque)
	(srfi :64)
	(srfi :26)
	(srfi :1)
	(sagittarius control)
	(sagittarius threads))

(test-begin "Deque utilities")

;; Tests from Gauche

(define (deque-basic-test what maker)
  (define q (maker))

  (test-equal (format "~a deque?" what) #f (deque? (cons 'a 'b)))
  (test-equal (format "~a deque?" what) #f (deque? 3))
  (test-equal (format "~a deque?" what) #f (deque? '()))
  (test-equal (format "~a deque?" what) #t (deque? q))
  (test-equal (format "~a deque-push!" what) #t
	      (begin (deque-push! q 'a) (deque? q)))
  (test-equal (format "~a deque-push!" what) #t
	      (begin (deque-push! q 'b) (deque? q)))
  (test-equal (format "~a deque-push!" what) #t
	      (begin (deque-push! q 'c) (deque? q)))

  (test-equal (format "~a deque-front" what) 'a (deque-front q))
  (test-equal (format "~a deque-rear" what) 'c (deque-rear q))

  (test-equal (format "~a deque-push!" what) '(a f)
	      (begin
		(deque-push! q 'd 'e 'f)
		(list (deque-front q) (deque-rear q))))
  
  (test-equal (format "~a deque-shift!" what) 'a (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'b (deque-shift! q))
  (test-equal (format "~a deque-empty?" what) #f (deque-empty? q))
  (test-equal (format "~a deque-shift!" what) 'c (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'd (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'e (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'f (deque-shift! q))
  (test-equal (format "~a deque-empty?" what) #t (deque-empty? q))

  (test-error (format "~a deque-shift! (error)" what) condition?
	      (deque-shift! q))
  (test-equal (format "~a deque-shift! (fallback)" what) "empty!" 
	      (deque-shift! q "empty!"))
  (test-error (format "~a deque-front (error)" what) condition? (deque-front q))
  (test-equal (format "~a deque-front (fallback)" what) "foo" 
	      (deque-front q "foo"))
  (test-error (format "~a deque-rear (error)" what) condition? (deque-rear q))
  (test-equal (format "~a deque-rear (fallback)" what) "foo" 
	      (deque-rear q "foo"))

  (test-equal (format "~a deque-unshift!" what) '(c a)
	      (begin
		(deque-unshift! q 'a) (deque-unshift! q 'b) (deque-unshift! q 'c)
		(list (deque-front q) (deque-rear q))))
  (test-equal (format "~a deque-unshift!" what) '(f a)
	      (begin
		(deque-unshift! q 'd 'e 'f)
		(list (deque-front q) (deque-rear q))))
  (test-equal (format "~a deque-shift!" what) 'f (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'e (deque-shift! q))
  (test-equal (format "~a deque-empty?" what) #f (deque-empty? q))
  (test-equal (format "~a deque-shift!" what) 'd (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'c (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'b (deque-shift! q))
  (test-equal (format "~a deque-shift!" what) 'a (deque-shift! q))
  (test-equal (format "~a deque-empty?" what) #t (deque-empty? q))

  (test-equal (format "~a deque-shift-all!" what) '(a b c d e)
	      (begin (deque-push! q 'a 'b 'c 'd 'e) (deque-shift-all! q)))
  (test-equal (format "~a deque-shift-all!" what) '() (deque-shift-all! q))
  (test-equal (format "~a deque-shift-all!" what) #t  (deque-empty? q))

  (test-equal (format "~a find-in-deque" what) #f 
	      (find-in-deque (cut eq? <> 'a) q))
  (test-equal (format "~a find-in-deque" what) 'a 
	      (begin (deque-push! q 'a 'b 'c 'd 'e)
		     (find-in-deque (cut eq? <> 'a) q)))
  (test-equal (format "~a find-in-deque" what) 'c
	      (find-in-deque (cut eq? <> 'c) q))
  (test-equal (format "~a find-in-deque" what) 'e
	      (find-in-deque (cut eq? <> 'e) q))
  (test-equal (format "~a find-in-deque" what) '#f
	      (find-in-deque (cut eq? <> 'f) q))

  (test-equal (format "~a any-in-deque?" what) 'ok
         (any-in-deque (^x (and (eq? x 'c) 'ok)) q))
  (test-equal (format "~a any-in-deque?" what) #f
         (any-in-deque (^x (and (eq? x 'z) 'ok)) q))
  (test-equal (format "~a every-in-deque?" what) #t (every-in-deque symbol? q))
  (test-equal (format "~a every-in-deque?" what) #f 
	      (every-in-deque (cut eq? <> 'a) q))

  (test-equal (format "~a remove-from-deque!" what) #f
	      (remove-from-deque! (cut eq? <> 'f) q))
  (test-equal (format "~a remove-from-deque!" what) #t
	      (remove-from-deque! (cut eq? <> 'e) q))
  (test-equal (format "~a remove-from-deque!" what) #f
	      (remove-from-deque! (cut eq? <> 'e) q))
  (test-equal (format "~a remove-from-deque!" what) #t
	      (remove-from-deque! (cut eq? <> 'a) q))
  (test-equal (format "~a remove-from-deque!" what) #t
	      (remove-from-deque! (cut memq <> '(b c)) q))
  (test-equal (format "~a remove-from-deque!" what) #t
	      (remove-from-deque! (cut eq? <> 'd) q))
  (test-equal (format "~a remove-from-deque!" what) #t
	      (deque-empty? q))
  (test-equal (format "~a remove-from-deque!" what) #f
         (remove-from-deque! (cut eq? <> 'd) q))


  (let1 q (maker)
    (test-equal (format "~a deque-push-unique!" what) '("a")
		(begin (deque-push-unique! q equal? "a")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b")
		(begin (deque-push-unique! q equal? "b")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b")
		(begin (deque-push-unique! q equal? "a")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b" "c" "d")
		(begin (deque-push-unique! q equal? "a" "b" "c" "d")
		       (deque->list q)))
    (test-equal (format "~a deque-unshift-unique!" what) '("e" "a" "b" "c" "d")
		(begin (deque-unshift-unique! q equal? "d" "e")
		       (deque->list q)))
    (set! q (maker))
    (test-equal (format "~a deque-unshift-unique!" what) '("e" "d")
           (begin (deque-unshift-unique! q equal? "d" "e")
                  (deque->list q)))
    (test-equal (format "~a deque-unshift-unique!" what) '("c" "b" "a" "e" "d")
           (begin (deque-unshift-unique! q equal? "a" "b" "c" "d" "e")
                  (deque->list q)))
    )
  (let1 q (maker)
    (test-equal (format "~a deque-push-unique!" what) '("a")
		(begin (deque-push-unique! q equal? "a")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b")
		(begin (deque-push-unique! q equal? "b")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b")
		(begin (deque-push-unique! q equal? "a")
		       (deque->list q)))
    (test-equal (format "~a deque-push-unique!" what) '("a" "b" "c" "d")
		(begin (deque-push-unique! q equal? "a" "b" "c" "d")
		       (deque->list q)))
    (test-equal (format "~a deque-unshift-unique!" what) '("e" "a" "b" "c" "d")
		(begin (deque-unshift-unique! q equal? "d" "e")
		       (deque->list q)))
    (test-equal (format "~a deque-pop!" what) "d" (deque-pop! q))
    (test-equal (format "~a deque-pop-all!" what) '("c" "b" "a" "e")
		(deque-pop-all! q))
    )
  )

(deque-basic-test "simple deque" make-deque)
(deque-basic-test "mtdeque"      make-mtdeque)

(let1 q (make-mtdeque :max-length 3)
  (test-equal "mtdeque room" 3 (mtdeque-room q))

  (test-equal "mtdeque maxlen" 'c
         (begin (deque-push! q 'a)
                (deque-push! q 'b)
                (deque-push! q 'c)
                (deque-rear q)))
  (test-error "mtdeque maxlen (deque-push! overflow)" condition?
	      (deque-push! q 'd))
  (test-equal "mtdeque maxlen (deque-push! unchanged after overflow)" '(a b c)
	      (deque->list q))
  (test-equal "mtdeque room" 0 (mtdeque-room q))
  (test-error "mtdeque maxlen (deque-push! multiarg overflow)" condition?
	      (begin (deque-shift! q)
		     (deque-push! q 'd 'e 'f)))
  (test-equal "mtdeque maxlen (deque-push! atomicity)" '(b c)
	      (deque->list q))
  (test-equal "mtdeque room" 1 (mtdeque-room q))
  
  (test-error "mtdeque maxlen (deque-unshift! overflow)" condition?
         (begin (deque-unshift! q 'a)
                (deque-unshift! q 'z)))
  (test-equal "mtdeque maxlen (deque-unshift! postcheck)" '(a b c)
         (deque->list q))
  (test-error "mtdeque maxlen (deque-unshift! multiarg overflow)" condition?
         (begin (deque-shift! q)
                (deque-unshift! q 'd 'e 'f)))
  (test-equal "mtdeque maxlen (deque-unshift! atomicity)" '(b c)
         (deque->list q))
  )

(let1 q (make-mtdeque :max-length 3)
  (test-equal "mtdeque room" 3 (mtdeque-room q))

  (test-equal "mtdeque maxlen (with deque-push-unique!)" 'c
	      (begin (deque-push-unique! q eq? 'a)
		     (deque-push-unique! q eq? 'b)
		     (deque-push-unique! q eq? 'c)
		     (deque-rear q)))
  (test-error "mtdeque maxlen (deque-push-unique! overflow)" condition?
         (deque-push-unique! q eq? 'd))
  (test-equal "mtdeque maxlen (deque-push-unique! unchanged after overflow)"
	      '(a b c) (deque->list q))
  (test-equal "mtdeque room" 0 (mtdeque-room q))
  (test-error "mtdeque maxlen (deque-push-unique! multiarg overflow)" condition?
	      (begin (deque-shift! q)
		     (deque-push-unique! q eq? 'c 'd 'e)))
  (test-equal "mtdeque maxlen (deque-push-unique! atomicity)" '(b c)
	      (deque->list q))
  (test-equal "mtdeque room" 1 (mtdeque-room q))
  (test-equal "mtdeque maxlen (deque-push-unique! duplicate multiarg)" '(b c d)
	      (begin (deque-push-unique! q eq? 'c 'd 'b 'd)
		     (deque->list q)))

  (test-error "mtdeque maxlen (deque-unshift-unique! overflow)" condition?
	      (begin (deque-shift! q)
		     (deque-unshift-unique! q eq? 'b)
		     (deque-unshift-unique! q eq? 'z)))
  (test-equal "mtdeque maxlen (deque-unshift-unique! postcheck)" '(b c d)
	      (deque->list q))
  (test-error "mtdeque maxlen (deque-unshift-unique! multiarg overflow)" condition?
	      (begin (deque-shift! q)
		     (deque-unshift-unique! q eq? 'c 'b 'a)))
  (test-equal "mtdeque maxlen (deque-unshift-unique! atomicity)" '(c d)
	      (deque->list q))
  (test-equal "mtdeque maxlen (deque-unshift-unique! duplicate multiarg)" '(b c d)
	      (begin (deque-shift! q)
		     (deque-unshift-unique! q eq? 'c 'b 'c 'd)
		     (deque->list q)))
  )

(test-equal "mtdeque room" +inf.0 (mtdeque-room (make-mtdeque)))


(define (test-producer-consumer name qs ndata nthreads)
  (define qr (make-mtdeque))
  (define data (iota ndata))
  (define (producer)
    (dolist [n data] (deque-push/wait! qs n))
    (dotimes [k nthreads] (deque-push/wait! qs #f)))
  (define (consumer)
    (let loop ([x (deque-shift/wait! qs)])
      (when x
        (deque-push! qr x)
        (sys-nanosleep #e1e7)
        (loop (deque-shift/wait! qs)))))

  (test-assert (format "synchronized deque ~a" name)
	       (lset= eqv? data
		      (let* ([cs (map (^_ (thread-start! 
					   (make-thread consumer)))
				      (iota nthreads))]
			     [p1 (make-thread producer)])
			(sys-nanosleep #e1e8)
			(thread-start! p1)
			(for-each thread-join! cs)
			(thread-join! p1)
			(deque->list qr)))))

(test-producer-consumer "(unbound deque length)"
                        (make-mtdeque)
                        100 3)

(test-producer-consumer "(bound deque length)"
                        (make-mtdeque :max-length 5)
                        100 3)

(test-producer-consumer "(zero-length deque)"
                        (make-mtdeque :max-length 0)
                        100 3)

(test-equal "deque-shift/wait! timeout" "timed out!"
	    (deque-shift/wait! (make-mtdeque) 0.01 "timed out!"))

(test-equal "deque-push/wait! timeout" "timed out!"
	    (let1 q (make-mtdeque :max-length 1)
	      (deque-push! q 'a)
	      (deque-push/wait! q 'b 0.01 "timed out!")))
(test-equal "deque-unshift/wait! timeout" "timed out!"
	    (let1 q (make-mtdeque :max-length 1)
	      (deque-push! q 'a)
	      (deque-unshift/wait! q 'b 0.01 "timed out!")))

(test-equal "zero-length-deque handshaking" '(5 4 3 2 1 0)
	    (let ([r '()]
		  [q0 (make-mtdeque :max-length 0)]
		  [q1 (make-mtdeque :max-length 0)])
	      (let1 t (thread-start!
		       (make-thread (^[]
				      (push! r 0)
				      (deque-shift/wait! q0)
				      (deque-push/wait! q1 'b)
				      (push! r 2)
				      (deque-shift/wait! q0)
				      (deque-push/wait! q1 'd)
				      (push! r 4)
				      (deque-shift/wait! q0))))
		(deque-push/wait! q0 'a)
		(push! r 1)
		(deque-shift/wait! q1)
		(deque-push/wait! q0 'c)
		(push! r 3)
		(deque-shift/wait! q1)
		(deque-push/wait! q0 'e)
		(push! r 5))
	      r))

(test-equal "zero-length-deque multiple reader" '(a a b b)
	    (let ([r0 #f] [r1 #f]
		  [q  (make-mtdeque :max-length 0)]
		  [qq (make-mtdeque :max-length 0)])
	      (let ([t0 (thread-start!
			 (make-thread (^[] (deque-push/wait! qq #t)
					(set! r0 (deque-shift/wait! q))
					(deque-push/wait! qq 'b))))]
		    [t1 (thread-start!
			 (make-thread (^[] (deque-push/wait! qq #t)
					(set! r1 (deque-shift/wait! q))
					(deque-push/wait! qq 'b))))])
		(deque-shift/wait! qq)
		(deque-shift/wait! qq)
		(deque-push/wait! q 'a)
		(deque-push/wait! q 'a)
		(let1 r (list (deque-shift/wait! qq) (deque-shift/wait! qq))
		  (cons* r0 r1 r)))))


(test-end)