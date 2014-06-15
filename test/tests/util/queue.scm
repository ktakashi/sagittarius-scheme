(import (rnrs)
	(util queue)
	(srfi :64)
	(srfi :26)
	(srfi :1)
	(sagittarius control)
	(sagittarius threads))

(test-begin "Queue utilities")

;; Tests from Gauche

(define (queue-basic-test what maker)
  (define q (maker))

  (test-equal (format "~a queue?" what) #f (queue? (cons 'a 'b)))
  (test-equal (format "~a queue?" what) #f (queue? 3))
  (test-equal (format "~a queue?" what) #f (queue? '()))
  (test-equal (format "~a queue?" what) #t (queue? q))
  (test-equal (format "~a enqueue!" what) #t (begin (enqueue! q 'a) (queue? q)))
  (test-equal (format "~a enqueue!" what) #t (begin (enqueue! q 'b) (queue? q)))
  (test-equal (format "~a enqueue!" what) #t (begin (enqueue! q 'c) (queue? q)))

  (test-equal (format "~a queue-front" what) 'a (queue-front q))
  (test-equal (format "~a queue-rear" what) 'c (queue-rear q))

  (test-equal (format "~a enqueue!" what) '(a f)
         (begin
           (enqueue! q 'd 'e 'f)
           (list (queue-front q) (queue-rear q))))

  (test-equal (format "~a dequeue!" what) 'a (dequeue! q))
  (test-equal (format "~a dequeue!" what) 'b (dequeue! q))
  (test-equal (format "~a queue-empty?" what) #f (queue-empty? q))
  (test-equal (format "~a dequeue!" what) 'c (dequeue! q))
  (test-equal (format "~a dequeue!" what) 'd (dequeue! q))
  (test-equal (format "~a dequeue!" what) 'e (dequeue! q))
  (test-equal (format "~a dequeue!" what) 'f (dequeue! q))
  (test-equal (format "~a queue-empty?" what) #t (queue-empty? q))

  (test-error (format "~a dequeue! (error)" what) condition? (dequeue! q))
  (test-equal (format "~a dequeue! (fallback)" what) "empty!" 
	      (dequeue! q "empty!"))
  (test-error (format "~a queue-front (error)" what) condition? (queue-front q))
  (test-equal (format "~a queue-front (fallback)" what) "foo" 
	      (queue-front q "foo"))
  (test-error (format "~a queue-rear (error)" what) condition? (queue-rear q))
  (test-equal (format "~a queue-rear (fallback)" what) "foo" 
	      (queue-rear q "foo"))

  (test-equal (format "~a queue-push!" what) '(c a)
	      (begin
		(queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
		(list (queue-front q) (queue-rear q))))
  (test-equal (format "~a queue-push!" what) '(f a)
	      (begin
		(queue-push! q 'd 'e 'f)
		(list (queue-front q) (queue-rear q))))
  (test-equal (format "~a queue-pop!" what) 'f (queue-pop! q))
  (test-equal (format "~a queue-pop!" what) 'e (queue-pop! q))
  (test-equal (format "~a queue-empty?" what) #f (queue-empty? q))
  (test-equal (format "~a queue-pop!" what) 'd (queue-pop! q))
  (test-equal (format "~a queue-pop!" what) 'c (queue-pop! q))
  (test-equal (format "~a queue-pop!" what) 'b (queue-pop! q))
  (test-equal (format "~a queue-pop!" what) 'a (queue-pop! q))
  (test-equal (format "~a queue-empty?" what) #t (queue-empty? q))

  (test-equal (format "~a dequeue-all!" what) '(a b c d e)
	      (begin (enqueue! q 'a 'b 'c 'd 'e) (dequeue-all! q)))
  (test-equal (format "~a dequeue-all!" what) '() (dequeue-all! q))
  (test-equal (format "~a dequeue-all!" what) #t  (queue-empty? q))

  (test-equal (format "~a find-in-queue" what) #f 
	      (find-in-queue (cut eq? <> 'a) q))
  (test-equal (format "~a find-in-queue" what) 'a 
	      (begin (enqueue! q 'a 'b 'c 'd 'e)
		     (find-in-queue (cut eq? <> 'a) q)))
  (test-equal (format "~a find-in-queue" what) 'c
	      (find-in-queue (cut eq? <> 'c) q))
  (test-equal (format "~a find-in-queue" what) 'e
	      (find-in-queue (cut eq? <> 'e) q))
  (test-equal (format "~a find-in-queue" what) '#f
	      (find-in-queue (cut eq? <> 'f) q))

  (test-equal (format "~a any-in-queue?" what) 'ok
         (any-in-queue (^x (and (eq? x 'c) 'ok)) q))
  (test-equal (format "~a any-in-queue?" what) #f
         (any-in-queue (^x (and (eq? x 'z) 'ok)) q))
  (test-equal (format "~a every-in-queue?" what) #t (every-in-queue symbol? q))
  (test-equal (format "~a every-in-queue?" what) #f 
	      (every-in-queue (cut eq? <> 'a) q))

  (test-equal (format "~a remove-from-queue!" what) #f
	      (remove-from-queue! (cut eq? <> 'f) q))
  (test-equal (format "~a remove-from-queue!" what) #t
	      (remove-from-queue! (cut eq? <> 'e) q))
  (test-equal (format "~a remove-from-queue!" what) #f
	      (remove-from-queue! (cut eq? <> 'e) q))
  (test-equal (format "~a remove-from-queue!" what) #t
	      (remove-from-queue! (cut eq? <> 'a) q))
  (test-equal (format "~a remove-from-queue!" what) #t
	      (remove-from-queue! (cut memq <> '(b c)) q))
  (test-equal (format "~a remove-from-queue!" what) #t
	      (remove-from-queue! (cut eq? <> 'd) q))
  (test-equal (format "~a remove-from-queue!" what) #t
	      (queue-empty? q))
  (test-equal (format "~a remove-from-queue!" what) #f
         (remove-from-queue! (cut eq? <> 'd) q))


  (let1 q (maker)
    (test-equal (format "~a enqueue-unique!" what) '("a")
		(begin (enqueue-unique! q equal? "a")
		       (queue->list q)))
    (test-equal (format "~a enqueue-unique!" what) '("a" "b")
		(begin (enqueue-unique! q equal? "b")
		       (queue->list q)))
    (test-equal (format "~a enqueue-unique!" what) '("a" "b")
		(begin (enqueue-unique! q equal? "a")
		       (queue->list q)))
    (test-equal (format "~a enqueue-unique!" what) '("a" "b" "c" "d")
		(begin (enqueue-unique! q equal? "a" "b" "c" "d")
		       (queue->list q)))
    (test-equal (format "~a queue-push-unique!" what) '("e" "a" "b" "c" "d")
		(begin (queue-push-unique! q equal? "d" "e")
		       (queue->list q)))
    (set! q (make-queue))
    (test-equal (format "~a queue-push-unique!" what) '("e" "d")
           (begin (queue-push-unique! q equal? "d" "e")
                  (queue->list q)))
    (test-equal (format "~a queue-push-unique!" what) '("c" "b" "a" "e" "d")
           (begin (queue-push-unique! q equal? "a" "b" "c" "d" "e")
                  (queue->list q)))
    )
  )

(queue-basic-test "simple queue" make-queue)
(queue-basic-test "mtqueue"      make-mtqueue)

(let1 q (make-mtqueue :max-length 3)
  (test-equal "mtqueue room" 3 (mtqueue-room q))

  (test-equal "mtqueue maxlen" 'c
         (begin (enqueue! q 'a)
                (enqueue! q 'b)
                (enqueue! q 'c)
                (queue-rear q)))
  (test-error "mtqueue maxlen (enqueue! overflow)" condition?
	      (enqueue! q 'd))
  (test-equal "mtqueue maxlen (enqueue! unchanged after overflow)" '(a b c)
	      (queue->list q))
  (test-equal "mtqueue room" 0 (mtqueue-room q))
  (test-error "mtqueue maxlen (enqueue! multiarg overflow)" condition?
	      (begin (dequeue! q)
		     (enqueue! q 'd 'e 'f)))
  (test-equal "mtqueue maxlen (enqueue! atomicity)" '(b c)
	      (queue->list q))
  (test-equal "mtqueue room" 1 (mtqueue-room q))
  
  (test-error "mtqueue maxlen (queue-push! overflow)" condition?
         (begin (queue-push! q 'a)
                (queue-push! q 'z)))
  (test-equal "mtqueue maxlen (queue-push! postcheck)" '(a b c)
         (queue->list q))
  (test-error "mtqueue maxlen (queue-push! multiarg overflow)" condition?
         (begin (dequeue! q)
                (queue-push! q 'd 'e 'f)))
  (test-equal "mtqueue maxlen (queue-push! atomicity)" '(b c)
         (queue->list q))
  )

(let1 q (make-mtqueue :max-length 3)
  (test-equal "mtqueue room" 3 (mtqueue-room q))

  (test-equal "mtqueue maxlen (with enqueue-unique!)" 'c
	      (begin (enqueue-unique! q eq? 'a)
		     (enqueue-unique! q eq? 'b)
		     (enqueue-unique! q eq? 'c)
		     (queue-rear q)))
  (test-error "mtqueue maxlen (enqueue-unique! overflow)" condition?
         (enqueue-unique! q eq? 'd))
  (test-equal "mtqueue maxlen (enqueue-unique! unchanged after overflow)"
	      '(a b c) (queue->list q))
  (test-equal "mtqueue room" 0 (mtqueue-room q))
  (test-error "mtqueue maxlen (enqueue-unique! multiarg overflow)" condition?
	      (begin (dequeue! q)
		     (enqueue-unique! q eq? 'c 'd 'e)))
  (test-equal "mtqueue maxlen (enqueue-unique! atomicity)" '(b c)
	      (queue->list q))
  (test-equal "mtqueue room" 1 (mtqueue-room q))
  (test-equal "mtqueue maxlen (enqueue-unique! duplicate multiarg)" '(b c d)
	      (begin (enqueue-unique! q eq? 'c 'd 'b 'd)
		     (queue->list q)))

  (test-error "mtqueue maxlen (queue-push-unique! overflow)" condition?
	      (begin (dequeue! q)
		     (queue-push-unique! q eq? 'b)
		     (queue-push-unique! q eq? 'z)))
  (test-equal "mtqueue maxlen (queue-push-unique! postcheck)" '(b c d)
	      (queue->list q))
  (test-error "mtqueue maxlen (queue-push-unique! multiarg overflow)" condition?
	      (begin (dequeue! q)
		     (queue-push-unique! q eq? 'c 'b 'a)))
  (test-equal "mtqueue maxlen (queue-push-unique! atomicity)" '(c d)
	      (queue->list q))
  (test-equal "mtqueue maxlen (queue-push-unique! duplicate multiarg)" '(b c d)
	      (begin (dequeue! q)
		     (queue-push-unique! q eq? 'c 'b 'c 'd)
		     (queue->list q)))
  )

(test-equal "mtqueue room" +inf.0 (mtqueue-room (make-mtqueue)))

(define (test-producer-consumer name qs ndata nthreads)
  (define qr (make-mtqueue))
  (define data (iota ndata))
  (define (producer)
    (dolist [n data] (enqueue/wait! qs n))
    (dotimes [k nthreads] (enqueue/wait! qs #f)))
  (define (consumer)
    (let loop ([x (dequeue/wait! qs)])
      (when x
        (enqueue! qr x)
        (sys-nanosleep #e1e7)
        (loop (dequeue/wait! qs)))))
  
  (test-assert (format "synchronized queue ~a" name)
	       (lset= eqv? data
		      (let* ([cs (map (^_ (thread-start! 
					   (make-thread consumer)))
				      (iota nthreads))]
			     [p1 (make-thread producer)])
			(sys-nanosleep #e1e8)
			(thread-start! p1)
			(for-each thread-join! cs)
			(thread-join! p1)
			(queue->list qr)))))

(test-producer-consumer "(unbound queue length)"
                        (make-mtqueue)
                        100 3)

(test-producer-consumer "(bound queue length)"
                        (make-mtqueue :max-length 5)
                        100 3)

(test-producer-consumer "(zero-length queue)"
                        (make-mtqueue :max-length 0)
                        100 3)

(test-equal "dequeue/wait! timeout" "timed out!"
	    (dequeue/wait! (make-mtqueue) 0.01 "timed out!"))
(test-equal "enqueue/wait! timeout" "timed out!"
	    (let1 q (make-mtqueue :max-length 1)
	      (enqueue! q 'a)
	      (enqueue/wait! q 'b 0.01 "timed out!")))
(test-equal "queue-push/wait! timeout" "timed out!"
	    (let1 q (make-mtqueue :max-length 1)
	      (enqueue! q 'a)
	      (queue-push/wait! q 'b 0.01 "timed out!")))

(test-equal "zero-length-queue handshaking" '(5 4 3 2 1 0)
	    (let ([r '()]
		  [q0 (make-mtqueue :max-length 0)]
		  [q1 (make-mtqueue :max-length 0)])
	      (let1 t (thread-start!
		       (make-thread (^[]
				      (push! r 0)
				      (dequeue/wait! q0)
				      (enqueue/wait! q1 'b)
				      (push! r 2)
				      (dequeue/wait! q0)
				      (enqueue/wait! q1 'd)
				      (push! r 4)
				      (dequeue/wait! q0))))
		(enqueue/wait! q0 'a)
		(push! r 1)
		(dequeue/wait! q1)
		(enqueue/wait! q0 'c)
		(push! r 3)
		(dequeue/wait! q1)
		(enqueue/wait! q0 'e)
		(push! r 5))
	      r))

(test-equal "zero-length-queue multiple reader" '(a a b b)
	    (let ([r0 #f] [r1 #f]
		  [q  (make-mtqueue :max-length 0)]
		  [qq (make-mtqueue :max-length 0)])
	      (let ([t0 (thread-start!
			 (make-thread (^[] (enqueue/wait! qq #t)
					(set! r0 (dequeue/wait! q))
					(enqueue/wait! qq 'b))))]
		    [t1 (thread-start!
			 (make-thread (^[] (enqueue/wait! qq #t)
					(set! r1 (dequeue/wait! q))
					(enqueue/wait! qq 'b))))])
		(dequeue/wait! qq)
		(dequeue/wait! qq)
		(enqueue/wait! q 'a)
		(enqueue/wait! q 'a)
		(let1 r (list (dequeue/wait! qq) (dequeue/wait! qq))
		  (cons* r0 r1 r)))))

(test-end)