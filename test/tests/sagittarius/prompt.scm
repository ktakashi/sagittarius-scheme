(import (rnrs)
	(sagittarius)
	(sagittarius vm))

(define-syntax test
  (syntax-rules (quote)
    ((_ expect (quote name) expr)
     (call/prompt
      (lambda ()
	(let ((r expr))
	  (unless (equal? expect r)
	    (format #t "~a - expected: ~s, result: ~s~%" 'name expect r))
	  r))))
    ((_ expect expr args ...)
     (test expect 'expr (expr args ...)))))
(define-syntax test-values
  (syntax-rules ()
    ((_ expect expr args ...)
     (let-values ((r (expr args ...)))
       (unless (equal? expect r)
	 (format #t "expected: ~s, result: ~s~%" expect r))
       (apply values r)))))

(define-syntax let/cc
  (syntax-rules ()
    ((_ k expr ...)
     (call/cc (lambda (k) expr ...)))))

(define-syntax err/rt-test
  (syntax-rules ()
    ((_ expr ignore)
     (guard (e (else (print 'expr 'ok)))
       expr
       (print 'expr 'ng)))))

(define call/cc-via-composable 
  (case-lambda 
   [(f) (call/cc-via-composable f (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-composable-continuation
     (lambda (k)
       (f (lambda vs
            (abort-current-continuation 
             tag 
             (lambda () 
               (call-with-continuation-prompt
                (lambda ()
                  (apply k vs))
                tag
                (lambda (thunk) (thunk)))))))))]))

(define call/cc-via-aborted-and-restored-composable 
  (case-lambda 
   [(f) (call/cc-via-composable f (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-composable-continuation
     (lambda (k)
       (abort-current-continuation
        tag
        (lambda ()
          (k (f (lambda vs
                  (abort-current-continuation 
                   tag 
                   (lambda () 
                     (call-with-continuation-prompt
                      (lambda ()
                        (apply k vs))
                      tag
                      (lambda (thunk) (thunk))))))))))))]))

(define call-with-continuation-prompt-for-composable
  (case-lambda
   [(f) (call-with-continuation-prompt-for-composable
         f
         (default-continuation-prompt-tag))]
   [(f tag)
    (call-with-continuation-prompt f
                                   tag
                                   (lambda (thunk) (thunk)))]))

(define (call/cc proc :optional (tag (default-continuation-prompt-tag)))
  (call-with-composable-continuation
   (lambda (ck)
     (define (k . args)
       (abort-current-continuation tag (lambda () (apply ck args))))
     (proc k))
   tag))
(define-syntax let/cc
  (syntax-rules ()
    ((_ k expr ...)
     (call/cc (lambda (k) expr ...)))))
(define call-with-current-continuation call/cc)

(define-syntax with-cc-variants
  (lambda (stx)
    (syntax-case stx ()
      [(k body)
       (with-syntax ([call/cc (datum->syntax #'k 'call/cc)]
                     [let/cc (datum->syntax #'k 'let/cc)]
                     [call-with-continuation-prompt
                      (datum->syntax #'k 'call-with-continuation-prompt)])
	 #'(begin
             (define (a-test call/cc call-with-continuation-prompt)
               (define-syntax let/cc
		 (syntax-rules ()
                   [(_ id bdy (... ...)) 
                    (call/cc (lambda (id) bdy (... ...)))]))
               body)
             ;;(a-test call/cc call-with-continuation-prompt)
             (a-test call/cc-via-composable
                     call-with-continuation-prompt-for-composable)
             (a-test call/cc-via-aborted-and-restored-composable
                     call-with-continuation-prompt-for-composable)))])))

(define-syntax let/prompt
  (syntax-rules ()
    ((_ ((var val)  ...) body ...)
     (let/prompt (default-continuation-prompt-tag) ((var val) ...) body ...))
    ((_ tag ((var val)  ...) body ...)
     (call-with-continuation-prompt
      (lambda ()
	(let ((var val) ...) body ...))
      tag))))
	

(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))
(define (current-milliseconds) (div (microsecond) 1000))
(define void values)
(define null '())
(define-syntax λ (identifier-syntax lambda))
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name pat ...) templ)
     (define-syntax name
       (syntax-rules ()
	 ((_ pat ...) templ))))))
;;----------------------------------------
;; Prompt escapes

;; Simple return
(test 10 call-with-continuation-prompt 
      (lambda () 10))
(test-values '(10 11) (lambda ()
                        (call-with-continuation-prompt 
                         (lambda () (values 10 11)))))
(test-values '() (lambda ()
                   (call-with-continuation-prompt 
                    (lambda () (values)))))

;; Aborts
(test 11 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11))
      (default-continuation-prompt-tag)
      values)
(test 11 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  (lambda () 11))))
(test 12 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  12))
      (default-continuation-prompt-tag)
      values)
(test 12 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  (lambda () 12)))
      (default-continuation-prompt-tag))
(test-values '(11 12)
             (lambda ()
               (call-with-continuation-prompt 
                (lambda () (abort-current-continuation
                            (default-continuation-prompt-tag)
                            11
                            12))
                (default-continuation-prompt-tag)
                values)))
(test-values '(11 12)
             (lambda ()
               (call-with-continuation-prompt 
                (lambda () (abort-current-continuation
                            (default-continuation-prompt-tag)
                            (lambda () (values 11
                                               12)))))))
(test 8 call-with-continuation-prompt 
      (lambda () (+ 17
                    (abort-current-continuation
                     (default-continuation-prompt-tag)
                     (lambda () 8)))))
(test 81 call-with-continuation-prompt 
      (lambda () (+ 17
                    (call-with-continuation-prompt 
                     (lambda ()
                       (abort-current-continuation
                        (default-continuation-prompt-tag)
                        (lambda () 81)))
                     (make-continuation-prompt-tag)))))
(let ([p (make-continuation-prompt-tag)])
  (test 810 call-with-continuation-prompt 
        (lambda () (+ 17
                      (call-with-continuation-prompt 
                       (lambda ()
                         (abort-current-continuation
                          p
                          810))
                       (make-continuation-prompt-tag))))
        p
        values))

;; Aborts with handler
(test 110 call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11))
      (default-continuation-prompt-tag)
      (lambda (x) (* x 10)))
(test 23
      call-with-continuation-prompt 
      (lambda () (abort-current-continuation
                  (default-continuation-prompt-tag)
                  11
                  12))
      (default-continuation-prompt-tag)
      (lambda (x y) (+ x y)))

;; Abort to a prompt in a d-w post that is deeper than a
;;  prompt with the same tag at the continuation-jump site:
(test 0
      values
      (let ([p1 (make-continuation-prompt-tag 'p1)]
            [p2 (make-continuation-prompt-tag 'p2)])
        (let/cc k
          (call-with-continuation-prompt
           (lambda ()
             (call-with-continuation-prompt
              (lambda ()
                (dynamic-wind
                    void
                    (lambda ()
                      (call-with-continuation-prompt
                       (lambda ()
                         (k 0))
                       p2))
                    (lambda ()
                      (abort-current-continuation p1 (lambda () 0)))))
              p1))
           p2))))

(err/rt-test (abort-current-continuation
              (make-continuation-prompt-tag 'px))
             exn:fail:contract:continuation?)

;; ----------------------------------------
;; Continuations

(with-cc-variants
 (test -17
       call-with-continuation-prompt
       (lambda () -17)))

(with-cc-variants
 (test 17
       call-with-continuation-prompt
       (lambda ()
         (let/cc k
           (k 17)))))

;;(test-breaks-ok)

(with-cc-variants
 (test 29
       'in-other-prompt1
       (let ([retry #f])
         (test 35
               call-with-continuation-prompt
               (lambda ()
                 (+ 18
                    (let/cc k
                      (set! retry k)
                      17))))
         (+ 1 (call-with-continuation-prompt
               (lambda ()
                 (retry 10)))))))

(with-cc-variants
 (test 60
       'in-other-prompt2
       (let ([retry #f])
         (test 35
               call-with-continuation-prompt
               (lambda ()
                 (+ 18
                    (let/cc k
                      (set! retry k)
                      17))))
         (+ 1 (call-with-continuation-prompt
               (lambda ()
                 (+ (call-with-continuation-prompt
                     (lambda ()
                       (retry 12)))
                    (call-with-continuation-prompt
                     (lambda ()
                       (retry 11))))))))))


;; Catch continuation in composed continuation:
(with-cc-variants
 (test 89
       'catch-composed
       (let ([k (call-with-continuation-prompt
                 (lambda ()
                   ((let/cc k (lambda () k)))))])
         (let ([k2 (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           (car (let/cc k2 (list k2)))))))])
           (call-with-continuation-prompt
            (lambda ()
              (k2 '(89))))))))

;; Grab continuation shallow inside meta-prompt with
;;  delimiting prompt deep in a different meta-prompt.
(with-cc-variants
 (let ([k (call-with-continuation-prompt
           (lambda ()
             ((call/cc
               (lambda (k) (lambda () k))))))])
   (test 10 call-with-continuation-prompt
         (lambda ()
           (let loop ([n 300])
             (if (zero? n)
                 (k (lambda ()
                      (let/cc k2 (k2 10))))
                 (cons n (loop (sub1 n)))))))))

;; Grab continuation deep inside meta-prompt with
;;  delimiting prompt shallow in a different meta-prompt.
(with-cc-variants
 (let ([k (call-with-continuation-prompt
           (lambda ()
             (let loop ([n 12])
               (if (zero? n)
                   ((call/cc
                     (lambda (k) (lambda () k))))
                   (cons 1 (loop (sub1 n)))))))])
   (test '(1 1 1 1 1 1 1 1 1 1 1 1 . 10) call-with-continuation-prompt
         (lambda ()
           ((list-tail k 12)
            (lambda ()
              (let/cc k2 (k2 10))))))))


;; Make sure a jump to an enclosing continuation doesn't
;; skip over a prompt:
(test 25 values (+ 5
                   (call-with-continuation-prompt
                    (lambda ()
                      (+ 1
                         (call/cc
                          (lambda (k)
                            (+ 17
                               (call-with-continuation-prompt
                                (lambda ()
                                  (k 1)))))))))))

;; ----------------------------------------
;; Overlapping continuations

;; Nested
#;(let ([p1 (make-continuation-prompt-tag)]
      [p2 (make-continuation-prompt-tag)])
  (let ([k1 #f]
        [k2 #f])
    (test '(p1 p2 100)
          call-with-continuation-prompt
          (lambda ()
            (cons 'p1
                  (call-with-continuation-prompt
                   (lambda ()
                     (cons 'p2
                           ((call/cc
                            (lambda (-k1)
                              (set! k1 -k1)
                              (call/cc (lambda (-k2)
                                         (set! k2 -k2)
                                         (lambda () '(100)))
                                       p2))
                            p1))))
                   p2)))
          p1)
    (err/rt-test (k1) exn:fail:contract:continuation?)
    (err/rt-test (k2) exn:fail:contract:continuation?)
    (err/rt-test (call-with-continuation-prompt 
                  (lambda () (k1))
                  p2)
                 exn:fail:contract:continuation?)
    (err/rt-test (call-with-continuation-prompt 
                  (lambda () (k2))
                  p1)
                 exn:fail:contract:continuation?)
    (test '(p1 p2 101) call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () '(101))))
          p1)
    (test '(p2 102) call-with-continuation-prompt
          (lambda ()
            (k2 (lambda () '(102))))
          p2)
    (test '(p1 p2 102-1) call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () (k2 (lambda () '(102-1))))))
          p1)))

;; Use default tag to catch a meta-continuation of p1.
;; Due to different implementations of the default tag,
;; this test is interesting in the main thread and
;; a sub thread:
#;(let ()
  (define (go)
    (let ([p1 (make-continuation-prompt-tag)])
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  ((call/cc (lambda (k) (lambda () k))
                            p1)))
                p1)])
        (let ([k2 (list
                   (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           (let/cc k k))))
                    p1))])
          (if (procedure? (car k2))
              ((car k2) 10)
              (test '(10) values k2))))))
  (go)
  (let ([finished #f])
    (thread-wait
     (thread (lambda ()
               (go)
               (set! finished 'finished))))
    (test 'finished values finished)))

;; Use default tag to catch a meta-continuation of p1,
;; then catch continuation again (i.e., loop).
#;(let ([finished #f])
  (define (go)
    (let ([p1 (make-continuation-prompt-tag)]
          [counter 10])
      (let ([k (call-with-continuation-prompt
                (lambda ()
                  ((call/cc (lambda (k) (lambda () k))
                            p1)))
                p1)])
        (let ([k2 (list
                   (call-with-continuation-prompt
                    (lambda ()
                      (k (lambda ()
                           ((let/cc k (lambda () k))))))
                    p1))])
          (if (procedure? (car k2))
              ((car k2) (lambda () 
                          (if (zero? counter)
                              10
                              (begin
                                (set! counter (sub1 counter))
                                ((let/cc k (lambda () k)))))))
              (test '(10) values k2))
          (set! finished 'finished)))))
  (go)
  (let ([finished #f])
    (thread-wait
     (thread (lambda ()
               (go)
               (set! finished 'finished))))
    (test 'finished values finished)))

;; ----------------------------------------
;; Composable continuations

(err/rt-test (call-with-composable-continuation
              (lambda (x) x)
              (make-continuation-prompt-tag 'px))
             exn:fail:contract:continuation?)

(let ([k (call-with-continuation-prompt
          (lambda ()
            (call-with-composable-continuation
             (lambda (k) k))))])
  (test 12 k 12)
  (test 13 k (k (k (k 13))))
  (test-values '(12 13) (lambda () (k 12 13))))

(let ([k (call-with-continuation-prompt
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k) (lambda () k))))))])
  (test 12 k (lambda () 12))
  (test-values '(12 13) (lambda () (k (lambda () (values 12 13)))))
  ;; Composition shouldn't introduce a prompt:
  (test 10 call-with-continuation-prompt
        (lambda ()
          (let ([k2 (k (lambda ()
                         (let/cc k2 k2)))])
            (if (procedure? k2)
                (k2 10)
                k2))))
  ;; Escape from composed continuation:
  (let ([p (make-continuation-prompt-tag)])
    (test 8 call-with-continuation-prompt 
          (lambda ()
            (+ 99 (k (lambda () (abort-current-continuation p 8)))))
          p
          values))
  (test 8 call-with-continuation-prompt 
        (lambda ()
          (+ 99 (k (lambda () (abort-current-continuation 
                               (default-continuation-prompt-tag)
                               8)))))
        (default-continuation-prompt-tag)
        values))

;; Etc.
(let ([k1 (call-with-continuation-prompt
           (lambda ()
             ((call-with-composable-continuation
               (lambda (k)
                 (lambda () k))))))]
      [k2 (call-with-continuation-prompt
           (lambda ()
             ((call-with-composable-continuation
               (lambda (k)
                 (lambda () k))))))])
  (test 1000
        call-with-continuation-prompt
        (lambda ()
          (k1 (lambda () (k2 (lambda () 1000))))))
  (test -1000 k1 (lambda () (k2 (lambda () -1000))))
  
  (let ([k3 (call-with-continuation-prompt
             (lambda ()
               (k1 (lambda ()
                     ((call-with-composable-continuation
                       (lambda (k)
                         (lambda () k))))))))])
    (test 1001
          call-with-continuation-prompt
          (lambda ()
            (k3 (lambda () 1001))))
    (test -1001 k3 (lambda () -1001))
    (test 1002
          call-with-continuation-prompt
          (lambda ()
            (k1 (lambda () (k3 (lambda () 1002))))))
    (test -1002 k1 (lambda () (k3 (lambda () -1002)))))
  
  (let ([k4 (call-with-continuation-prompt
             (lambda ()
               (k1
                (lambda ()
                  ((call-with-composable-continuation
                    (lambda (k)
                      (lambda () k))))))))])
    (test -1003 k4 (lambda () -1003)))
    
  (let ([k5 (call-with-continuation-prompt
             (lambda ()
               ((k1
                 (lambda ()
                   (call-with-composable-continuation
                    (lambda (k)
                      (lambda () k))))))))])
    (test -1004 k5 (lambda () -1004))
    
    (let ([k6 (call-with-continuation-prompt
               (lambda ()
                 ((k5
                   (lambda ()
                     (call-with-composable-continuation
                      (lambda (k)
                        (lambda () k))))))))])
      (test -1005 k6 (lambda () -1005))))
    
  (let ([k7 (call-with-continuation-prompt
             (lambda ()
               ((k1
                 (lambda ()
                   ((k1
                     (lambda ()
                       (call-with-composable-continuation
                        (lambda (k)
                          (lambda () (lambda () k))))))))))))])
    (test -1006 k7 (lambda () (lambda () -1006)))
    (test '(-1007) call-with-continuation-prompt
          (lambda ()
            (list (k7 (lambda () (lambda () -1007)))))))
  
  )

;; Check that escape drops the meta-continuation:
(test 0
      'esc
      (let ([p1 (make-continuation-prompt-tag)])
        (let/cc esc
          (let ([k
                 (call-with-continuation-prompt
                  (lambda ()
                    ((call-with-composable-continuation
                      (lambda (k)
                        (lambda () k))
                      p1)))
                  p1)])
            (/ (k (lambda () (esc 0))))))))

;; ----------------------------------------
;; Dynamic wind

(test 89
      'dw
      (let ([k (dynamic-wind
                   void
                   (lambda () (let ([k+e (let/cc k (cons k void))])
                                ((cdr k+e) 89)
                                (car k+e)))
                   void)])
        (let/cc esc
          (k (cons void esc)))))

(let ([l null])
  (let ([k2
         (dynamic-wind
             (lambda () (set! l (cons 'pre0 l)))
             (lambda ()
               (let ([k (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre l)))
                               (lambda () (let ([k (let/cc k k)])
                                            k))
                               (lambda () (set! l (cons 'post l))))))])
                 (test '(post pre pre0) values l)
                 ;; Jump from one to the other:
                 (let ([k2 
                        (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre2 l)))
                               (lambda ()
                                 (dynamic-wind
                                     (lambda () (set! l (cons 'pre3 l)))
                                     (lambda ()
                                       (let/cc k2 (k k2)))
                                     (lambda () (set! l (cons 'post3 l)))))
                               (lambda () (set! l (cons 'post2 l))))))])
                   (test '(post pre post2 post3 pre3 pre2 post pre pre0) values l)
                   k2)))
             (lambda () (set! l (cons 'post0 l))))])
    (test '(post0 post pre post2 post3 pre3 pre2 post pre pre0) values l)
    ;; Restore in context with fewer DWs:
    (test 8 call-with-continuation-prompt (lambda () (k2 8)))
    (test '(post2 post3 pre3 pre2 post0 post pre post2 post3 pre3 pre2 post pre pre0) values l)
    ;; Restore in context with more DWs:
    (set! l null)
    (dynamic-wind
        (lambda () (set! l (cons 'pre4 l)))
        (lambda ()
          (dynamic-wind
              (lambda () (set! l (cons 'pre5 l)))
              (lambda ()
                (call-with-continuation-prompt k2))
              (lambda () (set! l (cons 'post5 l)))))
        (lambda () (set! l (cons 'post4 l))))
    (test '(post4 post5 post2 post3 pre3 pre2 pre5 pre4) values l)))

;; Like the meta-continuation test above, but add a dynamic wind
;;  to be restored in the p1 continuation:
(let/prompt ([p1 (make-continuation-prompt-tag)]
	     [did #f])
  (let ([k (call-with-continuation-prompt
            (lambda ()
              (dynamic-wind
                  (lambda ()
                    (set! did 'in))
                  (lambda ()
                    ((call/cc (lambda (k) (lambda () k))
                              p1)))
                  (lambda ()
                    (set! did 'out))))
            p1)])
    (set! did #f)
    (let ([k2 (list
               (call-with-continuation-prompt
                (lambda ()
                  (k (lambda ()
                       (test 'in values did)
                       ((let/cc k (lambda () k))))))
                p1))])
      (test 'out values did)
      (if (procedure? (car k2))
          ((car k2) (lambda ()
                      (test 'in values did)
                      10))
          (test '(10) values k2)))))

;; Composable continuations
(let ([l null])
  (let ([k2
         (dynamic-wind
             (lambda () (set! l (cons 'pre0 l)))
             (lambda ()
               (let ([k (call-with-continuation-prompt
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! l (cons 'pre l)))
                               (lambda () 
                                 ((call-with-composable-continuation
                                   (lambda (k) 
                                     (lambda () k)))))
                               (lambda () (set! l (cons 'post l))))))])
                 (test '(post pre pre0) values l)
                 (test 12 k (lambda () 12))
                 (test '(post pre post pre pre0) values l)
                 k))
             (lambda () (set! l (cons 'post0 l))))])
    (test '(post0 post pre post pre pre0) values l)
    (test 73 k2 (lambda () 73))
    (test '(post pre post0 post pre post pre pre0) values l)
    (set! l null)    
    ;; Add d-w inside k2:
    (let ([k3 (call-with-continuation-prompt
               (lambda ()
                 (k2 (lambda ()
                       (dynamic-wind
                           (lambda () (set! l (cons 'pre2 l)))
                           (lambda () 
                             ((call-with-composable-continuation
                               (lambda (k) 
                                 (lambda () k)))))
                           (lambda () (set! l (cons 'post2 l))))))))])
      (test '(post post2 pre2 pre) values l)
      (test 99 k3 (lambda () 99))
      (test '(post post2 pre2 pre post post2 pre2 pre) values l))
    (set! l null)    
    ;; Add d-w outside k2:
    (let ([k4 (call-with-continuation-prompt
               (lambda ()
                 (dynamic-wind
                     (lambda () (set! l (cons 'pre2 l)))
                     (lambda () 
                       (k2 (lambda ()
                             ((call-with-composable-continuation
                               (lambda (k) 
                                 (lambda () k)))))))
                     (lambda () (set! l (cons 'post2 l))))))])
      (test '(post2 post pre pre2) values l)
      (test 99 k4 (lambda () 99))
      (test '(post2 post pre pre2 post2 post pre pre2) values l))))

;; Jump back into post:    
(let/prompt ([l null]
	     [p1 (make-continuation-prompt-tag)]
	     [p2 (make-continuation-prompt-tag)]
	     [k2 #f])
  (define (out v) (set! l (cons v l)))
  (call-with-continuation-prompt
   (lambda ()
     (dynamic-wind
         (lambda () (out 'pre))
         (lambda ()
           (call-with-continuation-prompt
            (lambda ()
              (dynamic-wind
                  (lambda () (out 'pre2))
                  (lambda () (void))
                  (lambda ()
                    (call/cc (lambda (k)
                               (set! k2 k))
                             p2)
                    (out 'post2))))
            p2))
         (lambda () (out 'post1))))
   p1)
  (call-with-continuation-prompt
   (lambda ()
     (k2 10))
   p2)
  (test '(post2 post1 post2 pre2 pre) values l))

;; Jump into post, then back out
(let/prompt ([l null]
	     [p1 (make-continuation-prompt-tag)]
	     [p2 (make-continuation-prompt-tag)]
	     [k2 #f]
	     [count 0])
    (define (out v) (set! l (cons v l)))
    (let/cc esc
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () (out 'pre1))
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (dynamic-wind
                      (lambda () (out 'pre2))
                      (lambda () (void))
                      (lambda ()
                        (call/cc (lambda (k)
                                   (set! k2 k))
                                 p2)
                        (out 'post2)
                        (esc))))
                p2))
             (lambda () (out 'post1))))
       p1))
    (set! count (add1 count))
    (unless (= count 3)
      (call-with-continuation-prompt
       (lambda ()
         (k2 10))
       p2))
    (test '(post2 post2 post1 post2 pre2 pre1) values l))

(print "into post from escape")

;; Jump into post from an escape, rather than 
;;  from a result continuation
(let/prompt ([l null]
	     [p1 (make-continuation-prompt-tag 'p1)]
	     [p2 (make-continuation-prompt-tag 'p2)]
	     [k2 #f]
	     [count 0])
    (define (out v) (set! l (cons v l)))
    (let/cc esc
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () (out 'pre1))
             (lambda ()
               (call-with-continuation-prompt
                (lambda ()
                  (dynamic-wind
                      (lambda () (out 'pre2))
                      (lambda () (esc))
                      (lambda ()
                        (call/cc (lambda (k)
                                   (set! k2 k))
                                 p2)
                        (out 'post2))))
                p2))
             (lambda () (out 'post1))))
       p1))
    (set! count (add1 count))
    (unless (= count 3)
      (call-with-continuation-prompt
       (lambda ()
         (k2 10))
       p2))
    (test '(post2 post2 post1 post2 pre2 pre1) values l))

;; ----------------------------------------
;; Continuation marks



(define (non-tail v) (values v))



;; Check interaction of dynamic winds, continuation composition, and continuation marks


;; ----------------------------------------
;; Olivier Danvy's traversal

;; Shift & reset via composable and abort
(let ()
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      '()
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 (let ([v (cons (car xs)
                                                (call-with-continuation-prompt
                                                 (lambda ()
                                                   (k (cdr xs)))))])
                                   (lambda () v))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(1 2 3 4 5) traverse '(1 2 3 4 5)))

;; Shift & reset using composable and call/cc
(let ()
  (define call-in-application-context
    (call-with-continuation-prompt
     (lambda ()
       ((call-with-current-continuation
         (lambda (k) (lambda () k)))))))
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      '()
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (call-in-application-context
                                 (lambda ()
                                   (cons (car xs)
                                         (call-with-continuation-prompt
                                          (lambda ()
                                            (k (cdr xs))))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(1 2 3 4 5) traverse '(1 2 3 4 5)))

;; control and prompt using composable and abort
(let ()
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      (list-tail '() 0)
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (abort-current-continuation
                                 (default-continuation-prompt-tag)
                                 (lambda ()
                                   (cons (car xs)
                                         (k (cdr xs))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(5 4 3 2 1) traverse '(1 2 3 4 5)))

;; control and prompt using composable and call/cc
(let ()
  (define call-in-application-context
    (call-with-continuation-prompt
     (lambda ()
       ((call-with-current-continuation
         (lambda (k) (lambda () k)))))))
  (define traverse
    (lambda (xs)
      (letrec ((visit
                (lambda (xs)
                  (if (null? xs)
                      (list-tail '() 0)
                      (visit (call-with-composable-continuation
                              (lambda (k)
                                (call-in-application-context
                                 (lambda ()
                                   (cons (car xs)
                                         (k (cdr xs))))))))))))
        (call-with-continuation-prompt
         (lambda ()
           (visit xs))))))
  (test '(5 4 3 2 1) traverse '(1 2 3 4 5)))

;; ----------------------------------------
;; Check unwinding of runstack overflows on prompt escape

;; ----------------------------------------
;; Some repeats, but ensure a continuation prompt
;;  and check d-w interaction.

(let ([output null])
  (call-with-continuation-prompt
   (lambda () 
     (dynamic-wind
         (lambda () (set! output (cons 'in output)))
         (lambda ()
           (let ([finished #f])
             (define (go)
               (let ([p1 (make-continuation-prompt-tag)]
                     [counter 10])
                 (let ([k (call-with-continuation-prompt
                           (lambda ()
                             ((call/cc (lambda (k) (lambda () k))
                                       p1)))
                           p1)])
                   (let ([k2 (list
                              (call-with-continuation-prompt
                               (lambda ()
                                 (k (lambda ()
                                      ((let/cc k (lambda () k))))))
                               p1))])
                     (current-milliseconds)
                     (if (procedure? (car k2))
                         ((car k2) (lambda () 
                                     (if (zero? counter)
                                         10
                                         (begin
                                           (set! counter (sub1 counter))
                                           ((let/cc k (lambda () k)))))))
                         (values '(10) values k2))
                     (set! finished 'finished)))))
             (go)))
         (lambda () (set! output (cons 'out output)))))
   (default-continuation-prompt-tag)
   void)
  (test '(out in) values output))

(let ([output null])
  (call-with-continuation-prompt
   (lambda () 
     (dynamic-wind
         (lambda () (set! output (cons 'in output)))
         (lambda ()
           (let ([p1 (make-continuation-prompt-tag)])
             (let/cc esc
               (let ([k
                      (call-with-continuation-prompt
                       (lambda ()
                         ((call-with-composable-continuation
                           (lambda (k)
                             (lambda () k))
                           p1)))
                       p1)])
                 (/ (k (lambda () (esc 0))))))))
         (lambda () (set! output (cons 'out output)))))
   (default-continuation-prompt-tag)
   void)
  (test '(out in) values output))

;;----------------------------------------
;; tests invoking delimited captures in dynamic-wind pre- and post-thunks

;; Again, more checking of output
(let ([p1 (make-continuation-prompt-tag 'p1)]
      [p2 (make-continuation-prompt-tag 'p2)]
      [output null]
      [exit-k #f])
    ;; Set up a prompt tp jump to:
    (call-with-continuation-prompt
     (lambda ()
       (dynamic-wind
           (lambda () (void))
           (lambda () 
             ;; Set a prompt for tag p1:
             (call-with-continuation-prompt
              (lambda ()
                (dynamic-wind
                    (lambda () (void))
                    ;; inside d-w, jump out:
                    (lambda () (abort-current-continuation
                                p2
                                "done"))
                    (lambda ()
                      ;; As we jump out, capture a continuation 
                      ;; w.r.t. p1:
                      ((call/cc
                        (lambda (k) 
                          (set! exit-k k)
                          (lambda () 10))
                        p1))
                      (set! output (cons 'inner output)))))
              p1))
           (lambda ()
             ;; This post thunk is not in the
             ;;  delimited continuation captured
             ;; via tag p1:
             (set! output (cons 'outer output)))))
     p2
     (lambda (v)
       (set! output (cons 'orig output))))
    ;; Now call, redirecting the escape to here:
    (call-with-continuation-prompt
     (lambda ()
       (call-with-continuation-prompt
        (lambda ()
          (exit-k (lambda () 10)))
        p1))
     p2
     (lambda (v)
       (set! output (cons 'new output))))
    (test '(new inner orig outer inner) values output))

;; abort past a tag
(test 10
      values
      (let ([p1 (make-continuation-prompt-tag)]
            [p2 (make-continuation-prompt-tag)])
        (call-with-continuation-prompt
         (lambda ()
           (call/cc
            (lambda (k)
              (call-with-continuation-prompt
               (lambda ()
                 (k 10))
               p2))
            p1))
         p1)))

;; Test capturing and invoking a composable continuation in a post thunk
(let ()
  (define call/pt call-with-continuation-prompt)
  (define call/comp-cc call-with-composable-continuation)
  (define (go p0 direct?)
    (define accum null)
    (define (print v) (set! accum (append accum (list v))))
    (define a #f)
    (define do-a? #t)
    (call/pt
     (lambda ()
       (dynamic-wind
           (lambda () (print 1))
           (lambda ()
             (begin
               (dynamic-wind
                   (lambda () (print 2))
                   (lambda () 
                     ((call/cc (lambda (k)
                                 (begin
                                   (set! a k)
                                   (lambda () 12)))
                               p0)))
                   (lambda () (print 3)))
               (dynamic-wind
                   (lambda () (print 4))
                   (lambda ()
                     (if do-a?
                         (begin
                           (set! do-a? #f)
                           (a (lambda () 11)))
                         12))
                   (lambda ()
                     (begin
                       (print 5)
                       (call/comp-cc
                        (lambda (k)
                          (if direct?
                              (k 10)
                              (call/pt
                               (lambda ()
                                 (k 10))
                               p0
                               (lambda (x) x))))
                        p0))))))
           (lambda () (print 6))))
     p0
     (lambda (x) x))
    accum)
  (test '(1 2 3 4 5 1 6 2 3 4 5 1 6 6) go (default-continuation-prompt-tag) #t)
  (test '(1 2 3 4 5 1 6 2 3 4 5 1 6 6) go (make-continuation-prompt-tag) #t)
  (test '(1 2 3 4 5 1 2 3 4 5 1 6 6 2 3 4 5 1 6 6) go (default-continuation-prompt-tag) #f)
  (test '(1 2 3 4 5 1 2 3 4 5 1 6 6 2 3 4 5 1 6 6) go (make-continuation-prompt-tag) #f))

;; ----------------------------------------
;; Tests related to cotinuations that capture pre-thunk frames

;; Simple case:
(let ([t
       (lambda (wrapper)
         (test
          '(pre1 mid1 post1 pre2 mid1 post1 post2)
          'cc1
          (let ([k #f]
                [recs null])
            (define (queue v) (set! recs (cons v recs)))
            (call-with-continuation-prompt
             (lambda ()
               (dynamic-wind
                   (lambda ()
                     (queue 'pre1) 
                     (call-with-composable-continuation
                      (lambda (k0)
                        (set! k k0))))
                   (lambda () (queue 'mid1))
                   (lambda () (queue 'post1)))))
            (wrapper
             (lambda ()
               (dynamic-wind
                   (lambda () (queue 'pre2))
                   (lambda () (k))
                   (lambda () (queue 'post2)))))
            (reverse recs))))])
  (t (lambda (f) (f)))
  (t call-with-continuation-prompt))



;; Even more dynamic-winds:
(test
 '(pre0 pre1 mid1 post1 post0 
        pre1.5 pre2 pre0 mid1 post1 post0 post2 post1.5 
        pre3 pre1.5 pre2 pre0 mid1 post1 post0 post2 post1.5 post3)
 'cc3
 (let ([k #f]
       [k2 #f]
       [recs null])
   (define (queue v) (set! recs (cons v recs)))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda ()
            (queue 'pre0))
          (lambda ()
            (dynamic-wind
                (lambda ()
                  (queue 'pre1) 
                  ((call-with-composable-continuation
                    (lambda (k0)
                      (set! k k0)
                      void))))
                (lambda () (queue 'mid1))
                (lambda () (queue 'post1))))
          (lambda ()
            (queue 'post0)))))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda () (queue 'pre1.5))
          (lambda ()
            (dynamic-wind
                (lambda () (queue 'pre2))
                (lambda () (k (lambda ()
                                (call-with-composable-continuation
                                 (lambda (k0)
                                   (set! k2 k0))))))
                (lambda () (queue 'post2))))
          (lambda () (queue 'post1.5)))))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
          (lambda () (queue 'pre3))
          (lambda () (k2))
          (lambda () (queue 'post3)))))
   (reverse recs)))

;; Arrange for the captured pre-thunk to trigger extra cloning
;; of dynmaic wind records in continuation application:
(test
 '(pre1 pre2 post2 post1 pre1 pre2 post2 post1 last pre2 post2 post1)
 'cc4
 (let ([k #f]
       [k2 #f]
       [recs null]
       [tag (make-continuation-prompt-tag)])
   (define (queue v) (set! recs (cons v recs)))
   (call-with-continuation-prompt
    (lambda ()
      (dynamic-wind
       (lambda ()
         (queue 'pre1) 
         ((call-with-composable-continuation
           (lambda (k0)
             (set! k k0)
             void))))
       (lambda () 
         (dynamic-wind
          (lambda () (queue 'pre2))
          (lambda ()
            ((call-with-composable-continuation
              (lambda (k0)
                (set! k2 k0)
                void))))
          (lambda () (queue 'post2))))
       (lambda () (queue 'post1)))))
   (let ([k3
          (call-with-continuation-prompt
           (lambda ()
            (call-with-continuation-prompt
             (lambda ()
               (k2 (lambda ()
                     (call-with-composable-continuation
                      (lambda (k0)
                        (abort-current-continuation tag (lambda () k0)))))))))
           tag)])
     (queue 'last)
     (call-with-continuation-prompt
      (lambda ()
        (k void))
      tag))
   (reverse recs)))



;; ----------------------------------------
;; There's a "is the target prompt still in place?"
;;  check that should not happen when a composable
;;  continuation is applied. (Random testing discovered
;;  an incorrect check.)

(test
 12345
 'no-prompt-check-on-compose
 (let ()
   (define pt1 (make-continuation-prompt-tag))

   (define-syntax-rule (% pt body handler)
     (call-with-continuation-prompt
      (lambda () body)
      pt
      handler))

   ;; (lambda (v) v)
   ;; as a composable continuation:
   (define comp-id
     (%
      pt1
      (call-with-composable-continuation
       (λ (k) (abort-current-continuation pt1 k))
       pt1)
      (lambda (k) k)))

   ((% pt1
       (dynamic-wind
           (λ () (comp-id 2))
           (λ () 
              ;; As we jump back to this continuation,
              ;; it's ok that no `pt1' prompt is
              ;; in place anymore
              (call-with-composable-continuation
               (λ (k) (abort-current-continuation
                       pt1 
                       k))
               pt1))
           (λ () #f))
       (λ (x) x))
    12345)))

(test
 12345
 'no-prompt-post-check-on-compose
 (let ()
   (define pt1 (make-continuation-prompt-tag))
   
   (define-syntax-rule (% pt body handler)
     (call-with-continuation-prompt
      (lambda () body)
      pt
      handler))

   ((λ (y-comp-cont_7)
       ((λ (x-comp-cont_3)
           ((%
             pt1
             (x-comp-cont_3
              (λ ()
                 (y-comp-cont_7
                  (λ () (call-with-composable-continuation
                         (λ (k) (abort-current-continuation pt1 k))
                         pt1)))))
             (λ (x) x))
            12345))
        (%
         pt1
         (dynamic-wind
             (λ () (y-comp-cont_7 (λ () #f)))
             (λ () ((call-with-composable-continuation
                     (λ (k) (abort-current-continuation pt1 k))
                     pt1)))
             (λ () #f))
         (λ (x) x))))
    (%
     pt1
     (dynamic-wind
         (λ () #f)
         (λ () ((call-with-composable-continuation
                 (λ (k) (abort-current-continuation pt1 k)) 
                 pt1)))
         (λ () #f))
     (λ (x) x)))))

;; ----------------------------------------

