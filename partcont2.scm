;;
;; This file is included from continuation.scm
;;
;; We split this so that we can also run it with srfi-226 reference
;; implementation to compare behavior.  Be careful not to use Gauche's
;; extended syntax.

;; The comment before each test records results with different implementations.
;;
;; native : Gauche native partial continuation
;; meta   : gauche.partcont-meta, implementation using full continuation
;; srfi226: Srfi-226 reference implementation run on ChezScheme
;; racket : Racket r7rs + racket/control
(define (displaye v)
  (display v)
  (display v (current-error-port)) (newline (current-error-port)))
(define (printe . args)
  (for-each (lambda (v) (display v (current-error-port))) args)
  (newline (current-error-port)))


(test* "dynamic-wind + reset/shift 5"
       "[d01][d02][d01][d11][d12][d02][d11][d12][d11][d12]"
       (with-output-to-string
         (lambda ()
           (define k1 #f)
           (define k2 #f)
           (define k3 #f)
	   (define d01 (lambda () (displaye "[d01]")))
	   (define d02 (lambda () (displaye "[d02]")))
           (reset
            (reset
             (dynamic-wind
		 d01
		 (lambda ()
		   (define d11 (lambda () (displaye "[d11]")))
		   (define d12 (lambda () (displaye "[d12]")))
                   (shift k (set! k1 k))
                   (reset
                    (dynamic-wind
			d11
			(lambda ()
			  (shift k (set! k2 k))
			  (shift k (set! k3 k)))
			d12)))
		 d02))
	    (printe 'k1)
            (k1)
	    (printe 'k2)
            (k2)
	    (printe 'k3)
            (k3)))))
