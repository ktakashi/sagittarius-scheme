#!nounbound
(library (sagittarius compiler pass4)
    (export pass4 init-pass4)
    (import (core)
	    (core macro)
	    (for (compat r7rs) expand)
	    (sagittarius)
	    (sagittarius vm)
	    (sagittarius compiler util)
	    (sagittarius compiler iform)
	    (sagittarius compiler pass2))

(include "smatch.scm")

(define (init-pass4 . ignore) #f)

(define (pass4 iform library)
  ;; we need to use the one in iform it there is.
  (set! library (pass2/lookup-library iform library))
  (if (vm-nolambda-lifting?)
      iform
      (let ((dic (make-label-dic '())))
	(pass4/scan iform '() '() #t dic) ;; mark free variables
	(let ((lambda-nodes (label-dic-info dic)))
	  (if (or (null? lambda-nodes)
		  (and (null? (cdr lambda-nodes))
		       ($lambda-lifted-var (car lambda-nodes))))
	      iform			; shortcut
	      (let ((lifted (pass4/lift lambda-nodes library)))
		(if (null? lifted)
		    iform		; shortcut
		    (let ((iform. (pass4/subst iform (make-label-dic '()))))
		      ($seq `(,@(imap pass4/lifted-define lifted)
			      ,iform.))))))))))

;; lambda lifting
(define-syntax pass4/add-lvar
  (syntax-rules ()
    ((_ lvar bound free)
     (if (or (memq lvar bound) (memq lvar free)) free (cons lvar free)))))

(define (pass4/lifted-define lambda-node)
  (let ((id ($lambda-lifted-var lambda-node)))
    (library-defined-add! (id-library id) id)
    ($define ($lambda-src lambda-node)
	     '() ;;'(const) ;; somehow it doesn't work with const flag
	     id lambda-node)))

;; scan
(define-syntax pass4/scan*
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- iforms bs fs t? labels)
	(let ((iforms. (gensym)))
	  `(let ((,iforms. ,iforms))
	     (cond ((null? ,iforms.) ,fs)
		   ((null? (cdr ,iforms.))
		    (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels))
		   (else
		    (let loop ((,iforms. ,iforms.) (,fs ,fs))
		      (if (null? ,iforms.)
			  ,fs
			  (loop (cdr ,iforms.)
				(pass4/scan (car ,iforms.)
					    ,bs ,fs ,t? ,labels)))))))))))))

(define-syntax pass4/subst!
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- access-form labels)
	(smatch access-form
	  ((accessor expr)
	   (let ((org (gensym))
		 (result (gensym))
		 (setter (if (eq? accessor 'car)
			     'set-car!
			     (string->symbol (format "~a-set!" accessor)))))
	     `(let* ((,org (,accessor ,expr))
		     (,result (pass4/subst ,org ,labels)))
		(unless (eq? ,org ,result)
		  (,setter ,expr ,result))
		,expr)))))))))

(define-syntax pass4/subst*!
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- iforms labels)
	(let ((iforms. (gensym)))
	  `(let ((,iforms. ,iforms))
	     (cond ((null? ,iforms.))
		   ((null? (cdr ,iforms.)) (pass4/subst! (car ,iforms.)
							 ,labels))
		   (else
		    (let loop ((,iforms. ,iforms.))
		      (unless (null? ,iforms.)
			(pass4/subst! (car ,iforms.) ,labels)
			(loop (cdr ,iforms.)))))))))))))


(define (pass4/scan iform bs fs t? labels)
  ((vector-ref *pass4/lambda-lifting-table* (iform-tag iform))
   iform bs fs t? labels))

(define (pass4-scan/$DEFINE iform bs fs t? labels)
  (unless t?
    (error 'pass4/lambda-lifting "[internal] $DEFINE in non-toplevel"
	   (unwrap-syntax ($define-src iform))))
  (pass4/scan ($define-expr iform) bs fs #t labels))
(define (pass4-scan/$LREF iform bs fs t? labels)
  (pass4/add-lvar ($lref-lvar iform) bs fs))
(define (pass4-scan/$LSET iform bs fs t? labels)
  (let ((fs (pass4/scan ($lset-expr iform) bs fs t? labels)))
      (pass4/add-lvar ($lref-lvar iform) bs fs)))
(define (pass4-scan/$GSET iform bs fs t? labels)
  (pass4/scan ($gset-expr iform) bs fs t? labels))
(define (pass4-scan/$IF iform bs fs t? labels)
  (let* ((fs (pass4/scan ($if-test iform) bs fs t? labels))
	 (fs (pass4/scan ($if-then iform) bs fs t? labels)))
    (pass4/scan ($if-else iform) bs fs t? labels)))
(define (pass4-scan/$LET iform bs fs t? labels)
  (let* ((new-bs (append ($let-lvars iform) bs))
	 (bs (if (memv ($let-type iform) '(rec rec*)) new-bs bs))
	 (fs (pass4/scan* ($let-inits iform) bs fs t? labels)))
    (pass4/scan ($let-body iform) new-bs fs #f labels)))
(define (pass4-scan/$RECEIVE iform bs fs t? labels)
  (let ((fs (pass4/scan ($receive-expr iform) bs fs t? labels))
	(bs (append ($receive-lvars iform) bs)))
    (pass4/scan ($receive-body iform) bs fs #f labels)))
(define (pass4-scan/$LAMBDA iform bs fs t? labels)
  (let ((inner-fs (pass4/scan ($lambda-body iform)
			      ($lambda-lvars iform) '() #f labels)))
    (unless (eq? ($lambda-flag iform) 'dissolved)
      (label-dic-info-push! labels iform)
      (when t?
	($lambda-lifted-var-set! iform #t)))
    (cond (t? '())
	  (else ($lambda-free-lvars-set! iform inner-fs)
		(let loop ((inner-fs inner-fs) (fs fs))
		  (if (null? inner-fs)
		      fs
		      (loop (cdr inner-fs)
			    (pass4/add-lvar (car inner-fs) bs fs))))))))
(define (pass4-scan/$LABEL iform bs fs t? labels)
  (cond ((label-seen? labels iform) fs)
	(else (label-push! labels iform)
	      (pass4/scan ($label-body iform) bs fs #f labels))))
(define (pass4-scan/$SEQ iform bs fs t? labels)
  (pass4/scan* ($seq-body iform) bs fs t? labels))
(define (pass4-scan/$CALL iform bs fs t? labels)
  (let ((fs (if (eq? ($call-flag iform) 'jump)
		fs
		(pass4/scan ($call-proc iform) bs fs t? labels))))
    (pass4/scan* ($call-args iform) bs fs t? labels)))
(define (pass4-scan/$ASM iform bs fs t? labels)
  (pass4/scan* ($asm-args iform) bs fs t? labels))
(define (pass4-scan/$LIST iform bs fs t? labels)
  (pass4/scan* ($*-args iform) bs fs t? labels))
;; not interested
(define (pass4-scan/$LIBRARY iform bs fs t? labels) fs)
(define (pass4-scan/$IT iform bs fs t? labels) fs)
(define (pass4-scan/$UNDEF iform bs fs t? labels) fs)
(define (pass4-scan/$GREF iform bs fs t? labels) fs)
(define (pass4-scan/$CONST iform bs fs t? labels) fs)

(define *pass4/lambda-lifting-table* (generate-dispatch-table pass4-scan))

(define (pass4/lift lambda-nodes library)
  (let ((top-name #f))
    (let loop ((lms lambda-nodes) (results '()))
      (cond ((null? lms) results)
	    (($lambda-lifted-var (car lms))
	     (let ((n ($lambda-name (car lms))))
	       (set! top-name (if (identifier? n) (id-name n) n)))
	     ($lambda-lifted-var-set! (car lms) #f)
	     (loop (cdr lms) results))
	    (else
	     (let* ((lm (car lms))
		    (fvs ($lambda-free-lvars lm)))
	       (if (or (null? fvs)
		       (and (null? (cdr fvs))
			    (zero? (lvar-set-count (car fvs)))
			    (eq? (lvar-initval (car fvs)) lm)))
		   (let ((gvar (make-identifier (gensym "lambda") '() library)))
		     ($lambda-name-set!
		      lm
		      (list top-name (or (and-let* ((n ($lambda-name lm)))
					   (identifier->symbol n))
					 (id-name gvar))))
		     ($lambda-lifted-var-set! lm gvar)
		     (loop (cdr lms) (cons lm results)))
		   (loop (cdr lms) results))))))))

(define (pass4/subst iform labels)
  ((vector-ref *pass4/subst-table* (iform-tag iform)) iform labels))

(define (pass4-subst/$DEFINE iform labels)
  (pass4/subst! ($define-expr iform) labels))
(define (pass4-subst/$LREF iform labels)
  (or (and (= (lvar-set-count ($lref-lvar iform)) 0)
	   (let ((init (lvar-initval ($lref-lvar iform))))
	     (and (vector? init)
		  (has-tag? init $LAMBDA)
		  (let ((id ($lambda-lifted-var init)))
		    (and id
			 (lvar-ref--! ($lref-lvar iform))
			 (vector-set! iform 0 $GREF)
			 ($gref-id-set! iform id)
			 iform)))))
      iform))
(define (pass4-subst/$LSET iform labels)
  (pass4/subst! ($lset-expr iform) labels))
(define (pass4-subst/$GSET iform labels)
  (pass4/subst! ($gset-expr iform) labels))
(define (pass4-subst/$IF iform labels)
  (pass4/subst! ($if-test iform) labels)
  (pass4/subst! ($if-then iform) labels)
  (pass4/subst! ($if-else iform) labels))
(define (pass4-subst/$LET iform labels)
  (pass4/subst*! ($let-inits iform) labels)
  (pass4/subst! ($let-body iform) labels))
(define (pass4-subst/$RECEIVE iform labels)
  (pass4/subst! ($receive-expr iform) labels)
  (pass4/subst! ($receive-body iform) labels))
(define (pass4-subst/$LAMBDA iform labels)
  (pass4/subst! ($lambda-body iform) labels)
  (or (let ((id ($lambda-lifted-var iform)))
	(and id
	     ($gref id)))
      iform))
(define (pass4-subst/$LABEL iform labels)
  (unless (label-seen? labels iform)
    (label-push! labels iform)
    (pass4/subst! ($label-body iform) labels))
  iform)
(define (pass4-subst/$SEQ iform labels)
  (pass4/subst*! ($seq-body iform) labels)
  iform)
(define (pass4-subst/$CALL iform labels)
  (pass4/subst*! ($call-args iform) labels)
  (pass4/subst! ($call-proc iform) labels))
(define (pass4-subst/$ASM iform labels)
  (pass4/subst*! ($asm-args iform) labels)
  iform)
(define (pass4-subst/$LIST iform labels)
  (pass4/subst*! ($*-args iform) labels)
  iform)
(define (pass4-subst/$IT iform labels) iform)
(define (pass4-subst/$LIBRARY iform labels) iform)
(define (pass4-subst/$UNDEF iform labels) iform)
(define (pass4-subst/$GREF iform labels) iform)
(define (pass4-subst/$CONST iform labels) iform)

(define *pass4/subst-table* (generate-dispatch-table pass4-subst))
)
