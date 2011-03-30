;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax helper)
    (export <sid>
	    make-sid sid-name sid-expression sid-depth
	    sid?
	    sid-control

	    <ellipsis>
	    make-ellipsis ellipsis? ellipsis-sids set-ellipsis-sids!

	    syntax
	    id-memq variable? zero-or-more? expand
	    optimized-cons optimized-append
	    add-control! generate-ellipsis
	    parse-pattern)
    (import null
	    (sagittarius)
	    ;(sagittarius vm)
	    (core base)
	    (core misc)
	    (core errors)
	    (core struct))

  (define-struct <sid>
    (make-sid name expression depth control)
    sid?
    (lambda (i p)
      (format p "#<sid ~a ~a ~a>" (sid-name i) (sid-depth i) (sid-control i)))
    (name sid-name)
    (expression sid-expression)
    (depth   sid-depth) ;; do we need this?
    (control sid-control)
    (output-expression sid-output-expression set-sid-output-expression!))

  (define-struct <ellipsis>
    (make-ellipsis sids)
    ellipsis?
    #f
    (sids ellipsis-sids set-ellipsis-sids!))

  (define (id-memq id lst)
    (if (identifier? id)
	(memq (id-name id) lst)
	(memq id lst)))

  (define (optimized-cons rename compare a d)
    (cond ((and (pair? d)
		(compare (car d) (rename 'quote))
		(pair? (cdr d))
		(null? (cadr d))
		(null? (cddr d)))
	   `(,(rename 'list) ,a))
	  ((and (pair? d)
		(compare (car d) (rename 'list))
		(list? (cdr d)))
	   `(,(car d) ,a ,@(cdr d)))
	  (else
	   `(,(rename 'cons) ,a ,d))))

  (define (optimized-append rename compare x y)
    (if (and (pair? y)
	     (compare (car y) (rename 'quote))
	     (pair? (cdr y))
	     (null? (cadr y))
	     (null? (cddr y)))
	x
	`(,(rename 'append) ,x ,y)))

  (define (add-control! sid ellipses)
    (let loop ((sid sid) (ellipses ellipses))
      (let ((control (sid-control sid)))
	(when control
	  (if (pair? ellipses)
	      (let ((sids (ellipsis-sids (car ellipses))))
		(cond ((not (memq control sids))
		       (set-ellipsis-sids! (car ellipses)
					   (cons control sids)))
		      ((not (eq? control (car sids)))
		       (error 'syntax-rules "illegal control/ellipsis combination" control sids))))
	      (error 'syntax-rules "missing ellipsis in expression" sid ellipses))
	  (loop control (cdr ellipses))))))

)