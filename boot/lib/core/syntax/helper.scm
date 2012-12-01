;; -*- scheme -*-
;; This file is a part of Sagittarius Scheme system.
(library (core syntax helper)
    (export bar?
	    ellipsis?
	    ellipsis-pair?
	    ellipsis-splicing-pair?
	    ellipsis-quote?
	    count-pair

	    <sid>
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

  (define bar?
    (lambda (expr)
      (and (variable? expr)
	   (eq? (identifier->symbol expr) '_))))

  (define ellipsis?
    (lambda (expr)
      (and (variable? expr)
	   (eq? (identifier->symbol expr) '...))))
  
  (define ellipsis-pair?
    (lambda (form)
      (and (pair? form)
	   (pair? (cdr form))
	   (ellipsis? (cadr form)))))

  (define ellipsis-splicing-pair?
    (lambda (form)
      (and (pair? form)
	   (pair? (cdr form))
	   (ellipsis? (cadr form))
	   (pair? (cddr form))
	   (ellipsis? (caddr form)))))

  (define ellipsis-quote?
    (lambda (form)
      (and (pair? form)
	   (ellipsis? (car form))
	   (pair? (cdr form))
	   (null? (cddr form)))))

  (define (count-pair p)
    (let loop ((lst p) (n 0))
      (if (pair? lst) (loop (cdr lst) (+ n 1)) n)))

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

  (define (optimized-cons rename compare a d)
    (cond ((and (pair? d)
		(compare (car d) (rename 'syntax-quote))
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
	     (compare (car y) (rename 'syntax-quote))
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
		       (error "syntax template" "illegal control/ellipsis combination" control sids))))
	      (error "syntax template" "missing ellipsis in expression" sid ellipses))
	  (loop control (cdr ellipses))))))

)
