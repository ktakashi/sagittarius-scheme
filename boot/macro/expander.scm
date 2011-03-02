;; -*- Scheme -*-

;; for now...
(define-syntax append1
  (syntax-rules ()
    ((_ start last obj)
     (let ((tmp (cons obj '())))
       (if (null? start)
	   (begin
	     (set! start tmp)
	     (set! last tmp))
	   (begin
	     (set-cdr! last tmp)
	     (set! last (cdr last))))))))
(define-syntax append0
  (syntax-rules ()
    ((_ start last obj)
     (let ((tmp obj))
       (if (null? start)
	   (begin
	     (set! start tmp)
	     (unless (null? tmp)
	       (set! last (last-pair tmp))))
	   (begin
	     (set-cdr! last tmp)
	     (set! last (last-pair last))))))))

;; convert literal symbols into identifiers
(define preprocess-literals
  (lambda (literals library env)
    (let ((h '())
	  (t '()))
      (or (list? literals)
	  (error 'syntax-error "bad literal list in syntax-rules:" literal))
      (for-each (lambda (lp)
		  (cond ((identifier? lp)
			 (append1 h t lp))
			((symbol? lp)
			 (append1 h t (make-identifier lp env library) ))
			(else
			 (error 'syntax-error "literal list contains non-symbol:" literals))))
		literals)
      h)))

(define id-memq
  (lambda (name lst)
    (let ((n (if (identifier? name)
		 (id-name name)
		 name)))
      (let loop ((lp lst))
	(cond ((null? lp) #f)
	      ((eq? (id-name (car lp)) name) (car lp))
	      (else (loop (cdr lp))))))))

(define-simple-struct pvref '.pvref make-pvref
  src
  level
  count)
(define pvref?
  (lambda (o)
    (and (vector? o) (eq? (vector-ref o 0) '.pvref))))
(define pvref=?
  (lambda (p1 p2)
    (or (eq? p1 p2)
	(and (pvref? p1)
	     (pvref? p2)
	     (= (pvref-level p1) (pvref-level p2))
	     (= (pvref-count p1) (pvref-count p2))))))

(define add-pvar
  (lambda (ctx pat pvar)
    (let ((pvref (make-pvref pvar 
			     (syntax-pattern-level pat)
			     (pattern-context-pvcount ctx))))
      (if (assq pvar (pattern-context-pvars ctx))
	  (error 'syntax-error 
		 (format "pattern variable ~s appears more than once in the macro definition of ~s ~s"
			 pvar (pattern-context-name ctx) (pattern-context-form ctx))))
      (pvcount++ ctx)
      (pattern-context-pvars-set! ctx (cons (cons pvar pvref)
					    (pattern-context-pvars ctx)))
      (syntax-pattern-vars-set! pat (cons pvref (syntax-pattern-vars pat)))
      pvref)))

(define pvar-to-pvref
  (lambda (ctx pat pvar)
    (let ((q (assq pvar (pattern-context-pvars ctx))))
      (if (pair? q)
	  (let ((pvref (cdr q)))
	    (if (> (pvref-level pvref) (syntax-pattern-level pat))
		(error 'syntax-error (format "~s: pattern variable ~s is used in wrong level: ~s"
			       (pattern-context-name ctx) pvar
			       (pattern-context-form ctx))))
	    pvref)
	  pvar))))

;; compile a pettern or a template
;; In a pattern, replace literal symbold for identifiers; leave
;; non-literal symbols (i.e. pattern variables) as they are, but
;; records it's presence in the context. Also, when encounters
;; a repeatable subpattern, replace it with syntax-pattern node.
;; In a template, replace symbols for identifiers except pattern
;; variables.

(define compile-rule1
  (lambda (form spat ctx pattern?)
    (cond ((pair? form)
	   (let ((h '())
		 (t '()))
	     (let loop ((pp form))
	       (if (pair? pp)
		   (begin
		     (if (and (pair? (cdr pp))
			      (eq? (cadr pp) '...))
			 (begin
			   (if (and pattern?
				    (not (null? (cddr pp))))
			       (error 'syntax-error (format "bad ellipsis usage in macro definition of ~s: ~s"
					      (pattern-context-name ctx) (pattern-context-form ctx))))
			   (if (<= (pattern-context-maxlevel ctx)
				   (syntax-pattern-level spat))
			       (maxlevel++ ctx))
			   (let ((nspat (make-syntax-pattern (+ (syntax-pattern-level spat) 1) #t)))
			     (syntax-pattern-pattern-set! nspat (compile-rule1 (car pp) nspat ctx pattern?))
			     (append1 h t nspat)
			     (unless pattern?
				 (let ((vp '()))
				   (if (null? (syntax-pattern-vars nspat))
				       (error 'syntax-error (format "in definition of macro ~s: a template contains repetition of constant form ~s"
						      (pattern-context-name ctx) form)))
				   (let loop2 ((v (syntax-pattern-vars nspat)))
				     (if (>= (pvref-level (car v))
					     (syntax-pattern-level nspat))
					 (set! vp v)
					 (loop2 (cdr v))))
				   (if (null? vp)
				       (error 'syntax-error (format "in definition of macro ~s: template's ellipsis nesting is deeper then pattern's: ~s"
						      (pattern-context-name ctx) form)))))
			     (syntax-pattern-vars-set! spat (append (syntax-pattern-vars spat)
								    (syntax-pattern-vars nspat)))
			     (set! pp (cdr pp))))
			 (append1 h t (compile-rule1 (car pp) spat ctx pattern?)))
		     (loop (cdr pp)))
		   (begin
		     (unless (null? pp)
		       (append0 h t (compile-rule1 pp spat ctx pattern?)))
		     h)))))
	  ((vector? form)
	   (list->vector (compile-rule1 (vector->list form) spat ctx pattern?)))
	  ((or (symbol? form)
	       (identifier? form))
	   (if (eq? form '...)
	       (error 'syntax-error (format "bad ellipsis usage in macro definition of ~s: ~s"
			      (pattern-context-name ctx) (pattern-context-form ctx))))
	   (cond ((id-memq form (pattern-context-literals ctx))
		  => (lambda (q) q))
		 (pattern?
		  (add-pvar ctx spat form))
		 (else
		  (let ((pvref (pvar-to-pvref ctx spat form)))
		    (if (eq? pvref form)
			;; form is not a pattern variable. make it an identifier.
			(cond ((id-memq form (pattern-context-tvars ctx))
			       => (lambda (q) q))
			      (else
			       (let ((id (if (identifier? form)
					     form
					     (make-identifier form (pattern-context-env ctx)
							      (pattern-context-library ctx)))))
				 (pattern-context-tvars-set! ctx (cons id (pattern-context-tvars ctx)))
				 id)))
			(begin
			  (syntax-pattern-vars-set! spat (cons pvref (syntax-pattern-vars spat)))
			  pvref))))))
	  (else
	   form))))
			       
(define compile-rules
  (lambda (name literals rules library env)
    (define rest-context!
      (lambda (ctx)
	(pattern-context-pvars-set! ctx '())
	(pattern-context-tvars-set! ctx '())
	(pattern-context-pvcount-set! ctx 0)
	(pattern-context-maxlevel-set! ctx 0)))

    (let ((num-rules (length rules)))
      (or (>= num-rules 1)
	  (error 'syntax-error (format "malformed macro (too many rules) ~s: ~s"
			 name (cons 'syntax-rules (cons literals rules)))))
      (or (list? literals)
	  (error 'syntax-error (format "malformed macro (imporper literals) ~s: ~s"
			 name (cons 'syntax-rules (cons literals rules)))))
      (let ((ctx (make-pattern-context name
				       (preprocess-literals literals library env)
				       library env))
	    (sr (make-syntax-rules name num-rules 0)))
	(syntax-rules-rules-set! sr (make-vector num-rules))
	(let loop ((i 0)
		   (rp rules))
	  (if (= i num-rules)
	      sr ;; finish
	      (let ((pat (make-syntax-pattern 0 #f))
		    (tmpl (make-syntax-pattern 0 #f))
		    (rule (car rp)))
		(rest-context! ctx)
		(pattern-context-form-set! ctx (car rule))
		(or (pair? (pattern-context-form ctx))
		    (error 'syntax-error (format "malformed macro ~s: ~s, ~s"
				   name (cons 'syntax-rules (cons literals rules)) ctx)))
		(syntax-pattern-pattern-set! pat (compile-rule1 (cdr (pattern-context-form ctx))
								pat
								ctx
								#t))
		(pattern-context-form-set! ctx (cadr rule))
		(syntax-pattern-pattern-set! tmpl (compile-rule1 (pattern-context-form ctx)
								 tmpl
								 ctx
								 #f))
		(vector-set! (syntax-rules-rules sr)
			     i
			     (make-syntax-rule-branch (syntax-pattern-pattern pat)
						      (syntax-pattern-pattern tmpl)
						      (pattern-context-pvcount ctx)
						      (pattern-context-maxlevel ctx)))
		(if (> (pattern-context-pvcount ctx)
		       (syntax-rules-maxnumpvars sr))
		    (syntax-rules-maxnumpvars-set! sr (pattern-context-pvcount ctx)))
		(loop (+ i 1) (cdr rp)))))))))

(define compile-syntax-rules
  (lambda (name literals rules library env)
    (if (identifier? name)
	(set! name (id-name name)))
    ;; TODO check library
    (let ((sr (compile-rules name literals rules library env)))
      (make-macro name syntax-rule-transform sr))))

;;
;; matchvec
;; a sor of shallow binding tecnique is used to bind pattern
;; variable with matched patterns.
(define-simple-struct match-var '.match-var make-match-var
  branch ;; current level match
  sprout ;; current sprout
  root   ;; root of the tree
)

(define init-matchvec
  (lambda (mvec num-pvars)
    (let loop ((i 0))
      (if (= i num-pvars)
	  mvec
	  (begin
	    (vector-set! mvec i (make-match-var '() '() '()))
	    (loop (+ i 1)))))))

;; get value assosiated to the pvref. if exhausted, return undef
;; and set exhaust level in exlevel
(define get-pvref-value
  (lambda (pvref mvec indices exlevel)
    (let* ((level (pvref-level pvref))
	   (count (pvref-count pvref))
	   (tree  (match-var-root (vector-ref mvec count))))
      (let loop ((i 1))
	(if (<= i level)
	    (let loop2 ((j 0))
	      (if (< j (vector-ref indices i))
		  (if (pair? tree)
		      (begin
			(set! tree (cdr tree))
			(loop2 (+ j 1)))
		      (begin
			(vector-set! exlevel 0 i)
			(unbound)))
		  (if (pair? tree)
		      (begin
			(set! tree (car tree))
			(loop (+ i 1)))
		      (begin
			(vector-set! exlevel 0 i)
			(unbound)))))
	    tree)))))

(define has-trunc-null?
  (lambda (trunc)
    (let loop ((trunc trunc))
      (cond ((null? trunc) #f)
	    ((null? (cdr trunc)) trunc)
	    (else 
	     (loop (cdr trunc)))))))

(define grow-branch
  (lambda (rec level)
    (define sprout (cons '() '()))
    (define finish
      (lambda (trunc)
	(let loop ((trunc trunc))
	  (if (null? (cdr trunc))
	      (begin
		(match-var-sprout-set! rec sprout)
		(set-cdr! trunc (match-var-sprout rec)))
	      (loop (cdr trunc))))))
    (if (<= level 1) #f
	(begin
	  (if (null? (match-var-root rec))
	      (begin
		(match-var-root-set! rec sprout)
		(match-var-sprout-set! rec sprout)))
	  (if (== level 2) #f
	      (let ((trunc (match-var-root rec)))
		(let loop ((i 1)
			   (trunc trunc))
		  (cond ((has-trunc-null? trunc) => finish)
			((= i (- level 1)) (finish trunc))
			((null? (car trunc))
			 (set! i (+ i 1))
			 (let lp2 ((trunc trunc))
			   (unless (= i (- level 1))
			     (set-car! trunc sprout)
			     (lp (car trunc))))
			 (match-var-sprout-set! rec sprout)
			 (set-car! trunc (match-var-sprout rec)))
			(else
			 (loop (+ i 1) (car trunc)))))))))))

(define enter-subpattern
  (lambda (subpat mvec)
    (for-each (lambda (pvref)
		(let ((count (pvref-count pvref)))
		  (grow-branch (vector-ref mvec count)
			       (syntax-pattern-level subpat))))
	      (syntax-pattern-vars subpat))))

(define exit-subpattern
  (lambda (subpat mvec)
    (for-each (lambda (pvref)
		(let ((count (pvref-count pvref)))
		  (when (= (pvref-level pvref) (syntax-pattern-level subpat))
		    (let ((mvar (vector-ref mvec count)))
		      (cond ((= (syntax-pattern-level subpat) 1)
			     (match-var-root-set! mvar
						  (reverse! (match-var-branch mvar))))
			    (else
			     (set-car! (match-var-sprout mvar) (reverse! (match-var-branch mvar)))
			     (match-var-branch-set! mvar '())))))))
	      (syntax-pattern-vars subpat))))

(define match-subpattern
  (lambda (form pat env mvec)
    (enter-subpattern pat mvec)
    (let loop ((form form))
      (if (pair? form)
	  (if (match-syntax-rule (car form) (syntax-pattern-pattern pat) env mvec)
	      (loop (cdr form))
	      #f)
	  (if (null? form)
	      (begin
		(exit-subpattern pat mvec)
		#t)
	      #f)))))

(define match-insert
  (lambda (pvref matched mvec)
    (let* ((count (pvref-count pvref))
	   (mvar  (vector-ref mvec count)))
      (if (= (pvref-level pvref) 0)
	  (match-var-root-set! mvar matched)
	  (match-var-branch-set! mvar (cons matched (match-var-branch mvar)))))))
	  
(define match-identifier
  (lambda (id obj env)
    (cond ((symbol? obj)
	   (and (eq? (id-name id) obj)
		(identifier-binding-eqv? id obj env)))
	  ((identifier? obj)
	   (and (eq? (id-name id) (id-name obj))
		(eq? (id-envs id) (id-envs obj))))
	  (else #f))))

(define match-syntax-rule
  (lambda (form pattern env mvec)
    (cond ((pvref? pattern)
	   (match-insert pattern form mvec)
	   #t)
	  ((identifier? pattern)
	   (match-identifier pattern form env))
	  ((syntax-pattern? pattern)
	   (match-subpattern form pattern env mvec))
	  ((pair? pattern)
	   (let loop ((pattern pattern)
		      (form form))
	     (cond ((pair? pattern)
		    (let ((elt (car pattern)))
		      (cond ((syntax-pattern? elt)
			     (match-subpattern form elt env mvec))
			    ((not (pair? form)) #f)
			    (else
			     (if (match-syntax-rule (car form) elt env mvec)
				 (loop (cdr pattern) (cdr form))
				 #f)))))
		   (else
		    (if (null? pattern)
			(null? form)
			(match-syntax-rule form pattern env mvec))))))
	  ((vector? pattern)
	   (cond ((not (vector? form)) #f)
		 (else 
		  (let ((plen (vector-length pattern))
			(flen (vector-length form)))
		    (if (zero? plen)
			(zero? flen)
			(let ((elli (syntax-pattern? (vector-ref pattern (- plen 1)))))
			  (if (or (and (not elli)
				       (not (= plen flen)))
				  (and elli
				       (> (- plen 1) flen)))
			      #f
			      (let loop ((i 0))
				(if (< i (- plen elli))
				    (if (match-syntax-rule (vector-ref form i)
							   (vector-ref pattern i)
							   env mvec)
					(loop (+ i 1))
					#f)
				    (if elli
					(let ((h '())
					      (t '())
					      (pat (vector-ref pattern (- plen 1))))
					  (let loop2 ((i (- plen 1)))
					    (if (< i flen)
						(begin
						  (append1 h t (vector-ref form i))
						  (loop2 (+ i 1)))))
					  (match-subpattern h pat env mvec))
					#t))))))))))
	  (else
	   ;; literal
	   (equal? pattern form)))))

(define realize-template
  (lambda (branch mvec)
    (let ((indices (make-vector (+ (syntax-rule-branch-maxlevel branch) 1) 0))
	  (idlist '())
	  (exlevel (make-vector 1 0)))
      (define rec
	(lambda (template level)
	  (cond ((pair? template)
		 (let ((h '())
		       (t '()))
		   (let loop ((template template))
		     (if (pair? template)
			 (let* ((e (car template))
				(r (rec e level)))
			   (cond ((eq? r (unbound)) r)
				 ((syntax-pattern? e)
				  (append0 h t r)
				  (loop (cdr template)))
				 (else
				  (append1 h t r)
				  (loop (cdr template)))))
			 ;; end of loop
			 (if (null? template)
			     h
			     (let ((r (rec template level)))
			       (cond ((eq? r (unbound)) r)
				     ((null? h) r)
				     (else
				      (append0 h t r)
				      h))))))))
		((pvref? template)
		 (get-pvref-value template mvec indices exlevel))
		((syntax-pattern? template)
		 (let ((h '())
		       (t '()))
		   (vector-set! indices (+ level 1) 0)
		   (let loop ()
		     (let ((r (rec (syntax-pattern-pattern template) (+ level 1))))
		     (if (eq? r (unbound))
			 (if (< (vector-ref exlevel 0) (syntax-pattern-level template))
			     r
			     h)
			 (begin
			   (append1 h t r)
			   (vector-set! indices (+ level 1)
					(+ (vector-ref indices (+ level 1)) 1))
			   (loop)))))))
		((identifier? template)
		 (let ((p (assq template idlist)))
		   (if (pair? p)
		       (cdr p)
		       (let ((id (copy-identifier template)))
			 (set! idlist (cons (cons template id) idlist))
			 id))))
		((vector? template)
		 (let ((h '())
		       (t '())
		       (len (vector-length template)))
		   (let loop ((i 0))
		     (if (= i len)
			 (list->vector h)
			 (let ((e (vector-ref template i)))
			   (let ((r (rec e level)))
			     (if (eq? r (unbound))
				 r
				 (begin
				   (if (syntax-pattern? e)
				       (append0 h t r)
				       (append1 h t r))
				   (loop (+ i 1))))))))))

		(else template))))
      (rec (syntax-rule-branch-template branch) 0))))

(define syntax-rules-expand
  (lambda (form p1env sr)
    (let ((mvec (make-vector (syntax-rules-maxnumpvars sr)))
	  (env  (p1env-frames p1env)))
      #;(print (format "*** syntax-rule-transform: ~s" (unwrap-syntax form)))
      (let loop ((i 0))
	(if (= i (syntax-rules-numrules sr))
	    (error 'syntax-rules-expand (format "malformed ~s: ~s" (unwrap-syntax (car form)) (unwrap-syntax form)))
	    (let ((rule (vector-ref (syntax-rules-rules sr) i)))
	      #;(print (format "pattern ~a: ~s" i (syntax-rule-branch-pattern rule)))
	      (init-matchvec mvec (syntax-rule-branch-numpvars rule))
	      (if (match-syntax-rule (cdr form) (syntax-rule-branch-pattern rule) env mvec)
		  (begin
		    #;(print (format "success ~a" i))
		    (let ((expanded (realize-template rule mvec)))
		      #;(print (format "result: ~s~%" (unwrap-syntax expanded)))
		      #;(flush)
		      expanded))
		  (loop (+ i 1)))))))))

(define syntax-rule-transform
  (lambda (self form p1env data)
    (let ((r (syntax-rules-expand form p1env data)))
      r)))

;; for er-macro-transformer
(define macro-transform
  (lambda (self form p1env data)
    (let ((r (vm/apply data form p1env)))
      r)))

(define make-macro-transformer
  (lambda (name proc library)
    (make-macro name macro-transform proc library)))


;; macrexpand
(define %internal-macro-expand
  (lambda (expr p1env once?)
    (let loop ((expr expr))
      (cond ((null? expr) '())
	    ((not (pair? expr)) expr)
	    ;; ((xx ...) ...)
	    ((pair? (car expr))
	     (cons (loop (car expr))
		   (loop (cdr expr))))
	    (else
	     (let ((g #f)
		   (mac #f)
		   (sym (car expr)))
	       (cond ((identifier? sym)
		      (set! g (find-binding (id-library sym)
					    (id-name sym))))
		     ((symbol? sym)
		      (set! g (find-binding (vm-current-library)
					    sym))))
	       (if (macro? g)
		   (set! mac g))
	       (if mac
		   ;; expand and continue
		   (if once?
		       (call-macro-expander mac expr p1env)
		       (loop (call-macro-expander mac expr p1env)))
		   ;; symbol
		   (cons (car expr) (loop (cdr expr))))))))))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
