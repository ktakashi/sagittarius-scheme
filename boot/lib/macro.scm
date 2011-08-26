;; -*- mode: scheme; coding: utf-8 -*-

(define-constant LEXICAL 0)		; the save as compiler.scm
(define-constant PATTERN 2)		; not LEXICAL nor SYNTAX

;; also the same as compiler.scm
(define lvar?
  (lambda (obj)
    (and (vector? obj) (eq? (vector-ref obj 0) 'lvar))))

;; mac-env must be p1env
(define (lookup-lexical-name id mac-env)
  ;; lookup env and bindings
  ;; first env
  ;; except '_ and '...
  (if (or (eq? id '_) (eq? id '...))
      id
      (let ((lvar (p1env-lookup mac-env id LEXICAL)))
	(cond ((and (identifier? lvar)		; might be global
		    ;; check current library's bindings
		    ;; TODO correct? 
		    (find-binding (id-library lvar) (id-name lvar) #f))
	       => (lambda (gloc) lvar))
	      ((lvar? lvar) id)
	      (else id)))))

(define ellipsis?
  (lambda (expr)
    (and (symbol? expr)
	 (eq? expr '...))))

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

(define check-pattern
  (lambda (pat lites)

    (define check-duplicate-variable
      (lambda (pat lites)
        (let loop ((lst pat) (pool '()))
          (cond ((pair? lst)
                 (loop (cdr lst)
                       (loop (car lst) pool)))
                ((eq? lst '...) pool)
                ((eq? lst '_) pool)
                ((variable?  lst)
                 (if (memq lst lites)
                     pool
                     (if (memq lst pool)
                         (syntax-violation "syntax pattern" "duplicate pattern variables" pat lst)
                         (cons lst pool))))
                ((vector? lst)
                 (loop (vector->list lst) pool))
                (else pool)))))

    (define check-misplaced-ellipsis
      (lambda (pat lites)
        (let loop ((lst pat))
          (cond ((eq? lst '...)
                 (syntax-violation "syntax pattern" "improper use of ellipsis" pat))
                ((ellipsis-pair? lst)
                 (and (symbol? (car lst))
                      (memq (car lst) lites)
                      (syntax-violation "syntax pattern" "ellipsis following literal" pat lst))
                 (let loop2 ((lst (cddr lst)))
                   (and (pair? lst)
                        (if (ellipsis? (car lst))
			    (assertion-violation "syntax pattern" "ambiguous use of ellipsis" pat)
                            (loop2 (cdr lst))))))
                ((pair? lst)
                 (or (loop (car lst)) (loop (cdr lst))))
                ((vector? lst)
                 (loop (vector->list lst)))
                (else #f)))))

    (check-misplaced-ellipsis pat lites)
    (check-duplicate-variable pat lites)))

(define (extend-env newframe env)
  (acons PATTERN newframe env))

;; for global call.
(define .match-syntax-case (make-identifier 'match-syntax-case '() '(core syntax-case)))

(define collect-unique-ids ; exclude '...
  (lambda (expr)
    (let loop ((lst expr) (ans '()))
      (cond ((pair? lst)
             (loop (cdr lst)
                   (loop (car lst) ans)))
            ((eq? lst '...) ans)
            ((symbol? lst)
             (if (memq lst ans) ans (cons lst ans)))
            ((vector? lst)
             (loop (vector->list lst) ans))
            (else ans)))))

(define collect-vars-ranks
  (lambda (pat lites depth ranks)
    (cond ((eq? pat '_) ranks)
          ((variable? pat)
           (if (memq pat lites)
               ranks
               (acons pat depth ranks)))
          ((ellipsis-pair? pat)
           (collect-vars-ranks (cddr pat) lites depth
                               (if (symbol? (car pat))
                                   (acons (car pat) (+ depth 1) ranks)
                                   (collect-vars-ranks (car pat) lites (+ depth 1) ranks))))
          ((pair? pat)
           (collect-vars-ranks (cdr pat) lites depth
                               (collect-vars-ranks (car pat) lites depth ranks)))
          ((vector? pat)
           (collect-vars-ranks (vector->list pat) lites depth ranks))
          (else ranks))))

;; syntax-case compiler
(define (compile-syntax-case exp-name expr literals clauses library env mac-env)
  (define (rewrite form aliases)
    (let loop ((lst form))
      (cond ((pair? lst) (cons (loop (car lst)) (loop (cdr lst))))
	    ((and (symbol? lst) (assq lst aliases)) => cdr)
	    ((vector? lst) (list->vector (map loop (vector->list lst))))
	    (else lst))))

  ;; literal must be unwrapped
  (let ((lites (unwrap-syntax literals)))
    (or (and (list? lites) (for-all symbol? lites))
	(syntax-violation 'syntax-case "invalid literals" expr lites))
    (or (unique-id-list? lites)
	(syntax-violation 'syntax-case "duplicate literals" expr lites))
    (and (memq '_ lites)
	 (syntax-violation 'syntax-case "_ in literals" expr lites))
    (and (memq '... lites)
	 (syntax-violation 'syntax-case "... in literals" expr lites))

    (let* ((renames (map (lambda (id) (cons id (lookup-lexical-name id mac-env))) lites))
	   (lites   (rewrite lites renames)))
      (define (parse-pattern lst)
	(let ((pattern (rewrite (unwrap-syntax lst) renames)))
	  (check-pattern pattern lites)
	  (values pattern
		  (extend-env (collect-vars-ranks pattern lites 0 '())
			      env))))
      (values .match-syntax-case
	      lites
	      (p1env-lookup mac-env '.vars LEXICAL)
	      (map (lambda (clause)
		     (smatch clause
		       ((p expr)
			(receive (pattern env) (parse-pattern p)
			  (cons `(list ',pattern
				       #f
				       (lambda (.vars) ,expr))
				env)))
		       ((p fender expr)
			(receive (pattern env) (parse-pattern p)
			  (cons `(list ',pattern
				       (lambda (.vars) ,fender)
				       (lambda (.vars) ,expr))
				 env)))))
		   clauses))))
  )

(cond-expand
 (gauche
  (define-macro (apply-ex proc . rest)
    `(apply-proc ,proc ,@rest)))
 (sagittarius
  (define-syntax apply-ex
    (er-macro-transformer
     (lambda (f r c)
       `(apply ,(cadr f) ,@(cddr f)))))))

(define count-pair
  (lambda (lst)
    (let loop ((lst lst) (n 0))
      (if (pair? lst) (loop (cdr lst) (+ n 1)) n))))

(define match-ellipsis?
  (lambda (expr pat lites)
    (or (null? expr)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites)
             (match-ellipsis? (cdr expr) pat lites)))))

(define match-ellipsis-n?
  (lambda (expr pat n lites)
    (or (= n 0)
        (and (pair? expr)
             (match-pattern? (car expr) (car pat) lites)
             (match-ellipsis-n? (cdr expr) pat (- n 1) lites)))))

(define match-pattern?
  (lambda (expr pat lites)
    (define (compare a b)
      (or (eq? a b)
	  (eq? (identifier->symbol a)
	       (identifier->symbol b))))
    (cond ((eq? pat '_) #t)
          ((symbol? pat)
           (cond ((memq pat lites)
                  (and (variable? expr)
                       (compare pat expr)))
                 (else #t)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (or (symbol? (car pat))
                   (match-ellipsis? expr pat lites))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (if (= n 0)
                     (match-pattern? expr (cddr pat) lites)
                     (and (> n 0)
                          (match-ellipsis-n? expr pat n lites)
                          (match-pattern? (list-tail expr n) (cddr pat) lites))))))
          ((pair? pat)
           (and (pair? expr)
                (match-pattern? (car expr) (car pat) lites)
                (match-pattern? (cdr expr) (cdr pat) lites)))
          ((vector? pat)
           (and (vector? expr)
                (match-pattern? (vector->list expr) (vector->list pat) lites)))
          (else (equal? pat expr)))))

(define union-vars
  (lambda (vars evars)
    (if (null? evars)
        vars
        (union-vars (bind-var! (caar evars) (reverse (cdar evars)) vars)
                    (cdr evars)))))

(define bind-var!
  (lambda (pat expr vars)
    (cond ((eq? pat '_) vars)
          (else
           (let ((slot (assq pat vars)))
             (if slot
                 (begin (set-cdr! slot (cons expr (cdr slot))) vars)
                 (acons pat (list expr) vars)))))))

(define bind-null-ellipsis
  (lambda (pat lites vars)
    (let loop ((lst (collect-unique-ids (car pat))) (vars vars))
      (if (null? lst)
          vars
          (loop (cdr lst)
                (if (memq (car lst) lites)
                    vars
                    (bind-var! (car lst) '() vars)))))))

(define bind-ellipsis
  (lambda (expr pat lites vars evars)
    (if (null? expr)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis (cdr expr) pat lites vars
                       (bind-pattern (car expr) (car pat) lites evars)))))

(define bind-ellipsis-n
  (lambda (expr pat lites n vars evars)
    (if (= n 0)
        (if (null? evars)
            (bind-null-ellipsis pat lites vars)
            (union-vars vars evars))
        (bind-ellipsis-n (cdr expr) pat lites (- n 1) vars
                         (bind-pattern (car expr) (car pat) lites evars)))))

(define bind-pattern
  (lambda (expr pat lites vars)
    (cond ((symbol? pat)
           (if (memq pat lites)
               vars
               (bind-var! pat expr vars)))
          ((ellipsis-pair? pat)
           (if (and (null? (cddr pat)) (list? expr))
               (if (symbol? (car pat))
                   (bind-var! (car pat) expr vars)
                   (bind-ellipsis expr pat lites vars '()))
               (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
                 (bind-pattern (list-tail expr n) (cddr pat) lites
                               (if (and (= n 0) (symbol? (car pat)))
                                   (bind-var! (car pat) '() vars)
                                   (bind-ellipsis-n expr pat lites n vars '()))))))
          ((pair? pat)
           (bind-pattern (cdr expr) (cdr pat) lites
                         (bind-pattern (car expr) (car pat) lites vars)))
          ((vector? pat)
           (bind-pattern (vector->list expr) (vector->list pat) lites vars))
          (else vars))))


(define (match-syntax-case patvars literals expr . lst)
  (define (match form pat)
    (and (match-pattern? form pat literals)
	 (bind-pattern form pat literals '())))
  (let ((form expr))
    (let loop ((lst lst))
      (if (null? lst)
	  (syntax-violation (and (pair? form) (car form)) "invalid syntax" form)
	  (let ((clause (car lst)))
	    (let ((pat (car clause))
		  (fender (cadr clause))
		  (expr (caddr clause)))
	      (let ((vars (match form pat)))
		(if (and vars
			 (or (not fender)
			     (apply-ex fender (list (append vars patvars)))))
		    (apply-ex expr (list (append vars patvars)))
		    (loop (cdr lst))))))))))

;; compile (syntax ...)
(define .expand-syntax (make-identifier 'expand-syntax '() '(core syntax-case)))
(define collect-rename-ids
  (lambda (template ranks)
    (let ((ids (collect-unique-ids template)))
      (let loop ((lst ids))
        (if (null? lst)
            lst
            (if (assq (car lst) ranks)
                (loop (cdr lst))
                (cons (car lst) (loop (cdr lst)))))))))

(define parse-ellipsis-splicing
  (lambda (form)
    (let loop ((len 2) (tail (cdddr form)))
      (cond ((and (pair? tail) (eq? (car tail) '...))
             (loop (+ len 1) (cdr tail)))
            (else
             (values (list-head form len) tail len))))))

(define (check-template tmpl ranks)
  (define (control-patvar-exists? tmpl depth)
    (let loop ((lst tmpl) (depth depth))
      (cond ((variable? lst)
	     (>= (rank-of lst ranks) depth))
	    ((ellipsis-quote? lst)
	     (any1 (lambda (id) (>= (rank-of id ranks) depth)) (collect-unique-ids lst)))
	    ((ellipsis-splicing-pair? lst)
	     (receive (body tail len) (parse-ellipsis-splicing lst)
	       (or (loop body (+ depth 1))
		   (and (loop body 1)
			(loop tail depth)))))
	    ((ellipsis-pair? lst)
	     (or (loop (car lst) (+ depth 1))
		 (and (loop (car lst) 1)
		      (loop (cddr lst) depth))))
	    ((pair? lst)
	     (or (loop (car lst) depth)
		 (loop (cdr lst) depth)))
	    ((vector? lst)
	     (loop (vector->list lst) depth))
	    (else #f))))

  (define (check-escaped lst depth)
    (let loop ((lst lst))
      (cond ((variable? lst)
	     (and (< 0 (rank-of lst ranks) depth)
		  (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl lst)))
	    ((pair? lst)
	     (loop (car lst))
	     (loop (cdr lst)))
	    ((vector? lst)
	     (loop (vector->list lst))))))

  (if (and (= (safe-length tmpl) 2) (eq? (car tmpl) '...))
      (check-escaped (cadr tmpl) 0)
      (let loop ((lst tmpl) (depth 0))
	(cond ((variable? lst)
	       (and (eq? lst '...)
		    (syntax-violation "syntax template" "misplaced ellipsis" tmpl))
	       (and (> (rank-of lst ranks) depth)
		    (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl lst)))
	      ((ellipsis-quote? lst)
	       (check-escaped (cadr lst) depth))
	      ((ellipsis-splicing-pair? lst)
	       (receive (body tail len) (parse-ellipsis-splicing lst)
		 (and (= depth 0)
		      (or (control-patvar-exists? (car lst) len)
			  (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl lst)))
		 (loop body (+ depth 1))
		 (loop tail depth)))
	      ((ellipsis-pair? lst)
	       (cond ((variable? (car lst))
		      (let ((rank (rank-of (car lst) ranks)))
			(cond ((< rank 0)
			       (syntax-violation "syntax template" "misplace ellipsis following literal" tmpl (car lst)))
			      ((> rank (+ depth 1))
			       (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl (car lst)))
			      (else
			       (loop (cddr lst) depth)))))
		     ((pair? (car lst))
		      (and (= depth 0)
			   (or (control-patvar-exists? (car lst) (+ depth 1))
			       (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl (car lst))))
		      (loop (car lst) (+ depth 1))
		      (loop (cddr lst) depth))
		     ((null? (car lst))
		      (syntax-violation "syntax template" "misplaced ellipsis following empty list" tmpl))
		     (else
		      (syntax-violation "syntax template" "misplaced ellipsis following literal" tmpl (car lst)))))
	      ((pair? lst)
	       (loop (car lst) depth)
	       (loop (cdr lst) depth))
	      ((vector? lst)
	       (loop (vector->list lst) depth))))))

(define (compile-syntax exp-name tmpl env mac-env)
  (let* ((template (unwrap-syntax tmpl))
	 (ids (collect-unique-ids template))
	 (ranks (filter values
			(map (lambda (id)
			       (let ((p (p1env-lookup mac-env id PATTERN)))
				 (and (number? p)
				      (cons id p))))
			     ids))))
    ;; later
    (check-template template ranks)
    (let ((patvar (let ((v (p1env-lookup mac-env '.vars LEXICAL)))
		    (if (identifier? v)
			'()
			'.vars))))
      (if (symbol? template)
	  (let ((lex-id (lookup-lexical-name tmpl mac-env)))
	    (if (eq? template lex-id)
		(if (null? ranks)
		    `(,.expand-syntax ,patvar ',template '() ',template '())
		    `(,.expand-syntax ,patvar ',template ',(list (cons template 0)) ',template '()))
		(if (null? ranks)
		    `(,.expand-syntax ,patvar ',template '() ',lex-id '())
		    `(,.expand-syntax ,patvar ',template ',(list (cons template 0)) ',lex-id '()))))
	  (let ((lex-check-list (filter values
					(map (lambda (id)
					       (let ((lexname (lookup-lexical-name id mac-env)))
						 ;; TODO correct?
						 (and (identifier? lexname)
						      (not (null? (id-envs lexname)))
						      (cond ((eq? id lexname) #f)
							    (else (cons id lexname))))))
					     (collect-rename-ids template ranks)))))
	    `(,.expand-syntax ,patvar ',template ',ranks #f ',lex-check-list))))))

(define (expand-syntax vars template ranks identifier-lexname lexname-check-list)
  (define seen (make-eq-hashtable))
  (define (emit datum)
    (cond ((identifier? datum) datum)
	  ;;((symbol? datum) (wrap-syntax datum (current-usage-env) seen #t))
	  (else datum)))

  (define contain-wrapped-syntax-object?
    (lambda (lst)
      (let loop ((lst lst))
	(cond ((pair? lst)
	       (or (null? (car lst)) (loop (car lst)) (loop (cdr lst))))
	      ((vector? lst)
	       (let loop2 ((i (- (vector-length lst) 1)))
		 (and (>= i 0)
		      (or (loop (vector-ref lst i))
			  (loop2 (- i 1))))))
	      (else
	       (identifier? lst))))))

  (define (wrap-renamed-id lst renames)
    (let loop ((lst lst))
      (cond ((pair? lst)
	     (let ((a (loop (car lst))) (d (loop (cdr lst))))
	       (cond ((and (eq? (car lst) a) (eq? (cdr lst) d)) lst)
		     (else (cons a d)))))
	    ((identifier? lst) lst)
	    ((vector? lst)
	     (list->vector (loop (vector->list lst))))
	    ((symbol? lst)
	     (cond ((assq lst renames) => cdr)
		   ;; If transcribed expression contains pattern variable,
		   ;; we need to replace it.
		   ;; still we need this. Sucks!!
		   ((assq lst vars) => cadr)
		   (else 
		    (wrap-syntax lst (current-usage-env) seen))))
	    (else lst))))

  (define (partial-wrap-syntax-object lst renames)
    (let loop ((lst lst))
      (cond ((contain-wrapped-syntax-object? lst)
	     (cond ((pair? lst)
		    (let ((a (loop (car lst))) (d (loop (cdr lst))))
		      (cond ((and (eq? (car lst) a) (eq? (cdr lst) d)) lst)
			    (else (cons a d)))))
		   (else lst)))
	    ((null? lst) '())
	    ((variable? lst)
	     (cond ((assq lst renames) => cdr)
		   ;; If transcribed expression contains pattern variable,
		   ;; we need to replace it.
		   ;; still we need this. Sucks!!
		   ((assq lst vars) => cadr)
		   (else (wrap-syntax lst (current-usage-env) seen #t))))
	    ((vector? lst)
	     (wrap-renamed-id lst renames))
	    ((pair? lst)
	     (wrap-renamed-id lst renames))
	    ((null? lst) '())
	    (else lst))))
  (if (null? template)
      '()
      (let* ((use-env (current-usage-env))
	     (mac-env (current-macro-env))
	     (aliases (map (lambda (id)
			     (cons id (rename-id id)))
			   (collect-rename-ids template ranks)))
	     (renames (map (lambda (lst)
			     (cons (cdr lst) (p1env-lookup use-env (car lst) LEXICAL)))
			   aliases)))
	(if (null? (vector-ref use-env 1)) ;; no frames
	    (let ((form (transcribe-template template ranks vars aliases #f)))
	      (if (renamed-id? form)
		  form
		  (wrap-renamed-id form renames)))
	    (let ((form (transcribe-template template ranks vars aliases emit)))
	      (cond ((null? form) '())
		    ((identifier? form) form)
		    ((symbol? form) 
		     (wrap-syntax form use-env seen))
		    (else
		     (partial-wrap-syntax-object form renames))))))))


(define (rank-of name ranks)
  (let ((slot (assq name ranks)))
    (if slot (cdr slot) -1)))

(define (contain-rank-moved-var? tmpl ranks vars)
  (define (traverse-escaped lst depth)
    (let loop ((lst lst) (depth depth))
      (cond ((symbol? lst)
	     (< 0 (rank-of lst ranks) depth))
	    ((pair? lst)
	     (or (loop (car lst) depth)
		 (loop (cdr lst) depth)))
	    ((vector? lst)
	     (loop (vector->list lst) depth))
	    (else #f))))

  (let loop ((lst tmpl) (depth 0))
    (cond ((symbol? lst)
	   (< 0 (rank-of lst ranks) depth))
	  ((ellipsis-quote? lst)
	   (traverse-escaped (cadr lst) depth))
	  ((ellipsis-splicing-pair? lst)
	   (receive (body tail len) (parse-ellipsis-splicing lst)
	     (or (loop body (+ depth 1))
		 (loop tail depth))))
	  ((ellipsis-pair? lst)
	   (or (loop (car lst) (+ depth 1))
	       (loop (cddr lst) depth)))
	  ((pair? lst)
	   (or (loop (car lst) depth)
	       (loop (cdr lst) depth)))
	  ((vector? lst)
	   (loop (vector->list lst) depth))
	  (else #f))))


(define (adapt-to-rank-moved-vars form ranks vars)

  (define (rewrite-template-ranks-vars tmpl ranks vars)
    (let ((moved-ranks (make-eq-hashtable)) (moved-vars (make-eq-hashtable)))

      (define (make-infinite-list e)
	(let ((lst (list e)))
	  (begin (set-cdr! lst lst) lst)))

      (define (revealed name depth)
	(if (< 0 (rank-of name ranks) depth)
	    (let ((renamed (string->symbol (format "~a:~a" (gensym) name))))
	      (or (hashtable-ref moved-ranks renamed #f)
		  (let loop ((i (- depth (rank-of name ranks))) (var (subform-of name vars)))
		    (cond ((> i 0)
			   (loop (- i 1) (list (make-infinite-list (car var)))))
			  (else
			   (hashtable-set! moved-ranks renamed depth)
			   (hashtable-set! moved-vars renamed var)))))
	      renamed)
	    name))

      (define (traverse-escaped lst depth)
	(let loop ((lst lst) (depth depth))
	  (cond ((symbol? lst)
		 (revealed lst depth))
		((pair? lst)
		 (cons (loop (car lst) depth)
		       (loop (cdr lst) depth)))
		((vector? lst)
		 (list->vector (loop (vector->list lst) depth)))
		(else lst))))

      (let ((rewrited
	     (let loop ((lst tmpl) (depth 0))
	       (cond ((symbol? lst)
		      (revealed lst depth))
		     ((ellipsis-quote? lst)
		      (cons (car lst)
			    (traverse-escaped (cdr lst) depth)))
		     ((ellipsis-splicing-pair? lst)
		      (receive (body tail len) (parse-ellipsis-splicing lst)
			(append (loop body (+ depth 1)) (cons '... (loop tail depth)))))
		     ((ellipsis-pair? lst)
		      (cons (loop (car lst) (+ depth 1))
			    (cons '... (loop (cddr lst) depth))))
		     ((pair? lst)
		      (cons (loop (car lst) depth)
			    (loop (cdr lst) depth)))
		     ((vector? lst)
		      (list->vector (loop (vector->list lst) depth)))
		     (else lst)))))
	(values rewrited
		(append ranks (hashtable->alist moved-ranks))
		(append vars (hashtable->alist moved-vars))))))

  (if (contain-rank-moved-var? form ranks vars)
      (rewrite-template-ranks-vars form ranks vars)
      (values form ranks vars)))

(define collect-ellipsis-vars
  (lambda (tmpl ranks depth vars)
    (let ((ids (collect-unique-ids tmpl)))
      (filter values
              (map (lambda (slot)
                     (and (memq (car slot) ids)
                          (let ((rank (cdr (assq (car slot) ranks))))
                            (cond ((< rank depth) slot)
                                  ((null? (cdr slot)) slot)
                                  (else (cons (car slot) (cadr slot)))))))
                   vars)))))

(define consume-ellipsis-vars
  (lambda (ranks depth vars)
    (let ((exhausted #f) (consumed #f))
      ;; consumed exhausted return
      ;; #t       #t    --> #f        error, different size of matched subform
      ;; #t       #f    --> remains   more variable to reveal
      ;; #f       #t    --> #t        all variable revealed
      ;; #f       #f    --> ()        no variable revealed
      (let ((remains
             (let loop ((lst vars))
               (cond ((null? lst) lst)
                     ((< (rank-of (caar lst) ranks) depth)
                      (cons (car lst) (loop (cdr lst))))
                     ((null? (cdar lst))
                      (loop (cdr lst)))
                     ((null? (cddar lst))
                      (set! exhausted #t)
                      (loop (cdr lst)))
                     (else
                      (or (circular-list? (cdar lst)) (set! consumed #t))
                      (acons (caar lst) (cddar lst) (loop (cdr lst))))))))
        (if consumed
            (and (not exhausted) remains)
            (or exhausted '()))))))

(define (transcribe-template in-form in-ranks in-vars aliases emit)
  (define (remove-duplicates alist)
    (or (let loop ((lst alist))
	  (cond ((null? lst) alist)
		((assq (caar lst) (cdr lst)) #f)
		(else (loop (cdr lst)))))
	(let loop ((lst alist) (acc '()))
	  (cond ((null? lst) acc)
		((assq (caar lst) acc) (loop (cdr lst) acc))
		(else (loop (cdr lst) (cons (car lst) acc)))))))
  (receive (tmpl ranks vars)
      (adapt-to-rank-moved-vars in-form in-ranks (remove-duplicates in-vars))
    (define (expand-var tmpl vars)
      (cond ((assq tmpl vars)
	     => (lambda (slot)
		  (cond ((null? (cdr slot)) '())
			(emit (emit (cadr slot)))
			(else (cadr slot)))))
	    (else
	     (assertion-violation "syntax template" "subforms have different size of matched input"
				  `(template: ,in-form) `(subforms: ,@in-vars)))))
    (define (expand-ellipsis-var tmpl vars)
      (cond ((assq tmpl vars)
	     => (lambda (slot)
		  (cond ((null? (cdr slot)) '())
			(emit (map emit (cadr slot)))
			(else (cadr slot)))))
	    (else
	     (assertion-violation "syntax template" "subforms have different size of matched input"
				  `(template: ,in-form) `(subforms: ,@in-vars)))))

    (define (expand-ellipsis-template tmpl depth vars)
      (let loop ((expr '()) (remains (collect-ellipsis-vars tmpl ranks depth vars)))
	(cond ((pair? remains)
	       (loop (cons (expand-template tmpl depth remains) expr)
		     (consume-ellipsis-vars ranks depth remains)))
	      ((null? remains) '())
	      ((eq? remains #t) (reverse expr))
	      (else
	       (assertion-violation "syntax template" "subforms have different size of matched input"
                                        `(template: ,in-form) `(subforms: ,@in-vars))))))

    (define (expand-escaped-template tmpl depth vars)
      (cond ((symbol? tmpl)
	     (if (< (rank-of tmpl ranks) 0)
		 (cond ((assq tmpl aliases) => cdr) (else tmpl))
		 (expand-var tmpl vars)))
	    ((pair? tmpl)
	     (if (and emit (null? (car tmpl)))
		 (cons '()
		       (expand-escaped-template (cdr tmpl) depth vars))
		 (cons (expand-escaped-template (car tmpl) depth vars)
		       (expand-escaped-template (cdr tmpl) depth vars))))
	    ((vector? tmpl)
	     (list->vector (expand-escaped-template (vector->list tmpl) depth vars)))
	    (else tmpl)))

    (define (expand-template tmpl depth vars)
      (cond ((symbol? tmpl)
	     (if (< (rank-of tmpl ranks) 0)
		 (cond ((assq tmpl aliases) => cdr) (else tmpl))
		 (expand-var tmpl vars)))
	    ((ellipsis-quote? tmpl)
	     (expand-escaped-template (cadr tmpl) depth vars))
	    ((ellipsis-splicing-pair? tmpl)
	     (receive (body tail len) (parse-ellipsis-splicing tmpl)
	       (append (apply append (expand-ellipsis-template body (+ depth 1) vars))
		       (expand-template tail depth vars))))
	    ((ellipsis-pair? tmpl)
	     (cond ((symbol? (car tmpl))
		    (let ((rank (rank-of (car tmpl) ranks)))
		      (cond ((= rank (+ depth 1))
			     (append (expand-ellipsis-var (car tmpl) vars)
				     (expand-template (cddr tmpl) depth vars))))))
		   ((pair? (car tmpl))
		    (append (expand-ellipsis-template (car tmpl) (+ depth 1) vars)
			    (expand-template (cddr tmpl) depth vars)))))
	    ((pair? tmpl)
	     (if (and emit (null? (car tmpl)))
		 (cons '()
		       (expand-template (cdr tmpl) depth vars))
		 (cons (expand-template (car tmpl) depth vars)
		       (expand-template (cdr tmpl) depth vars))))
	    ((vector? tmpl)
	     (list->vector (expand-template (vector->list tmpl) depth vars)))
	    (else tmpl)))

    (if (and (= (safe-length tmpl) 2) (eq? (car tmpl) '...))
	(expand-escaped-template (cadr tmpl) 0 vars)
	(expand-template tmpl 0 vars))))

;; datum->syntax
(define (datum->syntax template-id datum)
  (or (identifier? template-id)
      (assertion-violation 'datum->syntax (format "expected identifier, but got ~s" template-id)))
  (let ((transformers-env (id-transformers-env template-id)))
    (if transformers-env
	(wrap-syntax datum transformers-env)
	(wrap-syntax datum (current-usage-env)))))

;; syntax->datum
(define (syntax->datum syntax)
  (unwrap-syntax syntax))

(define generate-temporaries
  (lambda (obj)
    (or (list? obj)
        (assertion-violation 'generate-temporaries
			     (format "expected list, but got ~s" obj)))
    (map (lambda (n) (wrap-syntax (gensym) (current-usage-env))) obj)))

(define (make-variable-transformer proc)
  (make-macro 'variable-transformer
	      (lambda (m expr p1env data)
		(proc (wrap-syntax expr p1env)))
	      '()
	      (current-usage-env)))

