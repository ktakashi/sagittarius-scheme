;; -*- scheme -*-
;; syntax-case compiler
(define-constant PATTERN 2) ;; the same as compiler.scm

(define extend-env
  (lambda (new old)
    ;; we need to keep env the same pointer as old.
    (append! old (list (cons PATTERN new)))))

(define unique-id-list?
  (lambda (lst)
    (and (list? lst)
         (not (let loop ((lst lst))
                (and (pair? lst)
                     (or (not (variable? (car lst)))
                         (id-memq (car lst) (cdr lst))
                         (loop (cdr lst)))))))))

(define ellipsis?
  (lambda (expr)
    (or (and (symbol? expr)
	     (eq? expr '...))
	(and (identifier? expr)
	     (eq? (id-name expr) '...)))))

(define (bar? expr)
      (or (and (symbol? expr)
	       (eq? expr '_))
	  (and (identifier? expr)
	       (eq? (id-name expr) '_))))

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
                ((ellipsis? lst) pool)
                ((bar? lst) pool)
                ((symbol? lst)
                 (if (memq lst lites)
                     pool
                     (if (memq lst pool)
                         (assertion-violation "syntax pattern" "duplicate pattern variables" pat lst)
                         (cons lst pool))))
                ((vector? lst)
                 (loop (vector->list lst) pool))
                (else pool)))))

    (define check-misplaced-ellipsis
      (lambda (pat lites)
        (let loop ((lst pat))
          (cond ((ellipsis? lst)
                 (assertion-violation "syntax pattern" "improper use of ellipsis" pat))
                ((ellipsis-pair? lst)
                 (and (symbol? (car lst))
                      (memq (car lst) lites)
                      (assertion-violation "syntax pattern" "ellipsis following literal" pat lst))
                 (let loop ((lst (cddr lst)))
                   (and (pair? lst)
                        (if (ellipsis? (car lst))
                            (assertion-violation "syntax pattern" "ambiguous use of ellipsis" pat)
                            (loop (cdr lst))))))
                ((pair? lst)
                 (or (loop (car lst)) (loop (cdr lst))))
                ((vector? lst)
                 (loop (vector->list lst)))
                (else #f)))))

    (check-misplaced-ellipsis pat lites)
    (check-duplicate-variable pat lites)))


(define collect-vars-ranks
  (lambda (pat lites depth ranks)    
    (cond ((bar? pat) ranks)
          ((variable? pat)
           (if (id-memq pat lites)
               ranks
               (acons pat depth ranks)))
          ((ellipsis-pair? pat)
           (collect-vars-ranks (cddr pat) lites depth
                               (if (variable? (car pat))
                                   (acons (car pat) (+ depth 1) ranks)
                                   (collect-vars-ranks (car pat) lites (+ depth 1) ranks))))
          ((pair? pat)
           (collect-vars-ranks (cdr pat) lites depth
                               (collect-vars-ranks (car pat) lites depth ranks)))
          ((vector? pat)
           (collect-vars-ranks (vector->list pat) lites depth ranks))
          (else ranks))))

(define compile-syntax-case
  (lambda (exp-name expr literals rules library env p1env)

    (define parse-pattern
      (lambda (pattern)
	(let ((ranks   (collect-vars-ranks pattern (unwrap-syntax literals) 0 '())))
	  (check-pattern pattern literals)
	  (values pattern ranks
		  (extend-env (map (lambda (a)
				     (cons (car a)
					   (cdr a)))
				   ranks)
			      env)))))

    (or (and (list? literals)
	     (for-all variable? literals))
	(assertion-violation 'syntax-case "invalid literals" expr literals))
    (or (unique-id-list? literals)
	(assertion-violation 'syntax-case "duplicate literals" expr literals))
    (and (memq '_ (unwrap-syntax literals))
	 (assertion-violation 'syntax-case "_ in literals" expr literals))
    (and (memq '... (unwrap-syntax literals))
	 (assertion-violation 'syntax-case "... in literals" expr literals))

    (let ((processes (map (lambda (clause)
			    (cond ((= (length clause) 2) ; without fender
				   (receive (pattern ranks env) (parse-pattern (car clause))
				     ;; i'm not sure if it's ok, but for now we don't have any way to
				     ;; remember the ranks
				     `(list ',pattern
					    ',ranks
					    #f
					    (lambda (,@(map car ranks)
						     . vars)
					      ,(cadr clause)))))
				  ((= (length clause) 3) ; with fender
				   (receive (pattern ranks env) (parse-pattern (car clause))
				     `(list ',pattern
					    ',ranks
					    (lambda (,@(map car ranks)
						      . vars)
					      ,(cadr clause))
					    (lambda (,@(map car ranks)
						     . vars) ,(caddr clause)))))
				  (else
				   (assertion-violation 'syntax-case
							"a clause must be either (<pattern> <expression) or (<pattern> <fender> <expression)"
							clause))))
			  rules)))
      `(match-syntax-case ',literals
			  ;;(wrap-syntax ,expr ,p1env)
			  ,expr
			  ,@processes))))

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
	  (and (symbol? a)
	       (identifier? b)
	       (eq? a (id-name b)))
	  (and (symbol? b)
	       (identifier? a)
	       (eq? b (id-name a)))))
    (cond ((bar? pat) #t)
          ((symbol? pat)
           (cond ((id-memq pat lites)
                  (and (variable? expr)
		       ;; if we can use compare from er-macro-transformer...
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

(define collect-unique-ids ; exclude '...
  (lambda (expr)
    (let loop ((lst expr) (ans '()))
      (cond ((pair? lst)
             (loop (cdr lst)
                   (loop (car lst) ans)))
            ((ellipsis? expr) ans)
            ((variable? lst)
             (if (id-memq lst ans) ans (cons lst ans)))
            ((vector? lst)
             (loop (vector->list lst) ans))
            (else ans)))))

(define union-vars
  (lambda (vars evars)
    (if (null? evars)
        vars
        (union-vars (bind-var! (caar evars) (reverse (cdar evars)) vars)
                    (cdr evars)))))

(define bind-var!
  (lambda (pat expr vars)
    (cond ((bar? pat) vars)
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
    (cond ((variable? pat)
           (if (id-memq pat lites)
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


;; this needs to be toplevel but how?
(define match-syntax-case
  (lambda (literals expr . process)
    ;; direct expression from define-syntax has p1env, so we need to unwrap it,
    ;; however if expression was defined by user or something, it doesn't have
    ;; it. so we need to check if it has ir or not.
    ;; TODO this might be too naive
    (let ((form (if (= (length expr) -1)
		    (wrap-syntax (car expr) (cdr expr))
		    expr)))
      (define match
	(lambda (form pat)
	  (and (match-pattern? form pat (unwrap-syntax literals))
	       (bind-pattern form pat (unwrap-syntax literals) '()))))
      ;; for now I don't consider renaming
      (let loop ((lst process))
	(if (null? lst)
	    (assertion-violation (and (pair? form)
				      (car form))
				 "invalid syntax" form)
	    (let ((clause (car lst)))
	      (let ((pat (car clause))
		    (patvars (cadr clause))
		    (fender (caddr clause))
		    (expr (cadddr clause)))
		(let ((vars (match form pat)))
		  (if (and vars
			   (or (not fender)
			       (apply fender (append (map cadr vars) vars))))
		      (apply expr (append (map cadr vars) vars))
		      (loop (cdr lst)))))))))))

;; syntax
;; first compile syntax
;; when a form is given, we bind the given variables.
(define compile-syntax
  (lambda (exp-name tmpl library env p1env)
    (let ((ids (collect-unique-ids tmpl)))
      (let ((ranks (filter values
			   (map (lambda (id)
				  (let ((rank (p1env-pvar-lookup p1env id
								 #;(if (identifier? id)
								     (id-name id)
								     id))))
				    ;; rank must be number
				    (and (not (variable? rank))
					 (cons id rank))))
				ids))))
	;; later
	;;(check-template tmpl ranks)
	(let ((patvar (map car ranks)))
	  (if (variable? tmpl)
	      (if (null? ranks)
		  `(expand-syntax ',patvar ',tmpl () () ,p1env)
		  `(expand-syntax ',patvar ',tmpl (list (cons ',tmpl 0)) vars ,p1env)))
	  (if (null? ranks)
	      `(expand-syntax ',patvar ',tmpl () () ,p1env)
	      `(expand-syntax ',patvar ',tmpl ',ranks vars ,p1env)))))))

(define expand-syntax
  (lambda (patvars template ranks vars p1env)
    (let ((frames (vector-ref p1env 1))
	  (library (vector-ref p1env 0)))
      (define emit
	(lambda (datum)
	  (cond ((identifier? datum) datum)
		((symbol? datum) (make-identifier datum frames library))
		(else datum))))

      (define contain-indentifier?
	(lambda (lst)
	  (let loop ((lst lst))
	    (cond ((pair? lst)
		   (or (null? (car lst))
		       (loop (car lst))
		       (loop (cdr lst))))
		  ((vector? lst)
		   (let loop2 ((i (- (vector-length lst) 1)))
		     (and (>= i 0)
			  (or (loop (vector-ref lst i))
			      (loop2 (- i 1))))))
		  (else
		   (identifier? lst))))))
      (define partial-identifier
	(lambda (lst)
	  ;; TODO this is a temporary solution
	  (define renamed-ids '())
	  (let loop ((lst lst))
	    (cond ((contain-indentifier? lst)
		   (cond ((pair? lst)
			  (let ((a (loop (car lst))) (d (loop (cdr lst))))
			    (cond ((and (eq? (car lst) a) (eq? (cdr lst) d)) lst)
				  (else (cons a d)))))
			 (else lst)))
		  ((null? lst) '())
		  ((symbol? lst)
		   (cond ((assq lst renamed-ids)
			  => cdr)
			 (else
			  (let ((id (make-identifier lst frames library)))
			    (set! renamed-ids (acons lst id renamed-ids))
			    id))))
		  ((vector? lst)
		   (loop (vector->list lst)))
		  ((pair? lst)
		   (cons (loop (car lst))
			 (loop (cdr lst))))
		  (else lst)))))
      (if (null? template)
	  '()
	  (let ((form (transcribe-template template ranks patvars vars emit)))
	    (cond ((null? form) '())
		  ((identifier? form) form)
		  ((symbol? form)
		   (make-identifier form frames library))
		  (else
		   (partial-identifier form))))))))

(define rank-of
  (lambda (name ranks)
    (let ((slot (assq name ranks)))
      (if slot (cdr slot) -1))))

(define subform-of
  (lambda (name vars)
    (cdr (assq name vars))))

(define contain-rank-moved-var?
  (lambda (tmpl ranks vars)

    (define traverse-escaped
      (lambda (lst depth)
        (let loop ((lst lst) (depth depth))
          (cond ((symbol? lst)
                 (< 0 (rank-of lst ranks) depth))
                ((pair? lst)
                 (or (loop (car lst) depth)
                     (loop (cdr lst) depth)))
                ((vector? lst)
                 (loop (vector->list lst) depth))
                (else #f)))))

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
            (else #f)))))


(define adapt-to-rank-moved-vars
  (lambda (form ranks vars)
    
    (define rewrite-template-ranks-vars
      (lambda (tmpl ranks vars)
        (let ((moved-ranks (make-core-hashtable)) (moved-vars (make-core-hashtable)))

          (define make-infinite-list
            (lambda (e)
              (let ((lst (list e)))
                (begin (set-cdr! lst lst) lst))))

          (define revealed
            (lambda (name depth)
              (if (< 0 (rank-of name ranks) depth)
                  (let ((renamed (string->symbol (format "~a:~a" (generate-temporary-symbol) name))))
                    (or (core-hashtable-ref moved-ranks renamed #f)
                        (let loop ((i (- depth (rank-of name ranks))) (var (subform-of name vars)))
                          (cond ((> i 0)
                                 (loop (- i 1) (list (make-infinite-list (car var)))))
                                (else
                                 (core-hashtable-set! moved-ranks renamed depth)
                                 (core-hashtable-set! moved-vars renamed var)))))
                    renamed)
                  name)))

          (define traverse-escaped
            (lambda (lst depth)
              (let loop ((lst lst) (depth depth))
                (cond ((symbol? lst)
                       (revealed lst depth))
                      ((pair? lst)
                       (cons (loop (car lst) depth)
                             (loop (cdr lst) depth)))
                      ((vector? lst)
                       (list->vector (loop (vector->list lst) depth)))
                      (else lst)))))

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
                    (append ranks (core-hashtable->alist moved-ranks))
                    (append vars (core-hashtable->alist moved-vars)))))))

    (if (contain-rank-moved-var? form ranks vars)
        (rewrite-template-ranks-vars form ranks vars)
        (values form ranks vars))))

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

(define transcribe-template
  (lambda (template ranks patvars vars emit)
    (define remove-duplicates
      (lambda (alist)
	(or (let loop ((lst alist))
	      (cond ((null? lst) alist)
		    ((assq (caar lst) (cdr lst)) #f)
		    (else (loop (cdr lst)))))
	    (let loop ((lst alist) (acc '()))
	      (cond ((null? lst) acc)
		    ((assq (caar lst) acc)
		     (loop (cdr lst) acc))
		    (else
		     (loop (cdr lst) (cons (car lst) acc))))))))
    (receive (tmpl ranks vars)
	(adapt-to-rank-moved-vars template ranks (remove-duplicates vars))

      (define expand-var
	(lambda (tmpl vars)
	  (cond ((assq tmpl vars)
		 => (lambda (slot)
		      (cond ((null? (cdr slot)) '())
			    ;; if we don't share this with syntax-rules
			    ;; we don't have check emit.
			    (emit (emit (cadr slot)))
			    (else (cadr slot)))))
		(else
		 (assertion-violation "syntax template"
				      "subforms have different size of matched input"
				      `(template: ,template) `(subforms: ,@vars))))))
      (define expand-ellipsis-var
	(lambda (tmpl vars)
	  (cond ((assq tmpl vars)
		 => (lambda (slot)
		      (cond ((null? (cdr slot)) '())
			    ;; if we don't share this with syntax-rules
			    ;; we don't have check emit.
			    (emit (map emit (cadr slot)))
			    (else (cadr slot)))))
		(else
		 (assertion-violation "syntax template"
				      "subforms have different size of matched input"
				      `(template: ,template) `(subforms: ,@vars))))))

      (define expand-ellipsis-template
        (lambda (tmpl depth vars)
          (let loop ((expr '()) (remains (collect-ellipsis-vars tmpl ranks depth vars)))
            (cond ((pair? remains)
                   (loop (cons (expand-template tmpl depth remains) expr)
                         (consume-ellipsis-vars ranks depth remains)))
                  ((null? remains) '())
                  ((eq? remains #t) (reverse expr))
                  (else
                   (assertion-violation "syntax template"
					"subforms have different size of matched input"
                                        `(template: ,template) `(subforms: ,@vars)))))))

      (define expand-escaped-template
        (lambda (tmpl depth vars)
          (cond ((variable? tmpl)
                 (if (< (rank-of tmpl ranks) 0)
		     tmpl
                     (expand-var tmpl vars)))
                ((pair? tmpl)
                 (if (and emit (null? (car tmpl)))
                     (cons '()
                           (expand-escaped-template (cdr tmpl) depth vars))
                     (cons (expand-escaped-template (car tmpl) depth vars)
                           (expand-escaped-template (cdr tmpl) depth vars))))
                ((vector? tmpl)
                 (list->vector (expand-escaped-template (vector->list tmpl) depth vars)))
                (else tmpl))))

      (define expand-template
        (lambda (tmpl depth vars)
          (cond ((variable? tmpl)
                 (if (< (rank-of tmpl ranks) 0)
		     tmpl
		     (expand-var tmpl vars)))
                ((ellipsis-quote? tmpl)
                 (expand-escaped-template (cadr tmpl) depth vars))
                ((ellipsis-splicing-pair? tmpl)
                 (reverse (body tail len) (parse-ellipsis-splicing tmpl)
                   (append (apply append (expand-ellipsis-template body (+ depth 1) vars))
                           (expand-template tail depth vars))))
                ((ellipsis-pair? tmpl)
                 (cond ((symbol? (car tmpl))
                        (let ((rank (rank-of (car tmpl) ranks)))
                          (cond ((= rank (+ depth 1))
                                 (append (expand-ellipsis-var (car tmpl) vars)
                                         (expand-template (cddr tmpl) depth vars)))
				(else
				 ;; error?
				 (assertion-violation "syntax template"
						      "missing ellipsis"
						      template)))))
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
                (else tmpl))))
      (if (and (= (safe-length tmpl) 2) (ellipsis? (car tmpl)))
          (expand-escaped-template (cadr tmpl) 0 vars)
	  (expand-template tmpl 0 vars)))))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
