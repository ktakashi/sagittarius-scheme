;; lib/vm.scm
;; dummy
(define *toplevel-variable* '())

(define (set-toplevel-variable! sym val)
  (set! *toplevel-variable* (acons sym val *toplevel-variable*)))
(define (gloc-ref g) g)
(define (gloc-bound? g) #t)

;; for declare-procedure
(define (parse-type type)
  (if (and (pair? type)
	   (= (length type) 2))
      (cadr type)
      #f))

;; this file is the collection of vm procedure.
;; these procedure must be written in C++
(define (insn-name insn)
  (let ((info (lookup-insn-name insn)))
    (car info)))

;; for better performance.
;; this will be called so many times in compiler.scm
(define p1env-lookup
  (lambda (p1env name lookup-as)
    (let ((name-ident? (identifier? name))
	  (frames (vector-ref p1env 1))
	  (ret #f))
      (let loop ((fp frames))
	(cond ((pair? fp)
	       (when (and name-ident?
			  (eq? (id-envs name) fp))
		 (set! name-ident? #f) ;; given name is no longer identifier
		 (set! name (id-name name)))
	       (when (> (caar fp) lookup-as)
		 (loop (cdr fp)))
	       (let loop2 ((tmp (cdar fp)))
		 (if (pair? tmp)
		     (let ((vp (car tmp)))
		       (if (eq? name (car vp))
			   (cdr vp)
			   (loop2 (cdr tmp))))
		     (loop (cdr fp)))))
	      (else
	       (if (symbol? name)
		   (make-identifier name '() (vector-ref p1env 0))
		   name)))))))

(define p1env-toplevel?
  (lambda (p1env)
    (not (any (lambda (frame) (eqv? (car frame) LEXICAL))
	      (vector-ref p1env 1)))))

;;==========================================================================
;; Identifiers:
;;
;; <name>             ::= <symbol>
;; <envs>             ::= (<env> ...)
;; <library>          ::= (<symbol> ...) | #f
;;
;; where
;;   <name>             : The symbolic name of the identifier in the source.
;;   <envs>             : p1env frames
;;   <library>          : Library name.
(define make-identifier
  (lambda (name envs library)
    (vector '.identifier
	    name
	    (if (null? envs)
		'()
		(get-binding-frame name envs))
	    (if (library? library)
		library
		(vm-current-library)))))

(define (id-name id)
  (vector-ref id 1))
(define (id-name-set! id v)
  (vector-set! id 1 v))
(define (id-envs id)
  (vector-ref id 2))
(define (id-envs-set! id v)
  (vector-set! id 2 v))
(define (id-library id)
  (vector-ref id 3))
(define (id-library-set! id v)
  (vector-set! id 3 v))

(define copy-identifier
  (lambda (id)
    (make-identifier (id-name id)
		     (id-envs id)
		     (id-library id))))

(define get-binding-frame
  (lambda (var env)
    (let loop ((frame env))
      (if (pair? frame)
	  (if (pair? (car frame))
	      (let loop2 ((fp (cdar frame)))
		(if (pair? fp)
		    (if (eq? (caar fp) var)
			frame
			(loop2 (cdr fp)))
		    (loop (cdr frame))))
	      (loop (cdr frame)))
	  '()))))
(define identifier-binding-eqv?
  (lambda (id sym env)
    (let ((bf (get-binding-frame sym env)))
      (eq? bf (id-envs id)))))

;; Sagittarius library systam
;; Top libraries
;; Just a hashtable: <key>   = library name
;;                   <value> = library
;; Each library
;; - name          : this library's name
;; - imported      : imported symbols for this library
;; - exported      : exported symbols from whis library
;; - binding table : binding table.
;; - transient     : #t not import after converted to c

;; libraries
;; this might be like this
;; hashtable -> ((version) . library instance)
;; ((lib1 => (((1) . <library>)
;;	      ((2) . <library>))
;; but on scheme VM it's just hashtable to be simple.
(define *libraries* (make-hashtable equal-hash equal?))
(define (make-library library)
  (let ((lib (vector '.library library '() #f (make-eq-hashtable) #f)))
    (hashtable-set! #;(vm-libraries) *libraries* library lib)
    lib))

(define (library? lib)
  (and (vector? lib)
       (> (vector-length lib) 1)
       (eq? (vector-ref lib 0) '.library)))
(define (library-name lib)
  (vector-ref lib 1))
(define (library-imported lib)
  (vector-ref lib 2))
(define (library-imported-set! lib spec)
  (vector-set! lib 2 spec))
(define (library-exported lib)
  (vector-ref lib 3))
(define (library-exported-set! lib spec)
  (vector-set! lib 3 spec))
(define (library-table lib)
  (vector-ref lib 4))
(define (library-transient lib)
  (vector-ref lib 5))
(define (library-transient-set! lib val)
  (vector-set! lib 5 val))

(define (%set-library lib)
  (or (library? lib)
      (error "library required but got " lib))
  (hashtable-set! #;(vm-libraries) *libraries* (library-name lib) lib))
;; TODO version number...
(define (find-library name create?)
  (let ((l (hashtable-ref #;(vm-libraries) *libraries* name #f)))
    (or l
	(if create?
	    (make-library name)
	    l))))

(define (%insert-binding library name value)
  (define (add-export lib name)
    (if (library-exported lib)
	(library-exported-set! lib (cons name (library-exported lib)))
	(library-exported-set! lib (list name))))

  (cond ((library? library)
	 (hashtable-set! (library-table library) name value))
	((hashtable-ref #;(vm-libraries) *libraries* library #f) ;; maybe just a name?
	 => (lambda (v)
	      (hashtable-set! (library-table v) name value)))
	(else
	 (let ((lib (make-library library)))
	   (hashtable-set! (library-table lib) name value)))))

(define (find-binding lib name callback)
  (cond ((library? lib)
	 (let ((r (hashtable-ref (library-table lib) name #f)))
	   (if r
	       r
	       (cond ((assq name *toplevel-variable*)
		      => cdr)
		     (else callback)))))
	((hashtable-ref *libraries* lib callback) ;; maybe just a name?
	 => (lambda (lib)
	      (let ((r (hashtable-ref (library-table lib) name #f)))
		(if r
		    r
		    (cond ((assq name *toplevel-variable*)
			   => cdr)
			  (else callback))))))
	(else callback)))

;(define *compiler-library* '(sagittarius compiler))
(define *current-library* 'user)
(define vm-current-library
  (lambda name
    (if (null? name)
	*current-library*
	(set! *current-library* (car name)))))

;; just stub
(define (import-library to from only except rename prefix trans?)
  (when trans?
    (library-transient-set! from #t))
  (let* ((lib (if (library? from)
		    from
		    (find-library from #f)))
	 (export-spec (library-exported lib)))
    (when (and lib
	       #;(not (assq lib (library-imported to))))
      (unless (assq lib (library-imported to))
	(library-imported-set! to
			       (acons lib
				      export-spec
				      (library-imported to))))
      (hashtable-for-each
       (lambda (k v)
	 (cond ((not export-spec)
		;; maybe null or user library
		(hashtable-set! (library-table to) k v))
	       ((memq k (car export-spec))
		;; no rename just put
		(hashtable-set! (library-table to) k v))
	       ((assq k (cdr export-spec)) =>
		(lambda (spec)
		  (hashtable-set! (library-table to) (cdr spec) v)))))
	 (library-table lib)))))
;; for vm.scm
(define (load-library to . from)
  (let loop ((from from))
    (if (null? from)
	#t
	(let ((name (car from)))
	  (let* ((lib (if (library? name)
			  name
			  (find-library name #f)))
		 (export-spec (library-exported lib)))
	    (when (and lib
		       #;(not (assq lib (library-imported to))))
	      (unless (assq lib (library-imported to))
		(library-imported-set! to
				       (acons lib
					      export-spec
					      (library-imported to))))
	      (hashtable-for-each
	       (lambda (k v)
		 (cond ((not export-spec)
			;; maybe null or user library
			(hashtable-set! (library-table to) k v))
		       ((memq k (car export-spec))
			;; no rename just put
			(hashtable-set! (library-table to) k v))
		       ((assq k (cdr export-spec)) =>
			(lambda (spec)
			  (hashtable-set! (library-table to) (cdr spec) v)))))
	       (library-table lib)))
	    (loop (cdr from)))))))

#;(define (import-only to from symbols)
  (library-imported-set! to
			 (acons from symbols (library-imported to)))
  (for-each (lambda (sym)
	      (hashtable-set! (library-table to)
			      sym
			      (hashtable-ref (library-table from)
					     sym)))
	    symbols))

#;(define (import-rename to from rename prefix?)
  (define (add-prefix prefix)
    (let ((keys (hashtable-keys (library-table from))))
      (let loop ((keys keys)
		 (r '()))
	(cond ((null? keys) r)
	      (else
	       (let ((renamed (string->symbol
			       (string-append (symbol->string prefix)
					      (symbol->string (car keys))))))
		 (loop (cdr keys) (cons (list (car keys) renamed) r))))))))
  (let ((renames rename))
    (if prefix?
	(set! renames (add-prefix rename)))
    (let loop ((renames renames))
      (unless (null? renames)
	(unless (= (length (car renames)) 2)
	  (error "syntax-error: malformed rename spec in import clause:" rename))
	(let ((org (caar renames))
	      (renamed (cadar renames)))
	  (hashtable-set! (library-table to) renamed
			  (hashtable-ref (library-table from) org)))
	(loop (cdr renames))))))

(define (make-syntax name proc . user-defined?)
  (if (null? user-defined?)
      (vector 'type:syntax name proc #f)
      (vector 'type:syntax name proc #t)))

(define (syntax? s)
  (and (vector? s) 
       (eq? (vector-ref s 0) 'type:syntax)))
(define (syntax-name s)
  (vector-ref s 1))
(define (syntax-proc s)
  (vector-ref s 2))
(define (builtin-syntax? s)
  (and (syntax? s)
       (not (vector-ref s 3))))
(define (user-defined-syntax? s)
  (and (syntax? s)
       (vector-ref s 3)))

(define (call-syntax-handler s expr p1env)
  (cond ((builtin-syntax? s)
	 ((syntax-proc s) expr p1env))
	(else
	 (error 'call-syntax-handler "bug?"))))

(define unwrap-syntax
  (lambda (form)
    (define rec
      (lambda (form history)
	(cond ((or (fixnum? form)
		   (char? form)
		   (boolean? form)) form)
	      ((memq form history) form)
	      ((pair? form)
	       (let* ((newh (cons form history))
		      (ca   (rec (car form) newh))
		      (cd   (rec (cdr form) newh)))
		 (if (and (eq? ca (car form))
			  (eq? cd (cdr form)))
		     form
		     (cons ca cd))))
	      ((identifier? form)
	       (id-name form))
	      ((and (vector? form)
		    (> (vector-length form) 1)
		    (eq? (vector-ref form 0) '.closure))
	       'closure)
	      ((library? form)
	       (library-name form))
	      ((vector? form)
	       (let ((len (vector-length form))
		     (newh (cons form history)))
		 (let loop ((i 0))
		   (cond ((= i len) form)
			 (else
			  (let* ((pe (vector-ref form i))
				 (e (rec pe newh)))
			    (cond ((eq? e pe)
				   (loop (+ i 1)))
				  (else
				   (let ((v (make-vector len #f)))
				     (let vcopy ((j 0))
				       (unless (= j i)
					 (vector-set! v j (vector-ref form j))
					 (vcopy (+ j 1))))
				     (vector-set! v i e)
				     (let vcopy ((j i))
				       (unless (= j len)
					 (vector-set! v j (vector-ref form j))
					 (vcopy (+ j 1))))
				     v)))))))))
	      (else form))))
    (rec form '())))



(define (make-macro name transformer data env . maybe-library)
  (vector 'type:macro name transformer data env
	  (if (null? maybe-library)
	      #f
	      (car maybe-library))))

(define (macro-name m)
  (vector-ref m 1))
(define (macro-transformer m)
  (vector-ref m 2))
(define (macro-data m)
  (vector-ref m 3))
(define (macro-env m)
  (vector-ref m 4))
(define (macro-library m)
  (vector-ref m 5))
(define (macro? m)
  (and (vector? m) (eq? (vector-ref m 0) 'type:macro)))
(define (call-macro-expander macro expr p1env)
  ((macro-transformer macro) macro expr p1env (macro-data macro)))
(define (unbound) (if #f #f))

(define (make-toplevel-closure cb)
  (make-closure cb 0))

;; for er-macro-transformer
(define macro-transform
  (lambda (self form p1env data)
    (let ((expander (apply-proc data '()))
	  (mac-env  (macro-env self)))
      (apply-proc expander (list (cons form (cons p1env mac-env))))
      #;(if (macro? expander)
	  ((macro-transformer expander) expander form p1env (macro-data expander))
	  (apply-proc expander (list (cons form p1env)))))))

(define make-macro-transformer
  (lambda (name proc env library)
    (make-macro name macro-transform proc env library)))

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


(define (%map-cons l1 l2) (map cons l1 l2))
;(define LEXICAL 0)
;(define SYNTAX 1)
;(define PATTERN 2)

;; this needs to be in C++. I don't want to double manage these values.
(define (pass3/let-frame-size) 2)
(define (pass3/frame-size) 4)

;; also need to be c++
;; code builder

;; TODO: this must be cpp.
;; ---> start
;; actual code builder
(define (make-array)
  (vector '.array (make-vector 2) 0))
(define (array-data a)
  (vector-ref a 1))
(define (array-data-set! a v)
  (vector-set! a 1 v))
(define (array-length a)
  (vector-ref a 2))
(define (array-length-set! a v)
  (vector-set! a 2 v))

(define array?
  (lambda (a)
    (and (vector? a)
	 (eq? (vector-ref a 0) '.array))))
(define array-data-length
  (lambda (array)
    (vector-length (array-data array))))
(define array-data-copy
  (lambda (src dst length)
    (do ((i 0 (+ i 1)))
	((>= i length) #f)
      (vector-set! dst i (vector-ref src i)))))

(define set-array-length!
  (lambda (array length)
    (array-length-set! array length)
    (if (>= length (array-data-length array))
      (let ((next-data (make-vector (* length 2))))
	(array-data-copy (array-data array) next-data length)
	(array-data-set! array next-data)))))

(define array-push!
  (lambda (array obj)
    (let* ((data (array-data array))
	   (length (array-length array)))
      (vector-set! data length obj)
      ;; extend array for next use
      (set-array-length! array (+ length 1)))))

(define array->list
  (lambda (array)
    (let ((data (array-data array))
	  (length (array-length array)))
      (let loop ((i 0)
		 (ret '()))
	(if (>= i length)
	    (reverse ret)
	    (loop (+ i 1) (cons (vector-ref data i) ret)))))))

;; code builder
;; code builder
;; properties:
;;   code   	 - for now just an array
;;  <below this is for closure>
;;   name        - closure name or #f
;;   argc   	 - argument count
;;   optional?   - #t it has optional arg, #f it has no optional arg
;;   freec       - free variable count
;;   maxStack    - estimated stack size
;;   src         - src info
;;  <below is for combine>
;;   packet      - previous instruction data.
;;   label-defs  - alist of (name . offset)
;;   label-refs   - alist of (name . offset-to-fill)

;; code-packet
;;  insn - vm instruction
;;  type - packet type
;;  arg0 - instruction value
;;  arg1 - instruction value
;;  obj  - object
(define-constant EMPTY 0)
(define-constant ARGUMENT0 1)
(define-constant ARGUMENT1 2)

(define undef (if #f #f))
(define (make-code-packet)
  (vector -1 EMPTY 0 0 undef))

(define (init-packet packet insn type arg0 arg1 o)
  (vector-set! packet 0 insn)
  (vector-set! packet 1 type)
  (vector-set! packet 2 arg0)
  (vector-set! packet 3 arg1)
  (vector-set! packet 4 o)
  packet)

(define (packet-insn packet)
  (vector-ref packet 0))
(define (packet-insn-set! packet insn)
  (vector-set! packet 0 insn))
(define (packet-type packet)
  (vector-ref packet 1))
(define (packet-type-set! packet type)
  (vector-set! packet 1 type))
(define (packet-arg0 packet)
  (vector-ref packet 2))
(define (packet-arg0-set! packet o)
  (vector-set! packet 2 o))
(define (packet-arg1 packet)
  (vector-ref packet 3))
(define (packet-arg1-set! packet o)
  (vector-set! packet 3 o))
(define (packet-obj packet)
  (vector-ref packet 4))

(define make-code-builder 
  (lambda ()
    (vector '.code-builder (make-array) #f 0 #f 0 0 '() (make-code-packet) '() '())))
(define code-builder-code
  (lambda (cb)
    (vector-ref cb 1)))
(define code-builder-code-set!
  (lambda (cb o)
    (array-data-set! (vector-ref cb 1) o)
    (array-length-set! (vector-ref cb 1) (vector-length o))))
(define code-builder-name
  (lambda (cb)
    (vector-ref cb 2)))
(define code-builder-name-set!
  (lambda (cb argc)
    (vector-set! cb 2 argc)))
(define code-builder-argc
  (lambda (cb)
    (vector-ref cb 3)))
(define code-builder-argc-set!
  (lambda (cb argc)
    (vector-set! cb 3 argc)))
(define code-builder-optional?
  (lambda (cb)
    (vector-ref cb 4)))
(define code-builder-optional-set!
  (lambda (cb o)
    (vector-set! cb 4 o)))
(define code-builder-freec
  (lambda (cb)
    (vector-ref cb 5)))
(define code-builder-freec-set!
  (lambda (cb o)
    (vector-set! cb 5 o)))
(define code-builder-maxstack
  (lambda (cb)
    (vector-ref cb 6)))
(define code-builder-maxstack-set!
  (lambda (cb o)
    (vector-set! cb 6 o)))
(define code-builder-src
  (lambda (cb)
    (vector-ref cb 7)))
(define code-builder-src-set!
  (lambda (cb o)
    (vector-set! cb 7 o)))
(define code-builder-add-src
  (lambda (cb src)
    (let ((index (array-length (code-builder-code cb)))
	  (old-src (code-builder-src cb)))
      (code-builder-src-set! cb (append old-src (list (cons index src)))))))
(define code-builder-packet
  (lambda (cb)
    (vector-ref cb 8)))
(define code-builder-packet-set!
  (lambda (cb o)
    (vector-set! cb 8 o)))
(define code-builder-label-defs
  (lambda (cb)
    (vector-ref cb 9)))
(define code-builder-label-defs-set!
  (lambda (cb l)
    (vector-set! cb 9 l)))
(define code-builder-label-refs
  (lambda (cb)
    (vector-ref cb 10)))
(define code-builder-label-refs-set!
  (lambda (cb l)
    (vector-set! cb 10 l)))

(define code-builder?
  (lambda (cb)
    (and (vector? cb)
	 (eq? (vector-ref cb 0) '.code-builder))))

(define label?
  (lambda (l)
    (and (vector? l)
	 (> (vector-length l) 0)
	 (eqv? (vector-ref l 0) 11 #;$LABEL))))

(define (cb-flush cb)
  (if (= (packet-type (code-builder-packet cb)) EMPTY)
      #t
      (let ((insn (merge-insn2 (packet-insn (code-builder-packet cb))
			       (packet-arg0 (code-builder-packet cb))
			       (packet-arg1 (code-builder-packet cb)))))
	(cond ((= (packet-type (code-builder-packet cb)) ARGUMENT0)
	       (array-push! (code-builder-code cb) insn))
	      ((= (packet-type (code-builder-packet cb)) ARGUMENT1)
	       (let ((obj (packet-obj (code-builder-packet cb))))
		 (array-push! (code-builder-code cb) insn)
		 (if (label? obj)
		     (begin
		       (code-builder-label-refs-set! cb 
						     (acons obj
							    (array-length (code-builder-code cb))
							    (code-builder-label-refs cb)))
		       (array-push! (code-builder-code cb) 0)) ; dummy
		     (array-push! (code-builder-code cb) obj)))))
	(code-builder-packet-set! cb (make-code-packet))
	#;(packet-type-set! (code-builder-packet cb) EMPTY))))
	     

(define (cb-put cb packet)
  (cond ((= (packet-type packet) ARGUMENT0)
	 (combine-insn-arg0 cb packet))
	((= (packet-type packet) ARGUMENT1)
	 (combine-insn-arg1 cb packet))
	((= (packet-type packet) ARGUMENT2)
	 (combine-insn-arg2 cb packet))
	(else (error "[internal] code-builder failed to emit code."))))

(define (combine-insn-arg0 cb packet)
  (cond ((= (packet-insn packet) PUSH)
	 (cond ((= (packet-insn (code-builder-packet cb)) LREF)
		(packet-insn-set! (code-builder-packet cb) LREF_PUSH))
	       ((= (packet-insn (code-builder-packet cb)) FREF)
		(packet-insn-set! (code-builder-packet cb) FREF_PUSH))
	       ((= (packet-insn (code-builder-packet cb)) GREF)
		(packet-insn-set! (code-builder-packet cb) GREF_PUSH))
	       ((= (packet-insn (code-builder-packet cb)) CONST)
		(packet-insn-set! (code-builder-packet cb) CONST_PUSH))
	       ((= (packet-insn (code-builder-packet cb)) CONSTI)
		(packet-insn-set! (code-builder-packet cb) CONSTI_PUSH))
	       (else
		(cb-flush cb)
		(code-builder-packet-set! cb packet))))
	((= (packet-insn packet) UNDEF)
	 ;; i don't want undef undef undef thing.
	 (cond ((= (packet-insn (code-builder-packet cb)) UNDEF)
		#t)
	       (else
		(cb-flush cb)
		(code-builder-packet-set! cb packet))))
	((= (packet-insn packet) CALL)
	 (cond ((= (packet-insn (code-builder-packet cb)) GREF)
		(packet-insn-set! (code-builder-packet cb) GREF_CALL)
		(packet-type-set! (code-builder-packet cb) ARGUMENT1)
		(packet-arg0-set! (code-builder-packet cb) (packet-arg0 packet)))
	       (else
		(cb-flush cb)
		(code-builder-packet-set! cb packet))))
	((= (packet-insn packet) TAIL_CALL)
	 (cond ((= (packet-insn (code-builder-packet cb)) GREF)
		(packet-insn-set! (code-builder-packet cb) GREF_TAIL_CALL)
		(packet-type-set! (code-builder-packet cb) ARGUMENT1)
		(packet-arg0-set! (code-builder-packet cb) (packet-arg0 packet)))
	       (else
		(cb-flush cb)
		(code-builder-packet-set! cb packet))))
	(else
	 (cb-flush cb)
	 (code-builder-packet-set! cb packet))))

(define (combine-insn-arg1 cb packet)
  (cond (else
	 (cb-flush cb)
	 (code-builder-packet-set! cb packet))))

;; insn value map
;; mmmmmmmm mmmmnnnn nnnnnnnn iiiiiiii
;; m = arg2 (if it's required)
;; n = arg1
;; i = instruction
;; insn must be under 255 (1 byte)
(define (merge-insn1 insn arg1)
  (bitwise-ior insn (bitwise-arithmatic-shift-left arg1 8)))

;; arg1 and arg2 must be under 12 bit
;; but on scheme vm, i'm not gonna check it
(define (merge-insn2 insn arg1 arg2)
  (bitwise-ior insn 
	       (bitwise-ior (bitwise-arithmatic-shift-left arg1 8)
			    (bitwise-arithmatic-shift-left arg2 20))))

(define (get-insn-value insn num index)
  (cond ((= num 1)
	 (bitwise-arithmatic-shift insn -8))
	((= num 2)
	 (cond ((= index 0)
		(bitwise-and (bitwise-arithmatic-shift insn -8) #xfff))
	       ((= index 1)
		(bitwise-arithmatic-shift insn -20))))))

(define (get-insn insn)
  (bitwise-and insn #xff))

;(merge-insn2 insn arg0 arg1)
;; only insn values
(define (cb-emit2! cb insn arg0 arg1)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 arg0 arg1 undef)))
(define (cb-emit1! cb insn arg0)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 arg0 0 undef)))
(define (cb-emit0! cb insn)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 0 0 undef)))

;; insn value with src info
(define (cb-emit2i! cb insn arg0 arg1 src)
  (code-builder-add-src cb src)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 arg0 arg1 undef)))
(define (cb-emit1i! cb insn arg0 src)
  (code-builder-add-src cb src)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 arg0 0 undef)))
(define (cb-emit0i! cb insn src)
  (code-builder-add-src cb src)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT0 0 0 undef)))

;; with object
(define (cb-emit0o! cb insn o)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT1 0 0 o)))
(define (cb-emit0oi! cb insn o src)
  (code-builder-add-src cb src)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT1 0 0 o)))
(define (cb-emit1oi! cb insn arg0 o src)
  (code-builder-add-src cb src)
  (cb-put cb (init-packet (make-code-packet) insn ARGUMENT1 arg0 0 o)))

(define cb-label-set!
  (lambda (cb label)
    (cb-flush cb)
    (code-builder-label-defs-set! cb 
				  (acons label (array-length (code-builder-code cb))
					 (code-builder-label-defs cb)))))

(define (cb-emit-closure! cb insn lambda-cb name argc opt? freec max-stack src)
  (code-builder-name-set! lambda-cb name)
  (code-builder-argc-set! lambda-cb argc)
  (code-builder-optional-set! lambda-cb opt?)
  (code-builder-freec-set! lambda-cb freec)
  (code-builder-maxstack-set! lambda-cb max-stack)
  (code-builder-add-src lambda-cb src)
  (cb-flush lambda-cb)
  (cb-emit0o! cb insn lambda-cb))
	
;; this needs to be moved to C++
(define (code-builder-finish-builder cb last)
  (define (builder-label-def label-defs label)
    (cond ((assq label label-defs)
	   => cdr)
	  (else 
	   (error "a label was refered but not defined.")
	   -1)))

  (define (rec cb)
    (let* ((size (array-length (code-builder-code cb)))
	   (code (array-data   (code-builder-code cb)))
	   (label-defs (code-builder-label-defs cb))
	   (label-refs (code-builder-label-refs cb))
	   (v (make-vector size NOP)))
      (for-each (lambda (l)
		  (let ((dest (builder-label-def label-defs (car l)))
			(operand (cdr l)))
		    (vector-set! code operand (- dest operand))))
		label-refs)
      (let loop ((i 0))
	(if (= i size)
	    (begin
	      (array-length-set! (code-builder-code cb) size)
	      (array-data-set! (code-builder-code cb) v))
	    (let ((o (vector-ref code i)))
	      (vector-set! v i o)
	      (if (code-builder? (vector-ref code i))
		  (rec (vector-ref code i)))
	      (loop (+ i 1)))))))

  (unless (= last NOP)
    (cb-emit0! cb last))
  (cb-flush cb)
  (rec cb)
  cb)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
