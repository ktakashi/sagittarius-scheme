;; -*- Scheme -*-
#|--
  LibraryName: (sagittarius cgen)
  Description: Translate scheme to C
  Usage:
  This library translate scheme like file to C for sagittarius.
  The expression of scheme like file is like this:
  <code>
  (library (sagittarius compiler procedure)
      (import :none)
      (export procedure-name)
    (define-c-proc procedure-name (arg0::Procedure) ::Object
      (result (SG_PROCEDURE_NAME arg0))))
  </code>
  And the result is like this:
  <code>
  #define LIBSAGITTARIUS_BODY
  #include <sagittarius.h>
  static SgObject proclib_procedure_name(SgObject *args, int argc, void *data_)
  {
    DeclareProcedureName("procedure-name");
    SgObject arg0_scm;
    SgProcedure *arg0;
    checkArgumentLength(1);
    argumentAsProcedure(1, arg0_scm, arg0);
    return SG_PROCEDURE_NAME(arg0);
  }
  static SG_DEFINE_SUBR(proclib_procedure_name_Stub, 1, 0, proclib_procedure_name, SG_FALSE, NULL);
  void Sg__Init_proclib()
  {
    SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius compiler procedure)"), SG_LITERAL_STRING)), TRUE);
    SgObject  s_proclib_procedure_name_Stub = Sg_Intern(Sg_MakeString(UC("procedure-name"), SG_LITERAL_STRING));
    Sg_InsertBinding(lib, s_proclib_procedure_name_Stub, proclib_procedure_name_Stub);
  }
  </code>
|#
#!compatible ;; for Ypsilon
(library (sagittarius cgen)
    (export cgen 
	    (rename 
	     (base:renderer renderer)
	     (base:define-cgen-stmt define-cgen-stmt)
	     (base:define-cgen-macro define-cgen-macro)
	     (base:register-macro! register-macro!)
	     (base:dispatch dispatch)))
    (import (rnrs (6))
	    (rnrs eval (6))
	    (sagittarius format)
	    (sagittarius cgen util)
	    (sagittarius)
	    (prefix (sagittarius cgen base) base:)
	    (match))
  ;; this must be (file) library but for now.
  (define (file-extention file)
    (let ((dot-index (string-index-right file #\.)))
      (substring file dot-index (string-length file))))
  (define (file-name-without-extention file)
    (let ((dot-index (string-index-right file #\.)))
      (substring file 0 dot-index)))

  ;; assume this library exports only cgen
  ;; so these properties are working like private properties.
  ;; I hope!
  ;(define *dispatch-table* #f)
  (define *library-name* #f)
  (define *import-spec* #f)
  (define *export-spec* #f)
  (define *c-proc-defs* #f)
  (define *procedure-subr-map* #f)
  (define *procedure-map* #f)
  (define *toplevel-variables* '())

  (define (init) 
    ;(set! *dispatch-table* (make-eq-hashtable))
    (set! *library-name* #f)
    (set! *import-spec* #f)
    (set! *export-spec* #f)
    (set! *c-proc-defs* #f)
    (set! *procedure-subr-map* (make-eq-hashtable))
    (set! *procedure-map* (make-eq-hashtable))
    (set! *toplevel-variables* '())
    (base:add-dispatch 'define-c-proc def-c-proc)
    (base:add-dispatch 'set-toplevel-variable! set-toplevel-variable!))

  ;; get information from library
  (define (pre-resolve)
    (let ((r (read)))
      (match r
	(('library libname
	     ('export . exports)
	     ('import . imports)
	   . body)
	 (set! *library-name* libname)
	 (set! *export-spec* exports)
	 (set! *import-spec* imports)
	 (set! *c-proc-defs* body))
	(else
	 (error 'pre-resolve "invalid cgen library form" r)))))

  ;; write header
  (define (write-header ofile)
    ((base:renderer) (format "/* This file is autmatically generated from ~s. DO NOT EDIT!!*/~%" ofile))
    ((base:renderer) (format "#define LIBSAGITTARIUS_BODY~%"))
    ((base:renderer) (format "#include <sagittarius.h>~%")))

  (define (is-ascii-symbol c)
    (char-set-contains? *c-delimiter-set* c)
    #;(or (and (char<=? #\! c)
	     (char<=? c #\/))
	(and (char<=? #\: c)
	     (char<=? c #\@))))

  (define (replace-string str compare char)
    (let ((len (string-length str))
	  (cs  (list->string `(,char))))
      (let loop ((i 0)
		 (r ""))
	(if (= i len)
	    r
	    (let ((c (string-ref str i)))
	      (cond ((compare c) 
		     (loop (+ i 1)
			   (string-append r cs)))
		    ((is-ascii-symbol c)
		     (loop (+ i 1)
			   (string-append r (format "~x" (char->integer c)))))
		    (else
		     (loop (+ i 1)
			   (string-append r
					  (list->string `(,c)))))))))))
  (define (space-or-bar? c)
    (or (char=? c #\space)
	(char=? c #\-)))
  (define (invalid-c-delim? c)
    (or (char=? c #\space)
	(char=? c #\-)
	(char=? c #\()
	(char=? c #\))))

  (define (resolve-name name type)
    (let ((base (string-append (replace-string (format "~s" *library-name*) invalid-c-delim? #\_)
			       (replace-string (symbol->string name) space-or-bar? #\_))))
      (case type
	((function) base)
	((stub)    (string-append base "_Stub"))
	((static)  (string-append "s_" base "_Stub"))
	(else error 'resolve-name "invalid type" type))))

  (define (resolve-argc args type)
    (define (type-dispatch count optional? opt)
      (case type
	((required) count)
	;((rest)     (if optional? 1 0))
	((optional) (cons optional? opt))
	((both)     (cons count (cons optional? opt)))
	(else (error 'resolve-argc "invalud type" type))))
    (let loop ((args args)
	       (count 0))
      (cond ((null? args) (type-dispatch count #f 0))
	    (else
	     (let ((c (car args)))
	       (cond ((eq? c ':rest)
		      (type-dispatch count 'rest 1))
		     ((eq? c ':optional)
		      ;; assume it's :optional value
		      ;; so get cddr
		      (type-dispatch count 'optional (length (cdr args))))
		     (else
		      (loop (cdr args) (+ count 1)))))))))

  (define (parse-args oargs)
    (define (check-name/type name/type var)
      (or (= (length name/type) 2)
	  (= (length name/type) 1)
	  (error 'parse-args (format "variable must be <name>::<type> or <name> but got ~s" var))))
    (define (get-type name/type)
      (if (= (length name/type) 1)
	  "Object"
	  (cadr name/type)))
    (let loop ((args oargs)
	       (r '()))
      (cond ((null? args) (reverse r))
	    ((eq? (car args) ':rest)
	     (let* ((rest (cadr args))
		    (name/type (string-split (symbol->string rest) c-identifier)))
	       (check-name/type name/type rest)
	       (let ((name (car name/type))
		     (type (get-type name/type)))
		 (reverse (acons type (list name) r)))))
	    ((eq? (car args) ':optional)
	     (let* ((opt (cdr args))
		   ;; construct (name type) alist
		    (name/types (map 
				 (lambda (name-type)
				   (cond ((symbol? name-type)
					  ;; no default value
					  (let ((name/type (string-split (symbol->string name-type) c-identifier)))
					    (check-name/type name/type name-type)
					    (cons (get-type name/type)
						  (list (car name/type)
							(if (string=? (get-type name/type) "Object")
							    'SG_UNBOUND
							    (begin
							      (base:warn (format "~a should have default value: ~s"
									    (get-type name/type)
									    oargs))
							      '()))))))
					 ((pair? name-type)
					  ;; with default value. cf) (c::fixnum 10)
					  (let ((name/type (string-split (symbol->string
									  (car name-type))
									 c-identifier)))
					    (check-name/type name/type name-type)
					    (cons (get-type name/type)
						  (cons (car name/type)
							(cdr name-type)))))))
				 opt)))
	       ;; :optional could have default value
	       (set! r (reverse r))
	       (set! r (append r name/types))
	       r))
	    (else
	     ;; TODO too naive
	     (let ((name/type (string-split (symbol->string (car args)) c-identifier)))
	       (check-name/type name/type (car args))
	       (let ((name (car name/type))
		     (type (get-type name/type)))
		 (loop (cdr args) (acons type (list name) r))))))))

  (define (resolve-args args type)
    (define (dispatch-type type/name index)
      (case (string->symbol (car type/name))
		  ((Object)
		   ((base:renderer) (format "  argumentRef(~a, ~a);~%" index (cadr type/name))))
		  ((fixnum boolean char number)
		   ((base:renderer) (format "  argumentAs~a(~a, ~a_scm, ~a);~%"
				       (string-titlecase (car type/name))
				       index
				       (cadr type/name)
				       (cadr type/name))))
		  (else
		   ((base:renderer) (format "  argumentAs~a(~a, ~a_scm, ~a);~%"
				       (car type/name)
				       index
				       (cadr type/name)
				       (cadr type/name))))))

    (let ((type/names (parse-args args)))
      (case type
      ((declare)
       (for-each (lambda (type/name)
		   (case (string->symbol (car type/name))
		     ((Object)
		      ((base:renderer) (format "  SgObject ~a;~%" (cadr type/name))))
		     ((fixnum boolean char)
		      ((base:renderer) (format "  SgObject ~a_scm;~%" (cadr type/name)))
		      ((base:renderer) (format "  int ~a;~%" (cadr type/name))))
		     ((number)
		      ((base:renderer) (format "  SgObject ~a_scm;~%" (cadr type/name)))
		      ((base:renderer) (format "  SgObject ~a;~%" (cadr type/name))))
		     (else
		      ((base:renderer) (format "  SgObject ~a_scm;~%" (cadr type/name)))
		      ((base:renderer) (format "  Sg~a *~a;~%" (car type/name) (cadr type/name))))))
		 type/names))
      ((check)
       ;; resolve-argc with 'both option returns 
       ;; (required-argc (optional-tag optional-count))
       (let* ((required/optional? (resolve-argc args 'both))
	      (required (car required/optional?))
	      (optional? (cdr required/optional?)))
	 ;; if it's just optional?
	 ;; we don't have to check length
	 (cond ((and (> required 0)
		     (eq? (car optional?) 'rest))
		((base:renderer) (format "  checkArgumentLengthAtLeast(~a);~%" required)))
	       ((eq? (car optional?) 'optional)
		((base:renderer) (format "  checkArgumentLengthBetween(~a, ~a);~%" required (+ required (cdr optional?)))))
	       ((not (car optional?))
		((base:renderer) (format "  checkArgumentLength(~a);~%" required))))
	 (for-each1-with-index
	  (lambda (index type/name)
	    (if (< index required)
		(dispatch-type type/name index)
		;; must be optional
		(cond ((eq? (car optional?) 'rest)
		       ((base:renderer) (format "  retrieveOptionalArguments(~a, ~a);~%" index (cadr type/name))))
		      ((eq? (car optional?) 'optional)
		       (let* ((count (cdr optional?))
			      (default (cddr type/name)))
			 ((base:renderer) (format "  if (argc >= ~a) {~%" (+ index 1)))
			 ((base:renderer) "  ")
			 (dispatch-type type/name index)
			 ((base:renderer) "  }")
			 (unless (null? default)
			   ((base:renderer) (format " else {~%"))
			   ((base:renderer) (format "    ~a = " (cadr type/name)))
			   (base:dispatch (caddr type/name))
			   ((base:renderer) (format ";~%"))
			   ((base:renderer) (format "  }~%")))
			 ((base:renderer) "\n")))
		      (else
		       (error 'resolve-args "wrong format for :optional/:rest keyword")))))
	  type/names)))
      (else
       (error 'resolve-args "invalid type" type)))))

  (define (resolve-return return)
    (let* ((s-return (format ":~s" return))
	   (tokens (string-split s-return)))
      (or (= (length tokens) 1)
	  (error 'resolve-return (format "return type must be ::<type> but got ~s" return)))
      (string->symbol (car tokens))))

  (define (resolve-c-body body type/names return)
    (define (return-type)
      (case return
	((Object void) 
	 ((base:renderer) (format "SgObject SG_RETURN = SG_UNDEF;~%")))
	((boolean fixnum char)
	 ((base:renderer) (format "int SG_RETURN;~%")))
	(else (error 'resolve-c-body "invalid return type"  return))))
    (define (return-value)
      (case return
	((Object) 'SG_RETURN)
	((void)   'SG_RETURN)
	((boolean) "SG_MAKE_BOOL(SG_RETURN)")
	((fixnum) "SG_MAKE_INT(SG_RETURN)")
	((char)  "SG_MAKE_CHAR(SG_RETURN)")
	(else (error 'resolve-c-body "invalid return type"  return))))
    (renderer-no-indent #f)
    (renderer-indent-incl!)
    ((base:renderer) (format "{~%"))
    (renderer-indent-incl!)
    (return-type)
    (base:dispatch-method `(begin ,@body) base:dispatch-method (lambda (k) k))
    ((base:renderer) (format "return ~a;~%" (return-value)))
    (renderer-indent-decl!)
    ((base:renderer) (format "}~%"))
    (renderer-indent-decl!))

  (define (resolve-decl decl-body)
    (for-each (lambda (body)
		(let ((key (car body)))
		  (case key
		    ((.include)
		     (let loop ((files (cdr body)))
		       (unless (null? files)
			 ((base:renderer) (format "#include ~s~%" (car files)))
			 (loop (cdr files))))))))
	      decl-body))

  (define (resolve-body)
    (base:dispatch-method `(begin ,@*c-proc-defs*) base:dispatch-method (lambda (k) k)))

  (define (add-toplevel-variable! name stub)
    (set! *toplevel-variables* (acons name stub *toplevel-variables*)))

  (define (def-c-proc body dispatch k)
    (define (body-impl name args inliner return c-body)
      ((base:renderer) (format "static SgObject ~a(SgObject *args, int argc, void *data_)~%"
			       (resolve-name name 'function)))
      ((base:renderer) (format "{~%"))
      (let ((type/names (parse-args args)))
	(resolve-args args 'declare)
	((base:renderer) (format "  DeclareProcedureName(~s);~%" (symbol->string name)))
	(resolve-args args 'check)
	(resolve-c-body c-body type/names (resolve-return return)))
      ((base:renderer) (format "}~%"))
      ((base:renderer) (format "static SG_DEFINE_SUBR(~a, ~a, ~a, ~a, ~a, NULL);~%~%"
			       (resolve-name name 'stub)
			       (resolve-argc args 'required)
			       (cdr (resolve-argc args 'optional))
			       (resolve-name name 'function)
			       (if inliner
				   (format "SG_MAKE_INT(~a)" inliner)
				   'SG_FALSE)))
      (hashtable-set! *procedure-map* name (resolve-name name 'function))
      (hashtable-set! *procedure-subr-map* name (resolve-name name 'stub)))
    (match body
      (('define-c-proc name args ('inline insn) return . c-body)
       (body-impl name args insn return c-body))
      (('define-c-proc name args return . c-body)
       (body-impl name args #f return c-body))))
  
  (define (set-toplevel-variable! body dispatch k)
    (or (= (length body) 3)
	(error 'set-toplevel-variable! 
	       (format "set-toplevel-variable! requires 2 arguments, but got ~a" (length (cdr body)))
	       body))
    (let ((name (cadr body))
	  (value (caddr body)))
      (add-toplevel-variable! name (resolve-name value 'stub))))


  (define (generate-init)
    (define (generate-name)
      (cond ((symbol? *library-name*) *library-name*)
	    ((pair? *library-name*)
	     (let loop ((name *library-name*)
			(r ""))
	       (if (null? name)
		   r
		   (loop (cdr name)
			 (string-append r "_" (symbol->string (car name)))))))
	    (else (error 'generate-init "invalid library name" 
			 *library-name*))))

    ((base:renderer) (format "void Sg__Init~a()~%" (generate-name)))
    ((base:renderer) (format "{~%"))
    ((base:renderer) (format "  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC(\"~s\"), SG_LITERAL_STRING)), TRUE);~%"
			*library-name*))
    (for-each
     (lambda (proc-name)
       (let ((subr-name (hashtable-ref *procedure-subr-map* proc-name #f)))
	 ((base:renderer) (format "  SG_PROCEDURE_NAME(&~a) = Sg_MakeString(UC(\"~a\"), SG_LITERAL_STRING);~%" subr-name proc-name))
	 ((base:renderer) (format "  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(\"~a\"), SG_LITERAL_STRING)), SG_OBJ(&~a));~%"
			     proc-name subr-name))))
     (vector->list (hashtable-keys *procedure-subr-map*)))
    (for-each
     (lambda (p)
       (let ((name (car p))
	     (value (cdr p)))
	 ((base:renderer) (format "  Sg_VMSetToplevelVariable(SG_INTERN(\"~a\"), &~a);~%" name value))))
     *toplevel-variables*)
    ((base:renderer) (format "}~%")))

  ;; entry point and only exported method
  (define (cgen file)
    (let* ((ext (file-extention file))
	   (body (file-name-without-extention file))
	   (out (string-append body ".c"))
	   (exit? #f))
      (base:init)
      (init)
      (if (file-exists? out)
	  (let ((stub-mtime (file-stat-mtime file))
		(out-mtime (file-stat-mtime out)))
	    (if (> stub-mtime out-mtime)
		(delete-file out)
		(begin
		  (base:warn "generated file is older than stub file. exit.")
		  (set! exit? #t)))))
      (unless exit?
	(with-output-to-file out
	  (lambda ()
	    (with-input-from-file file
	      (lambda ()
		(pre-resolve)
		(write-header file)
		(resolve-body)
		(generate-init))))))))
  )
