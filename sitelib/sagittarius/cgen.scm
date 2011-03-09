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
    (export cgen define-cgen-stmt define-cgen-macro register-macro!
	    dispatch)
    (import (rnrs (6))
	    (rnrs eval (6))
	    (only (srfi :13) string-index string-index-right string-map)
	    (sagittarius format)
	    (sagittarius cgen util)
	    (prefix (sagittarius cgen base) base:)
	    (match))
  ;; this must be (file) library but for now.
  (define (file-extention file)
    (let ((dot-index (string-index-right file #\.)))
      (substring file dot-index (string-length file))))
  (define (file-name-without-extention file)
    (let ((dot-index (string-index-right file #\.)))
      (substring file 0 dot-index)))

  ;; TODO remove
  (define-syntax acons
    (syntax-rules ()
      ((_ a b alist)
       (cons (cons a b) alist))))

  (define (warn msg)
    (display msg (current-error-port))
    (newline (current-error-port)))

  ;; assume this library exports only cgen
  ;; so these properties are working like private properties.
  ;; I hope!
  (define *dispatch-table* #f)
  (define *library-name* #f)
  (define *import-spec* #f)
  (define *export-spec* #f)
  (define *c-proc-defs* #f)
  (define *procedure-subr-map* #f)
  (define *procedure-map* #f)

  (define (init) 
    (set! *dispatch-table* (make-eq-hashtable))
    (set! *library-name* #f)
    (set! *import-spec* #f)
    (set! *export-spec* #f)
    (set! *c-proc-defs* #f)
    (set! *procedure-subr-map* (make-eq-hashtable))
    (set! *procedure-map* (make-eq-hashtable)))

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
    (format #t "/* This file is autmatically generated from ~s. DO NOT EDIT!!*/~%" ofile)
    (format #t "#define LIBSAGITTARIUS_BODY~%")
    (format #t "#include <sagittarius.h>~%"))

  (define (is-ascii-symbol c)
    (or (and (char<=? #\! c)
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
							    'SG_UNDEF
							    (begin
							      (warn (format "~a should have default value: ~s"
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
		   (format #t "  argumentRef(~a, ~a);~%" index (cadr type/name)))
		  ((fixnum boolean char number)
		   (format #t "  argumentAs~a(~a, ~a_scm, ~a);~%"
			   (string-titlecase (car type/name))
			   index
			   (cadr type/name)
			   (cadr type/name)))
		  (else
		   (format #t "  argumentAs~a(~a, ~a_scm, ~a);~%"
			   (car type/name)
			   index
			   (cadr type/name)
			   (cadr type/name)))))

    (let ((type/names (parse-args args)))
      (case type
      ((declare)
       (for-each (lambda (type/name)
		   (case (string->symbol (car type/name))
		     ((Object)
		      (format #t "  SgObject ~a;~%" (cadr type/name)))
		     ((fixnum boolean char)
		      (format #t "  SgObject ~a_scm;~%" (cadr type/name))
		      (format #t "  int ~a;~%" (cadr type/name)))
		     ((number)
		      (format #t "  SgObject ~a_scm;~%" (cadr type/name))
		      (format #t "  SgObject ~a;~%" (cadr type/name)))
		     (else
		      (format #t "  SgObject ~a_scm;~%" (cadr type/name))
		      (format #t "  Sg~a *~a;~%" (car type/name) (cadr type/name)))))
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
		(format #t "  checkArgumentLengthAtLeast(~a);~%" required))
	       ((eq? (car optional?) 'optional)
		(format #t "  checkArgumentLengthBetween(~a, ~a);~%" required (+ required (cdr optional?))))
	       ((not (car optional?))
		(format #t "  checkArgumentLength(~a);~%" required)))
	 (for-each1-with-index
	  (lambda (index type/name)
	    (if (< index required)
		(dispatch-type type/name index)
		;; must be optional
		(cond ((eq? (car optional?) 'rest)
		       (format #t "  retrieveOptionalArguments(~a, ~a);~%" index (cadr type/name)))
		      ((eq? (car optional?) 'optional)
		       (let* ((count (cdr optional?))
			      (default (cddr type/name)))
			 (format #t "  if (argc >= ~a) {~%" (+ index 1))
			 (display "  ")
			 (dispatch-type type/name index)
			 (format #t "  }")
			 (unless (null? default)
			   (format #t " else {~%")
			   (format #t "    ~a = " (cadr type/name))
			   (dispatch (caddr type/name))
			   (format #t ";~%")
			   (format #t "  }~%"))
			 (newline)))
		      (else
		       (error 'resolve-args "wrong format for :optional/:rest keyword")))))
	  type/names)))
      (else
       (error 'resolve-args "invalid type" type)))))

  (define (resolve-return return)
    (let* ((s-return (symbol->string return))
	   (tokens (string-split s-return)))
      (or (= (length tokens) 1)
	  (error 'resolve-return (format "return type must be ::<type> but got ~s" return)))
      (string->symbol (car tokens))))

  ;; generic method
  ;; assume if there is no registered dispatcher
  ;; it's function or macro call.
  (define (resolve-call body dispatch k)
    (let ((name (hashtable-ref *procedure-map* (car body) (car body))))
      (format #t "~a(" name)
      (let loop ((args (cdr body))
		 (i 0))
	(unless (null? args)
	  (unless (zero? i)
	    (display ", "))
	  (dispatch (car args) dispatch k)
	  (loop (cdr args) (+ i 1))))
      (format #t ")")))

  ;; for define-cgen-stmt and macro
  (define (dispatch body)
    (dispatch-method body dispatch-method (lambda (k) k)))

  (define (dispatch-method body dispatch k)
    (cond ((pair? body)
	   (let* ((head   (car body))
		  (method (hashtable-ref *dispatch-table* head resolve-call)))
	     (method body dispatch k)))
	  ((boolean? body)
	   (format #t "SG_MAKE_BOOL(~s)" (if body 'TRUE 'FALSE)))
	  ((string? body)
	   (format #t "UC(~s)" body))
	  (else
	   (format #t "~s" body))))


  (define (resolve-c-body body type/names return)
    (define (return-type)
      (case return
	((Object void) 
	 (format #t "SgObject SG_RETURN = SG_UNDEF;"))
	((boolean fixnum char)
	 (format #t "int SG_RETURN;"))
	(else (error 'resolve-c-body "invalid return type"  return))))
    (define (return-value)
      (case return
	((Object) 'SG_RETURN)
	((void)   'SG_RETURN)
	((boolean) "SG_MAKE_BOOL(SG_RETURN)")
	((fixnum) "SG_MAKE_INT(SG_RETURN)")
	((char)  "SG_MAKE_CHAR(SG_RETURN)")
	(else (error 'resolve-c-body "invalid return type"  return))))

    (format #t "  {~%")
    (return-type)
    ;;(format #t "    ~s SG_RETURN = SG_UNDEF;~%" (return-type))
    (for-each (lambda (body)
		(dispatch-method body dispatch-method (lambda (k) k))
		(display ";")(newline))
	      body)
    (format #t "    return ~a;~%" (return-value))
    (format #t "  }~%"))

  (define (resolve-decl decl-body)
    (for-each (lambda (body)
		(let ((key (car body)))
		  (case key
		    ((.include)
		     (let loop ((files (cdr body)))
		       (unless (null? files)
			 (format #t "#include ~s~%" (car files))
			 (loop (cdr files))))))))
	      decl-body))

  ;; plugin
  (define (register-macro! name expander)
    (hashtable-set! *dispatch-table* name expander))
  
  (define (cgen-lookup-macro name)
    (hashtable-ref *dispatch-table* name #f))

  (define-syntax define-cgen-macro
    (syntax-rules ()
      ((_ (op form) . body)
       (register-macro! 'op (lambda (form dispatch k) . body)))
      ((_ op op2)
       (register-macro! 'op (or (cgen-lookup-macro 'op2)
				(error 'define-cgen-macro "unknown cgen macro" 'op2))))))

  (define-syntax define-cgen-stmt
    (syntax-rules ()
      ;; recursion
      ((_ "clauses" op clauses ("where" defs ...))
       (define-cgen-macro (op form)
	 defs ...
	 (match form . clauses)))
      ((_ "clauses" op clauses ())
       (define-cgen-stmt "clauses" op clauses ("where")))
      ((_ "clauses" op (clause ...) (x . y))
       (define-cgen-stmt "clauses" op (clause ... x) y))
      ;; entry
      ((_ (op . args) . body)		; single pattern case
       (define-cgen-stmt "clauses" op (((_ . args) . body)) ()))
      ((_ op (pat . body) . clauses)	; (pat . body) rules out a single symbol
       (define-cgen-stmt "clauses" op ((pat . body)) clauses))
      ((_ op . clauses)
       (define-cgen-stmt "clauses" op () clauses))))

  (define (resolve-body)
    (define (body-impl name args inliner return c-body)
      (format #t "static SgObject ~a(SgObject *args, int argc, void *data_)~%"
	      (resolve-name name 'function))
      (format #t "{~%")
      (format #t "  DeclareProcedureName(~s);~%" (symbol->string name))
      (let ((type/names (parse-args args)))
	(resolve-args args 'declare)
	(resolve-args args 'check)
	(resolve-c-body c-body type/names (resolve-return return)))
      
      (format #t "}~%")
      (format #t "static SG_DEFINE_SUBR(~a, ~a, ~a, ~a, ~a, NULL);~%~%"
	      (resolve-name name 'stub)
	      (resolve-argc args 'required)
	      (cdr (resolve-argc args 'optional))
	      (resolve-name name 'function)
	      (if inliner
		  (format "SG_MAKE_INT(~a)" inliner)
		  'SG_FALSE))
      (hashtable-set! *procedure-map* name (resolve-name name 'function))
      (hashtable-set! *procedure-subr-map* name (resolve-name name 'stub)))
    (for-each (lambda (body)
		(match body
		  (('decl-code . decl-body)
		   (resolve-decl decl-body))
		  (('define-c-proc name args ('inline insn) return . c-body)
		   (body-impl name args insn return c-body))
		  (('define-c-proc name args return . c-body)
		   (body-impl name args #f return c-body))
		  (('define-cgen-stmt name . form)
		   (eval body (environment '(rnrs (6))
					   '(sagittarius cgen)
					   '(sagittarius format))))))
	      *c-proc-defs*))
  
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

    (format #t "void Sg__Init~a()~%" (generate-name))
    (format #t "{~%")
    (format #t "  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC(\"~s\"), SG_LITERAL_STRING)), TRUE);~%"
	    *library-name*)
    (for-each
     (lambda (proc-name)
       (let ((subr-name (hashtable-ref *procedure-subr-map* proc-name #f)))
	 (format #t "  SG_PROCEDURE_NAME(&~a) = Sg_MakeString(UC(\"~a\"), SG_LITERAL_STRING);~%" subr-name proc-name)
	 (format #t "  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(\"~a\"), SG_LITERAL_STRING)), SG_OBJ(&~a));~%"
		 proc-name subr-name)))
     (vector->list (hashtable-keys *procedure-subr-map*)))
    (format #t "}~%"))

  ;; entry point and only exported method
  (define (cgen file)
    (let* ((ext (file-extention file))
	   (body (file-name-without-extention file))
	   (out (string-append body ".c")))
      (init)
      (if (file-exists? out)
	  (delete-file out))
      (base:init *dispatch-table*)
      (with-output-to-file out
	(lambda ()
	  (with-input-from-file file
	    (lambda ()
	      (pre-resolve)
	      (write-header file)
	      (resolve-body)
	      (generate-init)))))))
  )
