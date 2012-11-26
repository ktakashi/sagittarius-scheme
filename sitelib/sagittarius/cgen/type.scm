;; -*- mode: scheme; coding: utf-8; -*-
#!compatible
#!read-macro=sagittarius/regex
(library (sagittarius cgen type)
    (export <cgen-type> cgen-type-from-name make-cgen-type
	    cgen-box-expr cgen-unbox-expr cgen-pred-expr
	    cgen-return-stmt
	    lookup-default-c-pred)
    (import (except (rnrs) define)
	    (sagittarius)
	    (sagittarius regex)
	    (rename (sagittarius control) (define-with-key define))
	    (srfi :2 and-let*)
	    (except (srfi :13 strings) string-upcase)
	    (srfi :14 char-sets)
	    (srfi :26 cut)
	    (clos user))

  (define-class <cgen-type> ()
    ((name     	  :init-keyword :name)
     (c-type   	  :init-keyword :c-type)
     (description :init-keyword :description)
     (c-predicate :init-keyword :c-predicate)
     (unboxer     :init-keyword :unboxer)
     (boxer       :init-keyword :boxer :init-value "SG_OBJ_SAFE")
     (maybe       :init-keyword :maybe :init-value #f)))

  (define *instance-pool* '())

  (define (cgen-type-from-name name)
    (or (find (lambda (type) (eq? (slot-ref type 'name) name))
	      *instance-pool*)
	(and-let* ((m (looking-at #/\?$/ (symbol->string name)))
		   (basename (string->symbol (m 'before)))
		   (basetype (cgen-type-from-name basename)))
	  (make <cgen-type> :name name :c-type (slot-ref basetype 'c-type)
		:description (format "~a or #f" (slot-ref basetype'description))
		:c-predicate (slot-ref basetype 'c-predicate)
		:unboxer     (slot-ref basetype 'unboxer)
		:boxer       (slot-ref basetype 'boxer)
		:maybe       basetype))))

  (define (make-cgen-type name c-type :optional (desc #f) (c-pred #f)
			  (unbox #f) (box #f))
    (define (strip<> name) (string-trim-both name (string->char-set "<>")))
    (define (replace- name)
      (regex-replace-all #/-/ (string-upcase name) "_"))
    (define (default-cpred name)
      (if (looking-at #/-/ name)
	  (string-append "SG_"
			 (replace- (strip<> name))
			 "_P")
	  (string-append "SG_" (string-upcase (strip<> name)) "P")))
    (define (default-unbox name)
      (string-append "SG_" (replace- (strip<> name))))
    (let* ((name-s (symbol->string name))
	   (type (make <cgen-type>
		   :name name :c-type c-type
		   :description (or desc name-s)
		   :c-predicate (or c-pred (default-cpred name-s))
		   :unboxer     (or unbox (default-unbox name-s))
		   :boxer       (or box "SG_OBJ_SAFE"))))
      ;; maybe we want an  instance pool
      (set! *instance-pool* (cons type *instance-pool*))))

  (for-each
   (cut apply make-cgen-type <>)
   '(;; numeric
     (<fixnum>  "long" "fixnum" "SG_INTP" "SG_INT_VALUE" "SG_MAKE_INT")
     (<integer> "SgObject" "exact integer" "Sg_IntegerP" "")
     (<number> "SgObject" "number" "SG_NUMBERP" "")
     ;; immediates
     (<boolean> "int" "boolean" "SG_BOOLP" "SG_BOOL_VALUE" "SG_MAKE_BOOL")
     (<char>    "SgChar" "character" "SG_CHARP" "SG_CHAR_VALUE" "SG_MAKE_CHAR")
     (<void>    "void" "void" "" "" "SG_VOID_RETURN_VALUE")
     (<top>     "SgObject" "scheme object" "" "")
     ;; scheme types
     (<pair> "SgPair*" "pair" "SG_PAIRP" "SG_PAIR" "SG_OBJ")
     (<list> "SgObject" "list" "SG_LISTP" "")
     (<vector> "SgVector*" "vector" "SG_VECTORP" "SG_VECTOR")
     (<bytevector> "SgByteVector*" "bytevector" "SG_BVECTORP" "SG_BVECTOR")
     (<string> "SgString*" "string" "SG_STRINGP" "SG_STRING")
     (<symbol> "SgSymbol*" "symbol" "SG_SYMBOLP" "SG_SYMBOL")
     (<keyword> "SgKeyword*" "keyword" "SG_KEYWORDP" "SG_KEYWORD")
     (<identifier> "SgIdentifier*" "identifier" "SG_IDENTIFIERP"
		   "SG_IDENTIFIER")
     (<syntax> "SgSyntax*" "syntax" "SG_SYNTAXP" "SG_SYNTAX")
     (<macro> "SgMacro*" "macro" "SG_MACROP" "SG_MACRO")
     (<gloc> "SgGloc*" "gloc" "SG_GLOCP" "SG_GLOC")
     (<library> "SgLibrary*" "library" "SG_LIBRARYP" "SG_LIBRARY")
     (<record-type> "SgRecordType*" "record-type" "SG_RECORD_TYPEP"
		    "SG_RECORD_TYPE")
     (<rtd> "SgRTD*" "record-type-descriptor" "SG_RTDP" "SG_RTD")
     (<rcd> "SgRCD*" "record-constructor-descriptor" "SG_RCDP" "SG_RCD")
     (<char-set> "SgCharSet*" "char-set" "SG_CHAR_SET_P" "SG_CHAR_SET")
     (<codec> "SgCodec*" "codec" "SG_CODECP" "SG_CODEC")
     (<transcoder> "SgTranscoder*" "transcoder" "SG_TRANSCODERP"
		   "SG_TRANSCODER")
     (<port> "SgPort*" "port" "SG_PORTP" "SG_PORT")
     (<input-port> "SgPort*" "input port" "SG_INPORTP" "SG_PORT")
     (<output-port> "SgPort*" "input port" "SG_OUTPORTP" "SG_PORT")
     (<input/output-port> "SgPort*" "input/output port" "SG_INOUTPORTP"
			  "SG_PORT")
     (<binary-port> "SgPort*" "binary port" "SG_BINARY_PORTP" "SG_PORT")
     (<textual-port> "SgPort*" "textual port" "SG_TEXTUAL_PORTP" "SG_PORT")
     (<procedure> "SgProcedure*" "procedure" "SG_PROCEDUREP" "SG_PROCEDURE")
     (<hashtable> "SgHashTable*" "hashtable" "SG_HASHTABLE_P" "SG_HASHTABLE")
     (<tree-map> "SgTreeMap*" "tree-map" "SG_TREEMAP_P" "SG_TREEMAP")
     ;; regex
     (<pattern> "SgPattern*" "pattern" "SG_PATTERNP" "SG_PATTERN")
     (<matcher> "SgMatcher*" "matcher" "SG_MATCHERP" "SG_MATCHER")
     ;; clos
     (<class> "SgClass*" "class" "SG_CLASSP" "SG_CLASS")
     (<method> "SgMethod*" "method" "SG_METHODP" "SG_METHOD")
     (<generic> "SgGeneric*" "generic" "SG_GENERICP" "SG_GENERIC")
     (<slot-accessor> "SgSlotAccessor*" "slot-accessor" "SG_SLOT_ACCESSORP"
		      "SG_SLOT_ACCESSOR")
     ;; thread, no mutex nor condition variable
     (<thread> "SgVM*" "thread" "SG_VMP" "SG_VM")
     ;; weak
     (<weak-vector> "SgWeakVector*" "weak vector" "SG_WEAK_VECTOR_P"
		    "SG_WEAK_VECTOR")
     (<weak-hashtable> "SgWeakHashTable*" "weak hashtable" "SG_WEAK_HASHTABLE_P"
		    "SG_WEAK_HASHTABLE")
     ;; for compiler
     (<code-builder> "SgCodeBuilder*" "code-builder" "SG_CODE_BUILDERP"
		     "SG_CODE_BUILDER")
     ))

  (define (cgen-box-expr type c-expr)
    (if (slot-ref type 'maybe)
	(format "SG_MAKE_MAYBE(~a, ~a)" (slot-ref type 'boxer) c-expr)
	(format "~a(~a)" (slot-ref type 'boxer) c-expr)))

  (define (cgen-unbox-expr type c-expr)
    (if (slot-ref type 'maybe)
	(format "SG_MAYBE(~a, ~a)" (slot-ref type 'unboxer) c-expr)
	(format "~a(~a)" (slot-ref type 'unboxer) c-expr)))

  (define (cgen-pred-expr type c-expr)
    (if (slot-ref type 'maybe)
	(format "SG_MAYBE_P(~a, ~a)" (slot-ref type 'c-predicate) c-expr)
	(format "~a(~a)" (slot-ref type 'c-predicate) c-expr)))
  (define (cgen-return-stmt expr)
    (format "SG_RETURN(~a);"  expr))

  (define (lookup-default-c-pred arg)
    (let ((boxer (slot-ref arg'boxer)))
      (let loop ((types *instance-pool*))
	(cond ((null? types) #f)
	      ((string=? boxer (slot-ref (car types) 'boxer))
	       (slot-ref (car types) 'c-predicate))
	      (else (loop (cdr types)))))))

)