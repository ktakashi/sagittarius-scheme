;; -*- Scheme -*-

(cond-expand
 (sagittarius
  (import (rnrs)))
 (else #t))

(define c-file   "../../src/builtin-keywords.c")
(define c-header "../../src/sagittarius/private/builtin-keywords.h")

(define (header out)
  (format out "/* This file is automatically generated by builtin-keywords.scm. DO NOT EDIT! */~%")
  (format out "#ifndef BUILTIN_PRIVATE_KEYWORD_H__~%")
  (format out "#define BUILTIN_PRIVATE_KEYWORD_H__~%")
  (format out "SG_EXTERN SgKeyword Sg_BuiltinKeywords[];~%")
  (let loop ((keywords keywords)
	     (i 0))
    (unless (null? keywords)
      (let ((p (car keywords)))
	(let ((def (cadr p)))
	  (format out "#define ~s SG_OBJ(&Sg_BuiltinKeywords[~a])~%" def i)))
      (loop (cdr keywords) (+ i 1))))
  (format out "#endif /* BUILTIN_PRIVATE_KEYWORD_H__ */~%"))

(define (body out)
  (format out "/* This file is automatically generated by builtin-keywords.scm. DO NOT EDIT! */~%")
  (format out "#define LIBSAGITTARIUS_BODY~%")
  (format out "#include \"sagittarius/private.h\"~%")
  (format out "SgKeyword Sg_BuiltinKeywords[] = {~%")
  (format out "#define ENTRY() {{SG_CLASS2TAG(SG_CLASS_KEYWORD)}, NULL}~%")
  (let loop ((keywords keywords))
    (unless (null? keywords)
      (format out "  ENTRY(),~%")
      (loop (cdr keywords))))
  (format out "#undef ENTRY~%")
  (format out "};~%")
  (format out "static void init_builtin_keywords()~%")
  (format out "{~%")
  (format out "#define STRING(s)    SG_MAKE_STRING(s)~%")
  (format out "#define INTERN(s, i) \\~%")
  (format out "  Sg_BuiltinKeywords[i].name = STRING(s); \\~%")
  (format out "  Sg_HashTableSet(keywords.table, Sg_BuiltinKeywords[i].name, SG_OBJ(&Sg_BuiltinKeywords[i]), 0)~%")
  (let loop ((keywords keywords)
	     (i 0))
    (unless (null? keywords)
      (format out "  INTERN(\"~a\", ~a);~%" (caar keywords) i)
      (loop (cdr keywords) (+ i 1))))
  (format out "#undef INTERN~%")
  (format out "}~%"))
  
(define (generate file-out header-out)
  (header header-out)
  (body   file-out))
  
(define (main args)
  (cond-expand
   (sagittarius
    (print "generating builtin symbold")
    (if (file-exists? c-header)
	(delete-file c-header))
    (if (file-exists? c-file)
	(delete-file c-file)))
   (else #t))
  (let ((c-file-out (open-output-file c-file))
	(c-header-out (open-output-file c-header)))
    (generate c-file-out c-header-out)
    (close-output-port c-file-out)
    (close-output-port c-header-out)))

(define keywords
  '((:lambda-list     SG_KEYWORD_LAMBDA_LIST)
    (:qualifier       SG_KEYWORD_QUALIFIER)
    (:specializers    SG_KEYWORD_SPECIALIZERS)
    (:generic         SG_KEYWORD_GENERIC)
    (:procedure       SG_KEYWORD_PROCEDURE)
    (:definition-name SG_KEYWORD_DEFINITION_NAME)
    (:primary         SG_KEYWORD_PRIMARY)
    (:before          SG_KEYWORD_BEFORE)
    (:after           SG_KEYWORD_AFTER)
    (:around          SG_KEYWORD_AROUND)
    (:init-value      SG_KEYWORD_INIT_VALUE)
    (:init-keyword    SG_KEYWORD_INIT_KEYWORD)
    (:init-thunk      SG_KEYWORD_INIT_THUNK)
    ;; for export
    (:all             SG_KEYWORD_ALL)
    (:export-reader-macro SG_KEYWORD_EXPORT_READER_MACRO)
    (:export-reader   SG_KEYWORD_EXPORT_READER)
    (:pipe            SG_KEYWORD_PIPE)
    (:stdin           SG_KEYWORD_STDIN)
    (:stdout          SG_KEYWORD_STDOUT)
    (:stderr          SG_KEYWORD_STDERR)
    (:null            SG_KEYWORD_NULL)
    ))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End: