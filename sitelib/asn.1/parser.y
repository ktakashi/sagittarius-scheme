;; -*- mode: scheme; coding: utf-8; -*-
(import (lalr))

(define asn.1-parser
  (lalr-parser
   (:output asn.1-parser "parser.scm")
   (:out-table "parser.out")
   (:expect 5)
   
   (:library (asn.1 parser))
   (:import (asn.1 types) (srfi :1 lists))

   ;; token definitions
   (WORD CLASS SEQUENCE SET CHOICE OF IMPLICIT EXPLICIT OPTIONAL LBRACE RBRACE
	 COMMA ANY ASSIGN NUMBER ENUM COMPONENTS POSTRBRACE DEFINED BY)
   ;; rules
   (top (slist)  : (list (cons '() $1))
	(module)
	)

   (module (WORD ASSIGN aitem)        : (list (cons $1 (list $3)))
	   (module WORD ASSIGN aitem) : (cond ((assoc $2 $1)
					       => (lambda (slot)
						    (set-cdr! slot (list $4))
						    $1))
					      (else (append! $1 (list (list $2 $4)))))
	   )

   (aitem (class plicit anyelem postrb) : (begin 
					    (asn.1-type-tag-set! $3 $1)
					    (if $2 (tag-explicit! $3) $3))
	  (celem)
	  )

   (anyelem (onelem)
	    (eelem)
	    (oelem)
	    (selem)
	    )

   (celem (COMPONENTS OF WORD) : (make-asn.1-type :type $1 :child $3)
	  )

   (seqset (SEQUENCE)
	   (SET)
	   )

   (selem (seqset OF class plicit sselem optional)
	  : (begin
	      (asn.1-type-tag-set! $5 $3)
	      (let ((ret (make-asn.1-type :type $1 :child (list $5)
					  :loop #t :optional $6)))
		(if $4 (tag-explicit! ret) ret)))
	  )

   (sselem (eelem)
	   (oelem)
	   (onelem)
	   )

   (onelem (SEQUENCE LBRACE slist RBRACE) : (make-asn.1-type :type $1 :child $3)
	   (SET      LBRACE slist RBRACE) : (make-asn.1-type :type $1 :child $3)
	   (CHOICE   LBRACE nlist RBRACE) : (make-asn.1-type :type $1 :child $3)
	   )

   (eelem (ENUM LBRACE elist RBRACE) : (make-asn.1-type :type $1))

   (oielem (WORD)     	 : (make-asn.1-type :type $1)
	   (SEQUENCE) 	 : (make-asn.1-type :type $1)
	   (SET)      	 : (make-asn.1-type :type $1)
	   (ANY defined) : (make-asn.1-type :type $1 :child #f :defined-by $2)
	   (ENUM)        : (make-asn.1-type :type $1)
	   )

   (defined ()                : '()
            (DEFINED BY WORD) : $3)

   (oelem (oielem)
	  )

   (nlist (nlist1)            : $1
	  (nlist1 POSTRBRACE) : $1
	  )

   (nlist1 (nitem)                   : (list $1)
	   (nlist1 POSTRBRACE nitem) : (append! $1 (list $3))
	   (nlist1 COMMA nitem)      : (append! $1 (list $3))
	   )

   (nitem (WORD class plicit anyelem)
	  : (let ((ret $4))
	      (asn.1-type-name-set! ret $1)
	      (asn.1-type-tag-set! ret $2)
	      (if $3 (tag-explicit! ret) ret))
	  )

   (slist ()                  : '()
	  (slist1)            : $1
	  (slist1 POSTRBRACE) : $1
	  )

   (slist1 (sitem)                   : (list $1)
	   (slist1 COMMA sitem)      : (append! $1 (list $3))
	   (slist1 POSTRBRACE sitem) : (append! $1 (list $3))
	   )

   (snitem (oelem optional) : (begin (asn.1-type-optional-set! $1 $2) $1)
	   (eelem)
	   (selem)
	   (onelem)
	   )

   (sitem (WORD class plicit snitem)
	  : (let ((ret $4))
	      (asn.1-type-name-set! ret $1)
	      (asn.1-type-tag-set! ret $2)
	      (if $3 (tag-explicit! ret) ret))
	  (celem)
	  (class plicit onelem)
	  : (let ((ret $3))
	      (asn.1-type-tag-set! $1)
	      (if $2 (tag-explicit! ret) ret))
	  )

   (optional ()         : #f
	     (OPTIONAL) : #t
	     )

   (class ()      : #f
	  (CLASS)
	  )

   (plicit ()         : #f
	   (EXPLICIT) : #t
	   (IMPLICIT) : #f
	   )
   ;; TODO implement enum
   (elist (eitem)
	  (elist COMMA eitem)
	  )
   (eitem (WORD NUMBER)
	  )
   (postrb ()
	   (POSTRBRACE)
	   )
   ))
   
	   