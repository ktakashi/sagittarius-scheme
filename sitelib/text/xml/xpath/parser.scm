;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/parser.scm - XPath parser
;;;
;;;   Copyright (c) 2019  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; XPath 3.1
;; reference
;; - https://www.w3.org/TR/2017/REC-xpath-31-20170321/
;;   A XPath 3.1 Grammer - A.1 EBNF
#!nounbound
(library (text xml xpath parser)
    (export $xpath:xpath
	    ;; for testing (for now, may export more for convenience but later)
	    $xpath:expr-single
	    $xpath:for-expr
	    $xpath:let-expr
	    $xpath:item-type
	    )
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    ;; need number of xml parsers
	    (text xml dom parser))
(define w* ($many $xml:s))
(define w+ ($many $xml:s 1))
(define (ws** p) ($do w* (r p) w* ($return r)))
(define (ws*+ p) ($do w* (r p) w+ ($return r)))
(define (ws++ p) ($do w+ (r p) w+ ($return r)))
#|
[121] Comment	   ::=      "(:" (CommentContents | Comment)* ":)"
|#
;; digits := [0-9]+
(define digits+ ($many ($char-set-contains? char-set:digit) 1))
(define digits* ($many ($char-set-contains? char-set:digit)))

;; [119] EscapeQuot	::= '""'
(define $xpath:escape-quot ($seq ($token "\"\"") ($return #\")))

;; [120] EscapeApos	::= "''"
(define $xpath:escape-apos ($seq ($token "''") ($return #\')))

;; [116] StringLiteral ::= ('"' (EscapeQuot | [^"])* '"')
;;                       | ("'" (EscapeApos | [^'])* "'")
(define $xpath:string-literal
  (let ((no-dq-set (char-set-difference char-set:full (char-set #\")))
	(no-sq-set (char-set-difference char-set:full (char-set #\'))))
    ($or ($let ((($eqv? #\"))
		(c* ($many ($or $xpath:escape-quot
				($char-set-contains? no-dq-set))))
		(($eqv? #\")))
	   ($return `(str ,(list->string c*))))
	 ($let ((($eqv? #\'))
		(c* ($many ($or $xpath:escape-apos
				($char-set-contains? no-sq-set))))
		(($eqv? #\')))
	   ($return `(str ,(list->string c*)))))))

;; [118] BracedURILiteral	::= "Q" "{" [^{}]* "}"
(define $xpath:braced-uri-literal
  (let ((cs (char-set-difference char-set:full (char-set #\{ #\}))))
    ($let ((($token "Q{"))
	   (n ($many ($char-set-contains? cs)))
	   (($eqv? #\})))
      ($return n))))
;; [117] URIQualifiedName	::= BracedURILiteral NCName
(define $xpath:uri-qualified-name
  ($let ((q $xpath:braced-uri-literal)
	 (n $xml:ncname))
    ($return `(eqname ,q ,n))))

;; [112] EQName	::= QName | URIQualifiedName
(define $xpath:eqname ($or $xml:qname $xpath:uri-qualified-name))
;; [60] VarName	::= EQName
(define $xpath:var-name
  ($let ((v $xpath:eqname)) ($return (string->symbol v))))

;; [13] SimpleLetBinding ::= "$" VarName ":=" ExprSingle
(define $xpath:simple-let-binding
  ($let ((($eqv? #\$))
	 (v $xpath:var-name)
	 ((ws** ($token ":=")))
	 (e ($lazy $xpath:expr-single)))
    ($return (list v e))))
;; [12] SimpleLetClause	::= "let" SimpleLetBinding ("," SimpleLetBinding)*
(define $xpath:simple-let-clause
  ($let (((ws*+ ($token "let")))
	 (b $xpath:simple-let-binding)
	 (b* ($many ($seq (ws** ($eqv? #\,)) $xpath:simple-let-binding))))
    ($return (cons b b*))))

;; [11] LetExpr	::= SimpleLetClause "return" ExprSingle
(define $xpath:let-expr
  ($let ((c $xpath:simple-let-clause)
	 ( (ws** ($token "return")) )
	 (e ($lazy $xpath:expr-single)))
    ;; 3.10 Let Expression
    ;; In:  `let $x := 4, $y := 3 return $x + $y`
    ;; Out: `let $x := 4 return let $y := 3 return $x + $y`
    ($return (if (null? (cdr c))
		 `(let ,(car c) ,e)
		 (fold-right (lambda (bind body) `(let ,bind ,body)) e c)))))

;; [99] ElementName ::= EQName
(define $xpath:element-name $xpath:eqname)
;; [101] TypeName	::= EQName
(define $xpath:type-name $xpath:eqname)
;; [100] SimpleTypeName	::= TypeName
(define $xpath:simple-type-name $xpath:type-name)

;; [10] SimpleForBinding ::= "$" VarName "in" ExprSingle
(define $xpath:simple-for-binding
  ($let ((($eqv? #\$))
	 (v $xpath:var-name)
	 ((ws++ ($token "in")))
	 (e ($lazy $xpath:expr-single)))
    ($return (list v e))))
;; [9] SimpleForClause ::= "for" SimpleForBinding ("," SimpleForBinding)*
(define $xpath:simple-for-clause
  ($let (((ws*+ ($token "for")))
	 (b $xpath:simple-for-binding)
	 (b* ($many ($seq (ws** ($eqv? #\,)) $xpath:simple-for-binding))))
    ($return (cons b b*))))
;; [8] ForExpr ::= SimpleForClause "return" ExprSingle
(define $xpath:for-expr
  ($let ((c $xpath:simple-for-clause)
	 ((ws** ($token "return")))
	 (e ($lazy $xpath:expr-single)))
    ;; 3.9 For Expressions
    ;; In `for $x in X, $y in Y return $x + $y`
    ;; Out: `for $x in X return for $y in Y return $x + $y`
    ($return (if (null? (cdr c))
		 `(for ,(car c) ,e)
		 (fold-right (lambda (bind body) `(for ,bind ,body)) e c)))))

;; [59]	VarRef ::= "$" VarName
(define $xpath:var-ref
  ($let ((($eqv? #\$))
	 (n $xpath:var-name))
    ($return `(ref ,n))))

;; Not sure if we should do this
(define (merge e e*)
  (if (null? e*)
      e
      (let ((e0 (car e*)))
	(if (and (pair? e) (eq? (car e0) (car e)))
	    (merge `(,@e ,(cadr e0)) (cdr e*))
	    (merge (list (car e0) e (cadr e0)) (cdr e*))))))
(define (op->symbol s)
  (cond ((char? s) (op->symbol (string s)))
	((string? s) (string->symbol s))
	(else s)))
(define-syntax define-concat-parser/merger
  (syntax-rules ()
    ((_ name base-parser separator-parser merger)
     (define name
       (let ((bp base-parser)
	     (sp (ws** separator-parser)))
	 ($let ((e base-parser)
		(e* ($many ($let ((s sp) (e bp))
			     ($return (list (op->symbol s) e))))))
	   ($return (merger e e*))))))))
(define-syntax define-concat-parser
  (syntax-rules ()
    ((_ name base-parser separator-parser)
     (define-concat-parser/merger name base-parser separator-parser merge))
    ((_ name op base-parser separator-parser)
     (begin
       (define sep ($seq separator-parser ($return 'op)))
       (define-concat-parser name base-parser sep)))))


(define colons (ws** ($token "::")))
;; [41]	ForwardAxis ::= ("child" "::")
;;                    | ("descendant" "::")
;;                    | ("attribute" "::")
;;                    | ("self" "::")
;;                    | ("descendant-or-self" "::")
;;                    | ("following-sibling" "::")
;;                    | ("following" "::")
;;                    | ("namespace" "::")
(define $xpath:forward-axis
  ($or ($seq (ws** ($token "child")) colons ($return 'child::))
       ($seq (ws** ($token "descendant")) colons ($return 'descendant::))
       ($seq (ws** ($token "attribute")) colons ($return 'attribute::))
       ($seq (ws** ($token "self")) colons ($return 'self::))
       ($seq (ws** ($token "descendant-or-self")) colons
	     ($return 'descendant-or-self::))
       ($seq (ws** ($token "following-sibling")) colons
	     ($return 'following-sibling::))
       ($seq (ws** ($token "following")) colons ($return 'following::))
       ($seq (ws** ($token "namespace")) colons ($return 'namespace::))))

;; [44] ReverseAxis ::= ("parent" "::")
;;                    | ("ancestor" "::")
;;                    | ("preceding-sibling" "::")
;;                    | ("preceding" "::")
;;                    | ("ancestor-or-self" "::")
(define $xpath:reverse-axis
  ($or ($seq (ws** ($token "parent")) colons ($return 'parent::))
       ($seq (ws** ($token "ancestor")) colons ($return 'ancestor::))
       ($seq (ws** ($token "preceding-sibling")) colons
	     ($return 'preceding-sibling::))
       ($seq (ws** ($token "preceding")) colons ($return 'preceding::))
       ($seq (ws** ($token "ancestor-or-self")) colons
	     ($return 'ancestor-or-self::))))
;; [45] AbbrevReverseStep ::= ".."
(define $xpath:abbrev-reverse-step ($seq ($token "..") ($return '..)))

;; [48]	Wildcard ::= "*"
;;                 | (NCName ":*")
;;                 | ("*:" NCName)
;;                 | (BracedURILiteral "*")	/* ws: explicit */
(define $xpath:wildcard
  ($or ($let ((n $xml:ncname) ((ws** ($token ":*")))) ($return `(,n *)))
       ($let (((ws** ($token "*:"))) (n $xml:ncname)) ($return `(* ,n)))
       ($let ((u $xpath:braced-uri-literal) ((ws** ($token "*"))))
	 ($return `(,u *)))
       ($seq ($eqv? #\*) ($return '*))))

;; [47] NameTest ::= EQName | Wildcard
(define $xpath:name-test ($or $xpath:wildcard $xpath:eqname))

;; [98] AttributeName ::= EQName
(define $xpath:attribute-name $xpath:eqname)
;; [91] AttribNameOrWildcard ::= AttributeName | "*"
(define $xpath:attribute-name-or-wildcard
  ($or $xpath:attribute-name ($seq ($eqv? #\*) ($return '*))))
;; [90] AttributeTest ::= "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
(define $xpath:attribute-test
  ($let* (((ws** ($token "attribute")))
	  ((ws** ($eqv? #\()))
	  (name $xpath:attribute-name-or-wildcard)
	  (type ($optional ($let (((ws** ($eqv? #\,)))
				 (t $xpath:type-name)
				 (opt? ($optional ($seq (ws** ($eqv? #\?))
							($return #t)))))
			     ($return `(,name of ,(if opt? `(? ,t) t))))
			   name))
	  ((ws** ($eqv? #\)))))
   ($return `(attribute ,type))))

;; [93] AttributeDeclaration ::= AttributeName
(define $xpath:attribute-declaration $xpath:attribute-name)
;; [92] SchemaAttributeTest ::= "schema-attribute" "(" AttributeDeclaration ")"
(define $xpath:schema-attribute-test
  ($let (((ws** ($token "schema-attribute")))
	 ((ws** ($eqv? #\()))
	 (decl $xpath:attribute-declaration)
	 ((ws** ($eqv? #\)))))
    ($return `(schema-attribute ,decl))))

;; [97] ElementDeclaration ::= ElementName
(define $xpath:element-declaration $xpath:element-name)
;; [96] SchemaElementTest ::= "schema-element" "(" ElementDeclaration ")"
(define $xpath:schema-element-test
  ($let (((ws** ($token "schema-element")))
	 ((ws** ($eqv? #\()))
	 (decl $xpath:element-declaration)
	 ((ws** ($eqv? #\)))))
    ($return `(schema-element ,decl))))

;; [84] AnyKindTest ::= "node" "(" ")"
(define $xpath:any-kind-test
  ($seq (ws** ($token "node"))
	(ws** ($eqv? #\())
	(ws** ($eqv? #\))) ($return '(node))))

;; [95] ElementNameOrWildcard ::= ElementName | "*"
(define $xpath:element-name-or-wildcard
  ($or $xpath:element-name ($seq ($eqv? #\*) ($return '*))))
;; [94] ElementTest ::= "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
(define $xpath:element-test
  ($let* (((ws** ($token "element")))
	  ((ws** ($eqv? #\()))
	  (name $xpath:element-name-or-wildcard)
	  (type ($optional ($let (((ws** ($eqv? #\,)))
				 (t $xpath:type-name)
				 (opt? ($optional ($seq (ws** ($eqv? #\?))
							($return #t)))))
			     ($return `(,name of ,(if opt? `(? ,t) t))))
			   name))
	  ((ws** ($eqv? #\)))))
    ($return `(element ,type))))

;; [85] DocumentTest ::= "document-node" "(" (ElementTest | SchemaElementTest)? ")"
(define $xpath:document-test
  ($let* (((ws** ($token "document-node")))
	  ((ws** ($eqv? #\()))
	  (e/s ($optional ($or $xpath:element-test $xpath:schema-element-test)))
	  ((ws** ($eqv? #\)))))
    ($return (if e/s `(document-node ,e/s) `(document-node)))))

;; [86] TextTest ::= "text" "(" ")"
(define $xpath:text-test
  ($seq (ws** ($token "text"))
	(ws** ($eqv? #\())
	(ws** ($eqv? #\))) ($return '(text))))
;; [87] CommentTest ::= "comment" "(" ")"
(define $xpath:comment-test
  ($seq (ws** ($token "comment"))
	(ws** ($eqv? #\())
	(ws** ($eqv? #\))) ($return '(comment))))
;; [88] NamespaceNodeTest ::= "namespace-node" "(" ")"
(define $xpath:namespace-node-test
  ($seq (ws** ($token "namespace-node"))
	(ws** ($eqv? #\())
	(ws** ($eqv? #\))) ($return '(namespace-node))))
;; [89] PITest ::= "processing-instruction" "(" (NCName | StringLiteral)? ")"
(define $xpath:pi-test
  ($let (((ws** ($token "processing-instruction")))
	 (($eqv? #\())
	 (v ($optional ($or $xml:ncname $xpath:string-literal)))
	 ((ws** ($eqv? #\)))))
    ($return `(processing-instruction ,v))))

;; [83] KindTest ::= DocumentTest
;;                 | ElementTest
;;                 | AttributeTest
;;                 | SchemaElementTest
;;                 | SchemaAttributeTest
;;                 | PITest
;;                 | CommentTest
;;                 | TextTest
;;                 | NamespaceNodeTest
;;                 | AnyKindTest
(define $xpath:kind-test
  ($or $xpath:document-test
       $xpath:element-test
       $xpath:attribute-test
       $xpath:schema-element-test
       $xpath:schema-attribute-test
       $xpath:pi-test
       $xpath:comment-test
       $xpath:text-test
       $xpath:namespace-node-test
       $xpath:any-kind-test))

;; [46] NodeTest ::= KindTest | NameTest
(define $xpath:node-test ($or $xpath:kind-test $xpath:name-test))

;; [43] ReverseStep ::= (ReverseAxis NodeTest) | AbbrevReverseStep
(define $xpath:reverse-step
  ($or ($let ((ra $xpath:reverse-axis) (t $xpath:node-test))
	 ($return (list ra t)))
       $xpath:abbrev-reverse-step))

;; [61]  ParenthesizedExpr ::= "(" Expr? ")"
(define $xpath:parenthesized-expr
  ($let* (((ws** ($eqv? #\()))
	  (e ($optional $xpath:expr '()))
	  ((ws** ($eqv? #\)))))
	 ($return e)))

;; [113] IntegerLiteral	::= Digits
(define $xpath:integer-literal
  ($let ((c* digits+)) ($return (string->number (list->string c*)))))
;; [114] DecimalLiteral	::= ("." Digits) | (Digits "." [0-9]*)
(define $xpath:decimal-literal
  ($or ($let (((ws** ($eqv? #\.))) (c* digits+))
	  ($return (string->number (list->string (cons #\. c*)))))
       ($let ((c1* digits+)
	      ((ws** ($eqv? #\.)))
	      (c2* digits*))
	  ($return (string->number 
		    (list->string (append c1* (cons #\. c2*))))))))
;; [115] DoubleLiteral ::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
(define $xpath:double-literal
  ($let ((i ($or ($let (((ws** ($eqv? #\.))) (c* digits+))
		   ($return (cons #\. c*)))
		 ($let ((c1* digits+)
			(c2* ($optional ($seq (ws** ($eqv? #\.)) digits*) #f)))
		   ($return (if c2* (append c1* (cons #\. c2*)) c1*)))))
	 (($or ($eqv? #\e) ($eqv? #\E)))
	 (sign ($optional ($or ($eqv? #\+) ($eqv? #\-)) #\+))
	 (f digits+))
   ($return (string->number (list->string `(,@i #\e ,sign ,@f))))))

;; [58] NumericLiteral ::= IntegerLiteral | DecimalLiteral | DoubleLiteral
(define $xpath:numeric-literal
  ($or $xpath:double-literal $xpath:decimal-literal $xpath:integer-literal))

;; [57] Literal ::= NumericLiteral | StringLiteral
(define $xpath:literal
  ($or $xpath:numeric-literal $xpath:string-literal))

;; [56] PrimaryExpr ::= Literal
;;                    | VarRef
;;                    | ParenthesizedExpr
;;                    | ContextItemExpr
;;                    | FunctionCall
;;                    | FunctionItemExpr
;;                    | MapConstructor
;;                    | ArrayConstructor
;;                    | UnaryLookup
(define $xpath:primary-expr
  ($or $xpath:literal $xpath:var-ref $xpath:parenthesized-expr))

;; [52] Predicate ::= "[" Expr "]"
(define $xpath:predicate
  ($let (( (ws** ($eqv? #\[)) )
	 (e ($lazy $xpath:expr))
	 ( (ws** ($eqv? #\])) ))
     ($return `(? ,e))))

;; [65] ArgumentPlaceholder ::= "?"
(define $xpath:argument-placeholder
  ($seq (ws** ($eqv? #\?)) ($return '?)))
;; [64] Argument ::= ExprSingle | ArgumentPlaceholder
(define $xpath:argument
  ($or ($lazy $xpath:expr-single)
       $xpath:argument-placeholder))
;; [50] ArgumentList ::= "(" (Argument ("," Argument)*)? ")"
(define $xpath:argument-list
  ($let (((ws** ($eqv? #\()))
	 (a* ($optional ($let* ((a $xpath:argument)
				(a* ($many ($seq (ws** ($eqv? #\,)))
					   $xpath:argument)))
			  ($return (cons a a*))) '()))
	 ((ws** ($eqv? #\)))))
    ($return a*)))

;; [54] KeySpecifier ::= NCName | IntegerLiteral | ParenthesizedExpr | "*"
(define $xpath:key-specifier
  ($or $xml:ncname $xpath:integer-literal $xpath:parenthesized-expr
       ($seq ($eqv? #\*) ($return '*))))
;; [53] Lookup ::= "?" KeySpecifier
(define $xpath:lookup
  ($let (( (ws** ($eqv? #\?)) )
	 (ks $xpath:key-specifier))
    ($return `(lookup ,ks))))

;; [49] PostfixExpr ::= PrimaryExpr (Predicate | ArgumentList | Lookup)*
(define $xpath:postfix-expr
  ($let ((p $xpath:primary-expr)
	 (rest ($optional ($or $xpath:predicate
			       $xpath:argument-list
			       $xpath:lookup))))
    ($return (if rest `(,p ,rest) p))))

;; [42] AbbrevForwardStep ::= "@"? NodeTest
(define $xpath:abbrev-forward-step
  ($do (at ($optional ($eqv? #\@))) (t $xpath:node-test)
       ($return (if at (list at t) t))))

;;;; [40] ForwardStep ::= (ForwardAxis NodeTest) | AbbrevForwardStep
(define $xpath:forward-step
  ($or ($let ((a $xpath:forward-axis) (t $xpath:node-test))
	 ($return (list a t)))
       $xpath:abbrev-forward-step))

;; [39] AxisStep ::= (ReverseStep | ForwardStep) PredicateList
(define $xpath:axis-step
  ($let ((s ($or $xpath:reverse-step $xpath:forward-step))
	 ;; TODO
	  #;(p $xpath:predicate-list))
    ($return s)))
;; [38] StepExpr ::= PostfixExpr | AxisStep
(define $xpath:step-expr ($or $xpath:postfix-expr $xpath:axis-step))
;; [37] RelativePathExpr ::= StepExpr (("/" | "//") StepExpr)*
(define-concat-parser/merger $xpath:relative-path-expr $xpath:step-expr
  ($or ($token "//") ($eqv? #\/)) cons)

;; [36] PathExpr ::= ("/" RelativePathExpr?)
;;                 | ("//" RelativePathExpr)
;;                 | RelativePathExpr /* xgc: leading-lone-slash */
(define $xpath:path-expr
  ($or ($let ((($token "//"))
	      (r $xpath:relative-path-expr))
	 ($return (if (pair? r)
		      (cons (list '// (car r)) (cdr r))
		      (list (list '// r)))))
       ($let ((($eqv? #\/))
	      (r ($optional $xpath:relative-path-expr '())))
	 ($return (cond ((null? r) (list '(/)))
			((pair? r) (cons (list '/ (car r)) (cdr r)))
			(else (list (list '/ r))))))
       $xpath:relative-path-expr))

;; [35] SimpleMapExpr ::= PathExpr ("!" PathExpr)*
(define-concat-parser $xpath:simple-map-expr $xpath:path-expr ($eqv? #\!))

;; [31] ValueExpr ::= SimpleMapExpr
(define $xpath:value-expr $xpath:simple-map-expr)

;; [30] UnaryExpr ::= ("-" | "+")* ValueExpr
(define $xpath:unary-expr
  ($let ((op* ($many (ws** ($or ($eqv? #\-) ($eqv? #\+)))))
	 (v $xpath:value-expr))
    ($return (if (null? op*) v (cons op* v)))))

;; [55] ArrowFunctionSpecifier ::= EQName | VarRef | ParenthesizedExpr
(define $xpath:arrow-function-specifier
  ($or $xpath:eqname $xpath:var-name $xpath:parenthesized-expr))

;; [29] ArrowExpr ::= UnaryExpr ( "=>" ArrowFunctionSpecifier ArgumentList )*
(define $xpath:arrow-expr
  ($let ((e $xpath:unary-expr)
	 (e* ($many ($let* (((ws++ ($token "=>" )))
			    (s $xpath:arrow-function-specifier)
			    (a $xpath:argument-list))
		       ($return (list '=> s a))))))
   ($return (merge e e*))))

(define-syntax define-type-parser
  (syntax-rules ()
    ((_ name base-parser type-parser tokens ...)
     (define name
       (let ((bp base-parser)
	     (tp type-parser)
	     (keyword (string->symbol (string-join (list tokens ...) "-"))))
	 ($let ((t bp)
		(s ($optional ($seq (ws++ ($token tokens)) ... tp))))
	   ($return (if s `(keyword t s) t))))))))

;; [77] SingleType ::= SimpleTypeName "?"?
(define $xpath:single-type
  ($let ((n $xpath:simple-type-name)
	 (q ($optional (ws** ($eqv? #\?)))))
    ($return (if q `(? ,n) n))))
;; [28] CastExpr ::= ArrowExpr ( "cast" "as" SingleType )?
(define-type-parser $xpath:cast-expr $xpath:arrow-expr
  $xpath:single-type "cast" "as")

;; [27] CastableExpr ::= CastExpr ( "castable" "as" SingleType )?
(define-type-parser $xpath:castable-expr $xpath:cast-expr
  $xpath:single-type "castable" "as")

;; [103] AnyFunctionTest ::= "function" "(" "*" ")"
(define $xpath:any-function-test
  ($seq (ws** ($token "function"))
	(ws** ($eqv? #\()) (ws** ($eqv? #\*)) (ws** ($eqv? #\)))
	($return '(function * (*)))))

;; [104] TypedFunctionTest ::= "function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType
(define $xpath:typed-function-test
  ($let (((ws** ($token "function")))
	 ((ws** ($eqv? #\()))
	 (arg ($lazy $xpath:sequence-type))
 	 (arg* ($many ($seq (ws** ($eqv? #\,)) ($lazy $xpath:sequence-type))))
	 ((ws** ($eqv? #\))))
	 ((ws** ($token "as")))
	 (rettype ($lazy $xpath:sequence-type)))
    ($return `(function ,rettype ,(cons arg arg*)))))

;; [102] FunctionTest ::= AnyFunctionTest | TypedFunctionTest
(define $xpath:function-test
  ($or $xpath:any-function-test $xpath:typed-function-test))


;; [111] ParenthesizedItemType ::= "(" ItemType ")"
(define $xpath:parenthesized-item-type
  ($let (((ws** ($eqv? #\()))
	 (i ($lazy $xpath:item-type))
	 ((ws** ($eqv? #\)))))
     ($return i)))

;; [106] AnyMapTest ::= "map" "(" "*" ")"
(define $xpath:any-map-test
  ($seq (ws** ($token "map"))
	(ws** ($eqv? #\()) (ws** ($eqv? #\*)) (ws** ($eqv? #\)))
	($return '(map *))))

;; [82] AtomicOrUnionType ::= EQName
(define $xpath:atomic-or-union-type $xpath:eqname)
;; [107] TypedMapTest ::= "map" "(" AtomicOrUnionType "," SequenceType ")"
(define $xpath:typed-map-test
  ($let (( (ws** ($token "map")) )
	 ( (ws** ($eqv? #\()) )
	 (t $xpath:atomic-or-union-type)
	 ( (ws** ($eqv? #\,)) )
	 (s ($lazy $xpath:sequence-type))
	 ( (ws** ($eqv? #\))) ))
    ($return `(map ,t ,s))))
  
;; [105] MapTest ::= AnyMapTest | TypedMapTest
(define $xpath:map-test ($or $xpath:any-map-test $xpath:typed-map-test))

;; [109] AnyArrayTest ::= "array" "(" "*" ")"
(define $xpath:any-array-test
  ($seq (ws** ($token "array"))
	(ws** ($eqv? #\()) (ws** ($eqv? #\*)) (ws** ($eqv? #\)))
	($return '(array *))))
  
;; [110] TypedArrayTest ::= "array" "(" SequenceType ")"
(define $xpath:typed-array-test
  ($let (( (ws** ($token "array")) )
	 ( (ws** ($eqv? #\()) )
	 (s ($lazy $xpath:sequence-type))
	 ( (ws** ($eqv? #\))) ))
    ($return `(array ,s))))

;; [108] ArrayTest ::= AnyArrayTest | TypedArrayTest
(define $xpath:array-test ($or $xpath:any-array-test $xpath:typed-array-test))

;; [81] ItemType ::= KindTest | ("item" "(" ")")
;;                 | FunctionTest | MapTest | ArrayTest
;;                 | AtomicOrUnionType | ParenthesizedItemType
(define $xpath:item-type
  ($or $xpath:kind-test
       ($seq (ws** ($token "item")) (ws** ($eqv? #\()) (ws** ($eqv? #\)))
	     ($return '(item)))
       $xpath:function-test
       $xpath:map-test
       $xpath:array-test
       $xpath:atomic-or-union-type
       $xpath:parenthesized-item-type))

;; [80] OccurrenceIndicator ::= "?" | "*" | "+" /* xgc: occurrence-indicators */
(define $xpath:occurrence-indicator
  ($or ($seq (ws** ($eqv? #\?)) ($return '?))
       ($seq (ws** ($eqv? #\*)) ($return '*))
       ($seq (ws** ($eqv? #\+)) ($return '+))))
;; [79] SequenceType ::= ("empty-sequence" "(" ")")
;;                     | (ItemType OccurrenceIndicator?)
(define $xpath:sequence-type
  ($or ($let (w* (($token "empty-sequence")) w* (($eqv? #\()) w* (($eqv? #\))))
	 ($return '(sequence)))
       ($let ((i $xpath:item-type) (o ($optional $xpath:occurrence-indicator)))
	 ($return (if o (list o i) i)))))

;; [26] TreatExpr ::= CastableExpr ( "treat" "as" SequenceType )?
(define-type-parser $xpath:treat-expr $xpath:castable-expr
  $xpath:sequence-type "treat" "as")

;; [25] InstanceofExpr ::= TreatExpr ( "instance" "of" SequenceType )?
(define-type-parser $xpath:instanceof-expr $xpath:treat-expr
  $xpath:sequence-type "instance" "of")

;; [24] IntersectExceptExpr ::=
;;          InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
(define-concat-parser $xpath:intersect-exept-expr $xpath:instanceof-expr
  ($or ($token "intersect") ($token "except")))

;; [23]	UnionExpr ::=
;;        IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
(define-concat-parser $xpath:union-expr union $xpath:intersect-exept-expr
  ($or ($token "union") ($eqv? #\|)))

;; [22]	MultiplicativeExpr ::=
;;        UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
(define-concat-parser $xpath:multiplcative-expr $xpath:union-expr
  ($or ($eqv? #\*) ($token "div") ($token "idiv") ($token "mod")))

;; [21] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
(define-concat-parser $xpath:additive-expr $xpath:multiplcative-expr
  ($or ($eqv? #\+) ($eqv? #\-)))

;; [20] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )?
(define-concat-parser $xpath:range-expr range $xpath:additive-expr
  ($token "to"))

;; [19] StringConcatExpr ::= RangeExpr ( "||" RangeExpr )*
(define-concat-parser $xpath:string-concat-expr concat $xpath:range-expr
  ($token "||"))

;; [33] ValueComp ::= "eq" | "ne" | "lt" | "le" | "gt" | "ge"
(define $xpath:value-comp
  ($or ($seq (ws** ($token "eq")) ($return 'eq))
       ($seq (ws** ($token "ne")) ($return 'ne))
       ($seq (ws** ($token "lt")) ($return 'lt))
       ($seq (ws** ($token "le")) ($return 'le))
       ($seq (ws** ($token "gt")) ($return 'gt))
       ($seq (ws** ($token "ge")) ($return 'ge))))
;; [32] GeneralComp ::= "=" | "!=" | "<" | "<=" | ">" | ">="
(define $xpath:general-comp
  ($or ($seq (ws** ($token "="))  ($return '=))
       ($seq (ws** ($token "!=")) ($return '!=))
       ($seq (ws** ($token "<=")) ($return '<=))
       ($seq (ws** ($token "<"))  ($return '<))
       ($seq (ws** ($token ">=")) ($return '>=))
       ($seq (ws** ($token ">"))  ($return '>))))

;; [34] NodeComp ::= "is" | "<<" | ">>"
(define $xpath:node-comp
  ($or ($seq (ws** ($token "is")) ($return 'is))
       ($seq (ws** ($token "<<")) ($return '<<))
       ($seq (ws** ($token ">>")) ($return '>>))))

;; [18] ComparisonExpr ::= StringConcatExpr ( (ValueComp
;;                         | GeneralComp
;;                         | NodeComp) StringConcatExpr )?
(define $xpath:comparison-expr
  ($let ((sc $xpath:string-concat-expr)
	 (comp? ($optional ($let ((c ($or $xpath:value-comp
					  $xpath:node-comp
					  $xpath:general-comp))
				  (e $xpath:string-concat-expr))
			    ($return (cons c e))))))
    ($return (if comp? `(,(car comp?) ,sc ,(cdr comp?)) sc))))

;; [17] AndExpr ::= ComparisonExpr ( "and" ComparisonExpr )*
(define-concat-parser $xpath:and-expr and $xpath:comparison-expr ($token "and"))

;; [16] OrExpr ::= AndExpr ( "or" AndExpr )*
(define-concat-parser $xpath:or-expr or  $xpath:and-expr ($token "or"))
;; [7] ExprSingle ::= ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr
(define $xpath:expr-single
  ($or $xpath:for-expr
       $xpath:let-expr
       ;; TODO
       ;; $xpath:quantified-expr
       ;; $xpath:if-expr
       $xpath:or-expr))

;; [6]  Expr  ::= ExprSingle ("," ExprSingle)*
(define $xpath:expr
  ($let ((es $xpath:expr-single)
	 (es* ($many ($seq ($eqv? #\,) $xpath:expr-single))))
    ($return (cons es es*))))

;; [1] 	XPath ::= Expr
(define $xpath:xpath $xpath:expr)

#|

[2]   	ParamList  ::=   	Param ("," Param)*
[3]   	Param	   ::=   	"$" EQName TypeDeclaration?
[4]   	FunctionBody ::=   	EnclosedExpr
[5]   	EnclosedExpr ::=   	"{" Expr? "}"

[14]   	QuantifiedExpr	   ::=   	("some" | "every") "$" VarName "in" ExprSingle ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle
[15]   	IfExpr	   ::=   	"if" "(" Expr ")" "then" ExprSingle "else" ExprSingle

[51]   	PredicateList	   ::=   	Predicate*

[62]   	ContextItemExpr	   ::=   	"."
[63]   	FunctionCall	   ::=   	EQName ArgumentList	/* xgc: reserved-function-names */
/* gn: parens */


[66]   	FunctionItemExpr	   ::=   	NamedFunctionRef | InlineFunctionExpr
[67]   	NamedFunctionRef	   ::=   	EQName "#" IntegerLiteral	/* xgc: reserved-function-names */
[68]   	InlineFunctionExpr	   ::=   	"function" "(" ParamList? ")" ("as" SequenceType)? FunctionBody
[69]   	MapConstructor	   ::=   	"map" "{" (MapConstructorEntry ("," MapConstructorEntry)*)? "}"
[70]   	MapConstructorEntry	   ::=   	MapKeyExpr ":" MapValueExpr
[71]   	MapKeyExpr	   ::=   	ExprSingle
[72]   	MapValueExpr	   ::=   	ExprSingle
[73]   	ArrayConstructor	   ::=   	SquareArrayConstructor | CurlyArrayConstructor
[74]   	SquareArrayConstructor	   ::=   	"[" (ExprSingle ("," ExprSingle)*)? "]"
[75]   	CurlyArrayConstructor	   ::=   	"array" EnclosedExpr
[76]   	UnaryLookup	   ::=   	"?" KeySpecifier

[78]   	TypeDeclaration	   ::=   	"as" SequenceType

|#

)
