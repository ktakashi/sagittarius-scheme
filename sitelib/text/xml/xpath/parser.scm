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
	    $xpath:expr-single
	    $xpath:for-expr)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (srfi :14 char-sets)
	    ;; need number of xml parsers
	    (text xml dom parser))
(define w* ($many $xml:s))
(define w+ ($many $xml:s 1))
(define (ws** p)
  ($let* ((w*) (r p) (w*))
   ($return r)))
(define (ws*+ p)
  ($let* ((w*) (r p) (w+))
    ($return r)))
(define (ws++ p)
  ($let* ((w+) (r p) (w+))
    ($return r)))
#|
[113] IntegerLiteral	::= Digits	
[114] DecimalLiteral	::= ("." Digits) | (Digits "." [0-9]*)
[115] DoubleLiteral	::= (("." Digits) | (Digits ("." [0-9]*)?)) [eE] [+-]? Digits
[116] StringLiteral	::= ('"' (EscapeQuot | [^"])* '"') | ("'" (EscapeApos | [^'])* "'")
[119] EscapeQuot	::= '""'	
[120] EscapeApos	::= "''"	
[121] Comment	   ::=      "(:" (CommentContents | Comment)* ":)"
|#

;; [118] BracedURILiteral	::= "Q" "{" [^{}]* "}"
(define $xpath:braced-uri-literal
  (let ((cs (char-set-difference char-set:full (char-set #\{ #\}))))
    ($let* ((($token "Q{"))
	    (n ($many ($char-set-contains? cs)))
	    (($eqv? #\})))
      ($return n))))
;; [117] URIQualifiedName	::= BracedURILiteral NCName
(define $xpath:uri-qualified-name
  ($let* ((q $xpath:braced-uri-literal)
	  (n $xml:ncname))
    ($return `(eqname ,q ,n))))

;; [112] EQName	::= QName | URIQualifiedName
(define $xpath:eqname ($or $xml:qname $xpath:uri-qualified-name))
;; [60] VarName	::= EQName
(define $xpath:var-name
  ($let* ((v $xpath:eqname)) ($return (string->symbol v))))
;; [11] LetExpr	::= SimpleLetClause "return" ExprSingle


;; [10] SimpleForBinding ::= "$" VarName "in" ExprSingle	
(define $xpath:simple-for-binding
  ($let* ((($eqv? #\$))
	  (v $xpath:var-name)
	  ((ws++ ($token "in")))
	  (e $xpath:expr-single))
    ($return (list v e))))
;; [9] SimpleForClause ::= "for" SimpleForBinding ("," SimpleForBinding)*
(define $xpath:simple-for-clause
  ($let* (((ws*+ ($token "for")))
	  (b $xpath:simple-for-binding)
	  (b* ($many ($seq (ws** ($eqv? #\,)) $xpath:simple-for-binding))))
    ($return (cons b b*))))
;; [8] ForExpr ::= SimpleForClause "return" ExprSingle
(define $xpath:for-expr
  ($let* ((c $xpath:simple-for-clause)
	  ((ws++ ($token "return")))
	  (e $xpath:expr-single))
    ;; 3.9 For Expressions
    ;; In `for $x in X, $y in Y return $x + $y`
    ;; Out: `for $x in X return for $y in Y return $x + $y`
    ($return (if (null? (cdr c))
		 `(for ,(car c) ,e)
		 (fold-right (lambda (bind body) `(for ,bind ,body)) e c)))))

;; [59]	VarRef ::= "$" VarName
(define $xpath:var-ref
  ($let* ((($eqv? #\$))
	  (n $xpath:var-name))
    ($return `(ref ,n))))

;; [31] ValueExpr ::= SimpleMapExpr	
;; [30] UnaryExpr ::= ("-" | "+")* ValueExpr
;; [29] ArrowExpr ::= UnaryExpr ( "=>" ArrowFunctionSpecifier ArgumentList )*
;; [28] CastExpr ::= ArrowExpr ( "cast" "as" SingleType )?

;; [27] CastableExpr ::= CastExpr ( "castable" "as" SingleType )?
;; [26] TreatExpr ::= CastableExpr ( "treat" "as" SequenceType )?

;; [25] InstanceofExpr ::= TreatExpr ( "instance" "of" SequenceType )?

;; [24] IntersectExceptExpr ::=
;;          InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*

;; [23]	UnionExpr ::=
;;        IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*

(define (merge e e*)
  (if (null? e*)
      e
      (let ((e0 (car e*)))
	(merge (list (car e0) e (cdr e0)) (cdr e*)))))

;; [22]	MultiplicativeExpr ::=
;;        UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
(define $xpath:multiplcative-expr
  ($let* ((a ($or $xpath:var-name $xpath:var-ref)) ;; for now
	  #;(a* ($many ($let* ((o ($or ($eqv? #\*)
				     ($token "div")
				     ($token "idiv")
				     ($token "mod")))
			     (e $xpath:union-expr))
			 ($return (cons o e))))))
    ($return a #;(merge a a*))))

;; [21] AdditiveExpr ::= MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
(define $xpath:additive-expr
  ($let* ((a $xpath:multiplcative-expr)
	  (a* ($many ($let* ((o (ws** ($or ($eqv? #\+) ($eqv? #\-))))
			     (e $xpath:multiplcative-expr))
		       ($return (cons (string->symbol (string o)) e))))))
    ($return (merge a a*))))

;; [20] RangeExpr ::= AdditiveExpr ( "to" AdditiveExpr )?
(define $xpath:range-expr
  ($let* ((a $xpath:additive-expr)
	  (a* ($many ($seq ($token "to") $xpath:additive-expr))))
    ($return (if (null? a*) a `(range ,a ,@a*)))))
  
;; [19] StringConcatExpr ::= RangeExpr ( "||" RangeExpr )*
(define $xpath:string-concat-expr
  ($let* ((r $xpath:range-expr)
	  (r* ($many ($seq ($token "||") $xpath:range-expr))))
    ($return (if (null? r*) r `(concat ,r ,@r*)))))

;; [18] ComparisonExpr ::= StringConcatExpr ( (ValueComp
;;                         | GeneralComp
;;                         | NodeComp) StringConcatExpr )?	
(define $xpath:comparison-expr
  ($let* ((sc $xpath:string-concat-expr))
	 ;; TODO with other thing
    ($return sc)))

;; [17] AndExpr ::= ComparisonExpr ( "and" ComparisonExpr )*
(define $xpath:and-expr
  ($let* ((e $xpath:comparison-expr)
	  (e* ($many ($seq ($token "and") $xpath:comparison-expr))))
    ($return (if (null? e*) e `(and ,e ,@e*)))))

;; [16] OrExpr ::= AndExpr ( "or" AndExpr )*
(define $xpath:or-expr
  ($let* ((e $xpath:and-expr)
	  (e* ($many ($seq ($token "or") $xpath:and-expr))))
    ($return (if (null? e*) e `(or ,e ,@e*)))))
;; [7] ExprSingle ::= ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr
(define $xpath:expr-single
  ($or $xpath:for-expr
       ;;$xpath:let-expr $xpath:quantified-expr
       ;;$xpath:if-expr
       $xpath:or-expr
       ))

;; [6]  Expr  ::= ExprSingle ("," ExprSingle)*
(define $xpath:expr
  ($let* ((es $xpath:expr-single)
	  (es* ($many ($seq ($eqv? #\,) $xpath:expr-single))))
    ($return (cons es es*))))

;; [1] 	XPath ::= Expr
(define $xpath:xpath $xpath:expr)

#|

[2]   	ParamList  ::=   	Param ("," Param)*	
[3]   	Param	   ::=   	"$" EQName TypeDeclaration?	
[4]   	FunctionBody ::=   	EnclosedExpr	
[5]   	EnclosedExpr ::=   	"{" Expr? "}"	

[12]   	SimpleLetClause	   ::=   	"let" SimpleLetBinding ("," SimpleLetBinding)*	
[13]   	SimpleLetBinding	   ::=   	"$" VarName ":=" ExprSingle	
[14]   	QuantifiedExpr	   ::=   	("some" | "every") "$" VarName "in" ExprSingle ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle	
[15]   	IfExpr	   ::=   	"if" "(" Expr ")" "then" ExprSingle "else" ExprSingle	

[32]   	GeneralComp	   ::=   	"=" | "!=" | "<" | "<=" | ">" | ">="	
[33]   	ValueComp	   ::=   	"eq" | "ne" | "lt" | "le" | "gt" | "ge"	
[34]   	NodeComp	   ::=   	"is" | "<<" | ">>"	
[35]   	SimpleMapExpr	   ::=   	PathExpr ("!" PathExpr)*	
[36]   	PathExpr	   ::=   	("/" RelativePathExpr?)
| ("//" RelativePathExpr)
| RelativePathExpr	/* xgc: leading-lone-slash */
[37]   	RelativePathExpr	   ::=   	StepExpr (("/" | "//") StepExpr)*	
[38]   	StepExpr	   ::=   	PostfixExpr | AxisStep	
[39]   	AxisStep	   ::=   	(ReverseStep | ForwardStep) PredicateList	
[40]   	ForwardStep	   ::=   	(ForwardAxis NodeTest) | AbbrevForwardStep	
[41]   	ForwardAxis	   ::=   	("child" "::")
| ("descendant" "::")
| ("attribute" "::")
| ("self" "::")
| ("descendant-or-self" "::")
| ("following-sibling" "::")
| ("following" "::")
| ("namespace" "::")	
[42]   	AbbrevForwardStep	   ::=   	"@"? NodeTest	
[43]   	ReverseStep	   ::=   	(ReverseAxis NodeTest) | AbbrevReverseStep	
[44]   	ReverseAxis	   ::=   	("parent" "::")
| ("ancestor" "::")
| ("preceding-sibling" "::")
| ("preceding" "::")
| ("ancestor-or-self" "::")	
[45]   	AbbrevReverseStep	   ::=   	".."	
[46]   	NodeTest	   ::=   	KindTest | NameTest	
[47]   	NameTest	   ::=   	EQName | Wildcard	
[48]   	Wildcard	   ::=   	"*"
| (NCName ":*")
| ("*:" NCName)
| (BracedURILiteral "*")	/* ws: explicit */
[49]   	PostfixExpr	   ::=   	PrimaryExpr (Predicate | ArgumentList | Lookup)*	
[50]   	ArgumentList	   ::=   	"(" (Argument ("," Argument)*)? ")"	
[51]   	PredicateList	   ::=   	Predicate*	
[52]   	Predicate	   ::=   	"[" Expr "]"	
[53]   	Lookup	   ::=   	"?" KeySpecifier	
[54]   	KeySpecifier	   ::=   	NCName | IntegerLiteral | ParenthesizedExpr | "*"	
[55]   	ArrowFunctionSpecifier	   ::=   	EQName | VarRef | ParenthesizedExpr	
[56]   	PrimaryExpr	   ::=   	Literal
| VarRef
| ParenthesizedExpr
| ContextItemExpr
| FunctionCall
| FunctionItemExpr
| MapConstructor
| ArrayConstructor
| UnaryLookup	
[57]   	Literal	   ::=   	NumericLiteral | StringLiteral	
[58]   	NumericLiteral	   ::=   	IntegerLiteral | DecimalLiteral | DoubleLiteral	

[61]   	ParenthesizedExpr	   ::=   	"(" Expr? ")"	
[62]   	ContextItemExpr	   ::=   	"."	
[63]   	FunctionCall	   ::=   	EQName ArgumentList	/* xgc: reserved-function-names */
/* gn: parens */
[64]   	Argument	   ::=   	ExprSingle | ArgumentPlaceholder	
[65]   	ArgumentPlaceholder	   ::=   	"?"	
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
[77]   	SingleType	   ::=   	SimpleTypeName "?"?	
[78]   	TypeDeclaration	   ::=   	"as" SequenceType	
[79]   	SequenceType	   ::=   	("empty-sequence" "(" ")")
| (ItemType OccurrenceIndicator?)	
[80]   	OccurrenceIndicator	   ::=   	"?" | "*" | "+"	/* xgc: occurrence-indicators */
[81]   	ItemType	   ::=   	KindTest | ("item" "(" ")") | FunctionTest | MapTest | ArrayTest | AtomicOrUnionType | ParenthesizedItemType	
[82]   	AtomicOrUnionType	   ::=   	EQName	
[83]   	KindTest	   ::=   	DocumentTest
| ElementTest
| AttributeTest
| SchemaElementTest
| SchemaAttributeTest
| PITest
| CommentTest
| TextTest
| NamespaceNodeTest
| AnyKindTest	
[84]   	AnyKindTest	   ::=   	"node" "(" ")"	
[85]   	DocumentTest	   ::=   	"document-node" "(" (ElementTest | SchemaElementTest)? ")"	
[86]   	TextTest	   ::=   	"text" "(" ")"	
[87]   	CommentTest	   ::=   	"comment" "(" ")"	
[88]   	NamespaceNodeTest	   ::=   	"namespace-node" "(" ")"	
[89]   	PITest	   ::=   	"processing-instruction" "(" (NCName | StringLiteral)? ")"	
[90]   	AttributeTest	   ::=   	"attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"	
[91]   	AttribNameOrWildcard	   ::=   	AttributeName | "*"	
[92]   	SchemaAttributeTest	   ::=   	"schema-attribute" "(" AttributeDeclaration ")"	
[93]   	AttributeDeclaration	   ::=   	AttributeName	
[94]   	ElementTest	   ::=   	"element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"	
[95]   	ElementNameOrWildcard	   ::=   	ElementName | "*"	
[96]   	SchemaElementTest	   ::=   	"schema-element" "(" ElementDeclaration ")"	
[97]   	ElementDeclaration	   ::=   	ElementName	
[98]   	AttributeName	   ::=   	EQName	
[99]   	ElementName	   ::=   	EQName	
[100]   	SimpleTypeName	   ::=   	TypeName	
[101]   	TypeName	   ::=   	EQName	
[102]   	FunctionTest	   ::=   	AnyFunctionTest
| TypedFunctionTest	
[103]   	AnyFunctionTest	   ::=   	"function" "(" "*" ")"	
[104]   	TypedFunctionTest	   ::=   	"function" "(" (SequenceType ("," SequenceType)*)? ")" "as" SequenceType	
[105]   	MapTest	   ::=   	AnyMapTest | TypedMapTest	
[106]   	AnyMapTest	   ::=   	"map" "(" "*" ")"	
[107]   	TypedMapTest	   ::=   	"map" "(" AtomicOrUnionType "," SequenceType ")"	
[108]   	ArrayTest	   ::=   	AnyArrayTest | TypedArrayTest	
[109]   	AnyArrayTest	   ::=   	"array" "(" "*" ")"	
[110]   	TypedArrayTest	   ::=   	"array" "(" SequenceType ")"	
[111]   	ParenthesizedItemType	   ::=   	"(" ItemType ")"	

|#

)
