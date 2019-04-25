;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; uri/parser.scm - PEG URI parser
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

;; reference
;; Appendix A.  Collected ABNF for URI
;; https://tools.ietf.org/html/rfc3986#appendix-A
#!nounbound
(library (rfc uri parser)
    (export uri-parse
	    uri-scheme&specific
	    uri-decompose-hierarchical
	    uri-decompose-authority

	    *uri:decode*
	    
	    ;; parsers
	    uri:uri-parser
	    uri:absolute-uri-parser
	    uri:uri-reference-parser
	    uri:scheme-parser
	    uri:hier-part-parser
	    uri:authority-parser
	    uri:host-parser
	    uri:port-parser
	    uri:query-parser
	    uri:fragment-parser
	    uri:path-parser
	    
	    ;; mid level
	    uri:relative-ref-parser
	    uri:relative-part-parser
	    uri:path-noscheme-parser
	    uri:path-absolute-parser
	    uri:path-abempty-parser
	    uri:path-empty-parser
	    uri:ip-literal-parser
	    uri:ipv-future-parser
	    uri:ipv6-address-parser
	    uri:ipv4-address-parser
	    uri:path-abempty-parser
	    uri:path-absolute-parser
	    uri:path-rootless-parser
	    uri:path-empty-parser
	    uri:segment-parser
	    uri:segment-nz-parser
	    uri:segment-nz-nc-parser
	    uri:reg-name-parser
	    uri:h16-parser
	    uri:dec-octet-parser
	    uri:ls32-parser

	    ;; low level (character)
	    uri:alpha
	    uri:digit
	    uri:hexdig
	    uri:pchar
	    uri:gen-delims
	    uri:sub-delims
	    uri:pct-encoded
	    uri:unreserved
	    uri:reserved)
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs))

(define *uri:decode* (make-parameter #f))

(define alpha-set (char-set-intersection char-set:ascii char-set:letter))
(define digit-set (char-set-intersection char-set:ascii char-set:digit))

(define uri:alpha ($char-set-contains? alpha-set))
(define uri:digit ($char-set-contains? digit-set))
(define uri:hexdig ($char-set-contains? char-set:hex-digit))

;; scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
(define uri:scheme-parser
  ($do (c uri:alpha)
       (c* ($many
	    ($or uri:alpha uri:digit ($eqv? #\+) ($eqv? #\-) ($eqv? #\.))))
       ($return (list->string (cons c c*)))))

;; unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
(define uri:unreserved
  ($or uri:alpha uri:digit ($eqv? #\-) ($eqv? #\.) ($eqv? #\_) ($eqv? #\~)))

;; pct-encoded   = "%" HEXDIG HEXDIG
(define uri:pct-encoded
  ($do (($eqv? #\%))
       (c1 uri:hexdig)
       (c2 uri:hexdig)
       ;; TODO inefficient...
       ($return (integer->char (string->number (string c1 c2) 16)))))

;; sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;;               / "*" / "+" / "," / ";" / "="
(define uri:sub-delims
  ($char-set-contains? (string->char-set "!$&'()*+,;=")))

;; pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
(define uri:pchar
  ($or uri:unreserved
       ;; add '%' in front of uri:pct-encoded...
       ($unless (*uri:decode*) ($eqv? #\%))
       uri:pct-encoded
       uri:sub-delims
       ($eqv? #\:)
       ($eqv? #\@)))

;; segment       = *pchar
(define uri:segment-parser
  ($do (c* ($many uri:pchar)) ($return (list->string c*))))

;; segment-nz    = 1*pchar
(define uri:segment-nz-parser
  ($do (c* ($many uri:pchar 1)) ($return (list->string c*))))

;; segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
;;                 ; non-zero-length segment without any colon ":"   
(define uri:segment-nz-nc-parser
  ($do (c* ($many
	    ($or uri:unreserved uri:pct-encoded uri:sub-delims ($eqv? #\@)) 1))
       ($return (list->string c*))))

;; path-absolute = "/" [ segment-nz *( "/" segment ) ]
(define uri:path-absolute-parser
  ($do (($eqv? #\/))
       (s* ($optional ($do (n uri:segment-nz-parser)
			   (s* ($many ($seq ($eqv? #\/) uri:segment-parser)))
			   ($return (cons n s*))) '()))
       ($return `(/ ,@s*))))

;; path-abempty  = *( "/" segment )
(define uri:path-abempty-parser
  ($do (p* ($many ($seq ($eqv? #\/) uri:segment-parser)))
       ($return (if (null? p*) '() `(/ ,@p*)))))

;; path-rootless = segment-nz *( "/" segment )
(define uri:path-rootless-parser
  ($do (s uri:segment-nz-parser)
       (s* ($many ($seq ($eqv? #\/) uri:segment-parser)))
       ($return `(! ,s ,@s*))))

;; path-empty    = 0<pchar>
(define uri:path-empty-parser
  ($do (($peek ($not uri:pchar))) ($return '())))
  
;; userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
(define uri:userinfo-parser
  ($do (c* ($many
	    ($or uri:unreserved uri:pct-encoded uri:sub-delims ($eqv? #\:))))
       ($return (list->string c*))))
;; port         = *DIGIT
(define uri:port-parser
  ($do (c* ($many uri:digit)) ($return (list->string c*))))

;; dec-octet     = DIGIT                 ; 0-9
;;               / %x31-39 DIGIT         ; 10-99
;;               / "1" 2DIGIT            ; 100-199
;;               / "2" %x30-34 DIGIT     ; 200-249
;;               / "25" %x30-35          ; 250-255
(define uri:dec-octet-parser
  ($or ($do (d uri:digit) ($return (string d)))
       ($do (d1 ($char-set-contains? (string->char-set "123456789")))
	    (d2 uri:digit)
	    ($return (string d1 d2)))
       ($do (d1 ($eqv? #\1))
	    (d* ($repeat uri:digit 2))
	    ($return (list->string (cons d1 d*))))
       ($do (d1 ($eqv? #\2))
	    (d2 ($char-set-contains? (string->char-set "01234")))
	    (d3 uri:digit)
	    ($return (string d1 d2 d3)))
       ($do (d1 ($eqv? #\2))
	    (d2 ($eqv? #\5))
	    (d3 ($char-set-contains? (string->char-set "012345")))
	    ($return (string d1 d2 d3)))))

;; IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
(define uri:ipv4-address-parser
  ($do (c1 uri:dec-octet-parser) (($eqv? #\:))
       (c2 uri:dec-octet-parser) (($eqv? #\:))
       (c3 uri:dec-octet-parser) (($eqv? #\:))
       (c4 uri:dec-octet-parser)
       ($return (string-append c1 "." c2 "." c3 "." c4))))

;; h16           = 1*4HEXDIG
(define uri:h16-parser
  ($do (c* ($many uri:hexdig 1 4))
       ($return (list->string c*))))
;; common utility...
;; h16-colon      = 1*4HEXDIG ":"
(define uri:h16-colon-parser
  ($do (h16 uri:h16-parser)
       (($eqv? #\:))
       (($peek ($not ($eqv? #\:)))) ;; we don't
       ($return (string-append h16 ":"))))

;; ls32          = ( h16 ":" h16 ) / IPv4address
(define uri:ls32-parser
  ($or ($do (h16c uri:h16-colon-parser)
	    (h16  uri:h16-parser)
	    ($return (string-append h16c h16)))
       uri:ipv4-address-parser))

;; IPv6address   =                            6( h16 ":" ) ls32
(define uri:ipv6-address-parser1
  ($do (c6 ($repeat uri:h16-colon-parser 6))
       (ls uri:ls32-parser)
       ($return (string-append (string-concatenate c6) ls))))
;;               /                       "::" 5( h16 ":" ) ls32
(define uri:ipv6-address-parser2
  ($do (($token "::"))
       (c6 ($repeat uri:h16-colon-parser 5))
       (ls uri:ls32-parser)
       ($return (string-append "::" (string-concatenate c6) ls))))
;;               / [               h16 ] "::" 4( h16 ":" ) ls32
(define uri:ipv6-address-parser3
  ($do (h ($optional uri:h16-parser ""))
       (($token "::"))
       (c6 ($repeat uri:h16-colon-parser 4))
       (ls uri:ls32-parser)
       ($return (string-append h "::" (string-concatenate c6) ls))))
;;               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
(define uri:ipv6-address-parser4
  ($do (h ($optional ($do (h1 ($optional uri:h16-colon-parser "") )
			  (h2 uri:h16-parser)
			  ($return (string-append h1 h2)))
		     ""))
       (($token "::"))
       (c6 ($repeat uri:h16-colon-parser 3))
       (ls uri:ls32-parser)
       ($return (string-append h "::" (string-concatenate c6) ls))))
;;               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
(define uri:ipv6-address-parser5
  ($do (h ($optional ($do (h1 ($many uri:h16-colon-parser 0 2))
			  (h2 uri:h16-parser)
			  ($return (string-append (string-concatenate h1) h2)))
		     ""))
       (($token "::"))
       (c6 ($repeat uri:h16-colon-parser 2))
       (ls uri:ls32-parser)
       ($return (string-append h "::" (string-concatenate c6) ls))))
;;               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
(define uri:ipv6-address-parser6
  ($do (h ($optional ($do (h1 ($many uri:h16-colon-parser 0 3))
			  (h2 uri:h16-parser)
			  ($return (string-append (string-concatenate h1) h2)))
		     ""))
       (($token "::"))
       (c uri:h16-colon-parser)
       (ls uri:ls32-parser)
       ($return (string-append h "::" c ls))))
;;               / [ *4( h16 ":" ) h16 ] "::"              ls32
(define uri:ipv6-address-parser7
  ($do (h ($optional ($do (h1 ($many uri:h16-colon-parser 0 4))
			  (h2 uri:h16-parser)
			  ($return (string-append (string-concatenate h1) h2)))
		     ""))
       (($token "::"))
       (ls uri:ls32-parser)
       ($return (string-append h "::" ls))))
;;               / [ *5( h16 ":" ) h16 ] "::"              h16
(define uri:ipv6-address-parser8
  ($do (h* ($optional ($do (h1 ($many uri:h16-colon-parser 0 5))
			   (h2 uri:h16-parser)
			   ($return (string-append (string-concatenate h1) h2)))
		      ""))
       (($token "::"))
       (h uri:h16-parser)
       ($return (string-append h* "::" h))))
;;               / [ *6( h16 ":" ) h16 ] "::"
(define uri:ipv6-address-parser9
  ($do (h ($optional ($do (h1 ($many uri:h16-colon-parser 0 6))
			  (h2 uri:h16-parser)
			  ($return (string-append (string-concatenate h1) h2)))
		     ""))
       (($token "::"))
       ($return (string-append h "::"))))
(define uri:ipv6-address-parser
  ($or uri:ipv6-address-parser1
       uri:ipv6-address-parser2
       uri:ipv6-address-parser3
       uri:ipv6-address-parser4
       uri:ipv6-address-parser5
       uri:ipv6-address-parser6
       uri:ipv6-address-parser7
       uri:ipv6-address-parser8
       uri:ipv6-address-parser9))

;; IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
(define uri:ipv-future-parser
  ($do (($eqv? #\v))
       (h* ($many uri:hexdig 1))
       (($eqv? #\.))
       (c* ($many ($or uri:unreserved uri:sub-delims ($eqv? #\:)) 1))
       ($return (string-append "v" (list->string h*) "." (list->string c*)))))

;; IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
(define uri:ip-literal-parser
  ($do (($eqv? #\[))
       (ip ($or uri:ipv6-address-parser uri:ipv-future-parser))
       (($eqv? #\]))
       ($return (string-append "[" ip "]"))))

;; reg-name      = *( unreserved / pct-encoded / sub-delims )
(define uri:reg-name-parser
  ($do (c* ($many ($or uri:unreserved uri:pct-encoded uri:sub-delims)))
       ($return (and (not (null? c*)) (list->string c*)))))
;; host          = IP-literal / IPv4address / reg-name
(define uri:host-parser
  ($or uri:ip-literal-parser uri:ipv4-address-parser uri:reg-name-parser))

;; authority     = [ userinfo "@" ] host [ ":" port ]
(define uri:authority-parser
  ($do (u ($optional ($do (u uri:userinfo-parser) (($eqv? #\@)) ($return u))))
       (host uri:host-parser)
       (p ($optional ($seq ($eqv? #\:) uri:port-parser)))
       ($return `(,u ,host ,p))))

;; hier-part     = "//" authority path-abempty
(define uri:authority-path-abempty
  ($do (($token "//"))
       (auth uri:authority-parser)
       (path uri:path-abempty-parser)
       ($return `(// ,auth ,path))))
;;               / path-absolute
;;               / path-rootless
;;               / path-empty
(define uri:hier-part-parser
  ($or uri:authority-path-abempty
       uri:path-absolute-parser
       uri:path-rootless-parser
       uri:path-empty-parser))

;; query         = *( pchar / "/" / "?" )
(define uri:query-parser
  ($do (c* ($many ($or uri:pchar ($eqv? #\/) ($eqv? #\?))))
       ($return (list->string c*))))

;; fragment      = *( pchar / "/" / "?" )
(define uri:fragment-parser uri:query-parser)

;; URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
(define uri:uri-parser
  ($do (s uri:scheme-parser)
       (($eqv? #\:))
       (h uri:hier-part-parser)
       (q ($optional ($seq ($eqv? #\?) uri:query-parser)))
       (f ($optional ($seq ($eqv? #\#) uri:fragment-parser)))
       ($return (list s h q f))))

;; absolute-URI  = scheme ":" hier-part [ "?" query ]
(define uri:absolute-uri-parser
  ($do (s uri:scheme-parser)
       (($eqv? #\:))
       (h uri:hier-part-parser)
       (q ($optional ($seq ($eqv? #\?) uri:query-parser)))
       ($return (list s h q))))

;; path-noscheme = segment-nz-nc *( "/" segment )
(define uri:path-noscheme-parser
  ($do (s uri:segment-nz-nc-parser)
       (s* ($many ($seq ($eqv? #\/) uri:segment-parser)))
       ($return `(! ,s ,@s*))))

;; relative-part = "//" authority path-abempty
;;               / path-absolute
;;               / path-noscheme
;;               / path-empty
(define uri:relative-part-parser
  ($or uri:authority-path-abempty
       uri:path-absolute-parser
       uri:path-noscheme-parser
       uri:path-empty-parser))

;; relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
(define uri:relative-ref-parser
  ($do (p uri:relative-part-parser)
       (q ($optional ($seq ($eqv? #\?) uri:query-parser)))
       (f ($optional ($seq ($eqv? #\#) uri:fragment-parser)))
       ($return (list p q f))))

;; URI-reference = URI / relative-ref
(define uri:uri-reference-parser
  ($or uri:uri-parser
       ($do (r uri:relative-ref-parser) ($return (cons #f r)))))

;; extra?
;; path          = path-abempty    ; begins with "/" or is empty
;;               / path-absolute   ; begins with "/" but not "//"
;;               / path-noscheme   ; begins with a non-colon segment
;;               / path-rootless   ; begins with a segment
;;               / path-empty      ; zero characters
(define uri:path-parser
  ($or uri:path-abempty-parser
       uri:path-absolute-parser
       uri:path-noscheme-parser
       uri:path-rootless-parser
       uri:path-empty-parser))

;; gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
(define uri:gen-delims
  ($char-set-contains? (string->char-set ":/?#[]@")))

;; reserved      = gen-delims / sub-delims
(define uri:reserved ($or uri:gen-delims uri:sub-delims))

(define scheme-colon-parser
  ($do (scheme uri:scheme-parser)
       (($eqv? #\:))
       ($return scheme)))
(define (uri-scheme&specific uri)
  (define lseq (generator->lseq (string->generator uri)))
  (let-values (((s v nl) (scheme-colon-parser lseq)))
    (if (parse-success? s)
	(values (string-downcase v) (list->string (lseq-realize nl)))
	(values #f uri))))

(define (->path-string segments)
  (cond ((null? segments) #f)
	((eq? (car segments) '/)
	 (string-append "/" (string-join (cdr segments) "/")))
	(else (string-join (cdr segments) "/"))))
	 
(define (uri-decompose-hierarchical specific)
  (define lseq (generator->lseq (string->generator specific)))
  (define (->authority auth)
    (let ((u (car auth))
	  (h (cadr auth))
	  (p (caddr auth)))
      (cond ((and u p) (string-append u "@" h ":" p))
	    (u         (string-append u "@" h))
	    (p         (string-append h ":" p))
	    (else      h))))
  (let-values (((s v nl) (uri:relative-ref-parser lseq)))
    (if (parse-success? s)
	(let ((rel (car v))
	      (q   (cadr v))
	      (f   (caddr v)))
	  (cond ((null? rel) (values #f #f q f))
		((eq? (car rel) '//)
		 (let ((auth (cadr rel)))
		   (values (->authority auth) (->path-string (caddr rel)) q f)))
		(else (values #f (->path-string rel) q f))))
	(values #f #f #f #f))))

;; returns (values userinfo host port)
(define (uri-decompose-authority auth)
  (define lseq (generator->lseq (string->generator auth)))
  (let-values (((s v nl) (uri:authority-parser lseq)))
    (if (parse-success? s)
	(values (car v) (cadr v) (caddr v))
	(values #f #f #f))))

(define (uri-parse uri)
  (define lseq (generator->lseq (string->generator uri)))
  (define (decompose hier)
    (cond ((null? hier) #f)
	  ((eq? (car hier) '//)
	   (let ((auth (cadr hier))
		 (path (caddr hier)))
	     (values (car auth) (cadr auth) (caddr auth) (->path-string path))))
	  (else (values #f #f #f (->path-string hier)))))
  ;; interestingly, it's reference...
  (let-values (((s v nl) (uri:uri-reference-parser lseq)))
    (if (parse-success? s)
	(let ((scheme (car v))
	      (hier   (cadr v))
	      (query  (caddr v))
	      (frag   (cadddr v)))
	  (let-values (((user-info host port path) (decompose hier)))
	    (values scheme
		    user-info
		    host
		    (and port (string->number port))
		    path
		    query
		    frag))))))
)
