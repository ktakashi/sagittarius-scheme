;;; -*- Scheme -*-
;;;
;;; mime.scm - RFC2045 Multipurpose Internet Mail Extensions utilities
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; Ref RFC2045 Multipurpose Internet Mail Extensions Part One
;; <http://www.ietf.org/rfc/rfc2045.txt>
;; Ref RFC2045 Multipurpose Internet Mail Extensions Part Two
;; <http://www.ietf.org/rfc/rfc2046.txt>
;; Ref RFC2045 Multipurpose Internet Mail Extensions Part Three
;; <http://www.ietf.org/rfc/rfc2047.txt>

;; The api names are from Gauche.
#!compatible
#< (sagittarius regex) >
(library (rfc mime)
    (export mime-parse-version
	    mime-parse-content-type
	    mime-compose-parameters
	    mime-compose-message
	    mime-compose-message-string
	    mime-decode-word
	    mime-decode-text
	    mime-encode-word
	    mime-encode-text
	    ;; mime-part record
	    ;; this is really incovenient...
	    <mime-part>
	    mime-part?
	    mime-part-type mime-part-type-set!
	    mime-part-subtype mime-part-subtype-set!
	    mime-part-parameters mime-part-parameters-set!
	    mime-part-transfer-encoding mime-part-transfer-encoding-set!
	    mime-part-headers mime-part-headers-set!
	    mime-part-parent mime-part-parent-set!
	    mime-part-index mime-part-index-set!
	    mime-part-content mime-part-content-set!
	    mime-part-source mime-part-source-set!

	    mime-parse-message)
    (import (except (rnrs) define)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (match)
	    (except (core) define) ; for /., maybe we want to something
	    (rename (sagittarius control) (define-with-key define))
	    (sagittarius regex)
	    (sagittarius)
	    (sagittarius io)
	    (encoding decoder)
	    (rfc :5322)
	    (rfc quoted-printable)
	    (rfc base64)
	    (slib queue)
	    (util list)
	    (math))

  (define *version-regex* #/^(\d+)\.(\d+)$/)

  ;; returns list of major and minor versions in integers
  (define (mime-parse-version field)
    (and-let* (( field )
	       (s (string-concatenate
		   (map (lambda (f) (format "~a" f)) (rfc5322-field->tokens field))))
	       (m (looking-at *version-regex* s)))
      (map (lambda (d) (string->number (m d))) '(1 2))))


  (define *ct-token-chars*
    (char-set-difference (ucs-range->char-set #x21 #x7e) (string->char-set "()<>@,;:\\\"/[]?=")))

  ;; RFC2045 Content-Type header field
  ;; returns (<type> <subtype (attribute . <value>) ...)
  (define (mime-parse-content-type field)
    (and field
	 (call-with-input-string field
	   (lambda (input)
	     (and-let* ((type (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? type) )
			( (eqv? #\/ (rfc5322-next-token input '())) )
			(subtype (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? subtype) ))
	       (cons* (string-downcase type)
		      (string-downcase subtype)
		      (mime-parse-parameters input)))))))

  ;; RFC2183 Content-Disposition header field
  ;; returns (<token> (<attribute> . <value>) ...)
  (define (mime-parse-content-disposition field)
    (and field
	 (call-with-input-string field
	   (lambda (input)
	     (and-let* ((token (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? token) ))
	       (cons (string-downcase token)
		     (mime-parse-parameters input)))))))

  ;; parse a parameter-values type header field
  ;;  ;paremter=value;parameter-value
  ;; => ((parameter . value) ...)
  (define (mime-parse-parameters :optional (input (current-input-port)))
    (let loop ((r '()))
      (cond ((and-let* (( (eqv? #\; (rfc5322-next-token input '())) )
			(attr (rfc5322-next-token input `(,*ct-token-chars*)))
			( (string? attr) )
			( (eqv? #\= (rfc5322-next-token input '())) )
			(val (rfc5322-next-token input `(,*ct-token-chars*
							 (,(string->char-set "\"")
							  . ,rfc5322-quoted-string))))
			( (string? val) ))
	       (cons attr val))
	     => (lambda (p) (loop (cons p r))))
	    (else (reverse! r)))))

  ;; Inverse of mime-parse-parameters
  ;; ((paramter . value) ...) => ;parameter=value;parameter=value ...
  (define (mime-compose-parameters pvs :key (port (current-output-port))
				            (start-column 0))
    (define quote-re #/[\"\\]/)
    (define (quote-value v)
      (if (string-every *ct-token-chars* v)
	  v
	  (string-append "\"" (regex-replace-all quote-re v "\\\\\\0") "\"")))
    (define (valid-name p)
      (let ((z (format "~a" p)))
	(if (string-every *ct-token-chars* z)
	    z
	    (assertion-violation 'mime-compose-parameters
				 "invalid parameter valur for rfc2822 header"
				 p))))
    (define (gen)
      (fold (lambda (pv column)
v	      (match pv
		((p . v)
		 (let* ((z (format "~a=~a" (valid-name p) (quote-value (format "~a" v))))
			(len (+ (string-length z) column)))
		   (cond ((> len 78)
			  (display ";\r\n") (display z) (string-length z))
			 (else
			  (display ";") (display z) (+ len column 2)))))
		(_ (assertion-violation 'mime-compose-parameters
					"bad parameter-value entry"
					pv))))
	    start-column pvs))
    (match port
      (#f (with-output-to-string gen))
      (#t (gen))
      ((? port?) (with-output-to-port port gen))))

  ;; RFC2047 header field encoding

  (define *mime-encoded-header-re*
    #/^=\?([-!#-'*+\w\^-~]+)\?([-!#-'*+\w\^-~]+)\?([!->@-~]+)\?=/)

  (define (%mime-decode-word word charset encoding body)
    (let ((decoder (lookup-decoder charset)))
      (if decoder
	  (cond ((string-ci=? encoding "q")
		 (decode decoder (quoted-printable-decode (string->utf8 body))))
		((string-ci=? encoding "b")
		 (decode decoder (base64-decode (string->utf8 body))))
		(else word))
	  word)))

  ;; decode rfc2047-encoded word, i.e. "=?...?="
  (define (mime-decode-word word)
    (cond ((looking-at *mime-encoded-header-re* word)
	   => (lambda (m)
		(if (equal? (m 'after) "")
		    (%mime-decode-word word (m 1) (m 2) (m 3))
		    word)))
	  (else word)))

  ;; decode the entire header field body, possibly a mixture of
  ;; encoded-words and orginary words.
  (define (mime-decode-text body)
    (let loop ((s body))
      (receive (pre rest) (string-scan s "=?" 'before*)
	(cond ((not pre) s)
	      ((looking-at *mime-encoded-header-re* rest)
	       => (lambda (m)
		    (string-append pre
				   (%mime-decode-word (m 0) (m 1) (m 2) (m 3))
				   (loop (m 'after)))))
	      (else s)))))

  (define (%canonical-encoding encoding)
    (case encoding
      ((B b base64) 'B)
      ((Q q quoted-printable) 'Q)
      (else
       (assertion-violation 'canonical-encoding
			    "unsupported MIME header encoding specifier"
			    encoding))))

  (define (mime-encode-word word :optional (charset 'utf-8)
					   (transfer-encoding 'base64))
    ;; decoder is just a codec.
    (let ((decoder (lookup-decoder charset))
	  (enc (%canonical-encoding transfer-encoding)))
      (format "=?~a?~a?~a?=" charset enc
	      ((if (eq? enc 'B)
		   base64-encode-string
		   quoted-printable-encode-string)
	       word (make-transcoder decoder (eol-style crlf))))))

  (define (mime-encode-text body :optional (charset 'utf-8)
					   (transfer-encoding 'base64)
					   (line-width 76)
					   (start-column 0)
					   (force #f))
    (check-arg symbol? charset 'mime-encode-text)
    (let ((enc (%canonical-encoding transfer-encoding))
	  (cslen (string-length (symbol->string charset)))
	  (pass-through? (and (not force)
			      (string-every char-set:ascii body))))
      (define (estimate-width s i)
	(let1 na (string-count s char-set:ascii 0 i)
	  (+ 6 cslen
	     (if (eq? enc 'B)
		 (ceiling (* (+ na (* (- i na) 3)) 4/3))
		 (let1 ng (string-count 
			   s
			   (char-set-union (ucs-range->char-set 
					    (char->integer #\!)
					    (char->integer #\<))
					   (ucs-range->char-set
					    (char->integer #\>)
					    (char->integer #\~))))
		   (+ (- na ng) (* 3 (+ ng (* (- i na) 3)))))))))
      (define (encode-word w)
	(mime-encode-word w charset enc))
      (define (encode str width adj)
	(or (and-let* ((estim (estimate-width str (string-length str)))
		       ( (< (* adj estim) width) )
		       (ew (encode-word str)))
	      (if (<= (string-length ew) width)
		  `(,ew)
		  (encode str width (* adj (/. (string-length ew) width)))))
	    (let loop ((k (min (string-length str) (quotient width 2))))
	      (let1 estim (* adj (estimate-width str k))
		(if (<= estim width)
		    (let1 ew (encode-word (string-take str k))
		      (if (<= (string-length ew) width)
			  (cons* ew "\r\n "
				 (encode (string-drop str k)
					 (- line-width 1) adj))
			  (loop (exact (floor (* k (/ width (string-length ew))))))))
		    (loop (exact (floor (* k (/ width estim))))))))))

      (define (fill str width)
	(if (<= (string-length str) width)
	    `(,str)
	    (or (and-let* ((pos (string-index-right str #\space 0 width)))
		  (cons* (string-take str pos) "\r\n "
			 (fill (string-drop str width) (- line-width 1))))
		(cons* (string-take str width) "\r\n "
		       (fill (string-drop str width) (- line-width 1))))))

      (cond ((or (not line-width) (zero? line-width))
	     (if pass-through? body (encode-word body)))
	    ((< line-width 30)
	     (assertion-violation 'mime-encode-word
				  (format "line width (~a) is too short to encode header field body: ~s"
					  line-width body)
				  body charset transfer-encoding line-width start-column force))
	    ((< (- line-width start-column) 30)
	     (string-concatenate
	      (cons "\r\n " (if pass-through?
				(fill body (- line-width 1))
				(encode body (- line-width 1) 1.0)))))
	    (else 
	     (string-concatenate
	      (if pass-through?
		  (fill body (- line-width start-column))
		  (encode body (- line-width start-column) 1.0)))))))

  ;; mime-port
  ;; we need to wrap with record
  (define-record-type (<mime-port> make-mime-port mime-port?)
    ;; actually only state is mutable, but for constructor
    (fields (mutable port mime-port-port mime-port-port-set!)
	    (mutable state mime-port-state mime-port-state-set!)
	    (mutable self mime-port-self mime-port-self-set!)) ;; ughh, tricky...
    (protocol
     (lambda (p)
       (lambda (boundary srcport)
	 (let ((r (p #f 'prologue '())))
	   (mime-port-self-set! r r)
	   (mime-port-port-set! r (%make-mime-port boundary srcport r))
	   r)))))

  (define (%make-mime-port boundary srcport self)
    (define q (make-queue))
    (define --boundary (string-append "--" boundary))

    (define eof (eof-object))
    (define (deq! q) (if (queue-empty? q) eof (dequeue! q)))

    (define (getc)
      (if (queue-empty? q)
	  (case (mime-port-state self)
	    ((prologue) (skip-prologue))
	    ((boundary eof) eof)
	    (else (newc)))
	  (dequeue! q)))

    (define (newc)
      (match (get-char srcport)
	((and #\x0d b) ;; CR, check to see LF
	 (let1 b2 (lookahead-char srcport)
	   (if (eqv? b2 #\x0a)
	       (begin
		 (get-char srcport)
		 (enqueue! q b)
		 (enqueue! q #\x0a)
		 (check-boundary))
	       b)))
	((and #\x0a b) ;; LF, check boundary
	 (enqueue! q b) (check-boundary))
	((? eof-object?) (mime-port-state-set! self 'eof) eof)
	(b b)))

    (define (check-boundary)
      (let loop ((b   (lookahead-char srcport))
		 (ind 0)
		 (max (string-length --boundary)))
	(cond ((eof-object? b) (deq! q))
	      ((= ind max)
	       (cond ((memv b '(#\x0d #\x0a)) ;; found boundary
		      (get-char srcport)	    ;; consume LF or CRLF
		      (when (and (eqv? #\x0d b)
				 (eqv? #\x0a (lookahead-char srcport)))
			(get-char srcport))
		      (dequeue-all! q)
		      (mime-port-state-set! self 'boundary)
		      eof)
		     ((eqv? b #\x2d)	;; maybe end boundary
		      (enqueue! q (get-char srcport))
		      (cond ((eqv? (lookahead-char srcport) #\x2d) ; yes
			     (get-char srcport)
			     (dequeue-all! q)
			     (skip-epilogue))
			    (else (deq! q))))
		     (else (deq! q))))
	      ((char=? b (string-ref --boundary ind))
	       (enqueue! q (get-char srcport))
	       (loop (lookahead-char srcport) (+ ind 1) max))
	      ((queue-empty? q) (newc))
	      (else (dequeue! q)))))

    (define (skip-prologue)
      (let loop ((b (check-boundary)))
	(cond ((eof-object? b)
	       (cond ((eq? (mime-port-state self) 'boundary)
		      (mime-port-state-set! self 'body)
		      (getc))
		     (else
		      (mime-port-state-set! self 'eof)
		      eof)))
	      ((queue-empty? q) (loop (newc)))
	      (else (dequeue-all! q) (loop (newc))))))

    (define (skip-epilogue)
      (let loop ((b (get-char srcport)))
	(if (eof-object? b)
	    (begin (mime-port-state-set! self 'eof) b)
	    (loop (get-char srcport)))))

    (define (read! s start count)
      (let loop ((ind start))
	(if (= ind count)
	    count
	    (let1 b (getc)
	      (if (eof-object? b)
		  ind
		  (begin
		    (string-set! s ind b)
		    (loop (+ ind 1))))))))

    (define (close)
      (close-input-port srcport))

    (make-custom-textual-input-port "mime-port" read! #f #f close))

  ;; basic streaming parser
  (define-record-type (<mime-part> make-mime-part mime-part?)
    (fields (mutable type mime-part-type mime-part-type-set!)
	    (mutable subtype mime-part-subtype mime-part-subtype-set!)
	    (mutable parameters mime-part-parameters mime-part-parameters-set!)
	    (mutable transfer-encoding mime-part-transfer-encoding mime-part-transfer-encoding-set!)
	    (mutable headers mime-part-headers mime-part-headers-set!)
	    (mutable parent mime-part-parent mime-part-parent-set!)
	    (mutable index mime-part-index mime-part-index-set!)
	    (mutable content mime-part-content mime-part-content-set!)
	    (mutable source mime-part-source mime-part-source-set!))	; only used for composing
    (protocol
     (lambda (p)
       (lambda args
	 (let-keywords* args ((type    "text")
			      (subtype "plain")
			      (parameters '())
			      (transfer-encoding #f)
			      (headers '())
			      (parent #f)
			      (index 0)
			      (content #f)
			      (source #f))
	   (p type subtype parameters transfer-encoding headers parent index content source))))))

  (define (mime-parse-message port headers handler)
    (internal-parse port headers handler #f 0
		    '("text" "plain" ("charset" . "us-ascii"))))

  (define (internal-parse port headers handler parent index default-type)
    (let* ((ctype (or (mime-parse-content-type (rfc5322-header-ref headers "content-type"))
		      default-type))
	   (enc   (rfc5322-header-ref headers "content-transfer-encoding" "7bit"))
	   (packet (make-mime-part
		    :type (car ctype)	     ;; type
		    :subtype (cadr ctype)    ;; subtype
		    :parameters (cddr ctype) ;; parameters
		    :transfer-encoding enc   ;; transfer-encoding
		    :headers headers
		    :parent parent
		    :index index)))
      (cond ((equal? (car ctype) "multipart")
	     (multipart-parse port packet handler))
	    ((equal? (car ctype) "message")
	     (message-parse port packet handler))
	    (else
	     (mime-part-content-set! packet (handler packet port))
	     packet))))

  (define (multipart-parse port packet handler)
    (let* ((boundary (or (cond ((assoc "boundary" (mime-part-parameters packet))
				=> cdr))
			 (assertion-violation 'multipart-parse
					      "No boundary given for multipart message"
					      (mime-part-headers packet))))
	   (default-type (if (equal? (mime-part-subtype packet) "digest")
			     '("message" "rfc822")
			     '("text" "plain" ("charset" . "us-ascii"))))
	   (mime-port (make-mime-port boundary port)))
      (let loop ((index 0)
		 (contents '()))
	(let* ((headers (rfc5322-read-headers (mime-port-port mime-port)))
	       (r (internal-parse (mime-port-port mime-port)
				  headers handler
				  packet index default-type)))
	  (case (mime-port-state mime-port)
	    ((boundary)
	     (mime-port-state-set! mime-port 'body)
	     (loop (+ index 1) (cons r contents)))
	    ((eof)
	     (mime-part-content-set! packet (reverse! (cons r contents)))
	     packet)
	    (else ;; parser returned without reading entire part.
	     ;; discard the rest of the part.
	     (get-string-all port)
	     packet))))))

  (define (message-parse port packet handler)
    (let* ((headers (rfc5322-read-headers port))
	   (r (internal-parse port headers handler packet 0
			      '("text" "plain" ("charset" . "us-ascii")))))
      (mime-part-content-set! packet (list r))
      packet))

  ;; body readers
;;  (define (mime-retrieve-body packet inp outp)
;;    (define (read-line/nl)
;;      (let loop ((c (get-char inp))
;;		 (chars '()))
;;	(cond ((eof-object? c)
;;	       (if (null? chars) c (list->string (reverse! chars))))
;;	      ((char=? #\newline) (list->string (reverse! (cons c chars))))
;;	      ((char=? #\return)
;;	       (let1 c (lookahead-char inp)
;;		 (if (char=? c #\newline)
;;		     (list->string (reverse! (cons* (get-char inp)
;;						    #\return
;;						    chars)))
;;		     (list->string (reverse! cons #\return chars)))))
;;	      (else (loop (get-char inp) (cons c chars))))))
;;
;;    (define (read-text decoder)
;;      (let loop ((line (read-line/nl)))
;;	(unless (eof-object? line)
;;	  (display (decoder line) outp)
;;	  (loop (read-line/nl)))))
;;
;;    (define (read-base64)
;;      (define (base64-output string out)
;;	(display (base64-decode-string string (make-transcoder (utf-8-codec) 'crlf)) out))
;;
;;      (let1 o (call-with-output-string
;;		(lambda (buf)
;;		  (let loop ((line (rfc5322-line-reader inp)))
;;		    (unless (eof-object? line)
;;		      (display line buf)
;;		      (loop (rfc5322-line-reader inp))))))
;;	(base64-output o outp)))
;;
;;    (let1 env (mime-part-transfer-encoding packet)
;;      (cond ((string-ci? enc "base64") (read-base64))
;;	    ((string-ci? enc "quoted-printable")
;;	     (read-text quoted-printable-decode-string))
;;	    ((member enc '("7bit" "8bit" "binary"))
;;	     ;; TODO we need binary port, but how should we get it?
;;	     (get-string-all inp)))))


  ;; compose
  (define (mime-compose-message parts
				:optional (port (current-output-port))
				:key (boundary (mime-make-boundary)))
    (dolist (p parts)
      (for-each (cut display <> port) `("\r\n--" ,boundary "\r\n"))
      (mime-generate-one-part (canonical-part p) port))
    (for-each (cut display <> port) `("\r\n--" ,boundary "--\r\n"))
    boundary)

  (define (mime-compose-message-string parts
				       :key (boundary (mime-make-boundary)))
    (values (call-with-output-string
	      (cut mime-compose-message parts <> :boundary boundary))
	    boundary))

  (define mime-make-boundary
    (let ((rc (secure-random RC4)))
      (lambda ()
	(format "boundary-~a" (number->string (* (random RC4 (expt 2 64))
						 (time-second (current-time)))
					      36)))))

  (define (canonical-part p)
    (match p
      ((? (cut mime-part? <>)) p)
      (((type subtype . params) (headers ...) body)
       (let1 hs (filter-map canonical-header headers)
	 (apply make-mime-part :type type :subtype subtype
		:parameters params :headers hs
		:transfer-encoding (rfc5322-header-ref hs "content-transfer-encoding")
		(match body
		  ((? string?) `(:content ,body))
		  (('file name) `(:source ,name))
		  (('subparts ps ...) `(:content ,(map canonical-part ps)))
		  (_ (assertion-violation 'canonical-part
					  "Invalid mime part body spec" body))))))
      (_ (assertion-violation 'canonical-part
			      "Invalid mime part spec" p))))

  (define (canonical-header header)
    (match header
      ((name . x) (cons (format "~a" name) x))
      (_ #f)))

  (define (mime-generate-one-part part port)
    (when (list? (mime-part-content part))
      (unless (member "boundary" (mime-part-parameters part))
	(mime-part-parameters-set! part
				   (append! (mime-part-parameters mime)
					    (list (cons "boundary" (mime-make-boundary))))))
      (let1 cte (mime-generate-part-header part port)
	(with-output-to-port port
	  (lambda ()
	    (cond
	     ((mime-part-source part) =>
	      (cut with-input-from-file <> (cut mime-generate-part-header
						part cte)))
	     ((list? (mime-part-content part))
	      (mime-compose-message
	       (mime-part-content part) port
	       :boundary 
	       (cond ((assoc "boundary" (mime-part-parameters)) => cdr)
		     (else #f))))
	     ((string? (mime-part-content part))
	      (with-input-from-string (mime-part-content part)
		(cut mime-generate-part-body part cte)))
	     (else (assertion-violation 'mime-generate-one-part
					"unsupported MIME part content"))))))))
  (define (mime-generate-part-header part port)
    (let1 cte (mime-part-transfer-encoding part)
      (rfc5322-write-headers
       `(("Content-type" ,(format "~a/~a~a" (mime-part-type part)
				  (mime-part-subtype part)
				  (mime-compose-parameters
				   (mime-part-parameters mime) #f)))
	 ,@(cond-list (cte => (cut list "content-transfer-encoding" <>)))
	 ,@(filter-map gen-header-1 (mime-part-headers part)))
       :output port :check :ignore)
      cte))

  (define (gen-header-1 h)
    (match h
      (("content-transfer-encoding" . _) #f)
      (("content-type" . _) #f)
      ((name (value pv ...))
       (let* ((sval (format "~a" value))
	      (spvs (mime-compose-parameters pv #f
			:start-column (+ (string-length name)
					 (string-length sval) 2))))
	 `(,name ,(if (null? pv) sval (string-append sval spvs)))))
      ((name value) h)))

  ;; we know current ports are textual
  (define (mime-generate-part-body part transfer-enc)
    (cond ((or (not transfer-enc) (member transfer-enc '("binary" "7bit")
					  string-ci=?))
	   ;; danger
	   (let ((in (get-string-all (current-input-port))))
	     (put-string (current-output-port) bv)))
	  ((string-ci=? transfer-enc "base64")
	   (base64-encode-string (get-string-all (current-input-port))))
	  ((string-ci=? transfer-enc "quoted-printable")
	   (quoted-printable-decode-string (get-string-all 
					    (current-input-port))))
	  (else 
	   (assertion-violation 'mime-generate-part-body
				"Unsupported transfer encoding encountered \
                                 while composing mime message" transfer-enc))))
  )
				   
