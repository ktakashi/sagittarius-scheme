;; -*- mode: scheme; coding: utf-8; -*-

(library (asn.1 lexer)
    (export make-lexer
	    &asn.1-lexical-error asn.1-lexical-error?
	    condition-asn.1-lexical-token)
    (import (rnrs)
	    (asn.1 parser) ;; for lexical-token and so
	    (asn.1 types)
	    (text parse)
	    (srfi :1 lists)
	    (srfi :2 and-let*)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (sagittarius)
	    )

  (define-condition-type &asn.1-lexical-error &asn.1-error
    make-asn.1-lexical-error asn.1-lexical-error?
    (token condition-asn.1-token))

  (define (raise-asn.1-lexical-error who message token . irritants)
    (raise
     (apply condition
	    (filter values
		    (list
		     (make-asn.1-lexical-error token)
		     (and who (make-who-condition who))
		     (make-message-condition message)
		     (make-irritants-condition irritants))))))

  (define reserved 
    '(("ANY"        . ANY)
      ("BY"         . BY)
      ("CHOICE"     . CHOICE)
      ("COMPONENTS" . COMPONENTS)
      ("DEFINED"    . DEFINED)
      ("ENUM"       . ENUM)
      ("ENUMERATED" . ENUMERATED)
      ("EXPLICIT"   . EXPLICIT)
      ("IMPLICIT"   . IMPLICIT)
      ("OF"         . OF)
      ("OPTIONAL"   . OPTIONAL)
      ("SEQUENCE"   . SEQUENCE)
      ("SET"        . SET)
      #;("::="        . ASSIGN)))

  (define reserved-char
    '((#\, . COMMA)
      (#\{ . LBRACE)
      (#\} . RBRACE)))

  (define class-tags 
    `(("APPLICATION" . ,CLASS_APPLICATION)
      ("UNIVERSAL"   . ,CLASS_UNIVERSAL)
      ("PRIVATE"     . ,CLASS_PRIVATE)
      ("CONTEXT"     . ,CLASS_CONTEXT)))

  (define word-charset (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))
  (define class-charset (string->char-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"))

  (define (make-lexer inport)
    (lambda ()
      (define stacked '())
      (define prev #f)
      (letrec* ((skip-space (lambda () (skip-while char-set:whitespace inport)))
		(next-char  (lambda () (skip-space) (get-char inport)))
		(peek-char  (lambda () (skip-space) (lookahead-char inport)))
		(next-token (lambda (set) (skip-space) (next-token-of set inport))))
	(let loop ()
	  (cond (prev 
		 ;; previous token
		 (let ((p prev))
		   (set! prev #f)
		   p))
		((null? stacked)
		 (let ((location (apply make-source-location (append! (port-info inport) '(-1 -1))))
		       (ch (peek-char)))
		   (cond ((eof-object? ch) '*eoi*)
			 ((and (char=? ch #\-)
			       (char=? (peek-next-char inport) #\-))
			  ;; comment
			  ;; white space must be skipped by now, so just discard a line
			  (get-line inport)(loop))
			 ((or (char=? ch #\,)
			      (char=? ch #\{) (char=? ch #\}))
			  (when (char=? ch #\})
			    (append! stacked (list 'POSTBRACE)))
			  (let ((slot (assv ch reserved-char)))
			    (make-lexical-token (cdr slot) location (list->string (list ch)))))
			 ;; we treat assign special way.
			 ((and (char=? ch #\:)
			       (char=? (peek-next-char inport) #\:)
			       (char=? (peek-next-char inport) #\=))
			  (get-char inport) ;; discard #\=
			  (make-lexical-token 'ASSIGN location "::="))
			 ((and (char-set-contains? word-charset ch)
			       (next-token word-charset))
			  => (lambda (token)
			       ;; reserved word or variable
			       (cond ((assoc token reserved)
				      => (lambda (slot)
					   (make-lexical-token (cdr slot) location token)))
				     ((and (or (string=? token "OCTET")
					       (string=? token "BIT"))
					   (and-let* ((next (next-token word-charset))
						      ( (set! prev next) )
						      ( (string=? next "STRING") ))
					     (make-lexical-token 'WORD location
								 (string-append token "-" next)))))
				     
				     ((and (string=? token "OBJECT")
					   (and-let* ((next (next-token word-charset))
						      ( (set! prev next) )
						      ( (string=? next "IDENTIFIER") ))
					     (make-lexical-token 'WORD location
								 (string-append token "-" next)))))
				     ((string=? token "RELATIVE-OID")
				      (make-lexical-token 'WORD location token))
				     (else
				      (make-lexical-token 'WORD location token)))))
			 ((char=? ch #\[)
			  ;; class
			  ;; discard #\[
			  (get-char inport)
			  (let* ((token (next-token class-charset))
				 (digit? (string->number token 10)))
			    (unless (or (assoc token class-tags)
					digit?)
			      (raise-asn.1-lexical-error 'lexer "invalid class tag token" token))
			    (if digit?
				(let ((next (next-char)))
				  ;; check #\]
				  (unless (char=? next #\])
				    (raise-asn.1-lexical-error 'lexer "no right bracket for class tag" token))
				  (make-lexical-token 'CLASS location (encode-tag CLASS_CONTEXT digit?)))
				(let ((tag  (assoc token class-tags))
				      (next (next-token char-set:digit)))
				  (unless tag
				    (raise-asn.1-lexical-error 'lexer "invalid class tag" token))
				  (when (string-null? next)
				    (raise-asn.1-lexical-error 'lexer "invalid class tag" token))
				  (let ((br (next-char)))
				    ;; check #\]
				    (unless (char=? br #\])
				      (raise-asn.1-lexical-error 'lexer "no right bracket for class tag" token))
				    (make-lexical-token 'CLASS location 
							(encode-tag (cdr tag) (string->number next 10))))))))
			 ((and-let* (( (char=? ch #\() )
				     (token (next-token char-set:digit))
				     ( (not (string-null? token)) )
				     ( (char=? (peek-char) #\)) ))
			    (make-lexical-token 'NUMBER token)))
			 (else
		       (raise-asn.1-lexical-error 'lexer "invalid token" ch)))))
		(else 
		 (let ((ret (car stacked)))
		   (set! stacked (cdr stacked))
		   (make-lexical-token ret #f #f))))))))
  )