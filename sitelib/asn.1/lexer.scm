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
      (","          . COMMA)
      ("{"          . LBRACE)
      ("}"          . RBRACE)
      ("::="        . ASSIGN)))

  (define class-tags 
    `(("APPLICATION" . ,CLASS_APPLICATION)
      ("UNIVERSAL"   . ,CLASS_UNIVERSAL)
      ("PRIVATE"     . ,CLASS_PRIVATE)
      ("CONTEXT"     . ,CLASS_CONTEXT)))

  (define word-charset (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-"))

  (define (make-lexer inport)
    (lambda ()
      (define stacked '())
      (letrec ((skip-space (lambda () (skip-while char-set:whitespace inport)))
	       (next-token (lambda () (next-token-of char-set:graphic inport))))
	(skip-space)
	(let loop ()
	  (if (null? stacked)
	      (let ((location (apply make-source-location (append! (port-info inport) '(-1 -1))))
		    (token (next-token)))
		(cond ((string-null? token) '*eoi*)
		      ((and (> (string-length token) 2) (string= token "--" 0 2))
		       ;; comment
		       (loop))
		      ((assoc token reserved string=?)
		       => (lambda (slot)
			    ;; a comma is not required after a '}' so to aid the
			    ;; parser we insert a fake token after any '}'
			    (if (string=? token "}")
				(append! stacked '(POSTRBRACE)))
			    (make-lexical-token (cdr slot) location token)))
		      ((and (or (string=? token "OCTET")
				(string=? token "BIT"))
			    (let ((next (next-token)))
			      (if (string=? next "STRING")
				  next
				  #f)))
		       => (lambda (after)
			    (make-lexical-token 'WORD location (string->symbol
								(format "~a-~a" token after)))))
		      ((and (string=? token "OBJECT")
			    (let ((next (next-token)))
			      (if (string=? next "IDENTIFIER")
				  next
				  #f)))
		       => (lambda (after)
			    (make-lexical-token 'WORD location (string->symbol
								(format "~a-~a" token after)))))
		      ((string=? token "RELATIVE-OID")
		       (make-lexical-token 'WORD location 'RELATIVE-OID))
		      ((string-every word-charset token)
		       (make-lexical-token 'WORD location (string->symbol token)))
		      ((and (char=? (string-ref token 0) #\[) ;; [APPLICATION|PRIVATE...]
			    (let ((next (if (= (string-length token) 1)
					    (next-token)
					    (substring token 1 (string-length token)))))
			      (cond ((assoc next class-tags string=)
				     => (lambda (slot)
					  (skip-space)
					  (let* ((n (next-token))
						 (with (char=? #\] (string-ref n (- (string-length n) 1))))
						 (brace (if with
							    (substring n (- (string-length n) 1) 1)
							    (next-token))))
					    (when with
					      (set! n (substring n 0 (- (string-length n) 1))))
					    (cond ((string->number n)
						   => (lambda (number)
							(make-lexical-token 'CLASS location (encode-tag (cdr slot) number))))
						  (else #f)))))
				    ((string->number next)
				     => (lambda (number)
					  (if (string=? (next-token) "]")
					      (make-lexical-token 'CLASS location (encode-tag CLASS_CONTEXT number))
					      #f)))
				    (else #f)))))
		      ((string->number token)
		       (make-lexical-token 'NUMBER location token))
		      (else
		       (raise-asn.1-lexical-error 'asn.1-lexer
						  (format "Parse error file ~a line ~a"
							  (source-location-input location)
							  (source-location-line location))
						  token))))
	      (let ((ret (car stacked)))
		(set! stacked (cdr stacked))
		(make-lexical-token ret #f #f)))))))
  )