;; -*- scheme -*-
#!compatible
(library (text sxml helper)
    (export parser-error
	    ssax:warn
	    make-errorer
	    begin0
	    inc dec |--|
	    char-return char-tab nl
	    cout cerr
	    make-char-quotator
	    ascii->char
	    ucscode->char
	    assert
	    string-rindex
	    substring?
	    )
    (import (rnrs)
	    (only (srfi :13 strings) string-contains string-index-right)
	    (only (sagittarius) format)
	    (only (sagittarius control) begin0))

  (define ascii->char integer->char)
  (define ucscode->char integer->char)

  (define-syntax inc
    (syntax-rules ()
      ((_ x) (+ x 1))))

  (define-syntax dec
    (syntax-rules ()
      ((_ x) (- x 1))))

  (define-syntax --
    (syntax-rules ()
      ((_ x) (dec x))))

  (define-syntax assert
    (syntax-rules (report:)
      ((assert "doit" (expr ...) (r-exp ...))
       (cond
	((and expr ...) => (lambda (x) x))
	(else
	 (assertion-violation 'assert
			      (format "assertion failure: ~a" (list '(and expr ...) r-exp ...))))))
      ((assert "collect" (expr ...))
       (assert "doit" (expr ...) ()))
      ((assert "collect" (expr ...) report: r-exp ...)
       (assert "doit" (expr ...) (r-exp ...)))
      ((assert "collect" (expr ...) expr1 stuff ...)
       (assert "collect" (expr ... expr1) stuff ...))
      ((assert stuff ...)
       (assert "collect" () stuff ...))))

  (define char-return #\return)
  (define nl "\n")
  (define char-tab #\tab)

  (define (cout . args)
    (for-each (lambda (x)
		(if (procedure? x) (x) (display x)))
	      args)
    (newline))

  (define (cerr . args)
    (for-each (lambda (x)
		(if (procedure? x)
		    (x (current-error-port))
		    (display x (current-error-port))))
	      args)
    (newline (current-error-port)))

  ;; from yuni/lib/ssax/raise
  (define (make-errorer who)
    (lambda (msg . more)
      (error who
             (call-with-string-output-port
               (lambda (sop)
                 (for-each (lambda (x) (display x sop))
                           (cons msg more)))))))

  (define-condition-type &port-position &condition
    make-port-position-condition port-position-condition?
    (pos condition-port-position))

  (define (make-f raise-it first who)
    (lambda (port msg . other-msg)
      (raise-it
       (condition
        first
        (make-who-condition who)
        (make-message-condition (call-with-string-output-port
                                 (lambda (sop)
                                   (for-each (lambda (x) (display x sop))
                                             (cons msg other-msg)))))
        (if (and (port? port) (port-has-port-position? port))
	    (make-port-position-condition (port-position port))
	    (condition))
        (make-irritants-condition (list port))))))

  (define parser-error
    (make-f raise (make-error) 'ssax:parser))
   
  (define ssax:warn
    ;; None of these condition types are &serious,
    ;; so a default exception handler should return (per the R6RS),
    ;; allowing the SSAX code which called this to continue.
    (make-f raise-continuable (make-warning) 'ssax))


  (define (make-errorer who)
    (lambda (msg . more)
      (error who
             (call-with-string-output-port
	      (lambda (sop)
		(for-each (lambda (x) (display x sop))
			  (cons msg more)))))))


  ;; make-char-quotator QUOT-RULES
  ;;
  ;; Given QUOT-RULES, an assoc list of (char . string) pairs, return
  ;; a quotation procedure. The returned quotation procedure takes a string
  ;; and returns either a string or a list of strings. The quotation procedure
  ;; check to see if its argument string contains any instance of a character
  ;; that needs to be encoded (quoted). If the argument string is "clean",
  ;; it is returned unchanged. Otherwise, the quotation procedure will
  ;; return a list of string fragments. The input straing will be broken
  ;; at the places where the special characters occur. The special character
  ;; will be replaced by the corresponding encoding strings.
  ;;
  ;; For example, to make a procedure that quotes special HTML characters,
  ;; do
  ;;	(make-char-quotator
  ;;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))
  (define (make-char-quotator char-encoding)
    (let ((bad-chars (map car char-encoding)))

      ;; Check to see if str contains one of the characters in charset,
      ;; from the position i onward. If so, return that character's index.
      ;; otherwise, return #f
      (define (index-cset str i charset)
	(let loop ((i i))
	  (and (< i (string-length str))
	       (if (memv (string-ref str i) charset) i
		   (loop (inc i))))))

      ;; The body of the function
      (lambda (str)
	(let ((bad-pos (index-cset str 0 bad-chars)))
	  (if (not bad-pos) str	; str had all good chars
	      (let loop ((from 0) (to bad-pos))
		(cond
		 ((>= from (string-length str)) '())
		 ((not to)
		  (cons (substring str from (string-length str)) '()))
		 (else
		  (let ((quoted-char
			 (cdr (assv (string-ref str to) char-encoding)))
			(new-to 
			 (index-cset str (inc to) bad-chars)))
		    (if (< from to)
			(cons
			 (substring str from to)
			 (cons quoted-char (loop (inc to) new-to)))
			(cons quoted-char (loop (inc to) new-to))))))))))))

  (define (substring? pat str) (string-contains str pat))
  (define string-rindex string-index-right)
  )