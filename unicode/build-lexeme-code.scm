#|

    produce followings from unicode data 6.1.0:

        lexeme.inc

|#
#< (sagittarius regex) >
(library (anonymous)

  (export)

  (import (rnrs)
	  (sagittarius)
	  (sagittarius regex)
	  (sagittarius control)
	  (util hashtables)
	  (srfi :13 strings))

  (define (ucd-file filename)
    (string-append (current-directory) "/unicode-6.1.0/" filename))

  (define (datum-file filename)
    (string-append (current-directory) "/ucd/" filename))

  (define (skip-whitespace! port)
    (and (not (port-eof? port))
	 ;;(nonblock-byte-ready? port)
	 (char-whitespace? (lookahead-char port))
	 (get-char port)
	 (skip-whitespace! port)))

  (define (read-ucd-line port)
    (skip-whitespace! port)
    (let ((ch (lookahead-char port)))
      (cond ((eof-object? ch)
	     (eof-object))
	    ((eq? ch #\#)
	     (get-line port)
	     (read-ucd-line port))
	    (else
	     (get-line port)))))

  (define (for-each-ucd-line proc input)
    (let loop ()
      (let ((line (read-ucd-line input)))
	(cond ((eof-object? line)
	       (undefined))
	      (else
	       (proc line)
	       (loop))))))

  (define (ascii-area-list s num?)
    (let loop ((cp 0) (acc '()))
      (cond ((> cp 127) (reverse acc))
	    ((or (char<=? #\a (integer->char cp) #\z)
		 (char<=? #\A (integer->char cp) #\Z)
		 (and num? (char<=? #\0 (integer->char cp) #\9))
		 (string-contains s (string (integer->char cp))))
	     (loop (+ cp 1) (cons (integer->char cp) acc)))
	    (else
	     (loop (+ cp 1) acc)))))

  (define (add-special-range-area ht)

    (define (put-range cn first last)
      (let loop ((cp first))
	(cond ((> cp last))
	      (else
	       (hashtable-set! ht cp cn)
	       (loop (+ cp 1))))))

    (put-range 'Lo #x3400 #x4DB5)
    (put-range 'Lo #x4E00 #x9FBB)
    (put-range 'Lo #xAC00 #xD7A3)
    (put-range 'Cs #xD800 #xDB7F)
    (put-range 'Cs #xDB80 #xDBFF)
    (put-range 'Cs #xDC00 #xDFFF)
    (put-range 'Co #xE000 #xF8FF)
    (put-range 'Lo #x20000 #x2A6D6)
    (put-range 'Co #xF0000 #xFFFFD)
    (put-range 'Co #x100000 #x10FFFD))
  
  (define (parse-unicodedata)
    (let ((re #/^([A-F0-9]{4,6});[^;]*;([a-zA-Z]{2});[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;(.*);[^;]*;[^;]*;[^;]*;([A-F0-9]{0,6});([A-F0-9]{0,6});([A-F0-9]{0,6})$/
	      ))

      (define ht-general-category (make-eqv-hashtable))

      (add-special-range-area ht-general-category)
      (call-with-input-file (ucd-file "UnicodeData.txt")
       (lambda (input)
	 (format #t "~%parsing UnicodeData.txt ...~!");
	 (for-each-ucd-line
	  (lambda (line)
	    (let ((m (re line)))
	      (let ((code-point       (string->number (m 1) 16))
		    (general-category (string->symbol (m 2))))
		(hashtable-set! ht-general-category code-point general-category))))
	  input)
	 (format #t " done~%~!")

	 (call-with-port
	  (open-file-output-port "lexeme.inc"
				 (file-options no-fail) (buffer-mode block)
				 (native-transcoder))
	  (lambda (output)

	    (define bv-c (make-bytevector (div #x110000 8)))
	    (define bv-s (make-bytevector (div #x110000 8)))

	    (define ascii-c (ascii-area-list "!?*/:<=>$%&^_~" #f))
	    (define ascii-s (ascii-area-list "!?*/:<=>$%&^_~.@+-" #t))
	    
	    (define (advance cp offset bit)
	      (let ((bit (+ bit bit)))
		(if (= bit #b100000000)
		    (list (+ cp 1) (+ offset 1) 1)
		    (list (+ cp 1) offset bit))))

	    (format #t "processing lexeme.inc...~!")
	    (put-string output "// -*- C -*- \n")
	    (let loop ((cp 0) (offset 0) (bit 1))
	      (cond ((< cp 128)
		     (and (memq (integer->char cp) ascii-c)
			  (bytevector-u8-set! bv-c offset (+ (bytevector-u8-ref bv-c offset) bit)))
		     (and (memq (integer->char cp) ascii-s)
			  (bytevector-u8-set! bv-s offset (+ (bytevector-u8-ref bv-s offset) bit)))
		     (apply loop (advance cp offset bit)))
		    ((<= #xd800 cp #xdfff) (apply loop (advance cp offset bit)))
		    ((> cp #x10ffff))
		    ((hashtable-ref ht-general-category cp 'Cn)
		     => (lambda (cc)
			  (cond ((memq cc '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))
				 (bytevector-u8-set! bv-c offset (+ (bytevector-u8-ref bv-c offset) bit))
				 (bytevector-u8-set! bv-s offset (+ (bytevector-u8-ref bv-s offset) bit)))
				((memq cc '(Nd Mc Me))
				 (bytevector-u8-set! bv-s offset (+ (bytevector-u8-ref bv-s offset) bit))))
			  (apply loop (advance cp offset bit))))))

	    (let ((bv bv-c))
	      (let ((bytes (bytevector-length bv)))
		(format output "static const uint8_t constituent[~a] = {" bytes)
		(let loop ((c 0))
		  (if (zero? (mod c 16))
		      (format output "~%"))
		  (let ((b (bytevector-u8-ref bv c)))
		    (cond ((= (+ c 1) bytes)
			   (format output "0x~x\n};\n" b))
			  (else
			   (format output "0x~x," b)
			   (loop (+ c 1))))))))
	    (let ((bv bv-s))
	      (let ((bytes (bytevector-length bv)))
		(format output "static const uint8_t subsequent[~a] = {" bytes)
		(let loop ((c 0))
		  (if (zero? (mod c 16))
		      (format output "~%"))
		  (let ((b (bytevector-u8-ref bv c)))
		    (cond ((= (+ c 1) bytes)
			   (format output "0x~x\n};\n" b))
			  (else
			   (format output "0x~x," b)
			   (loop (+ c 1))))))))
	    (format #t " done~%~!")))))))


  (parse-unicodedata)

  ) ;[end]

