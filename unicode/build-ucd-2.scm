#|

    produce followings from unicode data 5.2.0:

    "canonical-class.datum"
    "decompose.datum"
    "compose.datum"
    "compatibility.datum"

|#

#< (sagittarius regex) >
(library (anonymous)

  (export)

  (import (rnrs)
	  (sagittarius)
	  (sagittarius regex)
	  (sagittarius object)
	  (sagittarius control)
	  (util hashtables)
	  (match))

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

  ; (code-point) name (general-category) canonical-combining bidi decomposition numeric bidi-mirrored 
  ; unicode-1-name comment (simple-uppercase) (simple-lowercase) (simple-titlecase)

  (define ht-composit-exclusion (make-eqv-hashtable))
  (define ht-canonical-class (make-eqv-hashtable))
  (define ht-decompose (make-eqv-hashtable))
  (define ht-compose (make-eqv-hashtable))
  (define ht-compatibility (make-eqv-hashtable))

  (define (parse-composition-exclusions)
    (let ((re1 #/^([A-F0-9]{4,6}) *# .*$/)
          (re2 #/^([A-F0-9]{4,6})\\.\\.([A-F0-9]{4,6}) *# .*$/))
      (call-with-input-file (ucd-file "CompositionExclusions.txt")
        (lambda (input)
         (format #t "~%parsing CompositionExclusions.txt ...~!");
         (for-each-ucd-line
          (lambda (line)
            (if (not (zero? (string-length line)))
                (let ((m1 (re1 line)) (m2 (re2 line)))
                  (cond (m1 (let ((code-point (string->number (m1 1) 16)))
                              (hashtable-set! ht-composit-exclusion code-point #t)))
                        (m2 (let ((code-point-from (string->number (m2 1) 16))
                                  (code-point-to (string->number (m2 2) 16)))
                              (let loop ((i code-point-from))
                                (cond ((<= i code-point-to)
                                       (hashtable-set! ht-composit-exclusion i #t)
                                       (loop (+ i 1)))))))
                        (else
                         (assertion-violation 'parse-unicode-data "failed to parse compsition exclusions data"))))))
          input)
         (format #t " done~%~!")))))

  (define parse-hex-re1 #/^<[a-zA-Z]*> (.*)$/)
  (define parse-hex-re2 #/^([ A-F0-9]*)$/)
  (define parse-hex-inner-re1 #/^([A-F0-9]+)$/)
  (define parse-hex-inner-re2 #/^([A-F0-9]+) (.*)$/)

  (define (parse-hex s)
    (let ((m1 (parse-hex-re1 s))
          (m2 (parse-hex-re2 s)))
      (let ((hexnums (cond (m1 (m1 1))
                           (m2 (m2 1))
                           (else (assertion-violation 'parse-unicode-data "bad decomp in unicode data" s)))))
        (let loop ((s hexnums) (lst '()))
          (let ((m1 (parse-hex-inner-re1 s))
                (m2 (parse-hex-inner-re2 s)))
            (cond (m1
                   (reverse (cons (string->number (m1 1) 16) lst)))
                  (m2
                   (loop (m2 2)
                         (cons (string->number (m2 1) 16) lst)))
                  (else
                   (assertion-violation 'parse-unicode-data "bad decomp in unicode data" s))))))))
  
  (define (parse-unicodedata)
    (let ((re #/^([A-F0-9]{4,6});[^;]*;[a-zA-Z]{2};([0-9]{0,3});[^;]*;([^;]*);.*$/
              ))
      (call-with-input-file (ucd-file "UnicodeData.txt")
        (lambda (input)
          (format #t "~%parsing UnicodeData.txt ...~!");
          (for-each-ucd-line
           (lambda (line)
            (let ((m (re line)))
              (cond (m (let ((code-point (string->number (m 1) 16))
                             (canonical-class (string->number (m 2) 10))
                             (decomp (m 3)))
                         (or (<= 0 canonical-class 255)
                             (assertion-violation 'parse-unicode-data "bad canonical class in unicode data" canonical-class))
                         (and (not (= 0 canonical-class ))
                              (hashtable-set! ht-canonical-class code-point canonical-class))
                         (and (> (string-length decomp) 0)
                              (let ((compat (char=? (string-ref decomp 0) #\<)))
                                (and compat (hashtable-set! ht-compatibility code-point #t))
                                (let ((decomp-lst (parse-hex decomp)))
                                  (or compat
                                      (<= 1 (length decomp-lst) 2)
                                      (assertion-violation 'parse-unicode-data "bad canonical decomp in unicode data" decomp))
                                  (hashtable-set! ht-decompose code-point decomp-lst)
                                  (and (not compat)
                                       (not (hashtable-ref ht-composit-exclusion code-point #f))
                                       (hashtable-set! ht-compose
                                                       (if (= (length decomp-lst) 2)
                                                           (+ (* (car decomp-lst) #x10000) (cadr decomp-lst))
                                                           (car decomp-lst))
                                                       code-point)))))))
                    (else
                     (assertion-violation 'parse-unicode-data "failed to parse unicode data")))))
           input)
        (format #t " done~%~!")))))

  (parse-composition-exclusions)
  (parse-unicodedata)

  (call-with-port
   (open-file-output-port (datum-file "canonical-class.datum")
                          (file-options no-fail) (buffer-mode block)
                          (native-transcoder))
   (lambda (output)
        (format #t "processing canonical-class.datum ...~!")
        (put-datum output (hashtable->alist ht-canonical-class))
        (format #t " done~%~!")))

  (call-with-port
   (open-file-output-port (datum-file "decompose.datum")
                          (file-options no-fail) (buffer-mode block)
                          (native-transcoder))
      (lambda (output)
        (format #t "processing decompose.datum ...~!")
        (put-datum output (hashtable->alist ht-decompose))
        (format #t " done~%~!")))

  (call-with-port
   (open-file-output-port (datum-file "compose.datum")
                          (file-options no-fail) (buffer-mode block)
                          (native-transcoder))
   (lambda (output)
     (format #t "processing compose.datum ...~!")
     (put-datum output (hashtable->alist ht-compose))
     (format #t " done~%~!")))

  (call-with-port
   (open-file-output-port (datum-file "compatibility.datum") (file-options no-fail) (buffer-mode block) (native-transcoder))
   (lambda (output)
     (format #t "processing compatibility.datum ...~!")
     (put-datum output (hashtable->alist ht-compatibility))
     (format #t " done~%~!")))
  
  ) ;[end]
