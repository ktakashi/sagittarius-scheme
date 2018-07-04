#!/bin/sh
#| -*- mode:scheme; -*-
this_path=`dirname "$0"`
exec sagittarius -L${this_path}/../unicode $0 "$@"
|#
(add-load-path "unicode")
(import (rnrs)
	(sagittarius)
	(rfc http)
	(archive)
	(getopt)
	(util file)
	(only (binary io) open-chunked-binary-input/output-port)
	(build-ucd-1)
	(build-ucd-2)
	(build-charset)
	(build-lexeme-code)
	(build-ucd-code)
	(srfi :39))

(define-constant +host+ "www.unicode.org")
;; this is always the latest but we use explicit version.
;;(define-constant +latest+ "/Public/UNIDATA/UCD.zip")
(define-constant +latest+ "/Public/10.0.0/ucd/UCD.zip")
(define-constant +max-retry+ 5)
(define-constant +unicode-dir+ "unicode")
(define-constant +data-dir+ (build-path +unicode-dir+ "data"))
(define-constant +ucd-dir+ (build-path +unicode-dir+ "ucd"))

#|
List of interesting files

    unicode/data/CaseFolding.txt
    unicode/data/CompositionExclusions.txt
    unicode/data/DerivedCoreProperties.txt (used by extract-unicode-props.scm)
    unicode/data/GraphemeBreakProperty.txt (ditto)
    unicode/data/PropList.txt
    unicode/data/SpecialCasing.txt
    unicode/data/UnicodeData.txt
    unicode/data/WordBreakProperty.txt (not used)

|#
(define-constant +interesting-files+
  '("CaseFolding.txt"
    "CompositionExclusions.txt"
    "DerivedCoreProperties.txt"
    "GraphemeBreakProperty.txt"
    "PropList.txt"
    "SpecialCasing.txt"
    "UnicodeData.txt"
    "WordBreakProperty.txt"))

(define (path-filename p)
  (let-values (((dir base ext) (decompose-path p)))
    (if ext (string-append base "." ext) base)))

(define (download-ucd)
  (define out (open-chunked-binary-input/output-port))
  (define (flusher out headers) out)
  (define (destinator e) 
    (let ((name (path-filename (archive-entry-name e))))
      (if (member name +interesting-files+)
	  (begin (print "Extracting " name)
		 (build-path +data-dir+ name))
	  #f)))

  (define (check-files)
    (and (file-exists? +data-dir+)
	 (for-all (lambda (f) (file-exists? (build-path +data-dir+ f)))
		  +interesting-files+)))
  (unless (check-files)
    (create-directory* +data-dir+)
    (print "Downloading UCD.zip")
    (dynamic-wind values
	(lambda ()
	  (http-get +host+ +latest+
		    :receiver (http-oport-receiver out flusher)
		    :secure #t))
	(lambda () (set-port-position! out 0)))
    (call-with-input-archive-port 'zip out
      (lambda (in)
	(extract-all-entries in :destinator destinator :overwrite #t)))))

(define (compile-ucd)
  (parse-ucd-1)
  (parse-ucd-2)
  (parse-unicodedata)
  (build-ucd-code)
  (build-charset))

(define (process-unicode)
  (define lexeme.inc (build-path +unicode-dir+ "lexeme.inc"))
  (define charset.inc (build-path +unicode-dir+ "charset.inc"))
  (define (check-inc-files)
    (parameterize ((current-directory +unicode-dir+))
      (check-ucd-files)))

  (unless (and (file-exists? lexeme.inc) (file-exists? charset.inc)
	       (check-inc-files))
    (download-ucd)
    (create-directory* +ucd-dir+)
    (parameterize ((current-directory +unicode-dir+))
      (compile-ucd))
    ;; remove .datum file we don't need
    (delete-directory* +ucd-dir+)))

(define (clean-unicode-files dir)
  (print "Cleaning Unicode codepoints")
  (path-for-each dir (lambda (p type) 
		       (when (and (eq? type 'file) 
				  ;; keep build file
				  (not (string=? "scm" (path-extension p))))
			 (print "Removing file " p)
			 (delete-file p))))
  (when (file-exists? +data-dir+)
    (print "Removing " +data-dir+)
    (delete-directory* +data-dir+))
  (print "Done!"))

(define (main args)
  (with-args (cdr args)
      ((clean? (#\c "clean") #f #f))
    (if clean?
	(clean-unicode-files +unicode-dir+)
	(process-unicode))))
