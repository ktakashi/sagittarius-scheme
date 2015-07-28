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
(define-constant +latest+ "/Public/zipped/latest/UCD.zip")
(define-constant +unicode-dir+ "unicode")
(define-constant +data-dir+ (build-path +unicode-dir+ "data"))
(define-constant +ucd-dir+ (build-path +unicode-dir+ "ucd"))

(define (download-ucd)
  (define out (open-chunked-binary-input/output-port))
  (define (flusher sink hdrs) (set-port-position! out 0))
  (define (destinator e) 
    (let ((name (archive-entry-name e)))
      (print "Extracting " name)
      (build-path +data-dir+ name)))
  (unless (file-exists? +data-dir+)
    (create-directory* +data-dir+)
    (print "Downloading UCD.zip")
    (let-values (((s h b) 
		  (http-get +host+ +latest+
			    :receiver (http-oport-receiver out flusher))))
      (call-with-input-archive-port 'zip out
	(lambda (in)
	  (extract-all-entries in :destinator destinator))))))

(define (compile-ucd)
  (print (current-directory))
  (parse-ucd-1)
  (parse-ucd-2)
  (parse-unicodedata)
  (build-ucd-code)
  (build-charset))

(define (process-unicode)
  (define lexeme.inc (build-path +unicode-dir+ "lexeme.inc"))
  (unless (file-exists? lexeme.inc)
    (download-ucd)
    (unless (file-exists? +ucd-dir+)
      (create-directory* +ucd-dir+)
      (parameterize ((current-directory +unicode-dir+))
	(compile-ucd)))))

(define (clean-unicode-files dir)
  (path-for-each dir (lambda (p type) 
		       (when (and (eq? type 'file) 
				  ;; keep build file
				  (not (string=? "scm" (path-extension p))))
			 (delete-file p))))
  (delete-directory* +data-dir+)
  (delete-directory* +ucd-dir+))

(define (main args)
  (with-args (cdr args)
      ((clean? (#\c "clean") #f #f))
    (if clean?
	(clean-unicode-files +unicode-dir+)
	(process-unicode))))
