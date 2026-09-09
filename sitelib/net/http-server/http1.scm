;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http1.scm - HTTP/1.1 parser and writer
;;;

#!nounbound
#!read-macro=sagittarius/regex
(library (net http-server http1)
  (export http-server:http1-request?
	  *http-server:http1-driver*)
  (import (rnrs)
          (net socket)
          (net http-server types)
	  (net http-server request)
          (net http-server response)
	  (net http-server protocol)
	  (rfc :5322)
	  (sagittarius regex)
	  (srfi :2 and-let*)
	  (srfi :13 strings)
	  (util bytevector))

(define-record-type http-server:http1-request
  (parent http-server:request)
  (protocol (lambda (n)
	      (lambda (method target path query version headers body)
		((n method target path query version headers body #f '()))))))

(define +crlf+ #vu8(#x0d #x0a))
(define +crlf-crlf+ #vu8(#x0d #x0a #x0d #x0a))

(define bv-sub bytevector-copy)
(define bv-concat bytevector-concatenate)
(define find-bytes bytevector-contains)
(define trim string-trim-both)

(define (split-once s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) (values s #f))
            ((char=? (string-ref s i) ch)
             (values (substring s 0 i) (substring s (+ i 1) len)))
            (else
             (loop (+ i 1)))))))

(define (parse-request-line line)
  (cond ((#/^(.+)\s+(.+)\s+(.+)$/ line) =>
	 (lambda (m)
	   (values (string->symbol (m 1))
                   (m 2)
                   (m 3)
                   #f)))
	(else (values #f #f #f "Malformed request line"))))


(define (parse-target target)
  (let-values (((path query) (split-once target #\?)))
    (values (if (string=? path "") "/" path) query)))

(define (parse-content-length raw)
  (guard (e (else #f))
    (let ((n (string->number (trim raw))))
      (and (integer? n) (>= n 0) n))))

(define (consume-chunked buffer body-start)
  (define blen (bytevector-length buffer))
  (define u8 bytevector-u8-ref)
  (let loop ((i body-start) (chunks '()))
    (let ()
      (or (and-let* ((m (regex-matcher #/([0-9a-fA-F]+)\r\n/ buffer i))
		     ( (regex-looking-at m) )
		     (size (string->number (utf8->string (m 1)) 16)))
	    (if (< size 0)
		(values 'error 400 "Malformed chunk size")
		(let* ((chunk-start (regex-group-end m 0))
		       (chunk-end (+ chunk-start size)))
		  (cond ((> (+ chunk-end 2) blen)
			 (values 'need-more #f #f))
			((= size 0)
			 (cond ((> (+ chunk-end 2) blen)
				;; missing \r\n
				(values 'need-more #f #f))
			       ((and (= (u8 buffer chunk-end) #x0d)
				     (= (u8 buffer (+ chunk-end 1)) #x0a))
				(values 'ok
                                        (bv-concat (reverse chunks))
                                        (+ chunk-end 2)))
			       ((find-bytes buffer +crlf-crlf+ chunk-end) =>
				(lambda (end)
				  (values 'ok
                                          (bv-concat (reverse chunks))
                                          (+ end 4))))
			       (else (values 'need-more #f #f #f))))
			((or (not (= (u8 buffer chunk-end) #x0d))
                             (not (= (u8 buffer (+ chunk-end 1)) #x0a)))
                         (values 'error 400 "Malformed chunk payload"))
			(else
                         (loop (+ chunk-end 2)
                               (cons (bv-sub buffer chunk-start chunk-end)
				     chunks)))))))
	  (values 'need-more #f #f)))))


(define (make-remainder buffer next)
  (bv-sub buffer next (bytevector-length buffer)))

(define (make-http1-request* method target version headers body)
  (let-values (((path query) (parse-target target)))
    (make-http-server:http1-request method target path query
				    version headers body)))

;; the line must end with \r\n
(define (strict-read-line bin)
  (let-values (((out e) (open-bytevector-output-port)))
    (let loop ((cr? #f))
      (let ((u8 (get-u8 bin)))
	(cond ((eof-object? u8) (utf8->string (e)))
	      ;; \r\n
	      ((and (= u8 #x0a) cr?) (utf8->string (e)))
	      ((= u8 #x0d)
	       (when cr? (put-u8 out #x0d)) ;; emit previous \r
	       (loop #t))
	      (else (put-u8 out u8) (loop #f)))))))
	       

(define (http-server:http1-consume buffer 
				   :key (max-header-bytes 65536)
				        (max-body-bytes 1048576))
  (define (parse-header bin)
    (let ((headers (rfc5322-read-headers bin #t strict-read-line)))
      ;; assume RFC 5322 headers is alist, a bit bad but for now okay
      (make-http-server:headers headers)))
  (define head-ref http-server:headers-ref)
  (define (chunked? headers)
    (http-server:headers-contains-token? headers
					 "transfer-encoding"
                                         "chunked"))
  (define (read-chunked method target version headers buffer body-start)
    (let-values (((kind body next) (consume-chunked buffer body-start)))
      (cond ((eq? kind 'need-more)
             (values 'need-more #f #f buffer))
            ((eq? kind 'error)
             (values 'error body next buffer))
            ((> (bytevector-length body) max-body-bytes)
             (values 'error 413 "Request body too large" buffer))
            (else
             (values 'ok
                     (make-http1-request* method target version headers body)
		     #f
                     (make-remainder buffer next))))))

  (define (read-content method target version headers buffer cl body-start)
    (define (->content-length raw)
      (let ((n (string->number raw)))
	(and (integer? n) (>= n 0) n)))
    (define blen (bytevector-length buffer))
    (define len (->content-length cl))

    (cond ((not len)
	   (values 'error 400 "Malformed content-length" buffer))
	  ((> len max-body-bytes)
	   (values 'error 413 "Request body too large" buffer))
	  ((> (+ body-start len) blen)
	   (values 'need-more #f buffer))
	  (else
	   (let ((body (bv-sub buffer body-start (+ body-start len))))
             (values 'ok
                     (make-http1-request*
                      method target version headers body)
                     (make-remainder buffer (+ body-start len)))))))

  (let ((head-end (find-bytes buffer +crlf-crlf+ 0)))
    (cond ((not head-end)
           (if (> (bytevector-length buffer) max-header-bytes)
               (values 'error 431 "Headers too large" buffer)
               (values 'need-more #f #f buffer)))
          ((> head-end max-header-bytes)
           (values 'error 431 "Headers too large" buffer))
          (else
	   (let ((bin (open-bytevector-input-port buffer #f 0 head-end)))
	     (let-values (((method target version line-error)
			   (parse-request-line (strict-read-line bin))))
	       (cond (line-error (values 'error 400 line-error buffer))
		     ((not (or (string=? version "HTTP/1.1")
                               (string=? version "HTTP/1.0")))
                      (values 'error 505 "Unsupported HTTP version" buffer))
		     (else
		      (guard (e (else
				 (values 'error 400
					 (condition-message e) buffer)))
			(let* ((headers (parse-header bin))
			       (te (head-ref headers "transfer-encoding" #f))
                               (cl (head-ref headers "content-length" #f))
			       (body-start (+ head-end 4)))
			  (cond ((and te cl)
                                 (values 'error 400
                                         "Both transfer-encoding and content-length are present"
                                         buffer))
                                ((and te (chunked? headers))
				 (read-chunked method target version headers
					       buffer body-start))
				(cl (read-content method target version headers
						  buffer cl body-start))
				(else
				 (values 'ok
                                         (make-http1-request*
					  method target version headers #vu8())
					 #f
                                         (make-remainder buffer body-start))))))))))))))
				 
(define (body->bytevector body)
  (cond ((bytevector? body) body)
        ((string? body) (string->utf8 body))
        (else #f)))

(define utf8-transcoder (make-transcoder (utf-8-codec)))
(define (write-head! socket code reason headers)
  (let-values (((out extract) (open-bytevector-output-port utf8-transcoder)))
    (display "HTTP/1.1 " out)
    (display code out)
    (display " " out)
    (display reason out)
    (display "\r\n" out)
    (for-each (lambda (kv)
                (for-each (lambda (v)
                            (display (car kv) out)
                            (display ": " out)
                            (display v out)
                            (display "\r\n" out))
                          (cdr kv)))
              headers)
    (display "\r\n" out)
    (socket-send socket (extract))))

(define (request-close? req)
  (let ((hdrs (http-server:request-headers req)))
    (or (http-server:headers-contains-token? hdrs "connection" "close")
	(and (string=? (http-server:request-http-version req) "HTTP/1.0")
             (not (http-server:headers-contains-token? hdrs "connection" "keep-alive"))))))

(define (filter-response-headers headers)
  (let loop ((rest headers) (out '()))
    (cond ((null? rest) (reverse out))
          ((http-server:hop-by-hop-header? (caar rest))
           (if (or (string=? (caar rest) "connection")
                   (string=? (caar rest) "transfer-encoding"))
               (loop (cdr rest) (cons (car rest) out))
               (loop (cdr rest) out)))
          (else
           (loop (cdr rest) (cons (car rest) out))))))

(define (http-server:http1-write-response! socket req res)
  (define (ensure-header res name value)
    (unless (http-server:response-header-ref res name #f)
      (http-server:response-header-set! res name value)))
  (let* ((code (http-server:response-status res))
         (reason (or (http-server:response-reason res)
                     (http-server:reason-phrase code)))
         (body (body->bytevector (http-server:response-body res))))
    (if (not body)
        (let ((err (make-http-server:response 500)))
          (http-server:response-text! err "Unsupported response body type")
          (http-server:http1-write-response! socket req err))
        (let* ((close? (or (not req)
			   (request-close? req)
                           (http-server:headers-contains-token?
                            (http-server:response-headers res)
                            "connection"
                            "close")))
	       (value (if close? "close" "keep-alive")))
	  (ensure-header res "date" (http-server:current-http-date))
	  (ensure-header res "content-length" 
			 (number->string (bytevector-length body)))
	  (http-server:response-header-set! res "connection" value)
          (write-head!
           socket
           code
           reason
           (filter-response-headers
            (http-server:headers->alist (http-server:response-headers res))))
          (unless (or (eqv? code 204)
                      (eqv? code 304)
                      (and (<= 100 code) (< code 200))
		      (and req (eq? (http-server:request-method req) 'HEAD)))
            (socket-send socket body))
          close?))))

(define *http-server:http1-driver*
  (make-http-server:protocol-driver "http/1.1"
				    http-server:http1-consume
				    http-server:http1-write-response!))

)
