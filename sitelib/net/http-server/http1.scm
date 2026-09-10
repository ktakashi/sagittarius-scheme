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
	  (srfi :1 lists)
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


;; state offsets are absolute and valid while the buffer only grows by append.
(define-record-type http1-consume-state
  (fields (mutable stage)
          (mutable scan)
          (mutable line-end)
          (mutable method)
          (mutable target)
          (mutable version)
          (mutable headers)
          (mutable framing)
          (mutable body-start)
          (mutable length)
          (mutable chunks)
          (mutable chunk-scan)
          (mutable chunk-size))
  (protocol (lambda (p)
	      (lambda ()
		(p 'start 0 #f #f #f #f #f 'none 0 #f '() 0 0)))))

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

(define (resume-scan len)
  ;; keep overlap for delimiters that can be split across reads.
  (if (> len 3) (- len 3) 0))

(define (parse-header-bytevector bv)
  (let ((bin (open-bytevector-input-port bv)))
    (let ((headers (rfc5322-read-headers bin #t strict-read-line)))
      (make-http-server:headers headers))))

(define (http1-ready st buffer body next)
  (values 'ready
          (make-http1-request* (http1-consume-state-method st)
                               (http1-consume-state-target st)
                               (http1-consume-state-version st)
                               (http1-consume-state-headers st)
                               body)
          #f
          (make-remainder buffer next)
          #f))

(define (consume:start st buffer max-header-bytes next)
  (let* ((blen (bytevector-length buffer))
         (line-end (find-bytes buffer +crlf+ (http1-consume-state-scan st))))
    (cond ((not line-end)
           (if (> blen max-header-bytes)
               (values 'error 431 "Headers too large" buffer #f)
               (begin
                 (http1-consume-state-scan-set! st (resume-scan blen))
                 (values 'start #f #f buffer st))))
          ((> line-end max-header-bytes)
           (values 'error 431 "Headers too large" buffer #f))
          (else
           (let-values (((method target version line-error)
                         (parse-request-line
                          (utf8->string (bv-sub buffer 0 line-end)))))
             (cond (line-error
                    (values 'error 400 line-error buffer #f))
                   ((not (or (string=? version "HTTP/1.1")
                             (string=? version "HTTP/1.0")))
                    (values 'error 505 "Unsupported HTTP version" buffer #f))
                   (else
                    (http1-consume-state-method-set! st method)
                    (http1-consume-state-target-set! st target)
                    (http1-consume-state-version-set! st version)
                    (http1-consume-state-line-end-set! st line-end)
                    (http1-consume-state-scan-set! st line-end)
                    (http1-consume-state-stage-set! st 'line)
                    (next))))))))

(define (consume:line st buffer max-header-bytes max-body-bytes next)
  (define head-ref http-server:headers-ref)
  (define (chunked? headers)
    (http-server:headers-contains-token? headers
                                         "transfer-encoding"
                                         "chunked"))
  (let* ((blen (bytevector-length buffer))
         (head-end (find-bytes buffer +crlf-crlf+
                               (http1-consume-state-scan st))))
    (cond ((not head-end)
           (if (> blen max-header-bytes)
               (values 'error 431 "Headers too large" buffer #f)
               (begin
                 (http1-consume-state-scan-set! st (resume-scan blen))
                 (values 'line #f #f buffer st))))
          ((> head-end max-header-bytes)
           (values 'error 431 "Headers too large" buffer #f))
          (else
           (guard (e (else
                      (values 'error 400 (condition-message e) buffer #f)))
             (let* ((line-end (http1-consume-state-line-end st))
                    (headers (parse-header-bytevector
                              (bv-sub buffer (+ line-end 2) head-end)))
                    (te (head-ref headers "transfer-encoding" #f))
                    (cl (head-ref headers "content-length" #f))
                    (body-start (+ head-end 4)))
               (cond ((and te cl)
                      (values 'error 400
                              "Both transfer-encoding and content-length are present"
                              buffer
                              #f))
                     ((and te (chunked? headers))
                      (http1-consume-state-headers-set! st headers)
                      (http1-consume-state-body-start-set! st body-start)
                      (http1-consume-state-framing-set! st 'chunked)
                      (http1-consume-state-chunks-set! st '())
                      (http1-consume-state-chunk-size-set! st 0)
                      (http1-consume-state-chunk-scan-set! st body-start)
                      (http1-consume-state-stage-set! st 'header)
                      (next))
                     (cl
                      (let ((len (parse-content-length cl)))
                        (cond ((not len)
                               (values 'error 400
                                       "Malformed content-length"
                                       buffer
                                       #f))
                              ((> len max-body-bytes)
                               (values 'error 413
                                       "Request body too large"
                                       buffer
                                       #f))
                              (else
                               (http1-consume-state-headers-set! st headers)
                               (http1-consume-state-body-start-set! st body-start)
                               (http1-consume-state-framing-set! st 'length)
                               (http1-consume-state-length-set! st len)
                               (http1-consume-state-stage-set! st 'header)
                               (next)))))
                     (else
                      (http1-consume-state-headers-set! st headers)
                      (http1-consume-state-body-start-set! st body-start)
                      (http1-consume-state-framing-set! st 'none)
                      (http1-consume-state-stage-set! st 'header)
                      (next)))))))))

(define (consume:header st buffer max-body-bytes)
  (define (consume-chunk st buffer max-body-bytes)
    (define u8 bytevector-u8-ref)
    (define blen (bytevector-length buffer))
    (define i (http1-consume-state-chunk-scan st))
    (define line-end (find-bytes buffer +crlf+ i))

    (if (not line-end)
        (values 'header #f #f buffer st)
        (let ((size (string->number 
		     (utf8->string (bv-sub buffer i line-end)) 16)))
          (if (or (not size) (< size 0))
              (values 'error 400 "Malformed chunk size" buffer #f)
              (let* ((chunk-start (+ line-end 2))
                     (chunk-end (+ chunk-start size)))
                (cond ((> (+ chunk-end 2) blen)
                       (values 'header #f #f buffer st))
                      ((= size 0)
                       (cond ((and (= (u8 buffer chunk-end) #x0d)
                                   (= (u8 buffer (+ chunk-end 1)) #x0a))
                              (http1-ready st buffer
                                           (bv-concat
                                            (reverse!
					     (http1-consume-state-chunks st)))
                                           (+ chunk-end 2)))
                             ((find-bytes buffer +crlf-crlf+ chunk-end) =>
                              (lambda (end)
                                (http1-ready st buffer
                                             (bv-concat
                                              (reverse!
                                               (http1-consume-state-chunks st)))
                                             (+ end 4))))
                             (else
                              (values 'header #f #f buffer st))))
                      ((or (not (= (u8 buffer chunk-end) #x0d))
                           (not (= (u8 buffer (+ chunk-end 1)) #x0a)))
                       (values 'error 400 "Malformed chunk payload" buffer #f))
                      (else
                       (let ((n (+ (http1-consume-state-chunk-size st) size)))
                         (if (> n max-body-bytes)
                             (values 'error 413 "Request body too large"
				     buffer #f)
                             (begin
                               (http1-consume-state-chunks-set!
                                st
                                (cons (bv-sub buffer chunk-start chunk-end)
                                      (http1-consume-state-chunks st)))
                               (http1-consume-state-chunk-size-set! st n)
                               (http1-consume-state-chunk-scan-set! st
                                (+ chunk-end 2))
                               (consume-chunk st buffer max-body-bytes)))))))))))
  (case (http1-consume-state-framing st)
    ((none)
     (http1-ready st buffer #vu8() (http1-consume-state-body-start st)))
    ((length)
     (let* ((body-start (http1-consume-state-body-start st))
            (len (http1-consume-state-length st))
            (blen (bytevector-length buffer))
            (next (+ body-start len)))
       (if (> next blen)
           (values 'header #f #f buffer st)
           (http1-ready st buffer (bv-sub buffer body-start next) next))))
    ((chunked) (consume-chunk st buffer max-body-bytes))
    (else (values 'error 500 "Unknown body framing state" buffer #f))))

(define (http-server:http1-consume state buffer
                                   :key (max-header-bytes 65536)
                                        (max-body-bytes 1048576))
  (define st (if (and state (http1-consume-state? state))
                 state
                 (make-http1-consume-state)))
  (let loop ()
    (case (http1-consume-state-stage st)
      ((start) (consume:start st buffer max-header-bytes loop))
      ((line) (consume:line st buffer max-header-bytes max-body-bytes loop))
      ((header) (consume:header st buffer max-body-bytes))
      (else
       (values 'error 500 "Unknown consume stage" buffer #f)))))
				 
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
