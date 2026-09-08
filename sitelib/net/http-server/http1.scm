;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http1.scm - HTTP/1.1 parser and writer
;;;

#!nounbound
#!read-macro=sagittarius/regex
(library (net http-server http1)
  (export http-server:http1-request?
	  make-http-server:http1-request
          http-server:http1-request-method
          http-server:http1-request-target
          http-server:http1-request-path
          http-server:http1-request-query
          http-server:http1-request-version
          http-server:http1-request-headers
          http-server:http1-request-body

          http-server:http1-consume
          http-server:http1-write-response!)
  (import (rnrs)
          (net socket)
          (net http-server types)
          (net http-server response)
	  (sagittarius regex)
	  (srfi :2 and-let*)
	  (srfi :13 strings)
	  (util bytevector))

(define-record-type http-server:http1-request
  (fields method target path query version headers body))

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

(define (split-crlf s)
  (define len (string-length s))
  (let loop ((i 0) (start 0) (out '()))
    (cond ((>= i (- len 1))
           (reverse (cons (substring s start len) out)))
          ((and (char=? (string-ref s i) #\return)
                (char=? (string-ref s (+ i 1)) #\newline))
           (loop (+ i 2) (+ i 2) (cons (substring s start i) out)))
          (else
           (loop (+ i 1) start out)))))

(define (split-spaces s)
  (let ((len (string-length s)))
    (let loop ((i 0) (token "") (out '()))
      (if (= i len)
          (reverse (if (string=? token "") out (cons token out)))
          (let ((c (string-ref s i)))
            (if (char=? c #\space)
                (if (string=? token "")
                    (loop (+ i 1) "" out)
                    (loop (+ i 1) "" (cons token out)))
                (loop (+ i 1)
                      (string-append token (string c))
                      out)))))))

(define (parse-request-line line)
  (let ((parts (split-spaces line)))
    (if (= (length parts) 3)
        (values (string->symbol (car parts))
                (cadr parts)
                (caddr parts)
                #f)
        (values #f #f #f "Malformed request line"))))

(define (parse-headers lines)
  (let ((headers (make-http-server:headers)))
    (let loop ((rest lines))
      (cond ((null? rest) (values headers #f))
            (else
             (let-values (((name value) (split-once (car rest) #\:)))
               (if (not value)
                   (values #f "Malformed header line")
                   (begin
                     (http-server:headers-add! headers name (trim value))
                     (loop (cdr rest))))))))))

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
		(values 'error 400 "Malformed chunk size" #f)
		(let* ((chunk-start (regex-group-end m 0))
		       (chunk-end (+ chunk-start size)))
		  (cond ((> (+ chunk-end 2) blen)
			 (values 'need-more #f #f #f))
			((= size 0)
			 (cond ((> (+ chunk-end 2) blen)
				;; missing \r\n
				(values 'need-more #f #f #f))
			       ((and (= (u8 buffer chunk-end) #x0d)
				     (= (u8 buffer (+ chunk-end 1)) #x0a))
				(values 'ok
                                        (bv-concat (reverse chunks))
                                        (+ chunk-end 2)
                                        #f))
			       ((find-bytes buffer +crlf-crlf+ chunk-end) =>
				(lambda (end)
				  (values 'ok
                                          (bv-concat (reverse chunks))
                                          (+ end 4)
                                          #f)))
			       (else (values 'need-more #f #f #f))))
			((or (not (= (u8 buffer chunk-end) #x0d))
                             (not (= (u8 buffer (+ chunk-end 1)) #x0a)))
                         (values 'error 400 "Malformed chunk payload" #f))
			(else
                         (loop (+ chunk-end 2)
                               (cons (bv-sub buffer chunk-start chunk-end)
				     chunks)))))))
	  (values 'need-more #f #f #f)))))


(define (make-remainder buffer next)
  (bv-sub buffer next (bytevector-length buffer)))

(define (make-http1-request* method target version headers body)
  (let-values (((path query) (parse-target target)))
    (make-http-server:http1-request method target path query version headers body)))

(define (http-server:http1-consume buffer 
				   :key (max-header-bytes 65536)
				        (max-body-bytes 1048576))
  (let ((head-end (find-bytes buffer +crlf-crlf+ 0)))
    (cond ((not head-end)
           (if (> (bytevector-length buffer) max-header-bytes)
               (values 'error 431 "Headers too large" #f buffer)
               (values 'need-more #f #f buffer)))
          ((> head-end max-header-bytes)
           (values 'error 431 "Headers too large" #f buffer))
          (else
           (let* ((head-bv (bv-sub buffer 0 head-end))
                  (lines (split-crlf (utf8->string head-bv))))
             (if (null? lines)
                 (values 'error 400 "Missing request line" #f buffer)
                 (let-values (((method target version line-error)
                               (parse-request-line (car lines))))
                   (if line-error
                       (values 'error 400 line-error #f buffer)
                       (if (not (or (string=? version "HTTP/1.1")
                                    (string=? version "HTTP/1.0")))
                           (values 'error 505 "Unsupported HTTP version" #f buffer)
                           (let-values (((headers header-error)
                                         (parse-headers (cdr lines))))
                             (if header-error
                                 (values 'error 400 header-error #f buffer)
                                 (let ((te (http-server:headers-ref headers "transfer-encoding" #f))
                                       (cl (http-server:headers-ref headers "content-length" #f))
                                       (body-start (+ head-end 4)))
                                   (cond ((and te cl)
                                          (values 'error 400
                                                  "Both transfer-encoding and content-length are present"
                                                  #f
                                                  buffer))
                                         ((and te
                                               (http-server:headers-contains-token? headers
                                                                                    "transfer-encoding"
                                                                                    "chunked"))
                                          (let-values (((kind body next _)
                                                        (consume-chunked buffer body-start)))
                                            (cond ((eq? kind 'need-more)
                                                   (values 'need-more #f #f buffer))
                                                  ((eq? kind 'error)
                                                   (values 'error body next #f buffer))
                                                  ((> (bytevector-length body) max-body-bytes)
                                                   (values 'error 413 "Request body too large" #f buffer))
                                                  (else
                                                   (values 'ok
                                                           (make-http1-request* method target version headers body)
                                                           (make-remainder buffer next)
                                                           #f)))))
                                         (cl
                                          (let ((len (parse-content-length cl)))
                                            (if (not len)
                                                (values 'error 400 "Malformed content-length" #f buffer)
                                                (let ((blen (bytevector-length buffer)))
                                                  (cond ((> len max-body-bytes)
                                                         (values 'error 413 "Request body too large" #f buffer))
                                                        ((> (+ body-start len) blen)
                                                         (values 'need-more #f #f buffer))
                                                        (else
                                                         (let ((body (bv-sub buffer body-start (+ body-start len))))
                                                           (values 'ok
                                                                   (make-http1-request*
                                                                    method target version headers body)
                                                                   (make-remainder buffer (+ body-start len))
                                                                   #f))))))))
                                         (else
                                          (values 'ok
                                                  (make-http1-request* method target version headers #vu8())
                                                  (make-remainder buffer body-start)
                                                  #f)))))))))))))))

(define (body->bytevector body)
  (cond ((bytevector? body) body)
        ((string? body) (string->utf8 body))
        (else #f)))

(define (write-head! socket code reason headers)
  (let-values (((out extract) (open-string-output-port)))
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
    (socket-send socket (string->utf8 (extract)))))

(define (request-close? req)
  (or (http-server:headers-contains-token?
       (http-server:http1-request-headers req)
       "connection"
       "close")
      (and (string=? (http-server:http1-request-version req) "HTTP/1.0")
           (not (http-server:headers-contains-token?
                 (http-server:http1-request-headers req)
                 "connection"
                 "keep-alive")))))

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
  (let* ((code (http-server:response-status res))
         (reason (or (http-server:response-reason res)
                     (http-server:reason-phrase code)))
         (body (body->bytevector (http-server:response-body res))))
    (if (not body)
        (let ((err (make-http-server:response 500)))
          (http-server:response-text! err "Unsupported response body type")
          (http-server:http1-write-response! socket req err))
        (begin
          (unless (http-server:response-header-ref res "date" #f)
            (http-server:response-header-set! res "date" (http-server:current-http-date)))
          (unless (http-server:response-header-ref res "content-length" #f)
            (http-server:response-header-set! res "content-length"
                                              (number->string (bytevector-length body))))
          (let ((close? (or (request-close? req)
                            (http-server:headers-contains-token?
                             (http-server:response-headers res)
                             "connection"
                             "close"))))
            (if close?
                (http-server:response-header-set! res "connection" "close")
                (http-server:response-header-set! res "connection" "keep-alive"))
            (write-head!
             socket
             code
             reason
             (filter-response-headers
              (http-server:headers->alist (http-server:response-headers res))))
            (unless (or (eqv? code 204)
                        (eqv? code 304)
                        (and (<= 100 code) (< code 200))
                        (eq? (http-server:http1-request-method req) 'HEAD))
              (socket-send socket body))
            close?)))))
)
