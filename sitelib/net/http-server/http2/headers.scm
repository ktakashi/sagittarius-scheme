;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver state
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2 headers)
    (export parse-target
	    parse-http2-request-headers
	    response->hpack-headers
	    collect-push-headers
	    method->http-token)
    (import (rnrs)
	    (net http-server request)
	    (net http-server response)
	    (net http-server types))

(define (parse-target target)
  (let-values (((path query) (split-once target #\?)))
    (values (if (string=? path "") "/" path) query)))

(define (parse-http2-request-headers headers)
  (define header-map (make-http-server:headers))
  (define (fail message) (values #f #f #f #f #f message))
  (let loop ((rest headers)
	     (pseudo-open? #t)
	     (method #f)
	     (scheme #f)
	     (target #f)
	     (authority #f))
    (if (null? rest)
        (if (and method scheme target)
            (let-values (((path query) (parse-target target)))
              (when (and authority
                         (not (http-server:headers-ref header-map "host" #f)))
                (http-server:headers-add! header-map "host" authority))
              (values (string->symbol method)
                      target
                      path
                      query
                      header-map
                      #f))
            (fail "Missing required pseudo headers"))
        (let-values (((name value err pseudo?) (decode-header-entry (car rest))))
          (if err
              (fail err)
              (cond
               ((contains-uppercase-ascii? name)
                (fail "Uppercase header name is not allowed"))
               ((and pseudo? (not pseudo-open?))
                (fail "Pseudo header must appear before regular headers"))
               (pseudo?
                (cond
                 ((string=? name ":method")
                  (if method
                      (fail "Duplicate :method")
		      (loop (cdr rest)
			    pseudo-open? value scheme target authority)))
                 ((string=? name ":scheme")
                  (if scheme
                      (fail "Duplicate :scheme")
		      (loop (cdr rest)
			    pseudo-open? method value target authority)))
                 ((string=? name ":path")
                  (if target
                      (fail "Duplicate :path")
		      (loop (cdr rest)
			    pseudo-open? method scheme value authority)))
                 ((string=? name ":authority")
                  (if authority
                      (fail "Duplicate :authority")
		      (loop (cdr rest)
			    pseudo-open? method scheme target value)))
                 (else (fail "Unknown pseudo header"))))
               (else
		(cond
                 ((connection-specific-header? name)
                  (fail "Connection-specific header is not allowed in HTTP/2"))
                 ((and (string=? name "te")
                       (not (string-ci=? value "trailers")))
                  (fail "Only TE: trailers is allowed in HTTP/2"))
                 (else
                  (http-server:headers-add! header-map name value)
                  (loop (cdr rest) #f method scheme target authority))))))))))

(define (response->hpack-headers req res)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))

  (define (collect headers)
    (let loop ((rest headers) (out '()))
      (if (null? rest)
          (reverse out)
          (let ((name (http-server:normalize-header-name (caar rest)))
                (values (cdar rest)))
            (if (or (http-server:hop-by-hop-header? name)
                    (and (> (string-length name) 0)
                         (char=? (string-ref name 0) #\:)))
                (loop (cdr rest) out)
                (loop (cdr rest)
                      (append (map (lambda (v) (header-entry name v)) values)
                              out)))))))

  (let* ((status (http-server:response-status res))
         (reason (or (http-server:response-reason res)
                     (http-server:reason-phrase status)))
         (body (response-body->bytevector (http-server:response-body res))))
    (if (not body)
        (let ((err (make-http-server:response 500)))
          (http-server:response-text! err "Unsupported response body type")
          (response->hpack-headers req err))
        (let* ((skip-body?
		(or (status-has-no-body? status)
                    (and req (eq? (http-server:request-method req) 'HEAD))))
               (body-bytes (if skip-body? #vu8() body)))
          (unless (or skip-body?
                      (http-server:response-header-ref res "content-length" #f))
            (http-server:response-header-set! res
             "content-length" (number->string (bytevector-length body-bytes))))
          (unless (http-server:response-header-ref res "date" #f)
            (http-server:response-header-set! res "date"
                                              (http-server:current-http-date)))
          (values (cons (header-entry ":status" (number->string status))
                        (collect (http-server:headers->alist
                                  (http-server:response-headers res))))
                  body-bytes
                  skip-body?)))))

(define (collect-push-headers headers)
  (define (header-entry name value)
    (list (string->utf8 name) (string->utf8 value)))
  (let loop ((rest (http-server:headers->alist headers)) (out '()))
    (if (null? rest)
        (reverse out)
        (let ((name (http-server:normalize-header-name (caar rest)))
              (values (cdar rest)))
          (if (or (http-server:hop-by-hop-header? name)
                  (and (> (string-length name) 0)
                       (char=? (string-ref name 0) #\:)))
              (loop (cdr rest) out)
              (loop (cdr rest)
                    (append (map (lambda (v) (header-entry name v)) values)
                            out)))))))

(define (method->http-token method)
  (cond ((symbol? method) (string-upcase (symbol->string method)))
        ((string? method) (string-upcase method))
        (else
         (assertion-violation 'method->http-token
                              "Unsupported HTTP method"
                              method))))
;; helper
(define (split-once s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) (values s #f))
            ((char=? (string-ref s i) ch)
             (values (substring s 0 i) (substring s (+ i 1) len)))
            (else
             (loop (+ i 1)))))))

(define (connection-specific-header? name)
  (or (string=? name "connection")
      (string=? name "keep-alive")
      (string=? name "proxy-connection")
      (string=? name "transfer-encoding")
      (string=? name "upgrade")))

(define (entry->name&value e)
  (let ((name (and (pair? e) (car e)))
        (value (and (pair? e) (pair? (cdr e)) (cadr e))))
    (if (and (bytevector? name) (bytevector? value))
        (values name value)
        (values #f #f))))

(define (decode-header-entry e)
  (define (pseudo-header? name)
    (and (> (string-length name) 0)
	 (char=? (string-ref name 0) #\:)))
  (let-values (((name value) (entry->name&value e)))
    (if (not name)
        (values #f #f "Malformed HPACK entry" #f)
        (guard (ex (else (values #f #f "Malformed UTF-8 in header" #f)))
	  (let ((h (utf8->string name)))
          (values h (utf8->string value) #f (pseudo-header? h)))))))


(define (response-body->bytevector body)
  (cond ((bytevector? body) body)
        ((string? body) (string->utf8 body))
        (else #f)))

(define (status-has-no-body? code)
  (or (eqv? code 204)
      (eqv? code 304)
      (and (<= 100 code) (< code 200))))

(define (contains-uppercase-ascii? s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (and (< i n)
           (let ((c (string-ref s i)))
             (or (and (char>=? c #\A) (char<=? c #\Z))
                 (loop (+ i 1))))))))
)
