;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/router.scm - basic HTTP router
;;;

#!nounbound
(library (net http-server router)
    (export http-server:router?
            make-http-server:router
            http-server:router-add-route!
            http-server:router-dispatch
            http-server:make-router-handler)
    (import (rnrs)
            (net http-server request)
            (net http-server response))
  
(define-record-type route
  (fields method pattern segments handler))

(define-record-type (http-server:router
                     %make-http-server:router
                     http-server:router?)
  (fields (mutable routes)))

(define (make-http-server:router) (%make-http-server:router '()))

(define (split-path path)
  (define len (string-length path))
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i len)
           (let ((seg (substring path start i)))
             (reverse (if (string=? seg "") out (cons seg out)))))
          ((char=? (string-ref path i) #\/)
           (let ((seg (substring path start i)))
             (loop (+ i 1) (+ i 1)
                   (if (string=? seg "") out (cons seg out)))))
          (else
           (loop (+ i 1) start out)))))

(define (method->symbol m)
  (cond ((symbol? m) m)
        ((string? m) (string->symbol m))
        (else m)))

(define (http-server:router-add-route! router method pattern handler)
  (let ((r (make-route (method->symbol method)
                       pattern
                       (split-path pattern)
                       handler)))
    (http-server:router-routes-set!
     router
     (append (http-server:router-routes router) (list r)))))

(define (match-route route req)
  (let* ((path-seg (split-path (http-server:request-path req)))
         (route-seg (route-segments route)))
    (let loop ((r route-seg) (p path-seg) (vars '()))
      (cond ((and (null? r) (null? p)) (reverse vars))
            ((null? r) #f)
            ((and (pair? r) (string=? (car r) "*")) (reverse vars))
            ((null? p) #f)
            ((and (> (string-length (car r)) 0)
                  (char=? (string-ref (car r) 0) #\:))
             (loop (cdr r)
                   (cdr p)
                   (cons (cons (string->symbol (substring (car r) 1 (string-length (car r))))
                               (car p))
                         vars)))
            ((string=? (car r) (car p))
             (loop (cdr r) (cdr p) vars))
            (else #f)))))

(define (http-server:router-dispatch router req)
  (let* ((method (http-server:request-method req))
         (routes (http-server:router-routes router)))
    (let loop ((rest routes) (allowed '()))
      (cond ((null? rest)
             (values #f (reverse allowed)))
            (else
             (let* ((r (car rest))
                    (vars (match-route r req)))
               (if vars
                   (if (eq? method (route-method r))
                       (values (route-handler r) vars)
                       (loop (cdr rest) (cons (route-method r) allowed)))
                   (loop (cdr rest) allowed))))))))

(define (join-methods methods)
  (let loop ((rest methods) (first? #t) (out ""))
    (if (null? rest)
        out
        (loop (cdr rest)
              #f
              (if first?
                  (symbol->string (car rest))
                  (string-append out ", " (symbol->string (car rest))))))))

(define (http-server:make-router-handler router)
  (lambda (req res)
    (let-values (((handler vars) (http-server:router-dispatch router req)))
      (cond (handler
             (for-each (lambda (kv)
                         (http-server:request-attribute-set! req (car kv) (cdr kv)))
                       vars)
             (handler req res))
            ((pair? vars)
             (http-server:response-status-set! res 405)
             (http-server:response-header-set! res "allow" (join-methods vars))
             (http-server:response-text! res "Method Not Allowed")
             res)
            (else
             (http-server:response-status-set! res 404)
             (http-server:response-text! res "Not Found")
             res))))
  )
)
