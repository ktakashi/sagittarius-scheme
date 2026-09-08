;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/cache/memory.scm - in-memory cache implementation
;;;

#!nounbound
(library (net http-server cache memory)
  (export make-http-server:memory-cache)
  (import (rnrs)
	  (srfi :19)
          (srfi :18)
          (net http-server cache)
	  (net http-server request)
          (net http-server response)
          (net http-server types))

(define-record-type cache-entry
  (fields (mutable response)
          (mutable expires-at)
          (mutable accessed-at)))

(define (now-jiffy) (time-second (current-time)))

(define (->ttl-jiffy ttl-seconds)
  (if (and ttl-seconds (integer? ttl-seconds) (> ttl-seconds 0))
      ttl-seconds
      #f))

(define (evict-lru! table)
  (let ((victim #f)
        (best #f))
    (let-values (((ks vs) (hashtable-entries table)))
      (let loop ((i 0))
        (unless (= i (vector-length ks))
          (let ((k (vector-ref ks i))
                (v (vector-ref vs i)))
            (let ((a (cache-entry-accessed-at v)))
              (when (or (not best) (< a best))
                (set! best a)
                (set! victim k))))
          (loop (+ i 1)))))
    (when victim
      (hashtable-delete! table victim))))

(define (clone-response res)
  (let ((r (http-server:response-copy res)))
    (when (string? (http-server:response-body r))
      (http-server:response-body-set! r
        (string->utf8 (http-server:response-body r))))
    r))

(define (make-http-server:memory-cache :key (capacity 256))
  (let ((table (make-hashtable string-hash string=?))
        (lock (make-mutex)))
    (define (lookup key req)
      (mutex-lock! lock)
      (let ((entry (hashtable-ref table key #f))
            (now (now-jiffy)))
        (cond ((not entry)
               (mutex-unlock! lock)
               #f)
              ((and (cache-entry-expires-at entry)
                    (<= (cache-entry-expires-at entry) now))
               (hashtable-delete! table key)
               (mutex-unlock! lock)
               #f)
              (else
               (cache-entry-accessed-at-set! entry now)
               (let ((res (clone-response (cache-entry-response entry))))
                 (mutex-unlock! lock)
                 res)))))
    (define (store! key req res)
      (let* ((ttl (->ttl-jiffy (http-server:response-cache-ttl res)))
             (now (now-jiffy))
             (expires (and ttl (+ now ttl))))
        (mutex-lock! lock)
        (hashtable-set! table key
                        (make-cache-entry (clone-response res) expires now))
        (when (> (hashtable-size table) capacity)
          (evict-lru! table))
        (mutex-unlock! lock)
        #t))
    (define (invalidate! key)
      (mutex-lock! lock)
      (hashtable-delete! table key)
      (mutex-unlock! lock)
      #t)
    (define (clear!)
      (mutex-lock! lock)
      (hashtable-clear! table)
      (mutex-unlock! lock)
      #t)
    (make-http-server:cache 
     lookup store! invalidate! clear!
     (lambda (req)
       (let ((method (symbol->string (http-server:request-method req)))
             (target (http-server:request-target req)))
         (string-append method " " target))))))
)
