(library (www fastcgi)
    (export fcgx-init
	    fcgx-init-request
	    fcgx-accept
	    fcgx-finish
	    fcgx-puts
	    fcgx-read
	    fcgx-read-all
	    fcgx-getparam
	    fcgx-getenv

	    ;; high level WSGI style API
	    make-serve-function
	    ;; server
	    simple-server
	    simple-server-threaded
	    socket-server
	    socket-server-threaded

	    ;; for initialization
	    make-fcgi-context
	    init-fcgi
	    search-libfcgi
	    finish-fcgi
	    ;; should we export struct itself as well?
	    fcgx-request
	    make-fcgx-request)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius ffi)
	    (sagittarius socket)
	    (sagittarius control)
	    (srfi :1  lists)
	    (srfi :18 multithreading)
	    (srfi :39 parameters))


  ;; for FFI
  (define-class <fcgi-context> ()
    ((path    :init-keyword :path)
     (libfcgi :init-value #f :accessor context-libfcgi)
     (buffer-size :init-keyword :buffer-size :init-value 1024
		  :accessor context-buffer-size)))
  (define (make-fcgi-context path . rest)
    (apply make <fcgi-context> :path path rest))

  (define *default-platform-paths*
    (cond-expand
     (cygwin
      '("/usr/lib/libfcgi.dll" "/bin/cygfcgi-0.dll"))
     (linux
      '("/usr/lib/libfcgi.so.0"))
     (else
      ;; sorry we don't know other platfowm well ...
      '())))
  (define (search-libfcgi :optional (search-paths *default-platform-paths*))
    (let loop ((paths search-paths))
      (cond ((null? paths)
	     (cond-expand
	      (windows
	       ;; assume libfcgi.dll is on the path...
	       "libfcgi.dll")
	      (else #f)))
	    ((file-exists? (car paths)) (car paths))
	    (else (loop (cdr paths))))))

  (define default-context (make-parameter #f))
  (define (init-fcgi context)
    (default-context context)
    (let1 lib (open-shared-library (slot-ref context 'path))
      (when (null-pointer? lib)
	(assertion-violation 'init-fcgi
			     "open libfcgi failed." context))
      (context-libfcgi context lib)))

  (define (finish-fcgi :optional (context (default-context)))
    (close-shared-library (context-libfcgi context)))

  (define-syntax call-fcgi-func
    (syntax-rules ()
      ((_ context name ret args ...)
       (let1 lib (context-libfcgi context)
	 (unless lib
	   (assertion-violation 'call-fcgi-func
				"context is not initialized"))
	 (c-function lib ret name (args ...))))))

  (define-c-struct fcgx-request
    (int request-id)
    (int role)
    (void* in)				; fcgx_stream
    (void* out)				; fcgx_stream
    (void* err)				; fcgx_stream
    (void* envp) 			; char**
    (void* param-ptr)
    (int ipc-fd)			; < 0 means no connection
    (int is-begin-processed)
    (int keep-connection)
    (int app-status)
    (int nwriters)
    (int flags)
    (int listen-sock))

  (define (make-fcgx-request) (allocate-c-struct fcgx-request))

  ;; The low level APIs
  (define (fcgx-init :optional (context (default-context)))
    ((call-fcgi-func context FCGX_Init int)))

  ;; request must be fcgx-request pointer
  (define (fcgx-init-request request sock flags
			     :optional (context (default-context)))
    ((call-fcgi-func context FCGX_InitRequest int void* int int)
     request sock flags))

  (define (fcgx-accept req :optional (context (default-context)))
    ((call-fcgi-func context FCGX_Accept_r int void*) req))

  (define (fcgx-finish req :optional (context (default-context)))
    ((call-fcgi-func context FCGX_Finish_r int void*) req))

  (define (fcgx-puts req content :optional (context (default-context))
		     :key
		     (transcoder (native-transcoder))
		     (stream 'out))
    (let1 ostr (cond ((eq? stream 'err) (c-struct-ref req fcgx-request 'err))
		     (else (c-struct-ref req fcgx-request 'out)))
      ((call-fcgi-func context FCGX_PutS int char* void*)
       (string->bytevector
	(string-append content "\x0;")  ; make it null terminated
	transcoder) ostr)))

  ;; fcgx-read and fcgx-read-all are for reading POST data
  (define (fcgx-read req size :optional (context (default-context)))
    (let* ((buf (make-bytevector size 0))
	   (istr (c-struct-ref req fcgx-request 'in))
	   (readn ((call-fcgi-func context FCGX_GetStr int char* int void*)
		   buf size istr)))
      (if (= readn size)
	  buf
	  (bytevector-copy buf 0 readn))))

  (define (fcgx-read-all req :optional (context (default-context)))
    (let  ((buf-size (context-buffer-size context))
	   (out (open-output-bytevector)))
      (let loop ((content (fcgx-read req buf-size context)))
	(put-bytevector out content)
	(if (< (bytevector-length content) buf-size)
	    (get-output-bytevector out)
	    (loop (fcgx-read req buf-size context))))))

  (define (fcgx-getparam req key :optional (context (default-context))
			 :key (transcoder (native-transcoder)))
    (let1 env (c-struct-ref req fcgx-request 'envp)
      ((call-fcgi-func context FCGX_GetParam char* char* void*)
       (string->bytevector (string-append key "\x0;")
			   transcoder) env)))

  (define (fcgx-getenv req :optional (context (default-context))
		       :key (transcoder (native-transcoder)))
    (let1 env (c-struct-ref req fcgx-request 'envp)
      (let loop ((i 0) (r '()))
	(let1 p (deref env i)
	  (if (null-pointer? p)
	      (reverse! r)
	      (let1 s (pointer->string p transcoder)
		(receive (name value) (string-scan s "=" 'both)
		  (loop (+ i 1) (acons name value r)))))))))


  ;; High level WSGI style APIs
  (define (gen-start-response)
    (let ((save-status "200 OK")
	  (save-headers (default-headers)))
      (lambda (status headers)
	(when status
	  (set! save-status status))
	(when headers
	  (set! save-headers (merge-headers save-headers headers)))
	(values save-status save-headers))))

  (define (make-serve-function app)
    (lambda (request)
      (let* ((env (fcgx-getenv request))
	     (start-response (gen-start-response)))
	(set! env (acons 'POST-READER 
			 (lambda (:optional (size #f))
			   (if size
			       (fcgx-read request size)
			       (fcgx-read-all request)))
			 env))
	(let ((content (app env start-response)))
	  (let-values (((status headers) (start-response #f #f)))
	    (fcgx-puts request (format "Status: ~A\r\n" status))
	    (dolist (item headers)
	      (fcgx-puts request (format "~A: ~A\r\n" (car item) (cdr item))))
	    (fcgx-puts request "\r\n\r\n")
	    (dolist (item content)
	      (fcgx-puts request (format "~A" item))))))))

  ;; servers
  (define (server-on-fd proc fd)
    (fcgx-init)
    (let ((req (make-fcgx-request)))
      (fcgx-init-request req fd 1)
      (let loop ((rc (fcgx-accept req)))
	(cond ((zero? rc)
	       (proc req)
	       (fcgx-finish req)
	       (loop (fcgx-accept req)))
	      (else
	       "ACCEPT_ERROR")))))

  (define (server-on-fd-threaded proc sock threads)
    (fcgx-init)
    (dotimes (count (- threads 1))
      (thread-start! (make-thread (lambda ()
				    (server-run proc sock)))))
    (server-run proc sock))

  (define (simple-server proc :key (fcgi-path #f))
    (init-fcgi (make-fcgi-context (or fcgi-path (search-libfcgi))))
    (server-on-fd proc 0))
  (define (simple-server-threaded proc :key (fcgi-path #f) (threads 4))
    (init-fcgi (make-fcgi-context (or fcgi-path (search-libfcgi))))
    (server-on-fd-threaded proc 0 threads))

  (define (socket-server proc :key
			 (port "9000")
			 (fcgi-path #f))
    (init-fcgi (make-fcgi-context (or fcgi-path (search-libfcgi))))
    (let ((socket (make-server-socket port)))
      (with-exception-handler
       (lambda (e) (socket-close socket) (raise e))
       (lambda ()
	 (server-on-fd proc (socket-fd socket))
	 (socket-close socket)))))

  (define (socket-server-threaded proc :key
				  (port "9000")
				  (threads 4)
				  (fcgi-path #f))
    (init-fcgi (make-fcgi-context (or fcgi-path (search-paths))))
    (let ((socket (make-server-socket port)))
      (with-exception-handler
       (lambda (e) (socket-close socket) (raise e))
       (lambda ()
	 (server-on-fd-threaded proc (socket-fd socket) threads)
	 (socket-close socket)))))

 
  ;; helpers
  (define (default-headers) `(("X-powered-by" . "Sagittarius:www-fastcgi")
			      ("Content-Type" . "text/html")))

  (define (merge-headers old new)
    (lset-union (lambda (a b)
		  (string=? (car a) (car b))) new old))
)
