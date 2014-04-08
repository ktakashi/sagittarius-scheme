;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/transport.scm - AMQP v1.0 transport
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; reference:
;; http://docs.oasis-open.org/amqp/core/v1.0/os/amqp-core-transport-v1.0-os.html

(library (net mq amqp transport)
    (export amqp-make-client-connection
	    open-amqp-connection!
	    close-amqp-connection!
	    begin-amqp-session!
	    end-amqp-session!)
    (import (except (rnrs) fields)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius socket)
	    (clos user)
	    (net mq amqp types)
	    (rfc tls)
	    (binary data)
	    (binary pack)
	    (math random))

  ;; node
  (define-class <amqp-node> ()
    ((name :init-keyword :name :init-value #f)))
  ;; TODO
  (define-class <amqp-producer> (<amqp-node>) ())
  (define-class <amqp-consumer> (<amqp-node>) ())
  (define-class <amqp-queue> (<amqp-node>) ())

  ;; container
  (define-class <amqp-container> ()
    ((container-id :init-keyword :container-id :init-value #f)))
  ;; TODO
  (define-class <amqp-client> (<amqp-container>) ())
  (define-class <amqp-broker> (<amqp-container>) ())

  ;; connections
  (define-class <amqp-connection> ()
    ;; should connection manage sessions?
    ((principal :init-keyword :principal :init-value #f)
     ;; initial state is :start
     (state     :init-value :start)
     ;; socket-port
     (socket    :init-keyword :socket)
     ;; for informations
     (hostname  :init-keyword :hostname)
     (port      :init-keyword :port)))

  (define-class <amqp-session> ()
    ((name :init-keyword :name :init-value #f)
     (remote-channel :init-keyword :remote-channel)
     (next-outgoint-id :init-keyword :next-outgoing-id)
     (incoming-window :init-keyword :incoming-window)
     (outgoing-window :init-keyword :outgoing-window)
     ;; need this?
     ;;(handle-max :init-keyword :handle-max :init-value 1024)
     ;; private
     (connection :init-keyword :connection :init-value #f)))

  (define-class <amqp-link> ()
    ((name :init-keyword :name :init-value #f)
     (source :init-keyword :source :init-value #f)
     (target :init-keyword :target :init-value #f)
     (timeout :init-keyword :timeout :init-value -1)
     ;; private
     (session :init-keyword :session :init-value #f)))

  ;; will version be other than 1.0.0 for future?
  ;;(define-constant +version-prefix+ "AMQP\x0;")
  (define-constant +version-prefix+ #vu8(65 77 81 80 0)) ;; "AMQP\x0;"
  
  (define-constant +initial-end-states+ '(:opended :end))

  ;; socket can be either TLS or raw socket
  (define (amqp-make-client-connection host service
				       :key (major 1) (minor 0) (revision 0)
					    ;; not supported yet
					    (sasl? #f)
				       :allow-other-keys opts)
    
    (let* ((socket (make-client-socket host service))
	   (conn (make <amqp-connection> :socket (socket-port socket)
		       :hostname host :port service)))
      (negotiate-header conn major minor revision)
      (apply open-amqp-connection! conn opts)))

  (define (negotiate-header conn major minor revision)
    (define (construct-header out)
      (put-bytevector out +version-prefix+)
      (put-u8 out major)
      (put-u8 out minor)
      (put-u8 out revision))
    (let1 header (call-with-bytevector-output-port construct-header)
      (put-bytevector (~ conn 'socket) header)
      (let1 res (get-bytevector-n (~ conn 'socket) (bytevector-length header))
	(unless (bytevector=? header res)
	  (error 'negotiate-header "unknown protocol" (utf8->string res)))
	(set! (~ conn 'state) :hdr-exch))))

  ;; misc types
  (define-restricted-type milliseconds :uint)
  (define-restricted-type ietf-language-tag :symbol)
  (define-restricted-type fields :map)
  (define-restricted-type handle :uint)
  (define-restricted-type sequence-no :uint)
  (define-restricted-type transfer-number sequence-no)
  ;; error
  (define-composite-type error amqp:error:list #x00000000 #x0000001d
    ((condition   :type :symbol :mandatory #f)
     (description :type :string)
     (info        :type fields)))

  ;; define messages
  (define-composite-type open amqp:open:list #x00000000 #x00000010
    ((container-id     	   :type :string :mandatory #t)
     (hostname         	   :type :string)
     (max-frame-size   	   :type :uint :default 4294967295)
     (channel-max      	   :type :ushort :default 65535)
     (idel-type-out    	   :type milliseconds)
     (outgoing-locales 	   :type ietf-language-tag :multiple #t)
     (incoming-locales 	   :type ietf-language-tag :multiple #t)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties           :type fields)))

  (define (open-amqp-connection! conn . opts)
    (apply send-open-frame conn opts)
    (recv-open-frame conn)
    (when (eq? (~ conn 'state) :open-rcvd)
      (set! (~ conn 'state) :opened))
    conn)

  ;; should this be in type?
  (define (amqp-value->bytevector msg)
    (call-with-bytevector-output-port
     (lambda (out)
       (write-amqp-data out msg))))

  ;; wrap with frame
  (define (send-frame conn msg)
    (let ((bv (amqp-value->bytevector msg))
	  (port (~ conn 'socket)))
      ;; TODO extra header
      (put-u32 port (+ (bytevector-length bv) 8) (endianness big))
      (put-u8 port 2) ;; TODO DOF
      (put-u8 port 0) ;; AMQP frame
      (put-u16 port 0 (endianness big))
      (put-bytevector port bv)))

  (define (recv-frame conn)
    (let1 port (~ conn 'socket)
      (let-values (((size dof type specific) (get-unpack port "!LCCS")))
	(let* ((ext (get-bytevector-n port (- (* dof 4) 8)))
	       (data (get-bytevector-n port (- size (* dof 4)))))
	  (values ext (read-amqp-data (open-bytevector-input-port data)))))))
  
  (define (generate-container-id)
    (format "Sagittarius-~a-~a"
	    (sagittarius-version) 
	    ;; i don't remember how it should be but 9 bits returns 4 bytes
	    (bytevector->integer (read-sys-random 9))))

  ;; TODO make id looks like Sagittarius but unique
  (define (send-open-frame conn :key (id (generate-container-id))
			   :allow-other-keys opts)
    (let1 open (apply make-amqp-open :container-id id 
		      :hostname (~ conn 'hostname) opts)
      (send-frame conn open)
      (set! (~ conn 'state) :open-sent)))

  (define (recv-open-frame conn)
    (let-values (((ext open) (recv-frame conn)))
      (when (amqp-open? open) (set! (~ conn 'state) :open-rcvd))
      open))

  (define-composite-type close amqp:close:list #x00000000 #x00000018
    ((error :type error)))

  (define (close-amqp-connection! conn)
    (send-close-frame conn)
    (recv-close-frame conn)
    (set! (~ conn 'state) :end)
    ;; spec said SHOULD ...
    (shutdown-port (~ conn 'socket) SHUT_RDWR)
    (close-port (~ conn 'socket))
    conn)

  (define (send-close-frame conn :key error)
    (send-frame conn (make-amqp-close :error error))
    (set! (~ conn 'state) :close-sent))
  (define (recv-close-frame conn)
    (let-values (((ext close) (recv-frame conn)))
      (set! (~ conn 'state) :close-rcvd)
      close))

  (define-composite-type begin amqp:begin:list #x00000000 #x00000011
    ((remote-channel   :type :ushort)
     (next-outgoing-id :type transfer-number :mandatory #t)
     (incoming-window  :type :uint :mandatory #t)
     (outgoing-window  :type :uint :mandatory #t)
     (handle-max       :type handle :default 4294967295)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties       :type fields)))

  (define (begin-amqp-session! conn :key (id 0) 
			     (incoming-window 512)
			     (outgoing-window 512)
			     :allow-other-keys opts)
    (let1 begin (apply make-amqp-begin :next-outgoing-id id
		       :incoming-window incoming-window
		       :outgoing-window outgoing-window
		       opts)
      (send-frame conn begin)
      (let-values (((ext begin) (recv-frame conn)))
	(rlet1 session (make <amqp-session>
			 :connection conn
			 :remote-channel (if (slot-bound? begin 'remote-channel)
					     (~ begin 'remote-channel)
					     ;; ??
					     0)
			 :next-outgoing-id (~ begin 'next-outgoing-id)
			 :incoming-window (~ begin 'incoming-window)
			 :outgoing-window (~ begin 'outgoing-window))
	  ;; TODO add session to connection
	  session))))

  (define-composite-type end amqp:end:list #x00000000 #x00000017
    ((error :type error)))

  (define (end-amqp-session! session :key error)
    (send-frame (~ session 'connection) (make-amqp-end :error error))
    (recv-frame (~ session 'connection))
    (set! (~ session 'connection) #f) ;; invalidate it
    session)

  )