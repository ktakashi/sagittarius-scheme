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
	    ;; session
	    begin-amqp-session!
	    end-amqp-session!
	    ;; link
	    attach-amqp-link!
	    detach-amqp-link!
	    ;; misc
	    +amqp-sender+
	    +amqp-receiver+
	    ;; these are used in other messaging or so
	    seconds
	    fields
	    )
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

  (define-class <state-mixin> ()
    ((state :init-keyword :state)))
  ;; connections
  (define-class <amqp-connection> (<state-mixin>)
    ;; should connection manage sessions?
    ((principal :init-keyword :principal :init-value #f)
     ;; socket-port
     (socket    :init-keyword :socket)
     ;; for informations
     (hostname  :init-keyword :hostname)
     (port      :init-keyword :port)))

  (define-class <amqp-session> (<state-mixin>)
    ((name :init-keyword :name :init-value #f)
     (remote-channel :init-keyword :remote-channel)
     (next-outgoint-id :init-keyword :next-outgoing-id)
     (incoming-window :init-keyword :incoming-window)
     (outgoing-window :init-keyword :outgoing-window)
     ;; need this?
     (handle-max :init-keyword :handle-max :init-value 1024)
     ;; private
     (connection :init-keyword :connection :init-value #f)
     ;; for creating a link we need to assing unused handle
     (handles :init-value '())))

  (define-class <amqp-link> (<state-mixin>)
    ((name :init-keyword :name :init-value #f)
     (handle :init-keyword :handle)
     (source :init-keyword :source :init-value #f)
     (target :init-keyword :target :init-value #f)
     (timeout :init-keyword :timeout :init-value -1)
     (delivery-count :init-value 0)
     ;; will be set
     link-credit
     avaiable 
     (drain    :init-value #f)
     ;; private
     (session :init-keyword :session :init-value #f)))

  (define-class <amqp-sender-link> (<amqp-link>) ())
  (define-class <amqp-receiver-link> (<amqp-link>) ())

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
		       :state :start
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
	  (rnrs:error 'negotiate-header "unknown protocol" (utf8->string res)))
	(set! (~ conn 'state) :hdr-exch))))

  ;; misc types
  (define-restricted-type seconds      :uint)
  (define-restricted-type milliseconds :uint)
  (define-restricted-type ietf-language-tag :symbol)
  (define-restricted-type fields :map)
  (define-restricted-type handle :uint)
  (define-restricted-type sequence-no :uint)
  (define-restricted-type transfer-number sequence-no)
  (define-restricted-type role :boolean)
  (define-restricted-type sender-settle-mode :ubyte)
  (define-restricted-type receiver-settle-mode :ubyte)
  (define-restricted-type delivery-number sequence-no)
  (define-restricted-type delivery-tag :binary)
  (define-restricted-type message-format :uint)

  (define-constant +amqp-sender+   #f)
  (define-constant +amqp-receiver+ #t)

  ;; sender settle mode
  (define-constant +amqp-unsettled+ 0)
  (define-constant +amqp-settled+   1)
  (define-constant +amqp-mixed+     2)

  ;; receiver settle mode
  (define-constant +amqp-first+     0)
  (define-constant +amqp-second+    1)

  ;; error
  (define-composite-type error amqp:error:list #x00000000 #x0000001d
    ((condition   :type :symbol :mandatory #f)
     (description :type :string)
     (info        :type fields))
    :provides (error-condition))

  ;; define messages
  (define-composite-type open amqp:open:list #x00000000 #x00000010
    ((container-id     	   :type :string :mandatory #t)
     (hostname         	   :type :string)
     (max-frame-size   	   :type :uint :default 4294967295)
     (channel-max      	   :type :ushort :default 65535)
     (idle-time-out    	   :type milliseconds)
     (outgoing-locales 	   :type ietf-language-tag :multiple #t)
     (incoming-locales 	   :type ietf-language-tag :multiple #t)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties           :type fields))
    :provides (frame))

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

  (define (recv-frame conn :key (debug #f))
    (let1 port (~ conn 'socket)
      (let-values (((size dof type specific) (get-unpack port "!LCCS")))
	(let* ((ext (get-bytevector-n port (- (* dof 4) 8)))
	       (data (get-bytevector-n port (- size (* dof 4)))))
	  (when debug 
	    (display (pack "!LCCS" size dof type specific)) (newline)
	    (display data) (newline))
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
		      ;;:hostname (~ conn 'hostname) 
		      opts)
      (send-frame conn open)
      ;;(set! (~ conn 'principal) id)
      (set! (~ conn 'state) :open-sent)))

  (define (recv-open-frame conn)
    (let-values (((ext open) (recv-frame conn)))
      (when (amqp-open? open) (set! (~ conn 'state) :open-rcvd))
      open))

  (define-composite-type close amqp:close:list #x00000000 #x00000018
    ;; error is composite type and not bounded so use keyword ...
    ;; (sort of bad trick...)
    ((error :type :error)))

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
     (properties       :type fields))
    :provides (frame))

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
			 :state :mapped
			 :connection conn
			 :remote-channel (~ begin 'remote-channel)
			 :handle-max (~ begin 'handle-max)
			 :next-outgoing-id (~ begin 'next-outgoing-id)
			 :incoming-window (~ begin 'incoming-window)
			 :outgoing-window (~ begin 'outgoing-window))
	  ;; TODO add session to connection
	  session))))

  (define-composite-type end amqp:end:list #x00000000 #x00000017
    ((error :type :error)))

  (define (end-amqp-session! session :key error)
    (send-frame (~ session 'connection) (make-amqp-end :error error))
    (recv-frame (~ session 'connection))
    (set! (~ session 'connection) #f) ;; invalidate it
    (set! (~ session 'state) :unmmapped)
    session)

  (define-composite-type attach amqp:attach:list #x00000000 #x00000012
    ((name   :type :string :mandatory #t)
     (handle :type handle :mandatory #t)
     (role   :type role   :mandatory #t)
     (snd-settle-mode :type sender-settle-mode :default +amqp-mixed+)
     (rcv-settle-mode :type receiver-settle-mode :default +amqp-first+)
     (source :type :* :requires 'source)
     (target :type :* :requires 'target)
     (unsettled :type :map)
     (incomplete-unsettled :type :boolean :default #f)
     (initial-delivery-count :type sequence-no)
     (max-message-size :type :ulong)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties :type fields))
    :provides (frame))

  (define-composite-type detach amqp:detach:list #x00000000 #x00000016
    ((handle :type handle :mandatory #t)
     (closed :type :boolean :default #f)
     (error  :type :error))
    :provides (frame))

  (define (attach-amqp-link! session name role source target . opts)
    ;; TODO dummy, compute handle properly
    (define (compute-handle session) 1)
    (define conn (~ session 'connection))
    (define (finish handle source target)
      (let1 link (make (if role <amqp-receiver-link> <amqp-sender-link>)
		   :name name :handle handle :role role
		   :source source :target target
		   :timeout #f ;; TODO
		   :session session)
	(flow-control link)))
    (let* ((handle (compute-handle session))
	   (attach (apply make-amqp-attach :name name
			  :handle handle
			  :role role
			  :source source
			  :target target
			  :initial-delivery-count 0
			  opts)))
      (send-frame conn attach)
      (let-values (((ext attach) (recv-frame conn)))
	(if (slot-bound? attach 'target)
	    (finish handle (~ attach 'source) (~ attach 'target))
	    (let-values (((ext detach) (recv-frame conn)))
	      (let1 detach (make-amqp-detach :handle handle :closed #t)
		(send-frame conn detach)
		(finish handle (~ attach 'source) #f)))))))

  (define-composite-type transfer amqp:transfer:list #x00000000 #x00000014
    ((handle          :type handle :mandatory #t)
     (delivery-id     :type delivery-number)
     (delivery-tag    :type delivery-number)
     (message-format  :type message-format)
     (settled         :type :boolean)
     (more            :type :boolean :default #f)
     (rcv-settle-mode :type receiver-settle-mode)
     (state           :type :* :requires 'delivery-state)
     (resume          :type :boolean :default #f)
     (aborted         :type :boolean :default #f)
     (batchable       :type :boolean :default #f))
    :provides (frame))

  (define-composite-type flow amqp:flow:list #x00000000 #x00000013
    ((next-incoming-id :type transfer-number)
     (incoming-window  :type :uint :mandatory #t)
     (next-outgoing-id :type transfer-number :mandatory #t)
     (outgoing-window  :type :uint :mandatory #t)
     (handle           :type handle)
     (delivery-count   :type sequence-no)
     (link-credit      :type :uint)
     (available        :type :uint)
     (drain            :type :boolean :default #f)
     (echo             :type :boolean :default #f)
     (properties       :type fields))
    :provides (frame))


  (define (recv-flow-frame conn)
    (let-values (((ext flow) (recv-frame conn :debug #t))) flow))
  ;; well for may laziness...
  (define-method flow-control ((link <amqp-sender-link>))
    (let* ((conn (~ link 'session 'connection))
	   ;; only target is there
	   (flow (and (~ link 'target) (recv-flow-frame conn))))
      (when flow
	;; TODO copy
	(set! (~ link 'link-credit) (~ flow 'link-credit)))
      (let1 flow (make-amqp-flow 
		  ;; if the role is sender then this must be set
		  :next-incoming-id 0
		  :incoming-window #x7FFFFFFF
		  :next-outgoing-id 1
		  :outgoing-window 0
		  :handle (~ link 'handle)
		  :delivery-count 0
		  :link-credit #x64)
	(send-frame conn flow)
	link)))
  (define-method flow-control ((link <amqp-receiver-link>))
    )

  (define (detach-amqp-link! link :key error)
    (let ((detach (make-amqp-detach :handle (~ link 'handle) :closed #t
				    :error error))
	  (conn (~ link 'session 'connection)))
      (send-frame conn detach)
      ;; receive detach
      (recv-frame conn)
      link))
      
  )