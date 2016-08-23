;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/transport.scm - AMQP v1.0 transport
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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
	    amqp-connection?
	    close-amqp-connection!
	    ;; session
	    begin-amqp-session!
	    end-amqp-session!
	    amqp-session?
	    ;; link
	    attach-amqp-link!
	    detach-amqp-link!
	    amqp-sender-link?
	    amqp-receiver-link?
	    ;; misc
	    +amqp-sender+
	    +amqp-receiver+
	    ;; these are used in other messaging or so
	    seconds
	    milliseconds
	    fields
	    sequence-no
	    ;; needs in upper layer
	    send-frame
	    (rename (recv-frame&payload recv-frame))
	    send-transfer
	    recv-transfer
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
	    (math random)
	    (util bytevector))

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
  ;; TODO if we support sparse array, then we can make this default
  ;; value 65535 (we can do this but it consume too much memory...
  (define-constant +channel-max+ 1024)
  (define-class <amqp-connection> (<state-mixin>)
    ;; should connection manage sessions?
    ((principal  :init-keyword :principal :init-value #f)
     ;; socket-port
     (socket     :init-keyword :socket)
     (raw-socket :init-keyword :raw-socket)
     ;; for informations
     (hostname   :init-keyword :hostname)
     (port       :init-keyword :port)
     ;; frame size
     (frame-size :init-keyword :frame-size)
     ;; channels
     (remote-channels :init-form (make-vector +channel-max+ #f))))
  (define (amqp-connection? o) (is-a? o <amqp-connection>))

  ;; max hande count, should be sufficient
  ;; TODO if we support sparse array, then we can make this default
  ;; value 4294967295
  (define-constant +handle-max+ 1024)
  (define-class <amqp-session> (<state-mixin>)
    ((name :init-keyword :name :init-value #f)
     (remote-channel :init-keyword :remote-channel)
     (next-outgoing-id :init-keyword :next-outgoing-id :init-value 0)
     (next-incoming-id :init-keyword :next-outgoing-id)
     (incoming-window :init-keyword :incoming-window :init-value 0)
     (outgoing-window :init-keyword :outgoing-window :init-value 0)
     ;; remote
     (remote-next-outgoint-id :init-keyword :next-outgoing-id :init-value 0)
     (remote-next-incoming-id :init-keyword :next-outgoing-id)
     (remote-incoming-window :init-keyword :remote-incoming-window)
     (remote-outgoing-window :init-keyword :remote-outgoing-window)
     ;; private
     (connection :init-keyword :connection :init-value #f)
     ;; for creating a link we need to assing unused handle
     (remote-handles :init-form (make-vector +handle-max+ #f))
     (local-handles  :init-form (make-vector +handle-max+ #f))))
  (define (amqp-session? o) (is-a? o <amqp-session>))

  (define-class <amqp-link> (<state-mixin>)
    ((name :init-keyword :name :init-value #f)
     (handle :init-keyword :handle)
     (source :init-keyword :source :init-value #f)
     (target :init-keyword :target :init-value #f)
     (timeout :init-keyword :timeout :init-value -1)
     (delivery-count :init-value 0)
     (link-creadit :init-value 0)
     ;; will be set
     remote-delivery-count
     remote-link-credit
     avaiable 
     (drain    :init-value #f)
     ;; private
     (session :init-keyword :session :init-value #f)))

  (define-class <amqp-sender-link> (<amqp-link>) ())
  (define-class <amqp-receiver-link> (<amqp-link>) ())
  (define (amqp-sender-link? o) (is-a? o <amqp-sender-link>))
  (define (amqp-receiver-link? o) (is-a? o <amqp-receiver-link>))

  ;; will version be other than 1.0.0 for future?
  (define-constant +version-prefix+ #vu8(65 77 81 80)) ;; "AMQP"
  
  (define-constant +initial-end-states+ '(:opended :end))

  (define-constant +no-security+ 0)
  ;; TODO support them
  (define-constant +use-tls+     2)
  (define-constant +use-sasl+    3)

  ;; socket can be either TLS or raw socket
  (define (amqp-make-client-connection host service
				       :key (major 1) (minor 0) (revision 0)
					    ;; not supported yet
					    (security-level +no-security+)
				       :allow-other-keys opts)
    
    (let* ((socket (make-client-socket host service))
	   (conn (make <amqp-connection> :socket (socket-port socket)
		       :raw-socket socket
		       :state :start
		       :hostname host :port service)))
      (negotiate-header conn major minor revision security-level)
      (apply open-amqp-connection! conn opts)))

  (define (negotiate-header conn major minor revision security-mark)
    (define (construct-header out)
      (put-bytevector out +version-prefix+)
      (put-u8 out security-mark)
      (put-u8 out major)
      (put-u8 out minor)
      (put-u8 out revision))
    (let1 header (call-with-bytevector-output-port construct-header)
      (put-bytevector (~ conn 'socket) header)
      (let1 res (get-bytevector-n (~ conn 'socket) (bytevector-length header))
	(unless (bytevector=? header res)
	  (error 'negotiate-header "unknown protocol" 
		 `((request ,(utf8->string header))
		   (response ,(utf8->string res)))))
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
  (define-constant +max-frame-size+ #x100000) ;; (* 1024 1024)
  (define-composite-type open amqp:open:list #x00000000 #x00000010
    ((container-id     	   :type :string :mandatory #t)
     (hostname         	   :type :string)
     (max-frame-size   	   :type :uint :default +max-frame-size+)
     (channel-max      	   :type :ushort :default +channel-max+)
     (idle-time-out    	   :type milliseconds)
     (outgoing-locales 	   :type ietf-language-tag :multiple #t)
     (incoming-locales 	   :type ietf-language-tag :multiple #t)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties           :type fields))
    :provides (frame))

  (define (open-amqp-connection! conn . opts)
    (apply send-open-frame conn opts)
    (let1 open (recv-open-frame conn)
      (when (slot-bound? open 'max-frame-size)
	(set! (~ conn 'frame-size) (~ open 'max-frame-size))))
    (when (eq? (~ conn 'state) :open-rcvd)
      (set! (~ conn 'state) :opened))
    conn)

  ;; wrap with frame
  (define (send-frame conn performative :optional (payload #vu8()))
    (let ((bv (amqp-value->bytevector performative))
	  (port (~ conn 'socket)))
      ;; TODO extra header
      (put-u32 port (+ (bytevector-length bv) (bytevector-length payload) 8)
	       (endianness big))
      (put-u8 port 2) ;; TODO DOF
      (put-u8 port 0) ;; AMQP frame
      (put-u16 port 0 (endianness big))
      (put-bytevector port bv)
      (put-bytevector port payload)))

  ;; this won't receive payload, internal use only.
  (define (recv-frame conn)
    (let-values (((ext performative payload) (recv-frame&payload conn)))
      (values ext performative)))
  
  (define (recv-frame&payload conn)
    (let1 port (~ conn 'socket)
      (let-values (((size dof type specific) (get-unpack port "!LCCS")))
	(let* ((ext (get-bytevector-n port (- (* dof 4) 8)))
	       (data (get-bytevector-n port (- size (* dof 4))))
	       (in (open-bytevector-input-port data))
	       (perfom (read-amqp-data in)))
	  (values ext perfom (get-bytevector-all in))))))

  (define (generate-container-id)
    (format "Sagittarius-~a-~a"
	    (sagittarius-version) 
	    (bytevector->integer (read-sys-random 32))))

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
     (handle-max       :type handle :default +handle-max+)
     (offered-capabilities :type :symbol :multiple #t)
     (desired-capabilities :type :symbol :multiple #t)
     (properties       :type fields))
    :provides (frame))

  ;; TODO ID must be unique per session...
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
			 :handle-max +handle-max+
			 :next-incoming-id (~ begin 'next-outgoing-id)
			 :remote-incoming-window (~ begin 'incoming-window)
			 :remote-outgoing-window (~ begin 'outgoing-window))
	  (if (vector-ref (~ conn 'remote-channels) (~ begin 'remote-channel))
	      ;; like this?
	      (error 'begin-amqp-session! "the channel is already open")
	      (vector-set! (~ conn 'remote-channels)
			   (~ begin 'remote-channel)
			   session))))))

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
    (define (compute-handle session)
      (define handles (~ session 'local-handles))
      (let loop ((i 0))
	(cond ((= i +handle-max+) (error 'attach-amqp-link! "handle overflow"))
	      ((not (vector-ref handles i)) i)
	      (else (+ i 1)))))
    (define (add-handle map handle link)
      (vector-set! map handle link))
    (define conn (~ session 'connection))
    (define (make-link attach handle)
      (define source (~ attach 'source)) ;; must be there
      (define target (if (slot-bound? attach 'target) (~ attach 'target) #f))
      (rlet1 link (make (if role <amqp-receiver-link> <amqp-sender-link>)
		    :name name :handle handle :role role
		    :source source :target target
		    :timeout #f ;; TODO
		    :session session
		    :state :attached)
	(add-handle (~ session 'remote-handles) handle link)))
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
	(let1 link (make-link attach handle)
	  (if (slot-bound? attach 'target)
	      (flow-control link values)
	      (detach-amqp-link! link))))))

  (define-composite-type transfer amqp:transfer:list #x00000000 #x00000014
    ((handle          :type handle :mandatory #t)
     (delivery-id     :type delivery-number)
     (delivery-tag    :type delivery-tag)
     (message-format  :type message-format)
     (settled         :type :boolean)
     (more            :type :boolean :default #f)
     (rcv-settle-mode :type receiver-settle-mode)
     (state           :type :* :requires 'delivery-state)
     (resume          :type :boolean :default #f)
     (aborted         :type :boolean :default #f)
     (batchable       :type :boolean :default #f))
    :provides (frame))
  (define-method write-object ((t <amqp-transfer>) out)
    (format out "#<transfer ~a:~a ~a>"
	    (bytevector->hex-string (~ t 'delivery-tag))
	    (~ t 'delivery-id)
	    (~ t 'more)))

  (define-composite-type disposition amqp:disposition:list #x00000000 #x00000015
    ((role :type role :mandatory #t)
     (first :type delivery-number :mandatory #t)
     (last :type delivery-number)
     (settled :type :boolean :default #f)
     (state :type :* :requires 'delivery-state)
     (batchable :type :boolean :default #f)))

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

  ;; well for may laziness...
  ;; TODO this is not correct
  (define (sender-flow-control link flow)
    (define session (~ link 'session))
    ;; session info
    (let ((inext (if (slot-bound? flow 'next-incoming-id)
		     (~ flow 'next-incoming-id)
		     #f))
	  (iwin (~ flow 'incoming-window)))
      (if inext
	  (let1 iwin2 (- (+ inext iwin) (~ session 'next-outgoing-id))
	    (set! (~ session 'remote-next-incoming-id) inext)
	    (set! (~ session 'remote-incoming-window) iwin2))
	  (set! (~ session 'remote-incoming-window) iwin))
      (set! (~ session 'remote-next-outgoint-id) (~ flow 'next-outgoing-id))
      (set! (~ session 'remote-outgoing-window) (~ flow 'outgoing-window)))
    ;; link info
    (set! (~ link 'remote-link-credit) (~ flow 'link-credit))
    (set! (~ link 'remote-delivery-count) (~ flow 'delivery-count))
    link)

  (define-generic flow-control)
  (define-method flow-control ((link <amqp-sender-link>) k)
    (define session (~ link 'session))
    (define conn (~ session 'connection))
    (let-values (((ext flow) (recv-frame conn)))
      (sender-flow-control link flow)
      ;; we probably don't have to send this back...
      #;
      (let1 flow (make-amqp-flow 
		  ;; if the role is sender then this must be set
		  :next-incoming-id 0
		  :incoming-window #x7FFFFFFF
		  :next-outgoing-id 1
		  :outgoing-window 0
		  :handle (~ link 'handle)
		  :delivery-count 0
		  :link-credit (+ (~ link 'remote-link-credit)
				  (~ link 'remote-delivery-count)))
	(send-frame conn flow)
	(k link))
      (k link)))
  (define-method flow-control ((link <amqp-receiver-link>) k)
    (define session (~ link 'session))
    (define conn (~ session 'connection))
    ;; send flow
    ;; FIXME compute properly
    (send-frame conn (make-amqp-flow :incoming-window #x7FFFFFFF
				     :next-outgoing-id 1
				     :outgoing-window 0
				     :handle (~ link 'handle)
				     :link-credit #x64))
    ;; we don't have to receive flow here
    (k link))

  (define (detach-amqp-link! link :key error)
    (define (free-handle handle map) (vector-set! map handle #f))
    ;; TODO should we raise an error if the link is already detached?
    (unless (eq? (~ link 'state) :detached)
      (let ((detach (make-amqp-detach :handle (~ link 'handle) :closed #t
				      :error error))
	    (conn (~ link 'session 'connection)))
	(send-frame conn detach)
	(free-handle (~ link 'handle) (~ link 'session 'local-handles))
	;; receive detach
	;; At least ActiveMQ, it just sends all pending messages.
	;; So even when we send detach message however there might
	;; be some other messages in the socket. What we can is 
	;; that discard them until we receive response of detach.
	(let loop ()
	  (let-values (((ext detach) (recv-frame conn)))
	    (if (is-a? detach <amqp-detach>)
		(free-handle (~ detach 'handle) 
			     (~ link 'session 'remote-handles))
		(loop))))
	(set! (~ link 'state) :detached)))
    link)

  ;; transfer
  ;; state will be resolved by option
  (define (send-transfer link message-format message 
			 disposition-handler . opt)
    (define session (~ link 'session))
    (define connection (~ session 'connection))
    ;; upto 32 octed = 256 bits
    (define delivery-tag (read-sys-random 32))
    (define first-delivery-id (bytevector->integer (read-sys-random 32)))
    (define offset 0)
    (define (make-transfer message . opt)
      ;; id must be unique per message
      (let1 id (+ first-delivery-id offset)
	(set! offset (+ offset 1))
	(apply make-amqp-transfer :handle (~ link 'handle)
	       :delivery-id id
	       :delivery-tag delivery-tag
	       :message-format message-format
	       :settled #f
	       opt)))
    (define (make-disposition settled? state)
      (make-amqp-disposition 
       :role (is-a? link <amqp-receiver-link>)
       :first first-delivery-id
       :last first-delivery-id ;; for now we don't support this yet
       :settled settled?
       :state state))
    (define (handle-disposition link)
      (let-values (((ext disp) (recv-frame connection)))
	(if (is-a? disp <amqp-flow>)
	    (handle-disposition (sender-flow-control link disp))
	    ;; TODO disposition contains first and last
	    ;; I think we should check it...
	    (let1 sender-disp (disposition-handler disp make-disposition)
	      (send-frame connection sender-disp)
	      (if (~ sender-disp 'settled)
		  link
		  ;; TODO send the rest message
		  (handle-disposition link))))))
    (let ((frame-size (~ connection 'frame-size)))
      ;; can be sen in one go
      (if (< (bytevector-length message) (- frame-size 512))
	  (let1 transfer (make-transfer message :more #f)
	    (send-frame connection transfer message)
	    (handle-disposition link))
	  (error 'send-transfer "not supported yet"))))

  ;; FIXME this is too naive, even doesn't do proper flow control.
  ;; Receive one set of transfers. The broker may send more but
  ;; that will be handled receiver tries to retrieve more.
  ;; TODO consider +amqp-second+ option.
  (define (recv-transfer link disposition-handler :key (query #f))
    (define session (~ link 'session))
    (define connection (~ session 'connection))
    ;; TODO we may not want to create buffer port when just discarding
    (define (read-transfer)
      (let-values (((out extract) (open-bytevector-output-port)))
	(let loop ((first-id #f))
	  (let-values (((ext transfer payload)
			(recv-frame&payload connection)))
	    (put-bytevector out payload)
	    (if (~ transfer 'more)
		(loop (or first-id (~ transfer 'delivery-id)))
		(values (or first-id (~ transfer 'delivery-id))
			(~ transfer 'delivery-id)
			(extract)))))))
    (define (send-disposition first-id last-id type)
      (define (make-disposition settled? state)
	(make-amqp-disposition 
	 :role #t
	 :first first-id
	 :last last-id
	 :settled (eq? type 'accepted)
	 :state state))
      ;; send disposition
      (send-frame connection (disposition-handler type make-disposition)))
    (let loop ()
      (let-values (((first last value) (read-transfer)))
	(cond ((is-a? value <amqp-flow>)
	       ;; may be previous transfer's <flow> so handle it
	       ;; TODO flow control
	       (flow-control link (lambda (link) (loop))))
	      ((or (not query) (query value))
	       (send-disposition first last 'accepted)
	       value)
	      (else (send-disposition first last 'released) (loop))))))
	
  )
