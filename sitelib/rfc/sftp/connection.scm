;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/sftp/connection.scm - SFTP protocol types.
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

;; reference
;;  http://tools.ietf.org/html/draft-ietf-secsh-filexfer-02
#!nounbound
(library (rfc sftp connection)
    (export make-client-sftp-connection
	    open-client-sftp-connection!
	    close-client-sftp-connection!
	    sftp-password-authentication
	    sftp-keyboard-interactive-authentication
	    sftp-public-key-authentication
	    call-with-sftp-connection
	    
	    ;; operations
	    sftp-open
	    sftp-close
	    sftp-read
	    sftp-write!
	    sftp-remove!
	    sftp-rename!
	    sftp-mkdir!
	    sftp-rmdir!
	    sftp-opendir
	    sftp-readdir
	    sftp-stat
	    sftp-lstat
	    sftp-fstat
	    sftp-setstat!
	    sftp-fsetstat!
	    sftp-readlink
	    sftp-symlink!
	    sftp-realpath
	    ;; for convenience
	    sftp-exists?

	    ;; utility
	    sftp-readdir-as-filenames
	    sftp-readdir-as-longnames

	    ;; helper
	    sftp-binary-receiver
	    sftp-file-receiver
	    sftp-oport-receiver

	    ;; low level
	    send-sftp-packet
	    recv-sftp-packet)
    (import (rnrs) 
	    (clos user) 
	    (clos core)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control)
	    (rfc ssh) 
	    (rfc sftp types)
	    (rfc sftp constants)
	    (binary data)
	    (binary pack)
	    (binary io)
	    (util port)
	    (util bytevector)
	    (srfi :26))

(define-class <sftp-connection> ()
  ((message-id :init-value 0)
   (transport :init-keyword :transport)
   (channel :init-keyword :channel)))

(define (read-sftp-packet-data type in)
  (ssh-read-message (sftp-class-lookup type) in))

(define (recv-sftp-packet connection)
  (define channel (~ connection 'channel))
  (define (receive-rest first size)
    (define in/out (open-chunked-binary-input/output-port))
    (define (finish in/out)
      (set-port-position! in/out 0)
      in/out)
    (put-bytevector in/out first 5)
    (let loop ((read-size (- (bytevector-length first) 5)))
      (if (>= read-size size)
	  (finish in/out)
	  (let1 next (ssh-recv-channel-data channel)
	    (put-bytevector in/out next)
	    (loop (+ read-size (bytevector-length next)))))))
  ;; packet format
  ;;   uint32          length
  ;;   byte            type
  ;;   byte[length-1]  data payload
  ;; so first 5 octets are not needed
  (let*-values (((first) (ssh-recv-channel-data channel))
		((len type) (unpack "!LC" first)))
    ;; skip length and type bytes
    (let1 in (receive-rest first (- len 1))
      (read-sftp-packet-data type in))))

(define (send-sftp-packet connection data)
  (let ((bv (ssh-message->bytevector data))
	(type (sftp-type-lookup (class-of data))))
    (unless type (error 'send-sftp-packet "unknown sftp packet type" data))
    (ssh-send-channel-data 
     (~ connection 'channel)
     (call-with-bytevector-output-port
      (lambda (out)
	(put-u32 out (+ (bytevector-length bv) 1) (endianness big))
	(put-u8 out type)
	(put-bytevector out bv))))))

(define (make-client-sftp-connection server port)
  (make <sftp-connection>
    :transport (open-client-ssh-transport!
		(make-client-ssh-transport server port))))

(define (sftp-password-authentication username password)
  (lambda (transport)
    (ssh-authenticate transport +ssh-auth-method-password+ username password)))
(define (sftp-keyboard-interactive-authentication username . opts)
  (lambda (transport)
    (apply ssh-authenticate transport +ssh-auth-method-keyboard-interactive+
	   username opts)))
(define (sftp-public-key-authentication username private-key public-key)
  (lambda (transport)
    (ssh-authenticate transport +ssh-auth-method-public-key+
		      username private-key public-key)))

(define (open-client-sftp-connection! connection :key (authenticate #f))
  (define transport (~ connection 'transport))
  
  (when authenticate (authenticate transport))
  
  (let ((channel (open-client-ssh-session-channel transport)))
    (set! (~ connection 'channel) channel)
    (ssh-request-subsystem channel "sftp")
    ;; no extension data for now
    (send-sftp-packet connection
      (make <sftp-fxp-init> :version +sftp-version3+))
    (let1 r (recv-sftp-packet connection)
      ;; assume rest is empty
      (unless (and (is-a? r <sftp-fxp-version>)
		   (= (~ r 'version) +sftp-version3+))
	(error 'open-client-sftp-connection!
	       "server respond non supported version" r))
      connection)))

(define (close-client-sftp-connection! connection)
  (close-ssh-channel (~ connection 'channel))
  (set! (~ connection 'channel) #f)
  (close-client-ssh-transport! (~ connection 'transport)))

(define (call-with-sftp-connection server port proc
				   ;; keep it backward compatible
				   ;; at least this layer
				   :key (username #f) (password #f)
				   :allow-other-keys opts)
  (define conn (make-client-sftp-connection server port))
  
  (let ((conn (if (and username password)
		  (let1 auth (sftp-password-authentication username password)
		    (open-client-sftp-connection! conn :authenticate auth))
		  (apply open-client-sftp-connection! conn opts))))
    (unwind-protect (proc conn)
      (close-client-sftp-connection! conn))))

(define (sftp-message-id! conn)
  (rlet1 id (~ conn 'message-id)
    (set! (~ conn 'message-id) (+ id 1))))
(define-syntax check-status
  (syntax-rules ()
    ((_ v)
     (when (and (is-a? v <sftp-fxp-status>)
		(not (= (~ v 'code) +ssh-fx-ok+)))
       ;; todo condition?
       (error #f (format "~a, code[~a]" (~ v 'message) (~ v 'code)))))))
;; with check
(define (recv-sftp-packet1 conn)
  (let1 r (recv-sftp-packet conn)
    (check-status r)
    r))

;;; high level APIs
;; 6.3 Opening, Creating, and Closing Files

(define (sftp-open conn filename pflags)
  (let1 id (sftp-message-id! conn)
    ;; for now don't consider attrs
    (send-sftp-packet conn (make <sftp-fxp-open> :id id :filename filename
				 :pflags pflags))
    (recv-sftp-packet1 conn)))

;; we don't check if the handle is closed or not but
;; as long as server allow us we don't raise any error.
(define (sftp-close conn handle)
  (send-sftp-packet conn (make <sftp-fxp-close> :id (sftp-message-id! conn)
			       :handle (~ handle 'handle)))
  ;; may fail so we don't check
  (recv-sftp-packet conn)
  #t)

;; 6.4 Reading and Writing

;; Reading is not a problem to specify big buffer size server
;; can send back its maximum if the specified size is more than
;; server maximum size. so we use 1MB
(define-constant +sftp-default-buffer-size+ 1048576) ;; (* 1024 1024)
(define (sftp-read conn handle/filename receiver
		   :key (buffer-size +sftp-default-buffer-size+)
			(offset 0))
  ;; structure of the message
  ;;   uint32 	length 4
  ;;   byte   	type   1
  ;;   uint32 	id     4
  ;;   string   handle 4 + n
  ;;   uint64   offset 8
  ;;   uint32   len    4
  (let* ((ohandle (if (is-a? handle/filename <sftp-fxp-handle>) 
		      handle/filename
		      (sftp-open conn handle/filename +ssh-fxf-read+)))
	 (handle (~ ohandle 'handle))
	 (n (bytevector-length handle))
	 (offset-offset (+ 13 n))
	 (buf-size (+ 25 n))
	 (len (- buffer-size offset-offset 8))
	 (buffer (make-bytevector buf-size)))
    (bytevector-u32-set! buffer 0 (- buf-size 4) (endianness big))
    (bytevector-u8-set! buffer 4 +ssh-fxp-read+)
    (bytevector-u32-set! buffer 9 n (endianness big))
    (bytevector-copy! handle 0 buffer 13 n)
    (bytevector-u32-set! buffer (+ offset-offset 8) len (endianness big))
    ;; ok now read it
    ;; read may be multiple part so we need to read it until
    ;; server respond <sftp-fxp-status>
    (let loop ((offset offset))
      (bytevector-u32-set! buffer 5 (sftp-message-id! conn) (endianness big))
      (bytevector-u64-set! buffer offset-offset offset (endianness big))
      (ssh-send-channel-data (~ conn 'channel) buffer)
      (let1 r (recv-sftp-packet conn)
	(if (is-a? r <sftp-fxp-status>)
	    (begin 
	      (unless (is-a? handle/filename <sftp-fxp-handle>)
		(sftp-close conn ohandle))
	      (let-values (((ignore r) (receiver -1 #f)))
		r))
	    ;; must be <sftp-fxp-data>
	    (let-values (((n ignore) (receiver offset (~ r 'data))))
	      (loop (+ offset n))))))))
(define (sftp-binary-receiver)
  (let-values (((out extract) (open-bytevector-output-port)))
    (lambda (offset data)
      (if (< offset 0)
	  (values -1 (extract))
	  (values (copy-binary-port out data) #f)))))
(define (sftp-file-receiver filename :key (options (file-options)))
  ;; by default we allocate 1MB buffer so it's more than internal
  ;; port buffer size. in this case without buffer is faster.
  (let1 out (open-file-output-port filename options 'none #f)
    (lambda (offset data)
      (if (< offset 0)
	  (values -1 (close-port out))
	  (values (copy-binary-port out data) #f)))))
(define (sftp-oport-receiver out)
  (unless (binary-port? out)
    (assertion-violation 'sftp-oport-receiver "binary port required" out))
  (lambda (offset data)
    (if (< offset 0)
	(values -1 (undefined))
	(values (copy-binary-port out data) #f))))

;; Writing size on the other hand, we can't control and there is
;; no way to know how much server can actually handle. So we use
;; a bit less than the reading buffer size. To make it safer, we
;; should use 32768 (3KB) buffer which is specified on RFC as a
;; SFTP packet's minimum size. As far as I tested (for now only
;; 2 server implementations though), none of server couldn't handle
;; 100KB packet. so for performance perspective we use this as the
;; default size.
(define (sftp-write! conn handle inport 
		     :key (buffer-size (* 1024 128))
			  (offset 0))
  (unless (is-a? handle <sftp-fxp-handle>)
    (error 'sftp-write! "need <sftp-fxp-handle>, call sftp-open first!"))
  ;; for optimisation, we use the raw structure of SFTP_FXP_WRITE.
  ;;     uint32     length     - 0
  ;;     byte       type       - 4
  ;;     uint32     id         - 5
  ;;     string     handle     - 9...n, u32 + data
  ;;      u32                  - 9
  ;;      byte[]               - 13
  ;;     uint64     offset     - 13 + n
  ;;     string     data       - 21...m, u32 + m
  ;;      u32                  - 21 + n
  ;;      byte[]               - 25 + n
  ;; We always send entire buffer, which means, the first 5 bytes
  ;; are fixed
  (let* ((hndl (~ handle 'handle))
	 (n (bytevector-length hndl))
	 (offset-offset (+ 13 n))
	 (datalen-offset (+ offset-offset 21))
	 (data-offset (+ datalen-offset 4))
	 (read-size (- buffer-size data-offset))
	 (buf  (make-bytevector buffer-size 0)))
    ;; prepare buffer
    (bytevector-u32-set! buf 0 (- buffer-size 4) (endianness big))
    (bytevector-u8-set! buf 4 +ssh-fxp-write+)
    (bytevector-u32-set! buf 9 n (endianness big)) ;; handle length
    (bytevector-copy! hndl 0 buf 13 n)
    ;; ok now read it
    ;; read may be multiple part so we need to read it until
    ;; server respond <sftp-fxp-status>
    (let loop ((offset offset) (i 0))
      (let ((r (get-bytevector-n! inport buf data-offset read-size)))
	(unless (eof-object? r)
	  ;; set ID
	  (bytevector-u32-set! buf 5 (sftp-message-id! conn) (endianness big))
	  ;; set offset
	  (bytevector-u64-set! buf offset-offset offset (endianness big))
	  (bytevector-u32-set! buf datalen-offset r (endianness big))
	  (ssh-send-channel-data (~ conn 'channel) buf)
	  (recv-sftp-packet1 conn)
	  (when (= r read-size) (loop (+ offset r) (+ i 1))))))))

;; 6.5 Removing and Renaming Files

(define (sftp-remove! conn filename)
  (send-sftp-packet conn (make <sftp-fxp-remove> :id (sftp-message-id! conn)
			       :filename filename))
  (recv-sftp-packet1 conn))

(define (sftp-rename! conn oldpath newpath)
  (send-sftp-packet conn (make <sftp-fxp-rename> :id (sftp-message-id! conn)
			       :oldpath oldpath :newpath newpath))
  (recv-sftp-packet1 conn))

;; 6.6 Creating and Deleting Directories
(define (sftp-mkdir! conn path . opts)
  (let1 attrs (apply make <sftp-attrs> opts)
    (send-sftp-packet conn (make <sftp-fxp-mkdir> :id (sftp-message-id! conn)
				 :path path :attrs attrs))
    (recv-sftp-packet1 conn)))

(define (sftp-rmdir! conn path . opts)
  (send-sftp-packet conn (make <sftp-fxp-rmdir> :id (sftp-message-id! conn)
				 :path path))
  (recv-sftp-packet1 conn))


;; 6.7 Scanning Directories
(define (sftp-opendir conn path)
  (let1 id (sftp-message-id! conn)
    (send-sftp-packet conn (make <sftp-fxp-opendir> :id id :path path))
    (recv-sftp-packet1 conn)))
(define (sftp-readdir conn handle/path)
  (let1 handle (if (is-a? handle/path <sftp-fxp-handle>) 
		   handle/path
		   (sftp-opendir conn handle/path))
    (send-sftp-packet conn (make <sftp-fxp-readdir>
			     :id (sftp-message-id! conn)
			     :handle (~ handle 'handle)))
    (let1 r (recv-sftp-packet1 conn)
      (unless (is-a? handle/path <sftp-fxp-handle>)
	(sftp-close conn handle))
      ;; no reason to return sftp-fxp-name anyway
      (~ r 'names 'names))))
;; strip off to mere string
(define (sftp-readdir-as-filenames conn handle/path)
  (let1 r (sftp-readdir conn handle/path)
    (map (cut ~ <> 'filename) r)))
(define (sftp-readdir-as-longnames conn handle/path)
  (let1 r (sftp-readdir conn handle/path)
    (map (cut ~ <> 'longname) r)))

;; 6.8 Retrieving File Attributes
(define (sftp-stat conn path)
  (send-sftp-packet conn (make <sftp-fxp-stat> :id (sftp-message-id! conn)
			       :path path))
  (recv-sftp-packet1 conn))
(define (sftp-lstat conn path)
  (send-sftp-packet conn (make <sftp-fxp-lstat> :id (sftp-message-id! conn)
			       :path path))
  (recv-sftp-packet1 conn))
;; it's users responsibility to make sure it's a handle
(define (sftp-fstat conn handle)
  (send-sftp-packet conn (make <sftp-fxp-fstat> :id (sftp-message-id! conn)
			       :handle (~ handle 'handle)))
  (recv-sftp-packet1 conn))

;; 6.9 Setting File Attributes
(define (sftp-setstat! conn path . opts)
  (send-sftp-packet conn (make <sftp-fxp-setstat> :id (sftp-message-id! conn)
			       :path path :attrs (apply make <sftp-attrs> opts)))
  (recv-sftp-packet1 conn))
(define (sftp-fsetstat! conn handle . opts)
  (send-sftp-packet conn (make <sftp-fxp-fsetstat> :id (sftp-message-id! conn)
			       :handle (~ handle 'handle)
			       :attrs (apply make <sftp-attrs> opts)))
  (recv-sftp-packet1 conn))

;; 6.10 Dealing with Symbolic links
(define (sftp-readlink conn path)
  (send-sftp-packet conn (make <sftp-fxp-readlink> :id (sftp-message-id! conn)
			       :path path))
  (let1 r (recv-sftp-packet1 conn)
    (~ (car (~ r 'names 'names)) 'filename)))
(define (sftp-symlink! conn linkpath targetpath)
  (send-sftp-packet conn (make <sftp-fxp-symlink> :id (sftp-message-id! conn)
			       :linkpath linkpath :targetpath targetpath))
  (recv-sftp-packet1 conn))

;; 6.11 Canonicalizing the Server-Side Path Name
(define (sftp-realpath conn path)
  (send-sftp-packet conn (make <sftp-fxp-realpath> :id (sftp-message-id! conn)
			       :path path))
  (let1 r (recv-sftp-packet1 conn)
    (~ (car (~ r 'names 'names)) 'filename)))

;; for convenience
(define (sftp-exists? conn path)
  (send-sftp-packet conn (make <sftp-fxp-stat> :id (sftp-message-id! conn)
			       :path path))
  ;; must return status
  (let1 r (recv-sftp-packet conn)
    ;; simple isn't it?
    (is-a? r <sftp-fxp-attrs>)))

)
