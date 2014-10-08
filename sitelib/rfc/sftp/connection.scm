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
(library (rfc sftp connection)
    (export make-client-sftp-connection
	    sftp-close-connection
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
  ;; TODO use chunked port...
  (define (receive-rest first size)
    (define (finish bv)
      (define (read-as-chunk size)
	(list->vector (align-bytevectors bv size)))
      (->chunked-binary-input-port read-as-chunk))
    (let loop ((read-size (bytevector-length first)) (bv (list first)))
      (if (>= read-size size)
	  (finish (reverse! bv))
	  (let1 next (ssh-recv-channel-data channel)
	    (loop (+ read-size (bytevector-length next))
		  (cons next bv))))))
  ;; packet format
  ;;   uint32          length
  ;;   byte            type
  ;;   byte[length-1]  data payload
  ;; so first 5 octets are not needed
  (let*-values (((first) (ssh-recv-channel-data channel))
		((len type) (unpack "!LC" first)))
    ;; skip length and type bytes
    (let1 in (receive-rest (bytevector-copy first 5) (- len 1))
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

(define (send-sftp-packet-using-port connection data)
  (let ((in/out (open-chunked-binary-input/output-port))
	(type (sftp-type-lookup (class-of data))))
    (unless type (error 'send-sftp-packet "unknown sftp packet type" data))
    ;;(put-u32 in/out (+ (bytevector-length bv) 1) (endianness big))
    ;;(put-u8 in/out type)
    (set-port-position! in/out 5)
    (write-message (class-of data) data in/out)
    (let ((p (port-position in/out)))
      (set-port-position! in/out 0)
      (put-u32 in/out (- p 4) (endianness big))
      (put-u8 in/out type))
    (set-port-position! in/out 0)
    (ssh-send-channel-data (~ connection 'channel) in/out)
    (close-port in/out)))

(define (make-client-sftp-connection server port 
				     :key (username #f) (password #f))
  (let ((transport (make-client-ssh-transport server port)))
    ;; TODO supporting other authentication method 
    (when (and username password)
      (ssh-authenticate transport +ssh-auth-method-password+ username password))
    ;; TODO handle error to disconnect!
    (let* ((channel (open-client-ssh-session-channel transport))
	   (connection (make <sftp-connection> :transport transport 
			     :channel channel)))
      (ssh-request-subsystem channel "sftp")
      ;; no extension data for now
      (send-sftp-packet connection 
			(make <sftp-fxp-init> :version +sftp-version3+))
      (let1 r (recv-sftp-packet connection)
	;; assume rest is empty
	(unless (and (is-a? r <sftp-fxp-version>)
		     (= (~ r 'version) +sftp-version3+))
	  (error 'make-client-ssh-transport 
		 "server respond non supported version" r))
	connection))))
(define (sftp-close-connection connection)
  (close-ssh-channel (~ connection 'channel))
  (ssh-disconnect (~ connection 'transport)))

(define (call-with-sftp-connection server port proc . opts)
  (let ((conn (apply make-client-sftp-connection server port opts)))
    (unwind-protect (proc conn)
      (sftp-close-connection conn))))

(define (sftp-message-id! conn)
  (rlet1 id (~ conn 'message-id)
    (set! (~ conn 'message-id) (+ id 1))))
(define-syntax check-status
  (syntax-rules ()
    ((_ v)
     (when (and (is-a? v <sftp-fxp-status>)
		(not (= (~ v 'code) +ssh-fx-ok+)))
       ;; todo condition?
       (error #f (format "~a, code[~a]" (utf8->string (~ v 'message))
			 (~ v 'code)))))))
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
  (let* ((ohandle (if (is-a? handle/filename <sftp-fxp-handle>) 
		      handle/filename
		      (sftp-open conn handle/filename +ssh-fxf-read+)))
	 (handle (~ ohandle 'handle)))
    ;; ok now read it
    ;; read may be multiple part so we need to read it until
    ;; server respond <sftp-fxp-status>
    (let loop ((offset offset))
      (send-sftp-packet conn (make <sftp-fxp-read>
			       :id (sftp-message-id! conn)
			       :handle handle
			       :offset offset
			       :len buffer-size))
      (let1 r (recv-sftp-packet conn)
	(if (is-a? r <sftp-fxp-status>)
	    (begin 
	      (unless (is-a? handle/filename <sftp-fxp-handle>)
		(sftp-close conn ohandle))
	      (let-values (((ignore r)
			    (receiver -1 (open-bytevector-input-port #vu8()))))
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
  ;; by default we allocate 10MB buffer so it's more than internal
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
  (let ((hndl (~ handle 'handle))
	(buf  (make-bytevector buffer-size)))
    ;; ok now read it
    ;; read may be multiple part so we need to read it until
    ;; server respond <sftp-fxp-status>
    (let loop ((offset offset))
      (let ((r (get-bytevector-n! inport buf 0 buffer-size)))
	(unless (eof-object? r)
	  (send-sftp-packet-using-port
	   conn (make <sftp-fxp-write>
		  :id (sftp-message-id! conn)
		  :handle hndl
		  :offset offset
		  :data (if (= r buffer-size)
			    buf
			    (bytevector-copy buf 0 r))))
	  (recv-sftp-packet1 conn)
	  (loop (+ offset r)))))))

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
