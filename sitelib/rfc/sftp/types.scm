;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/sftp/types.scm - SFTP protocol types.
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
(library (rfc sftp types)
    (export sftp-class-lookup
	    sftp-type-lookup
	    +sftp-version3+

	    ;; flags for open
	    +ssh-fxf-read+
	    +ssh-fxf-write+
	    +ssh-fxf-append+
	    +ssh-fxf-creat+
	    +ssh-fxf-trunc+
	    +ssh-fxf-excl+
	    
	    ;; messages
	    <sftp-fxp-init>
	    <sftp-fxp-version>
	    <sftp-fxp-open>
	    <sftp-fxp-close>
	    <sftp-fxp-read>
	    <sftp-fxp-write>
	    <sftp-fxp-remove>
	    <sftp-fxp-rename>
	    <sftp-fxp-mkdir>
	    <sftp-fxp-rmdir>
	    <sftp-fxp-opendir>
	    <sftp-fxp-readdir>
	    <sftp-fxp-stat>
	    <sftp-fxp-lstat>
	    <sftp-fxp-fstat>
	    <sftp-fxp-setstat>
	    <sftp-fxp-fsetstat>
	    <sftp-fxp-readlink>
	    <sftp-fxp-symlink>
	    <sftp-fxp-realpath>
	    ;; from server
	    <sftp-fxp-status>
	    <sftp-fxp-handle>
	    <sftp-fxp-data>
	    <sftp-fxp-name>
	    <sftp-fxp-attrs>
	    ;; misc
	    ;; may need this?
	    <sftp-attrs>
	    )
    (import (rnrs) 
	    (sagittarius)
	    (sagittarius object) 
	    (sagittarius control)
	    (rfc ssh)
	    (rfc sftp constants)
	    (clos user)
	    (clos core)
	    (binary data) 
	    (binary pack))


;; sftp helper
;; handling raw channel data
;; channel data is mere bytevector

;; this is not in ssh
(define-method write-message ((type (eql :uint64)) o out array-size?)
  (if array-size?
      (dotimes (i array-size?)
	(put-bytevector out (pack "!Q" (vector-ref o i))))
      (put-bytevector out (pack "!Q" o))))
(define-method read-message ((t (eql :uint64)) in array-size?)
  (if array-size?
      (let1 v (make-vector array-size?)
	(dotimes (i array-size? v)
	  (vector-set! v i (get-unpack in "!Q"))))
      (get-unpack in "!Q")))

(define *type-table* (make-eqv-hashtable))
(define *class-table* (make-eq-hashtable))
(define (register! type name) 
  (hashtable-set! *type-table* type name)
  (hashtable-set! *class-table* name type))
(define (sftp-class-lookup type) (hashtable-ref *type-table* type #f))
(define (sftp-type-lookup class) (hashtable-ref *class-table* class #f))

(define-syntax define-sftp-type
  (syntax-rules ()
    ((_ name type slots reader writer)
     (begin
       (define-ssh-type name (<ssh-type>) slots reader writer)
       (define dummy (let () (register! type name)))))))
(define-syntax define-sftp-message
  (syntax-rules ()
    ((_ name type slots)
     (define-sftp-message name type slots :parents (<ssh-message>)))
    ((_ name type slots :parents (p ...))
     (begin
       (define-ssh-message name (p ...) slots)
       (define dummy (let () (register! type name)))))))

;; non sftp types (no label but defined in sftp memo)
(define (read-sftp-extension in)
  (let* ((type (read-message :string in #f))
	 (data (read-message :string in #f)))
    (values type data)))
(define (read-sftp-extensions in)
  (let loop ((r '()))
      (if (eof-object? (lookahead-u8 in))
	  (reverse! r)
	  (let-values (((type data) (read-sftp-extension in)))
	    (loop (acons type data r))))))

(define (write-sftp-extensions out extended)
  (for-each (lambda (p)
	      (write-message :string (car p) out #f)
	      (write-message :string (cdr p) out #f))
	    extended))

(define-ssh-type <sftp-extensions> (<ssh-type>)
  ((extensions '()))
  read-sftp-extensions
  write-sftp-extensions)
(define empty-extensions (make <sftp-extensions>))

(define-ssh-type <sftp-attrs> (<ssh-type>)
  ((size  #f)
   (uid   #f)
   (gid   #f)
   (permissions #f)
   (atime #f)
   (mtime #f)
   (extended #f))
  (lambda (in)
    (let ((flags (get-u32 in (endianness big))))
      (define (check f) (not (zero? (bitwise-and flags f))))
      (define (read-extended in)
	(let ((count (get-u32 in (endianness big))))
	  (let loop ((i 0) (r '()))
	    (if (= i count)
		(reverse! r)
		(let-values (((data type) (read-sftp-extension in)))
		  (loop (+ i 1) (acons type data r)))))))
      ;; do sequensially
      (let*-values (((size) (and (check +ssh-filexfer-attr-size+)
				 (get-u64 in (endianness big))))
		    ((uid gid) (if (check +ssh-filexfer-attr-uidgid+)
				   (get-unpack in "!LL")
				   (values #f #f)))
		    ((perm) (and (check +ssh-filexfer-attr-permissions+)
				 (get-u32 in (endianness big))))
		    ((atime mtime) (if (check +ssh-filexfer-attr-acmodtime+)
				       (get-unpack in "!LL")
				       (values #f #f)))
		    ((extended) (and (check +ssh-filexfer-attr-extended+)
				     (read-extended in))))
	(values size uid gid perm atime mtime extended))))
  (lambda (out size uid gid perm atime mtime extended)
    (define (write-extended out extended)
      (put-u32 out (length extended) (endianness big))
      (write-sftp-extensions out extended))
    (let ((flags (bitwise-ior 
		  (or (and size +ssh-filexfer-attr-size+) 0)
		  (or (and uid gid +ssh-filexfer-attr-uidgid+) 0)
		  (or (and perm +ssh-filexfer-attr-permissions+) 0)
		  (or (and atime mtime +ssh-filexfer-attr-acmodtime+) 0)
		  (or (and extended +ssh-filexfer-attr-extended+) 0))))
      (put-u32 out flags (endianness big))
      (and size (put-u64 out size (endianness big)))
      (and uid  (put-u32 out uid (endianness big)))
      (and gid  (put-u32 out gid (endianness big)))
      (and perm (put-u32 out perm (endianness big)))
      (and atime (put-u32 out atime (endianness big)))
      (and mtime (put-u32 out mtime (endianness big)))
      (and extended (write-extended out extended)))))
(define empty-attrs (make <sftp-attrs>))

;; types
;; supers
(define-ssh-message <sftp-version> (<ssh-message>)
  ((version :uint32)
   (extension-data <sftp-extensions> empty-extensions)))
(define-ssh-message <sftp-has-id> (<ssh-message>)
  ((id :uint32)))
(define-ssh-message <sftp-has-handle> (<sftp-has-id>)
  ((handle :string)))
(define-ssh-message <sftp-has-filename> (<sftp-has-id>)
  ((filename :string)))
(define-ssh-message <sftp-has-path> (<sftp-has-id>)
  ((path :string)))
(define-ssh-message <sftp-has-offset> (<sftp-has-handle>)
  ((offset :uint64)))

(define-ssh-message <sftp-has-path&attr> (<sftp-has-path>)
  ((attr <sftp-attrs> empty-attrs)))

;; 4. Protocol Initialization
(define-sftp-message <sftp-fxp-init> +ssh-fxp-init+ ()
  :parents (<sftp-version>))
(define-sftp-message <sftp-fxp-version> +ssh-fxp-version+ ()
  :parents (<sftp-version>))

;; 6.3 Opening, Creating, and Closing Files
(define-sftp-message <sftp-fxp-open> +ssh-fxp-open+
  ((pflags :uint32 0)
   (attrs <sftp-attrs> empty-attrs))
  :parents (<sftp-has-filename>))
(define-sftp-message <sftp-fxp-close> +ssh-fxp-close+ ()
  :parents (<sftp-has-handle>))

;; 6.4 Reading and Writing
(define-sftp-message <sftp-fxp-read> +ssh-fxp-read+ 
  ((len :uint32))
  :parents (<sftp-has-offset>))
(define-sftp-message <sftp-fxp-write> +ssh-fxp-write+ 
  ((data :string))
  :parents (<sftp-has-offset>))

;; 6.5 Removing and Renaming Files
(define-sftp-message <sftp-fxp-remove> +ssh-fxp-remove+ ()
  :parents (<sftp-has-filename>))
(define-sftp-message <sftp-fxp-rename> +ssh-fxp-rename+ 
  ((oldpath :string)
   (newpath :string))
  :parents (<sftp-has-id>))

;; 6.6 Creating and Deleting Directories
(define-sftp-message <sftp-fxp-mkdir> +ssh-fxp-mkdir+ ()
  :parents (<sftp-has-path&attr>))
(define-sftp-message <sftp-fxp-rmdir> +ssh-fxp-rmdir+ ()
  :parents (<sftp-has-path>))

;; 6.7 Scanning Directories
(define-sftp-message <sftp-fxp-opendir> +ssh-fxp-opendir+ ()
  :parents (<sftp-has-path>))
(define-sftp-message <sftp-fxp-readdir> +ssh-fxp-readdir+ ()
  :parents (<sftp-has-handle>))

;; 6.8 Retrieving File Attributes
(define-sftp-message <sftp-fxp-stat> +ssh-fxp-stat+ ()
  :parents (<sftp-has-path>))
(define-sftp-message <sftp-fxp-lstat> +ssh-fxp-lstat+ ()
  :parents (<sftp-has-path>))
(define-sftp-message <sftp-fxp-fstat> +ssh-fxp-fstat+ ()
  :parents (<sftp-has-handle>))

;; 6.9 Setting File Attributes
(define-sftp-message <sftp-fxp-setstat> +ssh-fxp-setstat+ ()
  :parents (<sftp-has-path&attr>))
(define-sftp-message <sftp-fxp-fsetstat> +ssh-fxp-fsetstat+ 
  ((attr <sftp-attrs> empty-attrs))
  :parents (<sftp-has-handle>))

;; 6.10 Dealing with Symbolic links
(define-sftp-message <sftp-fxp-readlink> +ssh-fxp-readlink+ ()
  :parents (<sftp-has-path>))
(define-sftp-message <sftp-fxp-symlink> +ssh-fxp-symlink+ 
  ((linkpath :string)
   (targetpath :string))
  :parents (<sftp-has-id>))

;; 6.11 Canonicalizing the Server-Side Path Name
(define-sftp-message <sftp-fxp-realpath> +ssh-fxp-realpath+ ()
  :parents (<sftp-has-path>))

;; 7. Responses from the Server to the Client
(define-sftp-message <sftp-fxp-status> +ssh-fxp-status+
  ((code :uint32)    ;; error/status code
   (message :string) ;; error message (utf-8)
   (language-tag :string))
  :parents (<sftp-has-id>))
(define-sftp-message <sftp-fxp-handle> +ssh-fxp-handle+ ()
  :parents (<sftp-has-handle>))
(define-sftp-message <sftp-fxp-data> +ssh-fxp-data+ 
  ((data :string))
  :parents (<sftp-has-id>))

;; helper
(define-class <sftp-name> ()
  ((filename :init-keyword :filename)
   (longname :init-keyword :longname)
   (attributes :init-keyword :attributes)))
(define-method write-object ((o <sftp-name>) out)
  (format out "#<sftp-name ~s>" (~ o 'filename)))

(define-ssh-type <sftp-names> (<ssh-type>)
  ((names '()))
  (lambda (in)
    (define (read-sftp-names in) 
      (let-values (((filename longname) (read-sftp-extension in)))
	(make <sftp-name> 
	  :filename (utf8->string filename) 
	  :longname (utf8->string longname)
	  :attributes (read-message <sftp-attrs> in))))
    (let ((c (get-unpack in "!L")))
      (let loop ((i 0) (r '()))
	(if (= i c)
	    (reverse! r)
	    (loop (+ i 1) (cons (read-sftp-names in) r))))))
  (lambda (out names)
    (put-u32 out (length names) (endianness big))
    (for-each (lambda (fla)
		(write-message :string out (~ fla 'filename))
		(write-message :string out (~ fla 'longname))
		(write-message <sftp-attrs> out (~ fla 'attributes)))
	      names)))
(define-sftp-message <sftp-fxp-name> +ssh-fxp-name+ 
  ((names <sftp-names>))
  :parents (<sftp-has-id>))
(define-sftp-message <sftp-fxp-attrs> +ssh-fxp-attrs+
  ((id    :uint32)
   (attrs <sftp-attrs> empty-attrs)))

;; 8. Vendor-Specific Extensions
;; TODO we don't handle this for now
(define-sftp-message <sftp-fxp-extended> +ssh-fxp-extended+
  ((extended-request :string))
  :parents (<sftp-has-id>))
(define-sftp-message <sftp-fxp-extended-reply> +ssh-fxp-extended-reply+ ()
  :parents (<sftp-has-id>))
)
