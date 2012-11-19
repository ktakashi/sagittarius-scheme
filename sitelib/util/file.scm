;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; file.scm - file utility
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; The API's names are from Gauche
#!read-macro=sagittarius/regex
(library (util file)
    (export file->list
	    file->string
	    file->sexp-list
	    file->string-list
	    decompose-path
	    path-extension
	    path-sans-extension
	    
	    temporary-directory
	    make-temporary-file

	    find-files

	    path-for-each path-map
	    delete-directory*
	    create-directory*
	    copy-directory
	    build-path*

	    null-device
	    ;;console-device
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (srfi :0)
	    (srfi :1)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :38)
	    (srfi :39 parameters)
	    (util port))

  ;; TODO should this get transcoder?
  (define (file->list reader path :key (transcoder (native-transcoder)))
    (call-with-input-file path
      (lambda (p)
	(port->list reader p))
      :transcoder transcoder))

  (define (file->string path)
    (car (file->list get-string-all path)))

  (define (file->sexp-list path)
    (file->list read/ss path))

  (define (file->string-list path)
    (file->list get-line path))

  (define *path-set* (string->char-set "\\/"))
  (define (path-filename path)
    (let* ((pos (string-index-right path (cond-expand
					  (sagittarius.os.windows 
					   *path-set*)
					  (else #\/)))))
      (string-copy path (or (and pos (+ pos 1)) 0))))

  (define (decompose-path path)
    (if (looking-at #/[\/\\]$/ path)
	(values (string-trim-right path path-set) #f #f)
	(let* ((delim-pos (string-index-right path
					      (cond-expand
					       (sagittarius.os.windows 
						*path-set*)
					       (else #\/))))
	       (dir (and delim-pos (substring path 0 delim-pos)))
	       (base (substring path (or (and delim-pos
					      (+ delim-pos 1))
					 0)
				(string-length path))))
	  (cond ((string-index-right base #\.)
		 => (lambda (pos)
		      (if (zero? pos)
			  ;; '.' at the beginning doesn't delimit extension
			  (values dir base #f)
			  (values dir
				  (string-take base pos)
				  (string-drop base (+ pos 1))))))
		(else (values dir base #f))))))

  (define (path-extension path)
    (let-values (((dir file ext) (decompose-path path))) ext))

  (define (path-sans-extension path)
    (cond ((path-extension path)
	   => (lambda (ext)
		(substring path 0
			   (- (string-length path) (string-length ext) 1))))
	  (else path)))

  (define %tmp
    (cond ((getenv "TMP"))
	  ((getenv "TEMP"))
	  ((getenv "USERPROFILE"))
	  ((getenv "TMPDIR"))
	  (else
	   (cond-expand
	    (windows (build-path (getenv "windir") "Temp"))
	    (else "/tmp"))))) ;; assume else is posix
  (define temporary-directory (make-parameter %tmp))
  (define (make-temporary-file :optional (prefix "tmp"))
    (define (gen) 
      (string-append prefix (number->string (microsecond) 32)))
    (let loop ((file (gen)))
      (if (file-exists? file)
	  (loop (gen))
	  (values (open-file-output-port file) file))))

  (define (find-files target :key (physical #t) (pattern #f) (all #t)
		      (sort string<=?) (recursive #t))
    (define rx-pattern (cond ((string? pattern) (regex pattern))
			     ((regex-pattern? pattern) pattern)
			     (else #f)))
    (define (check-pattern content)
      (or (not pattern)
	  (rx-pattern content)))
    (define (check path type)
      (and (not (eq? type 'directory))
	   (check-pattern (path-filename path))
	   path))
    (if (file-directory? target)
	(let ((r (filter values (path-map target check
					  :file-only #t
					  :physical physical
					  :all all :recursive recursive))))
	  (if sort (list-sort sort r) r))
	'()))

  ;; POSIX's nftw (sort of)
  ;; always depth
  ;; when proc returns #f, then it will stop
  (define (path-for-each path proc :key (physical #t) (file-only #f)
			 (absolute-path #t) (stop-on-false #f)
			 (all #t) (recursive #t))
    (define non-stop? (not stop-on-false))
    (define (rec path entries)
      (let loop ((entries entries))
	(unless (null? entries)
	  (let* ((entry (car entries))
		 (abs-path (build-path path entry)))
	    (cond ((and all (char=? (string-ref entry 0) #\.))
		   (if (file-directory? abs-path)
			 ;; ignore '.' and '..'
			 (loop (cdr entries))
			 (begin
			   (proc (if absolute-path abs-path entry) 'file)
			   (loop (cdr entries)))))
		  ((file-directory? abs-path)
		   ;; first do it recursively
		   (and (or (and (when recursive
				   (rec abs-path (read-directory abs-path))))
			    non-stop?)
			(or file-only
			    (proc (if absolute-path abs-path entry) 'directory)
			    non-stop?)
			(loop (cdr entries))))
		  ((file-symbolic-link? abs-path)
		   (and (or (not physical)
			    (proc (if absolute-path abs-path entry)
				  'symbolic-link)
			    non-stop?)
			(loop (cdr entries))))
		  (else
		   (and (or (proc (if absolute-path abs-path entry) 'file)
			    non-stop?)
			(loop (cdr entries)))))))))
    (when (file-exists? path)
      (cond ((file-directory? path)
	     (rec path (read-directory path)))
	    ((and (not file-only) (file-symbolic-link? path))
	     (proc path 'symbolic-link))
	    (else
	     (proc path 'file)))))

  (define (path-map path proc :key (physical #t) (file-only #f)
		    (absolute-path #t) (all #t) (recursive #t))
    (define (rec path entries)
      (let loop ((entries entries) (r '()))
	(if (null? entries)
	    r
	    (let* ((entry (car entries))
		   (abs-path (build-path path entry)))
	      (cond ((and all (char=? (string-ref entry 0) #\.))
		     (if (file-directory? abs-path)
			 ;; ignore '.' and '..'
			 (loop (cdr entries) r)
			 (let ((rp (proc (if absolute-path abs-path entry)
					 'file)))
			   (loop (cdr entries) (cons rp r)))))
		    ((file-directory? abs-path)
		     ;; first do it recursively
		     (let ((rp (if recursive
				   (rec abs-path (read-directory abs-path))
				   '())))
		       (if file-only
			   (loop (cdr entries) (append! r rp))
			   (let ((pr (proc (if absolute-path abs-path entry)
					   'directory)))
			     (loop (cdr entries) (append! (cons pr r) rp))))))
		    ((file-symbolic-link? abs-path)
		     (if physical
			 (loop (cdr entries) r)
			 (let ((rp (proc (if absolute-path abs-path entry)
					 'symbolic-link)))
			   (loop (cdr entries) (cons rp r)))))
		    (else
		     (let ((rp (proc (if absolute-path abs-path entry)
				     'file)))
		       (loop (cdr entries) (cons rp r)))))))))
    (if (file-exists? path)
	(cond ((file-directory? path)
	       (rec path (read-directory path)))
	      ((and (not file-only) (file-symbolic-link? path))
	       (list (proc path 'symbolic-link)))
	      (else
	       (list (proc path 'file))))
	'()))
  ;; rm -rf
  (define (delete-directory* path)
    ;; delete-directory can handle file as well
    (define (remove path type) (delete-directory path))
    (path-for-each path remove)
    ;; delete the top most
    (delete-directory path))
  ;; mkdir -p
  (define (create-directory* path)
    (define (rec p)
      (let-values (((dir base ext) (decompose-path p)))
	(cond ((file-exists? p)
	       (unless (file-directory? p)
		 (error 'create-directory*
			"non-directory is found during creating a directory"
			base path)))
	      (dir
	       (rec dir)
	       (unless (file-writable? dir)
		 (error 'create-directory*
			"directory unwritable during creating a directory"
			dir path))
	       (unless (equal? base ".") (create-directory p)))
	      (else
	       (unless (equal? base ".") (create-directory p))))))
    ;; some platform complains the last "/"
    (rec (string-trim-right path *path-set*)))

  ;; xcopy ... sort of
  (define (copy-directory base-path dst
			  :key (excludes '())
			  :allow-other-keys opt)
    (apply path-for-each
	   base-path
	   (lambda (opath type)
	     (and-let* (( (not (member opath excludes string-contains)) )
			(path (string-copy opath 
					   (+ (string-length base-path) 1))))
	       (if (eq? type 'directory)
		   (create-directory* (build-path dst path))
		   (let-values (((dir base ext) (decompose-path path)))
		     (create-directory* (build-path dst dir))
		     (copy-file opath (build-path dst path) #t)))))
	   opt))

  (define (build-path* . paths)
    (let ((len (length paths)))
      (case len
	;; treat trivial cases
	((0) "")			; should this case raise an error?
	((1) (car paths))
	((2) (build-path (car paths) (cadr paths)))
	(else
	 (receive (f l) (split-at paths (- len 1))
	   (let ((r (fold-right build-path "" f)))
	     (build-path r (car l))))))))

  (define (null-device)
    (cond-expand
     (windows "NUL")
     (else "/dev/null")))

;;   (define (console-device)
;;     (cond-expand
;;      (windows "CON")
;;      (else "/dev/tty")))

)
