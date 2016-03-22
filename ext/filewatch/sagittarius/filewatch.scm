;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; sagittarius/filewatch.scm - File monitoring
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius filewatch)
    (export make-filesystem-watcher
	    release-filesystem-watcher
	    filesystem-watcher?
	    filesystem-watcher-add-path!
	    filesystem-watcher-remove-path!
	    ;; returns thread
	    filesystem-watcher-start-monitoring!
	    filesystem-watcher-stop-monitoring!)
    (import (rnrs)
	    (clos user)
	    (sagittarius threads)
	    (sagittarius dynamic-module))

(load-dynamic-module "sagittarius--filewatch")

(define-class <filesystem-watcher> ()
  ((context :init-keyword :context :reader filesystem-watcher-context)
   (thread  :init-value #f :reader filesystem-watcher-thread)))

(define (make-filesystem-watcher) 
  (make <filesystem-watcher> :context (make-file-watch-context)))
(define (release-filesystem-watcher watcher)
  (let ((thread (filesystem-watcher-thread watcher)))
    (when (thread? thread) (filesystem-watcher-stop-monitoring! watcher)))
  (destroy-file-watch-context! (filesystem-watcher-context watcher))
  (slot-set! watcher 'context #f))

(define (filesystem-watcher? o) (is-a? o <filesystem-watcher>))

(define (filesystem-watcher-add-path! watcher path flags handler)
  (unless (procedure? handler) 
    (assertion-violation 'filesystem-watcher-add-path!
			 "handler must be a procedure" handler))
  (add-monitoring-path (filesystem-watcher-context watcher) path flags handler)
  watcher)

(define (filesystem-watcher-remove-path! watcher path)
  (remove-monitoring-path (filesystem-watcher-context watcher) path))

(define (filesystem-watcher-start-monitoring! watcher)
  (let ((thread (thread-start! 
		 (make-thread 
		  (lambda () 
		    (start-monitoring! 
		     (filesystem-watcher-context watcher)))))))
    (slot-set! watcher 'thread thread)))

(define (filesystem-watcher-stop-monitoring! watcher)
  (let ((thread (filesystem-watcher-thread watcher)))
    (unless (thread? thread)
      (assertion-violation 'filesystem-watcher-stop-monitoring!
			   "watcher is not started yet" watcher))
    (thread-interrupt! thread)
    (thread-join! thread)
    (slot-set! watcher 'thread #f)))

)
	    
