;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver constant
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2 const)
    (export +default-max-concurrent-streams+
	    +default-header-table-size+
	    +default-max-frame-size+
	    +default-initial-window-size+
	    +default-priority-wire-weight+)
    (import (rnrs)
	    (rfc http2 frame))

(define +default-max-concurrent-streams+ 100)
(define +default-header-table-size+ 4096)
(define +default-max-frame-size+ +http2-initial-frame-buffer-size+)
(define +default-initial-window-size+ +http2-default-window-size+)
(define +default-priority-wire-weight+ 15)

)
