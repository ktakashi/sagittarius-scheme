;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/sftp/constants.scm - SFTP protocol constants.
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

(library (rfc sftp constants)
    (export :all)
    (import (only (sagittarius) define-constant))

(define-constant +ssh-fxp-init+                1)
(define-constant +ssh-fxp-version+             2)
(define-constant +ssh-fxp-open+                3)
(define-constant +ssh-fxp-close+               4)
(define-constant +ssh-fxp-read+                5)
(define-constant +ssh-fxp-write+               6)
(define-constant +ssh-fxp-lstat+               7)
(define-constant +ssh-fxp-fstat+               8)
(define-constant +ssh-fxp-setstat+             9)
(define-constant +ssh-fxp-fsetstat+           10)
(define-constant +ssh-fxp-opendir+            11)
(define-constant +ssh-fxp-readdir+            12)
(define-constant +ssh-fxp-remove+             13)
(define-constant +ssh-fxp-mkdir+              14)
(define-constant +ssh-fxp-rmdir+              15)
(define-constant +ssh-fxp-realpath+           16)
(define-constant +ssh-fxp-stat+               17)
(define-constant +ssh-fxp-rename+             18)
(define-constant +ssh-fxp-readlink+           19)
(define-constant +ssh-fxp-symlink+            20)
(define-constant +ssh-fxp-status+            101)
(define-constant +ssh-fxp-handle+            102)
(define-constant +ssh-fxp-data+              103)
(define-constant +ssh-fxp-name+              104)
(define-constant +ssh-fxp-attrs+             105)
(define-constant +ssh-fxp-extended+          200)
(define-constant +ssh-fxp-extended-reply+    201)

(define-constant +ssh-filexfer-attr-size+          #x00000001)
(define-constant +ssh-filexfer-attr-uidgid+        #x00000002)
(define-constant +ssh-filexfer-attr-permissions+   #x00000004)
(define-constant +ssh-filexfer-attr-acmodtime+     #x00000008)
(define-constant +ssh-filexfer-attr-extended+      #x80000000)

(define-constant +ssh-fxf-read+            #x00000001)
(define-constant +ssh-fxf-write+           #x00000002)
(define-constant +ssh-fxf-append+          #x00000004)
(define-constant +ssh-fxf-creat+           #x00000008)
(define-constant +ssh-fxf-trunc+           #x00000010)
(define-constant +ssh-fxf-excl+            #x00000020)

;; may be only for future (server side stuff)
(define-constant +ssh-fx-ok+                            0)
(define-constant +ssh-fx-eof+                           1)
(define-constant +ssh-fx-no-such-file+                  2)
(define-constant +ssh-fx-permission-denied+             3)
(define-constant +ssh-fx-failure+                       4)
(define-constant +ssh-fx-bad-message+                   5)
(define-constant +ssh-fx-no-connection+                 6)
(define-constant +ssh-fx-connection-lost+               7)
(define-constant +ssh-fx-op-unsupported+                8)

(define-constant +sftp-version3+ 3)

)