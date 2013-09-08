;;; kernel.scm
;;; Implementation of the sweet-expressions project by readable mailinglist.
;;;
;;; Copyright (C) 2005-2013 by David A. Wheeler and Alan Manuel K. Gloria
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; Warning: For portability use eqv?/memv (not eq?/memq) to compare characters.
;;; A "case" is okay since it uses "eqv?".

;;; This file includes:
;;; - Compatibility layer - macros etc. to try to make it easier to port
;;;   this code to different Scheme implementations (to deal with different
;;;   module systems, how to override read, getting position info, etc.)
;;; - Re-implementation of "read", beginning with its support procedures.
;;;   There is no standard Scheme mechanism to override just *portions*
;;;   of read, and we MUST make {} delimiters, so we have to re-implement read.
;;;   We also need to get control over # so we know which ones are comments.
;;;   If you're modifying a Scheme implementation, just use that instead.
;;; - Curly Infix (c-expression) implementation
;;; - Neoteric expression (n-expression) implementation
;;; - Sweet-expression (t-expression) implementation
;;; - Implementation of writers for curly-infix and neoteric expressions -
;;;   a "-simple" implementation, and a separate -shared/-cyclic version.
;;;   The "-simple" one is separate so that you can use just it.
;;; Note that a lot of the code is in the compatibility layer and
;;; re-implementation of "read"; implementing the new expression languages
;;; is actually pretty easy.


; -----------------------------------------------------------------------------
; Compatibility Layer
; -----------------------------------------------------------------------------
; The compatibility layer is composed of:
;
;   (readable-kernel-module-contents (exports ...) body ...)
;   - a macro that should package the given body as a module, or whatever your
;     scheme calls it (chicken eggs?), preferably with one of the following
;     names, in order of preference, depending on your Scheme's package naming
;     conventions/support
;       (readable kernel)
;       readable/kernel
;       readable-kernel
;       sweetimpl
;   - The first element after the module-contents name is a list of exported
;     procedures.  This module shall never export a macro or syntax, not even
;     in the future.
;   - If your Scheme requires module contents to be defined inside a top-level
;     module declaration (unlike Guile where module contents are declared as
;     top-level entities after the module declaration) then the other
;     procedures below should be defined inside the module context in order
;     to reduce user namespace pollution.
;
;   (my-peek-char port)
;   (my-read-char port)
;   - Performs I/O on a "port" object.
;   - The algorithm assumes that port objects have the following abilities:
;     * The port automatically keeps track of source location
;       information.  On R5RS there is no source location
;       information that can be attached to objects, so as a
;       fallback you can just ignore source location, which
;       will make debugging using sweet-expressions more
;       difficult.
;   - "port" or fake port objects are created by the make-read procedure
;     below.
;   
;   (make-read procedure)
;   - The given procedure accepts exactly 1 argument, a "fake port" that can
;     be passed to my-peek-char et al.
;   - make-read creates a new procedure that supports your Scheme's reader
;     interface.  Usually, this means making a new procedure that accepts
;     either 0 or 1 parameters, defaulting to (current-input-port).
;   - If your Scheme doesn't keep track of source location information
;     automatically with the ports, you may again need to wrap it here.
;   - If your Scheme needs a particularly magical incantation to attach
;     source information to objects, then you might need to use a weak-key
;     table in the attach-sourceinfo procedure below and then use that
;     weak-key table to perform the magical incantation.
;
;   (invoke-read read port)
;   - Accepts a read procedure, which is a (most likely built-in) procedure
;     that requires a *real* port, not a fake one.
;   - Should unwrap the fake port to a real port, then invoke the given
;     read procedure on the actual real port.
;
;   (get-sourceinfo port)
;   - Given a fake port, constructs some object (which the algorithm treats
;     as opaque) to represent the source information at the point that the
;     port is currently in.
;
;   (attach-sourceinfo pos obj)
;   - Attaches the source information pos, as constructed by get-sourceinfo,
;     to the given obj.
;   - obj can be any valid Scheme object.  If your Scheme can only track
;     source location for a subset of Scheme object types, then this procedure
;     should handle it gracefully.
;   - Returns an object with the source information attached - this can be
;     the same object, or a different object that should look-and-feel the
;     same as the passed-in object.
;   - If source information cannot be attached anyway (your Scheme doesn't
;     support attaching source information to objects), just return the
;     given object.
;
;   (replace-read-with f)
;   - Replaces your Scheme's current reader.
;   - Replace 'read and 'get-datum at the minimum.  If your Scheme
;     needs any kind of involved magic to handle load and loading
;     modules correctly, do it here.
;
;   (parse-hash no-indent-read char fake-port)
;   - a procedure that is invoked when an unrecognized, non-R5RS hash
;     character combination is encountered in the input port.
;   - this procedure is passed a "fake port", as wrapped by the
;     make-read procedure above.  You should probably use my-read-char
;     and my-peek-char in it, or at least unwrap the port (since
;     make-read does the wrapping, and you wrote make-read, we assume
;     you know how to unwrap the port).
;   - if your procedure needs to parse a datum, invoke
;     (no-indent-read fake-port).  Do NOT use any other read procedure.  The
;     no-indent-read procedure accepts exactly one parameter - the fake port
;     this procedure was passed in.
;     - no-indent-read is either a version of curly-infix-read, or a version
;       of neoteric-read; this specal version accepts only a fake port.
;       It is never a version of sweet-read.  You don't normally want to
;       call sweet-read, because sweet-read presumes that it's starting
;       at the beginning of the line, with indentation processing still
;       active.  There's no reason either must be true when processing "#".
;   - At the start of this procedure, both the # and the character
;     after it have been read in.
;   - The procedure returns one of the following:
;       #f  - the hash-character combination is invalid/not supported.
;       (normal value) - the datum has value "value".
;       (scomment ()) - the hash-character combination introduced a comment;
;             at the return of this procedure with this value, the
;             comment has been removed from the input port.
;             You can use scomment-result, which has this value.
;       (datum-commentw ()) - #; followed by whitespace.
;       (abbrev value) - this is an abbreviation for value "value"
;
;   hash-pipe-comment-nests?
;   - a Boolean value that specifies whether #|...|# comments
;     should nest.
;
;   my-string-foldcase
;   - a procedure to perform case-folding to lowercase, as mandated
;     by Unicode.  If your implementation doesn't have Unicode, define
;     this to be string-downcase.  Some implementations may also
;     interpret "string-downcase" as foldcase anyway.


; On Guile 2.0, the define-module part needs to occur separately from
; the rest of the compatibility checks, unfortunately.  Sigh.
(cond-expand
  (guile
    ; define the module
    ; this ensures that the user's module does not get contaminated with
    ; our compatibility procedures/macros
    (define-module (readable kernel)))
  (else #t))
(cond-expand
; -----------------------------------------------------------------------------
; Guile Compatibility
; -----------------------------------------------------------------------------
  (guile

    ; properly get bindings
    (use-modules (guile))

    ; On Guile 1.x defmacro is the only thing supported out-of-the-box.
    ; This form still exists in Guile 2.x, fortunately.
    (defmacro readable-kernel-module-contents (exports . body)
      `(begin (export ,@exports)
              ,@body))

    ; Enable R5RS hygenic macro system (define-syntax) - guile 1.X
    ; does not automatically provide it, but version 1.6+ enable it this way
    (use-syntax (ice-9 syncase))

    ; Guile was the original development environment, so the algorithm
    ; practically acts as if it is in Guile.
    ; Needs to be lambdas because otherwise Guile 2.0 acts strangely,
    ; getting confused on the distinction between compile-time,
    ; load-time and run-time (apparently, peek-char is not bound
    ; during load-time).
    (define (my-peek-char fake-port)
      (if (eof-object? (car fake-port))
          (car fake-port)
          (let* ((port (car fake-port))
                 (char (peek-char port)))
            (if (eof-object? char)
                (set-car! fake-port char))
            char)))
    (define (my-read-char fake-port)
      (if (eof-object? (car fake-port))
          (car fake-port)
          (let* ((port (car fake-port))
                 (char (read-char port)))
            (if (eof-object? char)
                (set-car! fake-port char))
            char)))

    (define (make-read f)
      (lambda args
        (let ((port (if (null? args) (current-input-port) (car args))))
          (f (list port)))))

    (define (invoke-read read fake-port)
      (if (eof-object? (car fake-port))
          (car fake-port)
          (read (car fake-port))))

    ; create a list with the source information
    (define (get-sourceinfo fake-port)
      (if (eof-object? (car fake-port))
          #f
          (let ((port (car fake-port)))
            (list (port-filename port)
                  (port-line port)
                  (port-column port)))))
    ; destruct the list and attach, but only to cons cells, since
    ; only that is reliably supported across Guile versions.
    (define (attach-sourceinfo pos obj)
      (cond
        ((not pos)
          obj)
        ((pair? obj)
          (set-source-property! obj 'filename (list-ref pos 0))
          (set-source-property! obj 'line     (list-ref pos 1))
          (set-source-property! obj 'column   (list-ref pos 2))
          obj)
        (#t
          obj)))

    ; To properly hack into 'load and in particular 'use-modules,
    ; we need to hack into 'primitive-load.  On 1.8 and 2.0 there
    ; is supposed to be a current-reader fluid that primitive-load
    ; hooks into, but it seems (unverified) that each use-modules
    ; creates a new fluid environment, so that this only sticks
    ; on a per-module basis.  But if the project is primarily in
    ; sweet-expressions, we would prefer to have that hook in
    ; *all* 'use-modules calls.  So our primitive-load uses the
    ; 'read global variable if current-reader isn't set.

    (define %sugar-current-load-port #f)
    ; replace primitive-load
    (define primitive-load-replaced #f)
    (define (setup-primitive-load)
      (cond
        (primitive-load-replaced
           (values))
        (#t
          (module-set! (resolve-module '(guile)) 'primitive-load
            (lambda (filename)
              (let ((hook (cond
                            ((not %load-hook)
                              #f)
                            ((not (procedure? %load-hook))
                              (error "%load-hook must be procedure or #f"))
                            (#t
                              %load-hook))))
                (cond
                  (hook
                    (hook filename)))
                (let* ((port      (open-input-file filename))
                       (save-port port))
                  (define (load-loop)
                    (let* ((the-read
                             (or
                                 ; current-reader doesn't exist on 1.6
                                 (if (string=? "1.6" (effective-version))
                                     #f
                                     (fluid-ref current-reader))
                                 read))
                           (form (the-read port)))
                      (cond
                        ((not (eof-object? form))
                          ; in Guile only
                          (primitive-eval form)
                          (load-loop)))))
                  (define (swap-ports)
                    (let ((tmp %sugar-current-load-port))
                      (set! %sugar-current-load-port save-port)
                      (set! save-port tmp)))
                  (dynamic-wind swap-ports load-loop swap-ports)
                  (close-input-port port)))))
          (set! primitive-load-replaced #t))))

    (define (replace-read-with f)
      (setup-primitive-load)
      (set! read f))

    ; Below implements some guile extensions, basically as guile 2.0.
    ; On Guile, #:x is a keyword.  Keywords have symbol syntax.
    ; On Guile 1.6 and 1.8 the only comments are ; and #!..!#, but
    ; we'll allow more.
    ; On Guile 2.0, #; (SRFI-62) and #| #| |# |# (SRFI-30) comments exist.
    ; On Guile 2.0, #' #` #, #,@ have the R6RS meaning; on older
    ; Guile 1.8 and 1.6 there is a #' syntax but we have yet
    ; to figure out what exactly they do, and since those are becoming
    ; obsolete, we'll just use the R6RS meaning.
    (define (parse-hash no-indent-read char fake-port)
      ; (let* ((ver (effective-version))
      ;        (c   (string-ref ver 0))
      ;        (>=2 (and (not (char=? c #\0)) (not (char=? c #\1))))) ...)
      (cond
        ((char=? char #\:)
          ; On Guile 1.6, #: reads characters until it finds non-symbol
          ; characters.
          ; On Guile 1.8 and 2.0, #: reads in a datum, and if the
          ; datum is not a symbol, throws an error.
          ; Follow the 1.8/2.0 behavior as it is simpler to implement,
          ; and even on 1.6 it is unlikely to cause problems.
          ; NOTE: This behavior means that #:foo(bar) will cause
          ; problems on neoteric and higher tiers.
          (let ((s (no-indent-read fake-port)))
            (if (symbol? s)
                `(normal ,(symbol->keyword s) )
                #f)))
        ; On Guile 2.0 #' #` #, #,@ have the R6RS meaning, handled in generics.
        ; Guile 1.6 and 1.8 have different meanings, but we'll ignore that.
        ; Guile's #{ }# syntax
        ((char=? char #\{ )  ; Special symbol, through till ...}#
          `(normal ,(list->symbol (special-symbol fake-port))))
        (#t #f)))

  ; Return list of characters inside #{...}#, a guile extension.
  ; presume we've already read the sharp and initial open brace.
  ; On eof we just end.  We could error out instead.
  ; TODO: actually conform to Guile's syntax.  Note that 1.x
  ; and 2.0 have different syntax when spaces, backslashes, and
  ; control characters get involved.
  (define (special-symbol port)
    (cond
      ((eof-object? (my-peek-char port)) '())
      ((eqv? (my-peek-char port) #\})
        (my-read-char port) ; consume closing brace
        (cond
          ((eof-object? (my-peek-char port)) '(#\}))
          ((eqv? (my-peek-char port) #\#)
            (my-read-char port) ; Consume closing sharp.
            '())
          (#t (append '(#\}) (special-symbol port)))))
      (#t (append (list (my-read-char port)) (special-symbol port)))))

    (define hash-pipe-comment-nests? #t)

    (define (my-string-foldcase s)
      (string-downcase s))

  ; Here's how to import SRFI-69 in guile (for hash tables);
  ; we have to invoke weird magic becuase guile will
  ; complain about merely importing a normal SRFI like this
  ; (which I think is a big mistake, but can't fix guile 1.8):
  ; WARNING: (guile-user): imported module (srfi srfi-69)
  ;                        overrides core binding `make-hash-table'
  ; WARNING: (guile-user): imported module (srfi srfi-69)
  ;                         overrides core binding `hash-table?'
  (use-modules ((srfi srfi-69)
               #:select ((make-hash-table . srfi-69-make-hash-table)
                         (hash-table? . srfi-69-hash-table?)
                         hash-table-set!
                         hash-table-update!/default
                         hash-table-ref
                         hash-table-ref/default
                         hash-table-walk
                         hash-table-delete! )))

  ; For "any"
  (use-modules (srfi srfi-1))

  ; There's no portable way to walk through other collections like records.
  ; Chibi has a "type" "type" procedure but isn't portable
  ; (and it's not in guile 1.8 at least).
  ; We'll leave it in, but stub it out; you can replace this with
  ; what your Scheme supports.  Perhaps the "large" R7RS can add support
  ; for walking through arbitrary collections.
  (define (type-of x) #f)
  (define (type? x) #f)

    )
  ;; For Sagittarius Scheme added by Takashi Kato
  (sagittarius
   ;; Use export syntax
    (define-syntax readable-kernel-module-contents
      (syntax-rules ()
        ((readable-kernel-module-contents (exports ...) body ...)
          (begin (export exports ...) body ...))))

    (define (my-peek-char port) (peek-char port))
    (define (my-read-char port) (read-char port))

    (define (make-read f)
      (lambda args
        (let ((real-port (if (null? args) (current-input-port) (car args))))
          (f real-port))))

    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (read port))
    ;; TODO we do have source info thing
    (define (get-sourceinfo _) #f)
    (define (attach-sourceinfo _ x) x)

    (define (replace-read-with f)
      (error 'replace-read-with "Not supported, use #!reader= instead"))

    ;; Well for this SRFI don't use regular expression :-P
    (define (parse-hash no-indent-read char fake-port) #f)

    (define hash-pipe-comment-nests? #t)
    (define my-string-foldcase string-foldcase)
    ;; hmm what is this?
    (define (type-of x) #f)
    (define (type? x) #f)

    (define (make-hash-table . dummy) (make-eq-hashtable))
    (define hash-table?     hashtable?)
    (define hash-table-set! hashtable-set!)
    (define hash-table-ref  hashtable-ref)
    (define hash-table-ref/default  hashtable-ref)
    (define hash-table-update!/default hashtable-update!)
    (define (hash-table-walk ht proc)
      (let-values (((keys values) (hashtable-entries ht)))
	(do ((len (vector-length keys)) (i 0 (+ i 1)))
	    ((= i len))
	  (proc (vector-ref keys i) (vector-ref values i)))))
    (define hash-table-delete! hashtable-delete!)

    ;; the reference implementation was only for guile...
    (define-syntax debug-set!
      (syntax-rules ()
	((_ bogus ...) (begin))))

    (define-syntax catch
      (syntax-rules ()
	((_ name thunk handler)
	 (guard (e ((eq? name e) (handler e))
		   (else (error name "unexpected throw")))
	   (thunk)))))
    (define (throw name) (raise name))
    (define force-output flush-output-port)
      
   )
; -----------------------------------------------------------------------------
; R5RS Compatibility
; -----------------------------------------------------------------------------
  (else
    ; assume R5RS with define-syntax

    ; On R6RS, and other Scheme's, module contents must
    ; be entirely inside a top-level module structure.
    ; Use module-contents to support that.  On Schemes
    ; where module declarations are separate top-level
    ; expressions, we expect module-contents to transform
    ; to a simple (begin ...), and possibly include
    ; whatever declares exported stuff on that Scheme.
    (define-syntax readable-kernel-module-contents
      (syntax-rules ()
        ((readable-kernel-module-contents exports body ...)
          (begin body ...))))

    ; We use my-* procedures so that the
    ; "port" automatically keeps track of source position.
    ; On Schemes where that is not true (e.g. Racket, where
    ; source information is passed into a reader and the
    ; reader is supposed to update it by itself) we can wrap
    ; the port with the source information, and update that
    ; source information in the my-* procedures.

    (define (my-peek-char port) (peek-char port))
    (define (my-read-char port) (read-char port))

    ; this wrapper procedure wraps a reader procedure
    ; that accepts a "fake" port above, and converts
    ; it to an R5RS-compatible procedure.  On Schemes
    ; which support source-information annotation,
    ; but use a different way of annotating
    ; source-information from Guile, this procedure
    ; should also probably perform that attachment
    ; on exit from the given inner procedure.
    (define (make-read f)
      (lambda args
        (let ((real-port (if (null? args) (current-input-port) (car args))))
          (f real-port))))

    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (read port))
    ; R5RS doesn't have any method of extracting
    ; or attaching source location information.
    (define (get-sourceinfo _) #f)
    (define (attach-sourceinfo _ x) x)

    ; Not strictly R5RS but we expect at least some Schemes
    ; to allow this somehow.
    (define (replace-read-with f)
      (set! read f))

    ; R5RS has no hash extensions
    (define (parse-hash no-indent-read char fake-port) #f)

    ; Hash-pipe comment is not in R5RS, but support
    ; it as an extension, and make them nest.
    (define hash-pipe-comment-nests? #t)

    ; If your Scheme supports "string-foldcase", use that instead of
    ; string-downcase:
    (define (my-string-foldcase s)
      (string-downcase s))

    ; Somehow get SRFI-69 and SRFI-1
    ))



; -----------------------------------------------------------------------------
; Module declaration and useful utilities
; -----------------------------------------------------------------------------
(readable-kernel-module-contents
  ; exported procedures
  (; tier read procedures
   curly-infix-read neoteric-read sweet-read
   ; set read mode
   set-read-mode
   ; replacing the reader
   replace-read restore-traditional-read
   enable-curly-infix enable-neoteric enable-sweet
   ; Various writers.
   curly-write-simple neoteric-write-simple
   curly-write curly-write-cyclic curly-write-shared
   neoteric-write neoteric-write-cyclic neoteric-write-shared)


  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; We always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define is-foldcase #f)

  ; special tag to denote comment return from hash-processing

  ; Define the whitespace characters, in relatively portable ways
  ; Presumes ASCII, Latin-1, Unicode or similar.
  (define tab (integer->char #x0009))             ; #\ht aka \t.
  (define linefeed (integer->char #x000A))        ; #\newline aka \n. FORCE it.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define line-tab (integer->char #x000D))
  (define form-feed (integer->char #x000C))
  (define vertical-tab (integer->char #x000b))
  (define space '#\space)

  ; Period symbol.  A symbol starting with "." is not
  ; validly readable in R5RS, R6RS, R7RS (except for
  ; the peculiar identifier "..."); with the
  ; R6RS and R7RS the print representation of
  ; string->symbol(".") should be |.| .  However, as an extension, this
  ; Scheme reader accepts "." as a valid identifier initial character,
  ; in part because guile permits it.
  ; For portability, use this formulation instead of '. in this
  ; implementation so other implementations don't balk at it.
  (define period-symbol (string->symbol "."))

  (define line-ending-chars (list linefeed carriage-return))

  ; This definition of whitespace chars is derived from R6RS section 4.2.1.
  ; R6RS doesn't explicitly list the #\space character, be sure to include!
  (define whitespace-chars-ascii
     (list tab linefeed line-tab form-feed carriage-return #\space))
  ; Note that we are NOT trying to support all Unicode whitespace chars.
  (define whitespace-chars whitespace-chars-ascii)

  ; If #t, handle some constructs so we can read and print as Common Lisp.
  (define common-lisp #f)

  ; If #t, return |...| symbols as-is, including the vertical bars.
  (define literal-barred-symbol #f)

  ; Returns a true value (not necessarily #t)
  (define (char-line-ending? char) (memv char line-ending-chars))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (memv c whitespace-chars)))

  ; Consume an end-of-line sequence, ('\r' '\n'? | '\n'), and nothing else.
  ; Don't try to handle reversed \n\r (LFCR); doing so will make interactive
  ; guile use annoying (EOF won't be correctly detected) due to a guile bug
  ; (in guile before version 2.0.8, peek-char incorrectly
  ; *consumes* EOF instead of just peeking).
  (define (consume-end-of-line port)
    (let ((c (my-peek-char port)))
      (cond
        ((eqv? c carriage-return)
          (my-read-char port)
          (if (eqv? (my-peek-char port) linefeed)
              (my-read-char port)))
        ((eqv? c linefeed)
          (my-read-char port)))))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (my-peek-char port)))
      (cond
        ((and (not (eof-object? c)) (not (char-line-ending? c)))
          (my-read-char port)
          (consume-to-eol port)))))

  (define (consume-to-whitespace port)
    ; Consume to whitespace
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((my-char-whitespace? c)
          '())
        (#t
          (my-read-char port)
          (consume-to-whitespace port)))))

  (define debugger-output #f)
  ; Quick utility for debugging.  Display marker, show data, return data.
  (define (debug-show marker data)
    (cond
      (debugger-output
        (display "DEBUG: ")
        (display marker)
        (display " = ")
        (write data)
        (display "\n")))
    data)

  (define (my-read-delimited-list my-read stop-char port)
    ; Read the "inside" of a list until its matching stop-char, returning list.
    ; stop-char needs to be closing paren, closing bracket, or closing brace.
    ; This is like read-delimited-list of Common Lisp, but it
    ; calls the specified reader instead.
    ; This implements a useful extension: (. b) returns b. This is
    ; important as an escape for indented expressions, e.g., (. \\)
    (consume-whitespace port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") c)
        ((char=? c stop-char)
          (my-read-char port)
          (attach-sourceinfo pos '()))
        ((memv c '(#\) #\] #\}))  (read-error "Bad closing character") c)
        (#t
          (let ((datum (my-read port)))
            (cond
               ((and (eq? datum period-symbol) (char=? c #\.))
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port)
                   (cond
                     ((eof-object? datum2)
                      (read-error "Early eof in (... .)")
                      '())
                     ((not (eqv? (my-peek-char port) stop-char))
                      (read-error "Bad closing character after . datum")
                      datum2)
                     (#t
                       (my-read-char port)
                       datum2))))
               (#t
                 (attach-sourceinfo pos
                   (cons datum
                     (my-read-delimited-list my-read stop-char port))))))))))

; -----------------------------------------------------------------------------
; Read preservation, replacement, and mode setting
; -----------------------------------------------------------------------------

  (define default-scheme-read read)
  (define replace-read replace-read-with)
  (define (restore-traditional-read) (replace-read-with default-scheme-read))

  (define (enable-curly-infix)
    (if (not (or (eq? read curly-infix-read)
                 (eq? read neoteric-read)
                 (eq? read sweet-read)))
        (replace-read curly-infix-read)))

  (define (enable-neoteric)
    (if (not (or (eq? read neoteric-read)
                 (eq? read sweet-read)))
        (replace-read neoteric-read)))

  (define (enable-sweet)
    (replace-read sweet-read))

  (define current-read-mode #f)
  
  (define (set-read-mode mode port)
    ; TODO: Should be per-port
    (cond
      ((eq? mode 'common-lisp)
        (set! common-lisp #t) #t)
      ((eq? mode 'literal-barred-symbol)
        (set! literal-barred-symbol #t) #t)
      ;; added fixes by Takashi Kato
      ((eq? mode 'fold-case)
        (set! is-foldcase #t) #t)
      ((eq? mode 'no-fold-case)
        (set! is-foldcase #f) #t)
      (#t (display "Warning: Unknown mode") #f)))

; -----------------------------------------------------------------------------
; Scheme Reader re-implementation
; -----------------------------------------------------------------------------

; We have to re-implement our own Scheme reader.
; This takes more code than it would otherwise because many
; Scheme readers will not consider [, ], {, and } as delimiters
; (they are not required delimiters in R5RS and R6RS).
; Thus, we cannot call down to the underlying reader to implement reading
; many types of values such as symbols.
; If your Scheme's "read" also considers [, ], {, and } as
; delimiters (and thus are not consumed when reading symbols, numbers, etc.),
; then underlying-read could be much simpler.
; We WILL call default-scheme-read on string reading (the ending delimiter
; is ", so that is no problem) - this lets us use the implementation's
; string extensions if any.

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
     (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
                   #\" #\;)                 ; Could add #\# or #\|
             whitespace-chars))

  (define (consume-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((eof-object? char))
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (my-read-char port)
          (consume-whitespace port)))))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (my-peek-char port)))
      (cond
         ((eof-object? c) '())
         ((memv c delims) '())
         (#t (my-read-char port) (cons c (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: " (current-error-port))
    (display message (current-error-port))
    (newline (current-error-port))
    ; Guile extension to flush output on stderr
    (force-output (current-error-port))
    ; Guile extension, but many Schemes have exceptions
    (throw 'readable)
    '())

  ; Return the number by reading from port, and prepending starting-lyst.
  ; Returns #f if it's not a number.
  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port neoteric-delimiters)))))

  ; Return list of digits read from port; may be empty.
  (define (read-digits port)
    (let ((c (my-peek-char port)))
      (cond
        ((memv c digits)
          (cons (my-read-char port) (read-digits port)))
        (#t '()))))


  (define char-name-values
    ; Includes standard names and guile extensions; see
    ; http://www.gnu.org/software/guile/manual/html_node/Characters.html
    '((space #x0020)
      (newline #x000a)
      (nl #x000a)
      (tab #x0009)
      (nul #x0000)
      (null #x0000)
      (alarm #x0007)
      (backspace #x0008)
      (linefeed #x000a)
      (vtab #x000b)
      (page #x000c)
      (return #x000d)
      (esc #x001b)
      (delete #x007f)
      ; Additional character names as extensions:
      (soh #x0001)
      (stx #x0002)
      (etx #x0003)
      (eot #x0004)
      (enq #x0005)
      (ack #x0006)
      (bel #x0007)
      (bs #x0008)
      (ht #x0009)
      (lf #x000a)
      (vt #x000b)
      (ff #x000c)
      (np #x000c) ; new page
      (cr #x000d)
      (so #x000e)
      (si #x000f)
      (dle #x0010)
      (dc1 #x0011)
      (dc2 #x0012)
      (dc3 #x0013)
      (dc4 #x0014)
      (nak #x0015)
      (syn #x0016)
      (etb #x0017)
      (can #x0018)
      (em #x0019)
      (sub #x001a)
      (fs #x001c)
      (gs #x001d)
      (rs #x001e)
      (sp #x0020)
      (del #x007f)
      ; Other non-guile names.
      ; rubout noted in: http://docs.racket-lang.org/reference/characters.html
      ; It's also required by the Common Lisp spec.
      (rubout #x007f)))
  
  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t ; More than one char; do a lookup.
              (let* ((cname (string->symbol
                       (string-downcase (list->string (cons c rest)))))
                     (found (assq cname char-name-values)))
                (if found
                    (integer->char (cadr found))
                    (read-error "Invalid character name")))))))))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; is-foldcase configuration value when processing symbols.
  ; TODO: Should be port-specific
  (define (fold-case-maybe port s)
    (if is-foldcase
        (my-string-foldcase s)
        s))

  (define (process-directive dir)
    (cond
      ; TODO: These should be specific to the port.
      ((string-ci=? dir "sweet")
        (enable-sweet))
      ((string-ci=? dir "no-sweet")
        (restore-traditional-read))
      ((string-ci=? dir "curly-infix")
        (replace-read curly-infix-read))
      ((string-ci=? dir "fold-case")
        (set! is-foldcase #t))
      ((string-ci=? dir "no-fold-case")
        (set! is-foldcase #f))
      (#t (display "Warning: Unknown process directive"))))

  ; Consume characters until "!#"
  (define (non-nest-comment port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c)
          (values))
        ((char=? c #\!)
          (let ((c2 (my-peek-char port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char port)
                  (values))
                (non-nest-comment port))))
        (#t
          (non-nest-comment port)))))

  (define (process-sharp-bang port)
    (let ((c (my-peek-char port)))
      (cond
        ((char=? c #\space) ; SRFI-22
          (consume-to-eol port)
          (consume-end-of-line port)
          scomment-result) ; Treat as comment.
        ((memv c '(#\/ #\.)) ; Multi-line, non-nesting #!/ ... !# or #!. ...!#
          (non-nest-comment port)
          scomment-result) ; Treat as comment.
        ((char-alphabetic? c)  ; #!directive
          (process-directive
            (list->string (read-until-delim port neoteric-delimiters)))
          scomment-result)
        ((or (eqv? c carriage-return) (eqv? c #\newline))
          ; Extension: Ignore lone #!
          (consume-to-eol port)
          (consume-end-of-line port)
          scomment-result) ; Treat as comment.
        (#t (read-error "Unsupported #! combination")))))

  ; A cons cell always has a unique address (as determined by eq?);
  ; we'll use this special cell to tag unmatched datum label references.
  ; An unmatched datum label will have the form
  ; (cons unmatched-datum-label-tag NUMBER)
  (define unmatched-datum-label-tag
    (cons 'unmatched-datum-label-tag 'label))
  (define (is-matching-label-tag? number value)
    (and
      (pair? value)
      (eq?  (car value) unmatched-datum-label-tag)
      (eqv? (cdr value) number)))

  ; Patch up datum, starting at position,
  ; so that all labels "number" are replaced
  ; with the value of "replace".
  ; Note that this actually OVERWRITES PORTIONS OF A LIST with "set!",
  ; so that we can maintain "eq?"-ness.  This is really wonky,
  ; but datum labels are themselves wonky.
  ; "skip" is a list of locations that don't need (re)processing; it
  ; should be a hash table but those aren't portable.
  (define (patch-datum-label-tail number replace position skip)
    (let ((new-skip (cons position skip)))
      (cond
        ((and (pair? position)
              (not (eq? (car position) unmatched-datum-label-tag))
              (not (memq position skip)))
          (if (is-matching-label-tag? number (car position))
              (set-car! position replace) ; Yes, "set!" !!
              (patch-datum-label-tail number replace (car position) new-skip))
          (if (is-matching-label-tag? number (cdr position))
              (set-cdr! position replace) ; Yes, "set!" !!
              (patch-datum-label-tail number replace (cdr position) new-skip)))
        ((vector? position)
          (do ((len (vector-length position))
               (k 0 (+ k 1)))
            ((>= k len) (values))
            (let ((x (vector-ref position k)))
              (if (is-matching-label-tag? number x)
                  (vector-set! position k replace)
                  (patch-datum-label-tail number replace x new-skip)))))
        (#t (values)))))

  (define (patch-datum-label number starting-position)
    (if (is-matching-label-tag? number starting-position)
        (read-error "Illegal reference as the labelled object itself"))
    (patch-datum-label-tail number starting-position starting-position '())
    starting-position)

  ; Gobble up the to-gobble characters from port, and return # ungobbled.
  (define (gobble-chars port to-gobble)
    (if (null? to-gobble)
        0
        (cond
          ((char-ci=? (my-peek-char port) (car to-gobble))
            (my-read-char port)
            (gobble-chars port (cdr to-gobble)))
          (#t (length to-gobble)))))

  (define scomment-result '(scomment ()))

  (define (process-sharp no-indent-read port)
    ; We've read a # character.  Returns what it represents as
    ; (stopper value); ('normal value) is value, ('scomment ()) is comment.
    ; Since we have to re-implement process-sharp anyway,
    ; the vector representation #(...) uses my-read-delimited-list, which in
    ; turn calls no-indent-read.
    ; TODO: Create a readtable for this case.
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) scomment-result) ; If eof, pretend it's a comment.
        ((char-line-ending? c) ; Extension - treat # EOL as a comment.
          scomment-result) ; Note this does NOT consume the EOL!
        (#t ; Try out different readers until we find a match.
          (my-read-char port)
          (or
            (and common-lisp
                 (parse-cl no-indent-read c port))
            (parse-hash no-indent-read c port)
            (parse-default no-indent-read c port)
            (read-error "Invalid #-prefixed string"))))))

  (define (parse-default no-indent-read c port)
              (cond ; Nothing special - use generic rules
                ((char-ci=? c #\t)
                  (if (memv (gobble-chars port '(#\r #\u #\e)) '(0 3))
                      '(normal #t)
                      (read-error "Incomplete #true")))
                ((char-ci=? c #\f)
                  (if (memv (gobble-chars port '(#\a #\l #\s #\e)) '(0 4))
                      '(normal #f)
                      (read-error "Incomplete #false")))
                ((memv c '(#\i #\e #\b #\o #\d #\x
                           #\I #\E #\B #\O #\D #\X))
                  (let ((num (read-number port (list #\# (char-downcase c)))))
                    (if num
                        `(normal ,num)
                        (read-error "Not a number after number start"))))
                ((char=? c #\( )  ; Vector.
                  (list 'normal (list->vector
                    (my-read-delimited-list no-indent-read #\) port))))
                ((char=? c #\u )  ; u8 Vector.
                  (cond
                    ((not (eqv? (my-read-char port) #\8 ))
                      (read-error "#u must be followed by 8"))
                    ((not (eqv? (my-read-char port) #\( ))
                      (read-error "#u8 must be followed by left paren"))
                    (#t (list 'normal (list->u8vector
                          (my-read-delimited-list no-indent-read #\) port))))))
                ((char=? c #\\)
                  (list 'normal (process-char port)))
                ; Handle #; (item comment).
                ((char=? c #\;)
                  (if (memv (my-peek-char port)
                            `(#\space #\tab ,linefeed ,carriage-return))
                    '(datum-commentw ())
                  (begin
                    (no-indent-read port)  ; Read the datum to be consumed.
                    scomment-result))) ; Return comment
                ; handle nested comments
                ((char=? c #\|)
                  (nest-comment port)
                  scomment-result) ; Return comment
                ((char=? c #\!)
                  (process-sharp-bang port))
                ((memv c digits) ; Datum label, #num# or #num=...
                  (let* ((my-digits (read-digits port))
                         (label (string->number (list->string
                                                   (cons c my-digits)))))
                    (cond
                      ((eqv? (my-peek-char port) #\#)
                        (my-read-char port)
                        (list 'normal (cons unmatched-datum-label-tag label)))
                      ((eqv? (my-peek-char port) #\=)
                        (my-read-char port)
                        (if (my-char-whitespace? (my-peek-char port))
                            (read-error "#num= followed by whitespace"))
                        (list 'normal
                          (patch-datum-label label (no-indent-read port))))
                      (#t
                        (read-error "Datum label #NUM requires = or #")))))
                ; R6RS abbreviations #' #` #, #,@
                ((char=? c #\')
                  '(abbrev syntax))
                ((char=? c #\`)
                  '(abbrev quasisyntax))
                ((char=? c #\,)
                  (let ((c2 (my-peek-char port)))
                    (cond
                      ((char=? c2 #\@)
                        (my-read-char port)
                        '(abbrev unsyntax-splicing))
                      (#t
                        '(abbrev unsyntax)))))
                ((or (char=? c #\space) (char=? c tab))
                  ; Extension - treat # (space|tab) as a comment to end of line.
                  ; This is not required by SRFI-105 or SRFI-110, but it's
                  ; handy because "# " is a comment-to-EOL in many other
                  ; languages (Bourne shells, Perl, Python, etc.)
                  (consume-to-eol port)
                  scomment-result) ; Return comment
                (#t #f)))

  (define (parse-cl no-indent-read c port)
    ; These are for Common Lisp; the "unsweeten" program
    ; can deal with the +++ ones.
    (cond
      ; In theory we could just abbreviate this as "function".
      ; However, this won't work in expressions like (a . #'b).
      ((char=? c #\')
        '(abbrev +++SHARP-QUOTE-abbreviation+++))
      ((char=? c #\:)
        '(abbrev +++SHARP-COLON-abbreviation+++))
      ((char=? c #\.)
        '(abbrev +++SHARP-DOT-abbreviation+++))
      ((char=? c #\+)
        '(abbrev +++SHARP-PLUS-abbreviation+++))
      ((char=? c #\P)
        '(abbrev +++SHARP-P-abbreviation+++))
      (#t #f)))

  ; Translate "x" to Common Lisp representation if we're printing CL.
  ; Basically we use a very unusual representation, and then translate it back
  (define (translate-cl x)
    (if common-lisp
      (case x
        ((quasiquote)       '+++CL-QUASIQUOTE-abbreviation+++)
        ((unquote)          '+++CL-UNQUOTE-abbreviation+++)
        ((unquote-splicing) '+++CL-UNQUOTE-SPLICING-abbreviation+++)
        (else x))
      x))
                  
  ; detect #| or |#
  (define (nest-comment fake-port)
    (let ((c (my-read-char fake-port)))
      (cond
        ((eof-object? c)
          (values))
        ((char=? c #\|)
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char fake-port)
                  (values))
                (nest-comment fake-port))))
        ((and hash-pipe-comment-nests? (char=? c #\#))
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\|)
                (begin
                  (my-read-char fake-port)
                  (nest-comment fake-port))
                (values))
            (nest-comment fake-port)))
        (#t
          (nest-comment fake-port)))))

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (my-read-char port) ; Remove .
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) period-symbol) ; period eof; return period.
        ((memv c '(#\' #\` #\, #\#)) period-symbol) ; End, for some CL code
        ((memv c digits) ; period digit - it's a number.
          (let ((num (read-number port (list #\.))))
            (if num
                num
                (read-error "period digit must be a number"))))
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))

  ; Read an inline hex escape (after \x), return the character it represents
  (define (read-inline-hex-escape port)
    (let* ((chars (read-until-delim port (append neoteric-delimiters '(#\;))))
           (n (string->number (list->string chars) 16)))
      (if (eqv? #\; (my-peek-char port))
          (my-read-char port)
          (read-error "Unfinished inline hex escape"))
      (if (not n)
          (read-error "Bad inline hex escape"))
      (integer->char n)))

  ; We're inside |...| ; return the list of characters inside.
  ; Do NOT call fold-case-maybe, because we always use literal values here.
  (define (read-symbol-elements port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c) '())
        ((eqv? c #\|)    '()) ; Expected end of symbol elements
        ((eqv? c #\\)
          (let ((c2 (my-read-char port)))
            (cond
              ((eof-object? c) '())
              ((eqv? c2 #\|)   (cons #\| (read-symbol-elements port)))
              ((eqv? c2 #\\)   (cons #\\ (read-symbol-elements port)))
              ((eqv? c2 #\a)   (cons (integer->char #x0007)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\b)   (cons (integer->char #x0008)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\t)   (cons (integer->char #x0009)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\n)   (cons (integer->char #x000a)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\r)   (cons (integer->char #x000d)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\f)   (cons (integer->char #x000c) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\v)   (cons (integer->char #x000b) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\x) ; inline hex escape =  \x hex-scalar-value ;
                (cons 
                      (read-inline-hex-escape port)
                      (read-symbol-elements port))))))
        (#t (cons c (read-symbol-elements port))))))

  ; Extension: When reading |...|, *include* the bars in the symbol, so that
  ; when we print it out later we know that there were bars there originally.
  (define (read-literal-symbol port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c) (read-error "EOF inside literal symbol"))
        ((eqv? c #\|)    '(#\|)) ; Expected end of symbol elements
        ((eqv? c #\\)
          (let ((c2 (my-read-char port)))
            (if (eof-object? c)
              (read-error "EOF after \\ in literal symbol")
              (cons c (cons c2 (read-literal-symbol port))))))
        (#t (cons c (read-literal-symbol port))))))

  ; Read |...| symbol (like Common Lisp)
  ; This is present in R7RS draft 9.
  (define (get-barred-symbol port)
    (my-read-char port) ; Consume the initial vertical bar.
    (string->symbol (list->string
      (if literal-barred-symbol
        (cons #\| (read-literal-symbol port))
        (read-symbol-elements port)))))

  ; This implements a simple Scheme "read" implementation from "port",
  ; but if it must recurse to read, it will invoke "no-indent-read"
  ; (a reader that is NOT indentation-sensitive).
  ; This additional parameter lets us easily implement additional semantics,
  ; and then call down to this underlying-read procedure when basic reader
  ; procedureality (implemented here) is needed.
  ; This lets us implement both a curly-infix-ONLY-read
  ; as well as a neoteric-read, without duplicating code.
  (define (underlying-read no-indent-read port)
    (consume-whitespace port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\")
          ; old readers tend to read strings okay, call it.
          ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
          (invoke-read default-scheme-read port))
        (#t
          ; attach the source information to the item read-in
          (attach-sourceinfo pos
            (cond
              ((char=? c #\#)
                (my-read-char port)
                (let ((rv (process-sharp no-indent-read port)))
                  (cond
                    ((eq? (car rv) 'scomment) (no-indent-read port))
                    ((eq? (car rv) 'datum-commentw)
                      (no-indent-read port) ; Consume following datum.
                      (no-indent-read port))
                    ((eq? (car rv) 'normal) (cadr rv))
                    ((eq? (car rv) 'abbrev)
                      (list (cadr rv) (no-indent-read port)))
                    (#t   (read-error "Unknown # sequence")))))
              ((char=? c #\.) (process-period port))
              ((or (memv c digits) (char=? c #\+) (char=? c #\-))
                (let*
                  ((maybe-number (list->string
                     (read-until-delim port neoteric-delimiters)))
                   (as-number (string->number maybe-number)))
                  (if as-number
                      as-number
                      (string->symbol (fold-case-maybe port maybe-number)))))
              ((char=? c #\')
                (my-read-char port)
                (list (attach-sourceinfo pos 'quote)
                  (no-indent-read port)))
              ((char=? c #\`)
                (my-read-char port)
                (list (attach-sourceinfo pos (translate-cl 'quasiquote))
                  (no-indent-read port)))
              ((char=? c #\,)
                (my-read-char port)
                  (cond
                    ((char=? #\@ (my-peek-char port))
                      (my-read-char port)
                      (list (attach-sourceinfo pos
                               (translate-cl 'unquote-splicing))
                       (no-indent-read port)))
                   (#t
                    (list (attach-sourceinfo pos (translate-cl 'unquote))
                      (no-indent-read port)))))
              ((char=? c #\( )
                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\) port))
              ((char=? c #\) )
                (my-read-char port)
                (read-error "Closing parenthesis without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\[ )
                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\] port))
              ((char=? c #\] )
                (my-read-char port)
                (read-error "Closing bracket without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\} )
                (my-read-char port)
                (read-error "Closing brace without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\| )
                ; Read |...| symbol (like Common Lisp and R7RS draft 9)
                (get-barred-symbol port))
              (#t ; Nothing else.  Must be a symbol start.
                (string->symbol (fold-case-maybe port
                  (list->string
                    (read-until-delim port neoteric-delimiters)))))))))))

; -----------------------------------------------------------------------------
; Curly Infix
; -----------------------------------------------------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (equal? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ; Return alternating parameters in a list (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
        lyst
        (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
  (define (transform-mixed-infix lyst)
     (cons '$nfx$ lyst))

  ; Given curly-infix lyst, map it to its final internal format.
  (define (process-curly lyst)
    (cond
     ((not (pair? lyst)) lyst) ; E.G., map {} to ().
     ((null? (cdr lyst)) ; Map {a} to a.
       (car lyst))
     ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
       lyst)
     ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
       (cons (cadr lyst) (alternating-parameters lyst)))
     (#t  (transform-mixed-infix lyst))))


  (define (curly-infix-read-real no-indent-read port)
    (let* ((pos (get-sourceinfo port))
            (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((eqv? c #\;)
          (consume-to-eol port)
          (curly-infix-read-real no-indent-read port))
        ((my-char-whitespace? c)
          (my-read-char port)
          (curly-infix-read-real no-indent-read port))
        ((eqv? c #\{)
          (my-read-char port)
          ; read in as infix
          (attach-sourceinfo pos
            (process-curly
              (my-read-delimited-list neoteric-read-real #\} port))))
        (#t
          (underlying-read no-indent-read port)))))

  ; Read using curly-infix-read-real
  (define (curly-infix-read-nocomment port)
    (curly-infix-read-real curly-infix-read-nocomment port))

; -----------------------------------------------------------------------------
; Neoteric Expressions
; -----------------------------------------------------------------------------

  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
  (define (neoteric-process-tail port prefix)
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        (cond
          ((eof-object? c) prefix)
          ((char=? c #\( ) ; Implement f(x)
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos (cons prefix
                  (my-read-delimited-list neoteric-read-nocomment #\) port)))))
          ((char=? c #\[ )  ; Implement f[x]
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos
              (cons (attach-sourceinfo pos '$bracket-apply$)
                (cons prefix
                  (my-read-delimited-list neoteric-read-nocomment #\] port))))))
          ((char=? c #\{ )  ; Implement f{x}
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos
              (let
                ((tail (process-curly
                   (my-read-delimited-list neoteric-read-nocomment #\} port))))
                (if (eqv? tail '())
                    (list prefix) ; Map f{} to (f), not (f ()).
                    (list prefix tail))))))
          (#t prefix))))


  ; This is the "real" implementation of neoteric-read.
  ; It directly implements unprefixed (), [], and {} so we retain control;
  ; it calls neoteric-process-tail so f(), f[], and f{} are implemented.
  (define (neoteric-read-real port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port))
       (result
         (cond
           ((eof-object? c) c)
           ((char=? c #\( )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\) port)))
           ((char=? c #\[ )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\] port)))
           ((char=? c #\{ )
             (my-read-char port)
             (attach-sourceinfo pos
               (process-curly
                 (my-read-delimited-list neoteric-read-nocomment #\} port))))
           ((my-char-whitespace? c)
             (my-read-char port)
             (neoteric-read-real port))
           ((eqv? c #\;)
             (consume-to-eol port)
             (neoteric-read-real port))
           (#t (underlying-read neoteric-read-nocomment port)))))
      (if (eof-object? result)
          result
          (neoteric-process-tail port result))))

  (define (neoteric-read-nocomment port)
    (neoteric-read-real port))

; -----------------------------------------------------------------------------
; Sweet Expressions (this implementation maps to the BNF)
; -----------------------------------------------------------------------------

  ; There is no standard Scheme mechanism to unread multiple characters.
  ; Therefore, the key productions and some of their supporting procedures
  ; return both the information on what ended their reading process,
  ; as well the actual value (if any) they read before whatever stopped them.
  ; That way, procedures can process the value as read, and then pass on
  ; the ending information to whatever needs it next.  This approach,
  ; which we call a "non-tokenizing" implementation, implements a tokenizer
  ; via procedure calls instead of needing a separate tokenizer.
  ; The ending information can be:
  ; - "stopper" - this is returned by productions etc. that do NOT
  ;     read past the of a line (outside of paired characters and strings).
  ;     It is 'normal if it ended normally (e.g., at end of line); else it's
  ;     'sublist-marker ($), 'group-split-marker (\\), 'collecting (<*),
  ;     'collecting-end (*>), 'scomment (special comments like #|...|#), or
  ;     'abbrevw (initial abbreviation with whitespace after it).
  ; - "new-indent" - this is returned by productions etc. that DO read
  ;     past the end of a line.  Such productions typically read the
  ;     next line's indent to determine if they should return.
  ;     If they should, they return the new indent so callers can
  ;     determine what to do next.  A "*>" should return even though its
  ;     visible indent level is length 0; we handle this by prepending
  ;     all normal indents with "^", and "*>" generates a length-0 indent
  ;     (which is thus shorter than even an indent of 0 characters).

  (define-syntax let-splitter
    (syntax-rules ()
      ((let-splitter (full first-value second-value) expr body ...)
        (let* ((full expr)
               (first-value (car full))
               (second-value (cadr full)))
               body ...))))
  ; Note: If your Lisp has macros, but doesn't support hygenic macros,
  ; it's probably trivial to reimplement this.  E.G., in Common Lisp:
  ; (defmacro let-splitter ((full first-value second-value) expr &rest body)
  ;   `(let* ((,full ,expr)
  ;           (,first-value (car ,full))
  ;           (,second-value (cadr ,full)))
  ;          ,@body))

  (define group-split (string->symbol "\\\\"))
  (define group-split-char #\\ ) ; First character of split symbol.
  (define non-whitespace-indent #\!) ; Non-whitespace-indent char.
  (define sublist (string->symbol "$"))
  (define sublist-char #\$) ; First character of sublist symbol.

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
            (len2 (string-length indentation2)))
      (and (> len1 len2)
             (string=? indentation2 (substring indentation1 0 len2)))))

  ; Does character "c" begin a line comment (;) or end-of-line?
  (define initial-comment-eol (list #\; #\newline carriage-return))
  (define (lcomment-eol? c)
    (or
       (eof-object? c)
       (memv c initial-comment-eol)))

  ; Return #t if char is space or tab.
  (define (char-hspace? char)
    (or (eqv? char #\space)
        (eqv? char tab)))

  ; Consume 0+ spaces or tabs
  (define (hspaces port)
    (cond ; Use "cond" as "when" for portability.
      ((char-hspace? (my-peek-char port))
        (my-read-char port)
        (hspaces port))))

  ; Return #t if char is space, tab, or !
  (define (char-ichar? char)
    (or (eqv? char #\space)
        (eqv? char tab)
        (eqv? char non-whitespace-indent)))

  (define (accumulate-ichar port)
    (if (char-ichar? (my-peek-char port))
        (cons (my-read-char port) (accumulate-ichar port))
        '()))

  (define (consume-ff-vt port)
    (let ((c (my-peek-char port)))
      (cond
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (my-read-char port)
          (consume-ff-vt port)))))

  ; Do 2-item append, but report read-error if the LHS is not a proper list.
  ; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
  (define (my-append lhs rhs)
    (cond
      ((eq? lhs empty-value) rhs)
      ((eq? rhs empty-value) lhs)
      ((list? lhs) (append lhs rhs))
      (#t
        (read-error "Must have proper list on left-hand-side to append data"))))

  ; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
  ; else returns ('normal n-expr).
  ; Note: If a *value* begins with #, process any potential neoteric tail,
  ; so weird constructs beginning with "#" like #f() will still work.
  (define (n-expr-or-scomment port)
    (if (eqv? (my-peek-char port) #\#)
        (let* ((consumed-sharp (my-read-char port))
               (result (process-sharp neoteric-read-nocomment port)))
          (cond
            ((eq? (car result) 'normal)
              (list 'normal (neoteric-process-tail port (cadr result))))
            ((eq? (car result) 'abbrev)
              (list 'normal
                (list (cadr result) (neoteric-read-nocomment port))))
            ((pair? result) result)
            (#t (read-error "Unsupported hash"))))
        (list 'normal (neoteric-read-nocomment port))))

  ; Read an n-expression.  Returns ('normal n-expr) in most cases;
  ; if it's a special marker, the car is the marker name instead of 'normal.
  ; Markers only have special meaning if their first character is
  ; the "normal" character, e.g., {$} is not a sublist.
  ; Call "process-sharp" if first char is "#".
  (define (n-expr port)
    (let ((c (my-peek-char port)))
      (let-splitter (results type expr)
                    (n-expr-or-scomment port)
        (if (eq? (car results) 'scomment)
            results
            (cond
              ((and (eq? expr sublist) (eqv? c sublist-char))
                (list 'sublist-marker '()))
              ((and (eq? expr group-split) (eqv? c group-split-char))
                (list 'group-split-marker '()))
              ((and (eq? expr '<*) (eqv? c #\<))
                (list 'collecting '()))
              ((and (eq? expr '*>) (eqv? c #\*))
                (list 'collecting-end '()))
              ((and (eq? expr '$$$) (eqv? c #\$))
                (read-error "$$$ is reserved"))
              ((and (eq? expr period-symbol) (eqv? c #\.))
                (list 'period-marker '()))
              (#t
                results))))))

  ; Check if we have abbrev+whitespace.  If the current peeked character
  ; is one of certain whitespace chars,
  ; return 'abbrevw as the marker and abbrev-procedure
  ; as the value (the cadr). Otherwise, return ('normal n-expr).
  ; We do NOT consume the peeked char (so EOL can be examined later).
  ; Note that this calls the neoteric-read procedure directly, because
  ; quoted markers are no longer markers. E.G., '$ is just (quote $).
  (define (maybe-initial-abbrev port abbrev-procedure)
    (let ((c (my-peek-char port)))
      (if (or (char-hspace? c) (eqv? c carriage-return) (eqv? c linefeed))
          (list 'abbrevw abbrev-procedure)
          (list 'normal
            (list abbrev-procedure (neoteric-read-nocomment port))))))

  ; Read the first n-expr on a line; handle abbrev+whitespace specially.
  ; Returns ('normal VALUE) in most cases.
  (define (n-expr-first port)
    (case (my-peek-char port)
      ((#\') 
        (my-read-char port)
        (maybe-initial-abbrev port 'quote))
      ((#\`) 
        (my-read-char port)
        (maybe-initial-abbrev port (translate-cl 'quasiquote)))
      ((#\,) 
        (my-read-char port)
        (if (eqv? (my-peek-char port) #\@)
            (begin
              (my-read-char port)
              (maybe-initial-abbrev port (translate-cl 'unquote-splicing)))
            (maybe-initial-abbrev port (translate-cl 'unquote))))
      ((#\#) 
        (let* ((consumed-sharp (my-read-char port))
               (result (process-sharp neoteric-read-nocomment port)))
          (cond
            ((eq? (car result) 'normal)
              (list 'normal (neoteric-process-tail port (cadr result))))
            ((eq? (car result) 'abbrev)
              (maybe-initial-abbrev port (cadr result)))
            (#t result))))
      (else
        (n-expr port))))

  ; Consume ;-comment (if there), consume EOL, and return new indent.
  ; Skip ;-comment-only lines; a following indent-only line is empty.
  (define (get-next-indent port)
    (consume-to-eol port)
    (consume-end-of-line port)
    (let* ((indentation-as-list (cons #\^ (accumulate-ichar port)))
           (c (my-peek-char port)))
      (cond
        ((eqv? c #\;)  ; A ;-only line, consume and try again.
          (get-next-indent port))
        ((lcomment-eol? c) ; Indent-only line
          (if (memv #\! indentation-as-list)
              (get-next-indent port)
              "^"))
        (#t (list->string indentation-as-list)))))

  ; Implement (scomment hs | datum-commentw hs n-expr hs)
  (define (skippable stopper port)
    (cond
    ((eq? stopper 'scomment)
      (hspaces port))
    ((eq? stopper 'datum-commentw)
      (hspaces port)
      (if (not (lcomment-eol? (my-peek-char port)))
        (begin
          (n-expr port)
          (hspaces port))
        (read-error "Datum comment start not followed a datum (EOL instead)")))
    (#t (read-error "skippable: Impossible case"))))

  ; Utility declarations and functions

  (define empty-value (string-copy "empty-value")) ; Represent no value at all

  (define (conse x y) ; cons, but handle "empty" values
    (cond
      ((eq? y empty-value) x)
      ((eq? x empty-value) y)
      (#t (cons x y))))

  (define (appende x y) ; append, but handle "empty" values
    (cond
      ((eq? y empty-value) x)
      ((eq? x empty-value) y)
      (#t (append y))))

  (define (list1e x) ; list, but handle "empty" values
    (if (eq? x empty-value)
        '()
        (list x)))

  (define (list2e x y) ; list, but handle "empty" values
    (if (eq? x empty-value)
        y
        (if (eq? y empty-value)
           x
           (list x y))))

  ; If x is a 1-element list, return (car x), else return x
  (define (monify x)
    (cond
      ((not (pair? x)) x)
      ((null? (cdr x)) (car x))
      (#t x)))

  ; Return contents (value) of collecting-content.  It does *not* report a
  ; stopper or ending indent, because it is *ONLY* stopped by collecting-end
  (define (collecting-content port)
    (let* ((c (my-peek-char port)))
      (cond
        ((eof-object? c)
         (read-error "Collecting tail: EOF before collecting list ended"))
        ((lcomment-eol? c)
          (consume-to-eol port)
          (consume-end-of-line port)
          (collecting-content port))
        ((char-ichar? c)
          (let* ((indentation (accumulate-ichar port))
                 (c (my-peek-char port)))
            (if (lcomment-eol? c)
                (collecting-content port)
                (read-error "Collecting tail: Only ; after indent"))))
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (consume-ff-vt port)
          (if (lcomment-eol? (my-peek-char port))
              (collecting-content port)
              (read-error "Collecting tail: FF and VT must be alone on line")))
        (#t
          (let-splitter (it-full-results it-new-indent it-value)
                        (it-expr port "^")
            (cond
              ((string=? it-new-indent "")
                ; Specially compensate for "*>" at the end of a line if it's
                ; after something else.  This must be interpreted as EOL *>,
                ; which would cons a () after the result.
                ; Directly calling list for a non-null it-value has
                ; the same effect, but is a lot quicker and simpler.
                (cond
                  ((null? it-value) it-value)
                  ((eq? it-value empty-value) '())
                  (#t (list it-value))))
              (#t (conse it-value (collecting-content port)))))))))

  ; Skip scomments and error out if we have a normal n-expr, implementing:
  ;    skippable* (n-expr error)?
  (define (n-expr-error port full)
    (if (not (eq? (car full) 'normal))
        (read-error "BUG! n-expr-error called but stopper not normal"))
    (if (lcomment-eol? (my-peek-char port))
        full ; All done!
        (let-splitter (n-full-results n-stopper n-value)
                      (n-expr port)
          (cond
            ((or (eq? n-stopper 'scomment) (eq? n-stopper 'datum-commentw))
              (skippable n-stopper port)
              (n-expr-error port full))
            ((eq? n-stopper 'normal)
              (read-error "Illegal second value after ."))
            (#t ; We found a stopper, return it with the value from "full"
              (list n-stopper (cadr full)))))))

  ; Returns (stopper value-after-period)
  (define (post-period port)
    (if (not (lcomment-eol? (my-peek-char port)))
        (let-splitter (pn-full-results pn-stopper pn-value)
                      (n-expr port)
          (cond
            ((or (eq? pn-stopper 'scomment) (eq? pn-stopper 'datum-commentw))
              (skippable pn-stopper port)
              (post-period port))
            ((eq? pn-stopper 'normal)
              (hspaces port)
              (n-expr-error port pn-full-results))
            ((eq? pn-stopper 'collecting)
              (hspaces port)
              (let ((cl (collecting-content port)))
                (hspaces port)
                (n-expr-error port (list 'normal cl))))
            ((eq? pn-stopper 'period-marker)
              (list 'normal period-symbol))
            (#t ; Different stopper; respond as empty branch with that stopper
              (list pn-stopper (list period-symbol)))))
        (list 'normal period-symbol))) ; Empty branch.

  ; Returns (stopper computed-value).
  ; The stopper may be 'normal, 'scomment (special comment),
  ; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
  (define (line-exprs port)
    (let-splitter (basic-full-results basic-special basic-value)
                  (n-expr-first port)
      (cond
        ((eq? basic-special 'collecting)
          (hspaces port)
          (let* ((cl-results (collecting-content port)))
            (hspaces port)
            (if (not (lcomment-eol? (my-peek-char port)))
                (let-splitter (rr-full-results rr-stopper rr-value)
                              (rest-of-line port)
                  (list rr-stopper (cons cl-results rr-value)))
                (list 'normal (list cl-results)))))
        ((eq? basic-special 'period-marker)
          (if (char-hspace? (my-peek-char port))
              (begin
                (hspaces port)
                (let-splitter (ct-full-results ct-stopper ct-value)
                              (post-period port)
                  (list ct-stopper (list ct-value))))
              (list 'normal (list period-symbol))))
        ((not (eq? basic-special 'normal)) basic-full-results)
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (let-splitter (br-full-results br-stopper br-value)
                            (rest-of-line port)
                (list br-stopper (cons basic-value br-value)))
              (list 'normal (list basic-value))))
        (#t 
          (list 'normal (list basic-value))))))

  ; Returns (stopper computed-value); stopper may be 'normal, etc.
  ; Read in one n-expr, then process based on whether or not it's special.
  (define (rest-of-line port)
    (let-splitter (basic-full-results basic-special basic-value)
                  (n-expr port)
      (cond
        ((or (eq? basic-special 'scomment) (eq? basic-special 'datum-commentw))
          (skippable basic-special port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (rest-of-line port)
              (list 'normal '())))
        ((eq? basic-special 'collecting)
          (hspaces port)
          (let* ((cl-results (collecting-content port)))
            (hspaces port)
            (if (not (lcomment-eol? (my-peek-char port)))
                (let-splitter (rr-full-results rr-stopper rr-value)
                              (rest-of-line port)
                  (list rr-stopper (cons cl-results rr-value)))
                (list 'normal (list cl-results)))))
        ((eq? basic-special 'period-marker)
          (if (char-hspace? (my-peek-char port))
              (begin
                (hspaces port)
                (post-period port))
              ; (list 'normal (list period-symbol)) ; To interpret as |.|
              (read-error "Cannot end line with '.'")))
        ((not (eq? basic-special 'normal)) (list basic-special '())) 
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (let-splitter (br-full-results br-stopper br-value)
                            (rest-of-line port)
                (list br-stopper (cons basic-value br-value)))
              (list 'normal (list basic-value))))
        (#t (list 'normal (list basic-value))))))

  ; Returns (new-indent computed-value)
  (define (body port starting-indent)
    (let-splitter (i-full-results i-new-indent i-value)
                  (it-expr port starting-indent)
      (if (string=? starting-indent i-new-indent)
          (if (eq? i-value period-symbol)
              (let-splitter (f-full-results f-new-indent f-value)
                            (it-expr port i-new-indent)
                (if (not (indentation>? starting-indent f-new-indent))
                    (read-error "Dedent required after lone . and value line"))
                (list f-new-indent f-value)) ; final value of improper list
              (if (eq? i-value empty-value)
                (body port i-new-indent)
                (let-splitter (nxt-full-results nxt-new-indent nxt-value)
                              (body port i-new-indent)
                  (list nxt-new-indent (cons i-value nxt-value)))))
          (list i-new-indent (list1e i-value))))) ; dedent - end list.

  ; Returns (new-indent computed-value)
  (define (it-expr-real port starting-indent)
    (let-splitter (line-full-results line-stopper line-value)
                  (line-exprs port)
      (if (and (not (null? line-value)) (not (eq? line-stopper 'abbrevw)))
          ; Production line-exprs produced at least one n-expression:
          (cond
            ((eq? line-stopper 'group-split-marker)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "Cannot follow split with end of line")
                  (list starting-indent (monify line-value))))
            ((eq? line-stopper 'sublist-marker)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "EOL illegal immediately after sublist"))
              (let-splitter (sub-i-full-results sub-i-new-indent sub-i-value)
                            (it-expr port starting-indent)
                (list sub-i-new-indent
                  (my-append line-value (list sub-i-value)))))
            ((eq? line-stopper 'collecting-end)
              ; Note that indent is "", forcing dedent all the way out.
              (list ""
                (if (eq? line-value empty-value)
                  '()
                  (monify line-value))))
            ((lcomment-eol? (my-peek-char port))
              (let ((new-indent (get-next-indent port)))
                (if (indentation>? new-indent starting-indent)
                    (let-splitter (body-full body-new-indent body-value)
                                  (body port new-indent)
                      (list body-new-indent (my-append line-value body-value)))
                    (list new-indent (monify line-value)))))
            (#t
              (read-error "Unexpected text after n-expression")))
          ; line-exprs begins with something special like GROUP-SPLIT:
          (cond
            ((eq? line-stopper 'datum-commentw)
              (hspaces port)
              (cond
                ((not (lcomment-eol? (my-peek-char port)))
                  (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                                (it-expr port starting-indent)
                    (list is-i-new-indent empty-value)))
                (#t
                  (let ((new-indent (get-next-indent port)))
                    (if (indentation>? new-indent starting-indent)
                      (let-splitter
                           (body-full-results body-new-indent body-value)
                           (body port new-indent)
                        (list body-new-indent empty-value))
                      (read-error "#;+EOL must be followed by indent"))))))
            ((or (eq? line-stopper 'group-split-marker)
                 (eq? line-stopper 'scomment))
              (hspaces port)
              (if (not (lcomment-eol? (my-peek-char port)))
                  (it-expr port starting-indent) ; Skip and try again.
                  (let ((new-indent (get-next-indent port)))
                    (cond
                      ((indentation>? new-indent starting-indent)
                        (body port new-indent))
                      (#t
                        (list new-indent empty-value))))))
            ((eq? line-stopper 'sublist-marker)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "EOL illegal immediately after solo sublist"))
              (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                            (it-expr port starting-indent)
                (list is-i-new-indent (list1e is-i-value))))
            ((eq? line-stopper 'abbrevw)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (let ((new-indent (get-next-indent port)))
                    (if (not (indentation>? new-indent starting-indent))
                        (read-error "Indent required after abbreviation"))
                    (let-splitter (ab-full-results ab-new-indent ab-value)
                                  (body port new-indent)
                      (list ab-new-indent
                        (append (list line-value) ab-value))))
                  (let-splitter (ai-full-results ai-new-indent ai-value)
                                (it-expr port starting-indent)
                    (list ai-new-indent
                      (list2e line-value ai-value)))))
            ((eq? line-stopper 'collecting-end)
              (list "" line-value))
            (#t 
              (read-error "Initial line-expression error"))))))

  ; Read it-expr.  This is a wrapper that attaches source info
  ; and checks for consistent indentation results.
  (define (it-expr port starting-indent)
    (let ((pos (get-sourceinfo port)))
      (let-splitter (results results-indent results-value)
                    (it-expr-real port starting-indent)
        (if (indentation>? results-indent starting-indent)
            (read-error "Inconsistent indentation"))
        (list results-indent (attach-sourceinfo pos results-value)))))

  (define (initial-indent-expr-tail port)
    (if (not (memv (my-peek-char port) initial-comment-eol))
        (let-splitter (results results-stopper results-value)
                      (n-expr-or-scomment port)
          (cond
            ((memq results-stopper '(scomment datum-commentw))
              (skippable results-stopper port)
              (initial-indent-expr-tail port))
            (#t results-value))) ; Normal n-expr, return one value.
        (begin
          (consume-to-eol port)
          (consume-end-of-line port)
          empty-value))) ; (t-expr-real port)

  ; Top level - read a sweet-expression (t-expression).  Handle special
  ; cases, such as initial indent; call it-expr for normal case.
  (define (t-expr-real port)
    (let* ((c (my-peek-char port)))
      (cond
        ; Check EOF early (a bug in guile before 2.0.8 consumes EOF on peek)
        ((eof-object? c) c)
        ((lcomment-eol? c)
          (consume-to-eol port)
          (consume-end-of-line port)
          (t-expr-real port))
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (consume-ff-vt port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (read-error "FF and VT must be alone on line in a sweet-expr"))
          (t-expr-real port))
        ((char-ichar? c) ; initial-indent-expr
          (accumulate-ichar port) ; consume and throw away ichars
          (initial-indent-expr-tail port))
        (#t
          (let-splitter (results results-indent results-value)
                        (it-expr port "^")
            (if (string=? results-indent "")
                (read-error "Closing *> without preceding matching <*")
                results-value))))))

  ; Top level - read a sweet-expression (t-expression).  Handle special
  (define (t-expr port)
    (let* ((te (t-expr-real port)))
      (if (eq? te empty-value)
          (t-expr port)
          te)))

  ; Skip until we find a line with 0 indent characters.
  ; We use this after read error to resync to good input.
  (define (read-to-unindented-line port)
    (let* ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char-line-ending? c)
          (consume-end-of-line port)
          (if (char-ichar? (my-peek-char port))
            (read-to-unindented-line port)))
        (#t
          (consume-to-eol port)
          (consume-end-of-line port)
          (read-to-unindented-line port)))))

  ; Call on sweet-expression reader - use guile's nonstandard catch/throw
  ; so that errors will force a restart.
  (define (t-expr-catch port)

    ; Default guile stack size is FAR too small
    (debug-set! stack 500000)

    (catch 'readable
      (lambda () (t-expr port))
      (lambda (key . args) (read-to-unindented-line port) (t-expr-catch port))))

; -----------------------------------------------------------------------------
; Write routines
; -----------------------------------------------------------------------------

  ; A list with more than this length and no pairs is considered "boring",
  ; and thus is presumed to NOT be a procedure call or execution sequence.
  (define boring-length 16)

  (define special-infix-operators
    '(and or xor))

  (define punct-chars
    (list #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
          #\.  #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
          #\_ #\` #\{ #\| #\} #\~))

  ; Returns #t if x is a list with exactly 1 element.  Improper lists are #f.
  (define (list1? x)
    (and (pair? x) (null? (cdr x))))

  ; Returns #t if x is a list with exactly 2 elements.  Improper lists are #f.
  (define (list2? x)
    (and (pair? x) (pair? (cdr x)) (null? (cddr x))))

  ; Does x contain a list of ONLY punctuation characters?
  ; An empty list is considered true.
  (define (contains-only-punctuation? x)
    (cond ((null? x) #t)
          ((not (pair? x)) #f)
          ((memq (car x) punct-chars)
           (contains-only-punctuation? (cdr x)))
          (#t #f)))

  ; Returns #t if x is a symbol that would typically be used in infix position.
  (define (is-infix-operator? x)
    (cond ((not (symbol? x)) #f)
          ((memq x special-infix-operators) #t)
          (#t
           (contains-only-punctuation?
             (string->list (symbol->string x))))))

  ; A possibly-improper list is long and boring if its length is at least
  ; num-to-go long and it's boring (it contains no pairs up to that length).
  ; A long-and-boring list is almost certainly NOT a function call or a
  ; body of some executable sequence - it's almost certainly a long
  ; boring list of data instead. If it is, we want to display it differently.
  ; This doesn't get stuck on circular lists; it always terminates after
  ; num-to-go iterations.
  (define (long-and-boring? x num-to-go)
    (cond
      ((pair? (car x)) #f)
      ((not (pair? (cdr x))) #f)
      ((<= num-to-go 1) #t)
      (#t (long-and-boring? (cdr x) (- num-to-go 1)))))

  (define (list-no-longer-than? x num-to-go)
    (cond
      ((not (pair? x)) #f)
      ((null? (cdr x)) #t) ; This is the last one!
      ((not (pair? (cdr x))) #f)
      ((<= num-to-go 0) #f)
      (#t (list-no-longer-than? (cdr x) (- num-to-go 1)))))

  ; Return #t if x should be represented using curly-infix notation {...}.
  (define (represent-as-infix? x)
    (and (pair? x)
         (pair? (cdr x))                ; At least 2 elements.
         (is-infix-operator? (car x))
         (list-no-longer-than? x 6)))

  (define (represent-as-inline-infix? x)
    (and (represent-as-infix? x) (not (list2? x)))) ; Must be 3+ elements

  ; Return #t if x should be represented as a brace suffix
  (define (represent-as-brace-suffix? x)
    (represent-as-infix? x))

  ; Define an association list mapping the Scheme procedure names which have
  ; abbreviations ==> the list of characters in their abbreviation
  (define abbreviations
    '((quote (#\'))
      (quasiquote (#\`))
      (unquote (#\,))
      (unquote-splicing (#\, #\@))
      ; Scheme syntax-rules. Note that this will abbreviate any 2-element
      ; list whose car is "syntax", whether you want that or not!
      (syntax  (#\# #\'))
      (quasisyntax (#\# #\`))
      (unsyntax-splicing (#\# #\, #\@)
      (unsyntax (#\# #\,)))))

  ; return #t if we should as a traditional abbreviation, e.g., '
  (define (represent-as-abbreviation? x)
    (and (list2? x)
         (assq (car x) abbreviations)))

  ; The car(x) is the symbol for an abbreviation; write the abbreviation.
  (define (write-abbreviation x port)
    (for-each (lambda (c) (display c port))
      (cadr (assq (car x) abbreviations))))


  ; Return list x's *contents* represented as a list of characters.
  ; Each one must use neoteric-expressions, space-separated;
  ; it will be surrounded by (...) so no indentation processing is relevant.
  (define (n-write-list-contents x port)
    (cond
      ((null? x) (values))
      ((pair? x)
        (n-write-simple (car x) port)
        (cond ((not (null? (cdr x)))
          (display " " port)
          (n-write-list-contents (cdr x) port))))
      (#t
        (display ". " port)
        (n-write-simple x port))))

  (define (c-write-list-contents x port)
    (cond
      ((null? x) (values))
      ((pair? x)
        (c-write-simple (car x) port)
        (cond ((not (null? (cdr x)))
          (display " " port)
          (c-write-list-contents (cdr x) port))))
      (#t
        (display ". " port)
        (c-write-simple x port))))

  ; Return tail of an infix expression, as list of chars
  ; The "op" is the infix operator represented as a list of chars.
  (define (infix-tail op x port)
    (cond
      ((null? x) (display "}" port))
      ((pair? x)
        (display " " port)
        (n-write-simple op port)
        (display " " port)
        (n-write-simple (car x) port)
        (infix-tail op (cdr x) port))
      (#t
        (display " " port)
        (n-write-simple x port)
        (display "}" port))))

  ; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
  (define (as-brace-suffix x port)
    (display "{" port)
    (if (list2? x)
      (begin
        (n-write-list-contents x port)
        (display "}" port))
      (begin
        (n-write-simple (cadr x) port)
        (infix-tail (car x) (cddr x) port))))

  (define (n-write-simple x port)
    (cond
      ((pair? x)
        (cond
          ((represent-as-abbreviation? x)              ; Format 'x
            (write-abbreviation x port)
            (n-write-simple (cadr x) port))
          ((long-and-boring? x boring-length)          ; Format (a b c ...)
            (display "(" port)
            (n-write-list-contents x port)
            (display ")" port))
          ((symbol? (car x))
            (cond
              ((represent-as-inline-infix? x)          ; Format {a + b}
                (display "{" port)
                (n-write-simple (cadr x) port)
                (infix-tail (car x) (cddr x) port))
              ((and (list1? (cdr x))                   ; Format f{...}
                (pair? (cadr x))
                (represent-as-infix? (cadr x)))
                  (n-write-simple (car x) port)
                  (as-brace-suffix (cadr x) port))
              (#t                                      ; Format f(...)
                (n-write-simple (car x) port)
                (display "(" port)
                (n-write-list-contents (cdr x) port)
                (display ")" port))))
          (#t                                          ; Format (1 2 3 ...)
            (display "(" port)
            (n-write-list-contents x port)
            (display ")" port))))
      ((vector? x)
        (display "#( " port) ; Surround with spaces, easier to implement.
        (for-each (lambda (v) (n-write-simple v port) (display " " port))
          (vector->list x))
        (display ")" port))
      (#t (write x port))))                            ; Default format.


  (define (c-write-simple x port)
    (cond
      ((pair? x)
        (cond
          ((represent-as-abbreviation? x)              ; Format 'x
            (write-abbreviation x port)
            (c-write-simple (cadr x) port))
          ((represent-as-inline-infix? x)              ; Format {a + b}
            (display "{" port)
            (n-write-simple (cadr x) port)
            (infix-tail (car x) (cddr x) port))
          (#t                                          ; Format (1 2 3 ...)
            (display "(" port)
            (c-write-list-contents x port)
            (display ")" port))))
      ((vector? x)
        (display "#( " port) ; Surround with spaces, easier to implement.
        (for-each (lambda (v) (c-write-simple v port) (display " " port))
          (vector->list x))
        (display ")" port))
      (#t (write x port))))                            ; Default format.


  ; Front entry - Use default port if none provided.
  (define (neoteric-write-simple x . rest)
    (if (pair? rest)
        (n-write-simple x (car rest))
        (n-write-simple x (current-output-port))))

  ; Front entry - Use default port if none provided.
  (define (curly-write-simple x . rest)
    (if (pair? rest)
        (c-write-simple x (car rest))
        (c-write-simple x (current-output-port))))


  ;; Write routines for cyclic and shared structures, based on srfi-38.
  ;; The original code was written by Alex Shinn in 2009 and placed in the
  ;; Public Domain.  All warranties are disclaimed.
  ;; Extracted from Chibi Scheme 0.6.1.


  ; We need hash tables for an efficient implementation.
  ; John Cowan on 10 April 2013 said:
  ; "... though not part of R7RS-small,
  ; SRFI 69 hash tables are fairly pervasive, because they are
  ; simple, have a reference implementation, and can be built on top of
  ; R6RS hashtables (which are standard). The only reason they aren't in
  ; more Schemes is that as SRFIs go, 69 is a fairly recent one. I wouldn't
  ; hesitate to use them."

  (define (extract-shared-objects x cyclic-only?)
    (let ((seen (srfi-69-make-hash-table eq?)))
      ;; find shared references
      (let find ((x x))
        (let ((type (type-of x)))
          (cond ;; only interested in pairs, vectors and records
           ((or (pair? x) (vector? x) (and type (type-printer type)))
            ;; increment the count
            (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
            ;; walk if this is the first time
            (cond
             ((> (hash-table-ref seen x) 1))
             ((pair? x)
              (find (car x))
              (find (cdr x)))
             ((vector? x)
              (do ((i 0 (+ i 1)))
                  ((= i (vector-length x)))
                (find (vector-ref x i))))
             (else
              (let ((num-slots (type-num-slots type)))
                (let lp ((i 0))
                  (cond ((< i num-slots)
                         (find (slot-ref type x i))
                         (lp (+ i 1))))))))
            ;; delete if this shouldn't count as a shared reference
            (if (and cyclic-only?
                     (<= (hash-table-ref/default seen x 0) 1))
                (hash-table-delete! seen x))))))
      ;; extract shared references
      (let ((res (srfi-69-make-hash-table eq?)))
        (hash-table-walk
         seen
         (lambda (k v) (if (> v 1) (hash-table-set! res k #t))))
        res)))

  (define (advanced-write-with-shared-structure x port cyclic-only? neoteric?)
    (let ((shared (extract-shared-objects x cyclic-only?))
          (count 0))
      ; Returns #t if this part is shared.
      (define (shared-object? x)
        (let ((type (type-of x)))
          (if (or (pair? x) (vector? x) (and type (type-printer type)))
            (let ((index (hash-table-ref/default shared x #f)))
              (not (not index)))
            #f)))
      ; Check-shared prints #n# or #n= as appropriate.
      (define (check-shared x prefix cont)
        (let ((index (hash-table-ref/default shared x #f)))
          (cond ((integer? index)
                 (display prefix port)
                 (display "#" port)
                 (write index port)
                 (display "#" port))
                (else
                 (cond (index
                        (display prefix port)
                        (display "#" port)
                        (write count port)
                        (display "=" port)
                        (hash-table-set! shared x count)
                        (set! count (+ count 1))))
                 (cont x index)))))
      (let wr ((x x) (neoteric? neoteric?))
        (define (infix-tail/ss op x port)
          (cond
            ((null? x) (display "}" port))
            ((pair? x)
              (display " " port)
              (wr op #t)
              (display " " port)
              (wr (car x) #t) ; Always neoteric inside infix list.
              (infix-tail/ss op (cdr x) port))
            (#t
              (display " .  " port)
              (n-write-simple x port)
              (display "}" port))))
        (check-shared
         x
         ""
         (lambda (x shared?)
           (cond
            ; Check for special formats first.
            ((and (represent-as-abbreviation? x) (not (shared-object? (cadr x))))
              ; Format 'x
              (write-abbreviation x port)
              (wr (cadr x) neoteric?))
            ((and (represent-as-inline-infix? x) (not (any shared-object? x)))
              ; Format {a + b}
              (display "{" port)
              (wr (cadr x) #t) ; Always neoteric inside {...}
              (infix-tail/ss (car x) (cddr x) port))
            ((and neoteric? (pair? x) (symbol? (car x)) (list1? (cdr x))
              (represent-as-infix? (cadr x))
              (not (shared-object? (cdr x)))
              (not (shared-object? (cadr x)))
              (not (any shared-object? (cadr x))))
              ; Format f{...}
              (wr (car x) neoteric?)
              (let ((expr (cadr x)))
                (if (list2? expr)
                  (begin
                    (display "{" port)
                    (wr (car expr) neoteric?)
                    (display " " port)
                    (wr (cadr expr) neoteric?)
                    (display "}" port))
                  (begin
                    (wr expr neoteric?)))))
            ((pair? x)
             ; Some format ending with closing paren - which one is it?
             (cond
               ((and neoteric? (symbol? (car x))
                 (not (long-and-boring? x boring-length))
                 (not (shared-object? (cdr x)))) ; I don't like the way it looks
                 ; Neoteric format a(b c ...)
                 (wr (car x) neoteric?)
                 (display "(" port)) ; )
               (#t
                 ; Default format, (a b c ...)
                 (display "(" port) ; )
                 (wr (car x) neoteric?)
                 (if (not (null? (cdr x))) (display " " port))))
             (let lp ((ls (cdr x)))
               (check-shared
                ls
                ". "
                (lambda (ls shared?)
                  (cond ((null? ls))
                        ((pair? ls)
                         (cond
                          (shared?
                           (display "(" port)
                           (wr (car ls) neoteric?)
                           (check-shared
                            (cdr ls)
                            ". "
                            (lambda (ls shared?)
                              (if (not (null? ls)) (display " " port))
                              (lp ls)))
                           (display ")" port))
                          (else
                           (wr (car ls) neoteric?)
                           (if (not (null? (cdr ls))) (display " " port))
                           (lp (cdr ls)))))
                        (else
                         (display ". " port)
                         (wr ls neoteric?))))))
             (display ")" port))
            ((vector? x)
             (display "#(" port)
             (let ((len (vector-length x)))
               (cond ((> len 0)
                      (wr (vector-ref x 0) neoteric?)
                      (do ((i 1 (+ i 1)))
                          ((= i len))
                        (display " " port)
                        (wr (vector-ref x i) neoteric?)))))
             (display ")" port))
            ((let ((type (type-of x)))
               (and (type? type) (type-printer type)))
             => (lambda (printer) (printer x wr port)))
            (else
             (write x port))))))))


  (define (curly-write-shared x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #f #f))

  (define (curly-write-cyclic x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #t #f))

  (define (neoteric-write-shared x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #f #t))

  (define (neoteric-write-cyclic x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #t #t))

  ; Since we have "cyclic" versions, the -write forms use them:
  (define neoteric-write neoteric-write-cyclic)
  (define curly-write curly-write-cyclic)


; -----------------------------------------------------------------------------
; Exported Interface
; -----------------------------------------------------------------------------

  (define curly-infix-read (make-read curly-infix-read-nocomment))
  (define neoteric-read (make-read neoteric-read-nocomment))
  (define sweet-read (make-read t-expr-catch))

  )

; vim: set expandtab shiftwidth=2 :
