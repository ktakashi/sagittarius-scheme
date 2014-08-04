;; for #[...] read
(library (tmp-reader)
    (export :export-reader-macro)
    (import (rnrs)
	    (sagittarius reader)
	    (srfi :14 char-sets))

  (define-dispatch-macro #\# #\[ (char-set-reader port subc param)
    (let loop ((cs (char-set)))
      (let ((c (get-char port)))
	(if (char=? c #\])
	    cs
	    (let ((nc (lookahead-char port)))
	      (cond ((char=? c #\\)
		     (let ((c2 (get-char port)))
		       ;; support minimal chars s and w
		       (cond ((char=? c2 #\s)
			      (loop (char-set-union! cs char-set:whitespace)))
			     ((char=? c2 #\S)
			      (loop (char-set-union! 
				     cs 
				     (char-set-complement char-set:whitespace))))
			     ((char=? c2 #\w)
			      (loop (char-set-union! cs char-set:letter)))
			     (else
			      (loop (char-set-adjoin! cs c2))))))
		    ((char=? nc #\-)
		     (get-char port)
		     (let ((c2 (get-char port)))
		       (if (char=? c2 #\])
			   (char-set-adjoin! cs c nc)
			   (loop (ucs-range->char-set!
				  (char->integer c)
				  (+ (char->integer c2) 1)
				  #f
				  cs)))))
		    
		    (else (loop (char-set-adjoin! cs c)))))))))
)

#!read-macro=tmp-reader
(import (rnrs)
	(srfi :64)
	(text parse) 
	(sagittarius io)
	(sagittarius control))  ;; for short names

(test-begin "Text parse")

(define-syntax test* 
  (syntax-rules (test-error)
    ((_ name (test-error) expr)
     (test-error name condition? expr))
    ((_ name expect expr)
     (test-equal name expect expr))))

;; From gauche
;; a part of text data is taken from Oleg's vinput-parse.scm
;;  http://pobox.com/~oleg/ftp/Scheme/parsing.html

(define (test-find-string input pattern . max-chars)
  (call-with-input-string input
    (lambda (p)
      (let* ((n (apply find-string-from-port? pattern p max-chars))
             (c (read-char p)))
        (list n (if (eof-object? c) 'eof c))))))

(test* "find-string-from-port?" '(7 #\d)
       (test-find-string "bacacabd" "acab"))
(test* "find-string-from-port?" '(7 #\d)
       (test-find-string "bacacabd" "acab" 100))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "acad"))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "acad" 100))
(test* "find-string-from-port?" '(#f #\a)
       (test-find-string "bacacabd" "bd" 5))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd" 9))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd"))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd" 8))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "be" 20))


(define (test-parseutil proc input . args)
  (call-with-input-string input
    (lambda (p)
      (let* ((c (apply proc (append args (list p))))
             (n (read-char p)))
        (list (if (eof-object? c) 'eof c)
              (if (eof-object? n) 'eof n))))))

(define (test-assert-curr-char str clist)
  (test-parseutil assert-curr-char str clist "zz"))

(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" '(#\a #\space)))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" #[a ]))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" #[a\s]))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" '(#\a #[\s])))
(test* "assert-curr-char" '(#\a #\space)
       (test-assert-curr-char "a bcd" '(#\a #\space)))
(test* "assert-curr-char" '(#\a #\space)
       (test-assert-curr-char "a bcd" #[a ]))
(test* "assert-curr-char" (test-error)
       (test-assert-curr-char "bcd" #[a ]))
(test* "assert-curr-char" (test-error)
       (test-assert-curr-char "" #[a ]))
(test* "assert-curr-char" '(eof eof)
       (test-assert-curr-char "" '(#\a #\space *eof*)))

(test* "skip-until number" '(#f #\a)
       (test-parseutil skip-until " abcd" 1))
(test* "skip-until number" (test-error)
       (test-parseutil skip-until " abcd" 10))
(test* "skip-until number" '(#f eof)
       (test-parseutil skip-until " abcd" 5))
(test* "skip-until cset" '(#\space #\a)
       (test-parseutil skip-until " abcd" '(#\a #\space)))
(test* "skip-until cset" '(#\space #\a)
       (test-parseutil skip-until " abcd" #[a ]))
(test* "skip-until cset" '(#\c #\space)
       (test-parseutil skip-until "xxxc bcd" #[abc ]))
(test* "skip-until cset" '(#\c eof)
       (test-parseutil skip-until "xxxc" #[abc ]))
(test* "skip-until cset" (test-error)
       (test-parseutil skip-until "xxxc" #[def]))
(test* "skip-until cset" '(eof eof)
       (test-parseutil skip-until "xxxc" '(#[def] *eof*)))
(test* "skip-until cset" '(#\c eof)
       (test-parseutil skip-until "xxxc" '(#[c-f] *eof*)))
(test* "skip-until proc" '(#\c #\space)
       (test-parseutil skip-until "xxxc bcd"
                       (^x (not (eqv? x #\x)))))
(test* "skip-until proc" '(eof eof)
       (test-parseutil skip-until "xxx"
                       (^x (not (eqv? x #\x)))))
(test* "skip-until proc" (test-error)
       (test-parseutil skip-until "yyyy"
                       (^x (eqv? x #\x))))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd" '(#\a #\space #\x)))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd" #[ax ]))
(test* "skip-while" '(#\y #\y)
       (test-parseutil skip-while "yxxxd" #[ax ]))
(test* "skip-while" '(eof eof)
       (test-parseutil skip-while "xxxa" #[ax ]))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd"
                       (^x (eqv? x #\x))))
(test* "skip-while" '(#\y #\y)
       (test-parseutil skip-while "yxxxd"
                       (^x (eqv? x #\x))))
(test* "skip-while" '(eof eof)
       (test-parseutil skip-while "yxxxd"
                       (^x (and (char? x)
				(char-alphabetic? x)))))

(test* "next-token" '("" #\d)
       (test-parseutil next-token "xxxd" #[ax ] #[d] "next token"))
(test* "next-token" '("bc" #\d)
       (test-parseutil next-token "xxxabcd" #[ax ] #[d] "next token"))
(test* "next-token" '("aeio" #\tab)
       (test-parseutil next-token "   aeio\tnjj" #[\s] #[\s] "next token"))
(test* "next-token" (test-error)
       (test-parseutil next-token "   aeio" #[\s] #[\s] "next token"))
(test* "next-token" '("aeio" eof)
       (test-parseutil next-token "   aeio" #[\s] '(#[\s] *eof*) "next token"))
(test* "next-token" '("aeio" #\tab)
       (test-parseutil next-token "   aeio\tnjj"
                       (^x (and (char? x)
				(char-whitespace? x)))
                       (^x (or (eof-object? x)
			       (char-whitespace? x)))
                       "next token"
                       ))

(test* "next-token-of" '("" #\x)
       (test-parseutil next-token-of "xxxd" #[a-c]))
(test* "next-token-of" '("" #\x)
       (test-parseutil next-token-of "xxxd" #[a-d]))
(test* "next-token-of" '("xxx" #\d)
       (test-parseutil next-token-of "xxxd" #[ax]))
(test* "next-token-of" '("anmb" #\-)
       (test-parseutil next-token-of "anmb-runge" #[\w]))
(test* "next-token-of" '("rnge!rg0#$@" #\space)
       (test-parseutil next-token-of "rnge!rg0#$@ bag" #[\S]))
(test* "next-token-of" '("xxx" #\d)
       (test-parseutil next-token-of "xxxd"
                       (^x (eqv? x #\x))))
(test* "next-token-of" '("xxxx" eof)
       (test-parseutil next-token-of "xxxx"
                       (^x (eqv? x #\x))))

(test* "read-string" '("aaaa" #\a)
       (test-parseutil read-string "aaaaa" 4))
(test* "read-string" '("aaaaa" eof)
       (test-parseutil read-string "aaaaa" 5))
(test* "read-string" '("aaaaa" eof)
       (test-parseutil read-string "aaaaa" 6))
(test* "read-string" '("" #\a)
       (test-parseutil read-string "aaaaa" 0))
(test* "read-string" '("" #\a)
       (test-parseutil read-string "aaaaa" -1))
(test* "read-string" '("" eof)
       (test-parseutil read-string "" 7))


(test-end)