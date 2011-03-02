(define-module pp
  (use srfi-1)
  (use util.list)
  (use util.match)
  (use text.tree)
  (export pretty-print
          x->pp
          ))

(select-module pp)

;; <doc> =
;;      | ()                    (nil)
;;      | "..."                 (text)
;;      | #\newline             (line)
;;      | (<integer> <doc> ...) (nest)
;;      | (group <doc> ...)     (group)
;;      | (<doc> ...)           (cons)

(define (pp-fits? width xs)
  (and (not (negative? width))
       (match xs
         (() #t)
         (((i m ()) ys ...)
          (pp-fits? width ys))
         (((i m ('group doc ...)) ys ...)
          (pp-fits? width (cons (list i 'flat doc) ys)))
         (((i m ([? integer? j] x ...)) ys ...)
          (pp-fits? width
                   (cons (list (+ i j) m x) ys)))
         (((i m [? string? s]) ys ...)
          (pp-fits? (- width (string-length s)) ys))
         (((i 'flat #\newline) ys ...)
          (pp-fits? (- width 1) ys))
         (((i 'break #\newline) ys ...)
          #t)
         (((i m (y1 . ys)) zs ...)
          (pp-fits? width
                    (cons* (list i m y1) (list i m ys) zs))))))

(define (pp-make-tree width k xs)
  (match xs
    (() "")
    (((i m ()) ys ...)
     (pp-make-tree width k ys))
    (((i m ('group doc ...)) ys ...)
     (let1 mode (if (pp-fits? (- width k) (cons (list i 'flat doc) ys))
                    'flat
                    'break)
       (pp-make-tree width k (cons (list i mode doc) ys))))
    (((i m ([? integer? j] x ...)) ys ...)
     (pp-make-tree width k (cons (list (+ i j) m x) ys)))
    (((i m [? string? s]) ys ...)
     (cons s (pp-make-tree width (+ k (string-length s)) ys)))
    (((i 'flat #\newline) ys ...)
     (cons #\space (pp-make-tree width (+ k 1) ys)))
    (((i 'break #\newline) ys ...)
     (cons* #\newline (make-string i #\space) (pp-make-tree width i ys)))
    (((i m (y1 . ys)) zs ...)
     (pp-make-tree width k (cons* (list i m y1) (list i m ys) zs)))))

(define-method x->pp (obj)
  (write-to-string obj))

(define-method x->pp ((xs <list>))
  (match xs
    (() "()")
    (('quote obj)
     (list "'" (x->pp obj)))
    (('quasiquote obj)
     (list "`" (x->pp obj)))
    (('unquote obj)
     (list "," (x->pp obj)))
    (('unquote-splicing obj)
     (list ",@" (x->pp obj)))
    (('define (name&args ..1) body ..1)
     `(group "(define " (group (8 ,@(x->pp name&args)))
                (2 #\newline (group ,@(map x->pp body)))
                ")"))
    (_
     (let loop ((xs (cdr xs))
                (rs (list (x->pp (car xs)))))
       (cond ((null? xs)
              `(group "(" (1 ,@(reverse rs)) ")"))
             ((pair? xs)
              (loop (cdr xs) (cons* (x->pp (car xs)) #\newline rs)))
             (else
              (loop '() (cons* (x->pp xs) #\newline "." #\newline rs))))))))

(define-method x->pp ((v <vector>))
  `(group "#(" (2 ,@(intersperse #\newline (map x->pp (vector->list v)))) ")"))


(define-method pretty-print (obj (port <port>) (width <integer>))
  (%pp obj port width))

(define-method pretty-print (obj (port <port>))
  (%pp obj port #f))

(define-method pretty-print (obj (width <integer>))
  (%pp obj #f width))

(define-method pretty-print (obj)
  (%pp obj #f #f))

(define (%pp obj port width)
  (let1 p (or port (current-output-port))
    (write-tree (pp-make-tree (or width 78) 0 `((0 flat ,(x->pp obj))))
                p)
    (newline p)))

(provide "pp")
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
