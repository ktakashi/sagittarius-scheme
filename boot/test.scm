;; -*- Scheme -*-
;;(import (rnrs))
(define form '(a ...))

(cond ((not (pair? form))
       (print form))
      
      (else
       (print 'error)))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
