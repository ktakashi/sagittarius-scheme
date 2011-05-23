;; -*- scheme -*-
;; from Ypsilon
(library (core parameters)
  (export make-parameter parameterize)
  (import (core)
	  (core syntax-rules)
          (core base))

  (define-syntax parameterize-aux
    (syntax-rules ()
      ((_ () ((save new param value) ...) body ...)
       (let ((save #f) ... (new value) ...)
         (dynamic-wind
          (lambda () (set! save (param)) ... (param new) ...)
          (lambda () body ...)
          (lambda () (param save) ...))))
      ((_ ((e1 e2) . more) (stash ...) body ...)
       (parameterize-aux more (stash ... (tmp1 tmp2 e1 e2)) body ...))))

  (define-syntax parameterize
    (syntax-rules ()
      ((_ ((e1 e2) ...) body ...)
       (parameterize-aux ((e1 e2) ...) () body ...))))

  )