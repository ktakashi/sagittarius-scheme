;; I hate R6RS library system!
(cond-expand
 (gauche #f)
 (sagittarius
  (dump-library-symbols '(sagittarius compiler util) 'define-enum)))
;;;;
;; IForm
(define-enum .intermediate-tags.
  $UNDEF
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $LAMBDA
  $RECEIVE
  $LABEL
  $SEQ
  $CALL
  $ASM
  $IT
  $LIST)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
