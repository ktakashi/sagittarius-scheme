
#!r6rs
(library (srfi :6 basic-string-ports)
  (export
    open-input-string
    open-output-string
    get-output-string)
  (import
    (only (rnrs base) define)
    (only (rnrs io ports) open-string-input-port)
    (only (sagittarius) open-output-string get-output-string))
  
  (define (open-input-string str)
    (open-string-input-port str))
)
