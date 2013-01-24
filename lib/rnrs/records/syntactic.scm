;; -*- scheme -*-
;; originally from Ypsilon
(library (rnrs records syntactic (6))
    (export define-record-type
	    record-type-descriptor
	    record-constructor-descriptor
	    fields mutable immutable parent protocol
	    sealed opaque nongenerative parent-rtd)
    (import (core record))
) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
