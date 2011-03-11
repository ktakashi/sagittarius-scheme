;; -*- scheme -*-
(library (rnrs syntax-case (6))
    (export make-variable-transformer
	    syntax-case
	    _ ...
	    syntax
	    identifier? bound-identifier=? free-identifier=?
	    syntax->datum datum->syntax generate-temporaries
	    with-syntax quasisyntax unsyntax unsyntax-splicing
	    syntax-violation)
    (import (core)
	    (sagittarius)
	    (core syntax-case)
	    (core with-syntax))

) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
