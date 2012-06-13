(library $library-name$
    (export :all)
    (import (rnrs)
	    (sagittarius))
  (load-dynamic-library "$package-name$")
)