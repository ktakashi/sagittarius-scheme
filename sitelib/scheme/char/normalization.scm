;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme char normalization)
    (export string-ni<=? string-ni<? string-ni=?
	    string-ni>=? string-ni>?)
    (import (rename (rnrs)
		    (string=? string-ni=?)
		    (string<? string-ni<?)
		    (string>? string-ni>?)
		    (string<=? string-ni<=?)
		    (string>=? string-ni>=?))))