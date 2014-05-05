;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme lazy)
    (export delay force delay-force make-promise promise?)
    (import (core promise)))
