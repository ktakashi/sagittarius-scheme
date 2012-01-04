;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (srfi :45)
    (export lazy eager delay force)
    (import (srfi :45 lazy)))