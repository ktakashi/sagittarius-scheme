;; -*- mode:scheme; coding: utf-8; -*-
(library (srfi :45 lazy)
    (export (rename (delay-force lazy)) eager delay force)
    (import (core promise))
)
