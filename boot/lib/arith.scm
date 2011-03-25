(library (core arithmetic)
    (export :all)
    (import null
	    (core errors))
;;;;;
;; arithmetic
(define gcd
  (lambda args
    (define recn
      (lambda (arg args)
	(if (null? args)
	    arg
	    (recn (%gcd arg (car args)) (cdr args)))))
    (let ((args (map (lambda (arg)
		       (unless (integer? arg)
			 (assertion-violation 'gcd
					      (wrong-type-argument-message "integer" arg)
					      args))
		       (abs arg))
		     args)))
      (cond ((null? args) 0)
	    ((null? (cdr args)) (car args))
	    (else (recn (car args) (cdr args)))))))

(define lcm
  (lambda args
    (define lcm2
      (lambda (u v)
	(let ((g (%gcd u v)))
	  (if (zero? u) 0 (* (quotient u g) v)))))
    (define recn
      (lambda (arg args)
	(if (null? args)
	    arg
	    (recn (lcm2 arg (car args)) (cdr args)))))
    (let ((args (map (lambda (arg)
		       (unless (integer? arg)
			 (assertion-violation 'lcm
					      (wrong-type-argument-message "integer" arg)
					      args))
		       (abs arg))
		     args)))
      (cond ((null? args) 1)
	    ((null? (cdr args)) (car args))
	    (else (recn (car args) (cdr args)))))))

(define (div-and-mod x y)
  (let ((d (div x y))
	(m (mod x y)))
    (values d m)))

(define (div0-and-mod0 x y)
  (let ((d0 (div0 x y))
	(m0 (mod0 x y)))
    (values d0 m0)))

(define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
  (let* ((n     ei1)
         (start ei2)
         (end   ei3)
         (count ei4)
         (width (- end start)))
    (if (positive? width)
        (let* ((count (mod count width))
               (field0
                (bitwise-bit-field n start end))
               (field1 (bitwise-arithmetic-shift-left
                        field0 count))
               (field2 (bitwise-arithmetic-shift-right
                        field0
                        (- width count)))
               (field (bitwise-ior field1 field2)))
          (bitwise-copy-bit-field n start end field))
        n)))

;; Originally from Ypsilon Scheme
(define (bitwise-reverse-bit-field ei1 ei2 ei3)
  (let* ((n ei1)
         (start ei2)
         (end ei3)
         (width (- end start)))
    (if (positive? width)
        (let loop ((reversed 0) (field (bitwise-bit-field n start end)) (width width))
          (if (zero? width)
              (bitwise-copy-bit-field n start end reversed)
              (if (zero? (bitwise-and field 1))
                  (loop (bitwise-arithmetic-shift reversed 1)
                        (bitwise-arithmetic-shift-right field 1)
                        (- width 1))
                  (loop (bitwise-ior (bitwise-arithmetic-shift reversed 1) 1)
                        (bitwise-arithmetic-shift-right field 1)
                        (- width 1)))))
        n)))
)