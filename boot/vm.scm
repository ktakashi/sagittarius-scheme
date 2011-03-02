;; -*- Scheme -*-

;; memo for library
;; Gauche's module system looks very similar with library for R6RS.
;; Of course it's not the same.
;; I may refer it so I just memorize it for later use.
;; Gauche's module has top level module named 'modules' which has
;; hashtable and it contains all modules.
;; Earch module has hastable which contains defined symbol such as
;; *SYNTAX*.
;; At this point find-module is just refer from top level module by
;; symbol name and insert-binding is set symbol into a module as if
;; it's defined like (define sym ...)!!
;; So, my library system might look like this.
;;
;; All Libraries
;;  - libraries table : hash table or something
;; A Library
;;  - imported        : maybe it also needs to contain information of
;;                      renamed symbol
;;  - exported        : exported symbols from this library
;;  - binding tagle   : hash table or something

;(define *libraries* (make-hash-table 'equal?))
;(define (vm-libraries) *libraries*)

(cond-expand
 (gauche
  (add-load-path ".")
  (load "compiler.scm")
  (use pp)
  (use srfi-1)
  (use util.match)
  (use file.util)
  (define hashtable-values hash-table-values)
  (define bitwise-ior logior)
  (define bitwise-and logand)
  (define bitwise-arithmatic-shift-left ash)
  (define bitwise-arithmatic-shift ash)
  ))

(define *stack* (make-vector 1000))

(define (push x s)
  (vector-set! *stack* s x)
  (+ s 1))
(define (index s i)
  (vector-ref *stack* (- s i 1)))

(define (index-set! s i v)
  (vector-set! *stack* (- s i 1) v))

(define *namespcace* (make-eq-hashtable))
(define (namespace-set! name value)
  (hashtable-set! *namespcace* name value))
(define (namespace-ref name)
  (hashtable-ref *namespcace* name #f))

(define-macro (case/unquote obj . clauses)
  (let ((tmp 'it))
    (define (expand-clause clause)
      (match clause
        [((item) . body)
         `((eqv? ,tmp ,item) ,@body)]
        [((item ...) . body)
         (let1 ilist (list 'quasiquote
                           (map (cut list 'unquote <>) item))
           `((memv ,tmp ,ilist) ,@body))]
        [(else . body)
         `(else ,@body)]))
    `(let ((,tmp ,obj))
       (cond ,@(map expand-clause clauses)))))

(define-simple-struct closure '.closure %closure
  body-code
  body-pc
  argc
  freec
  option?
  max-stack
  src
  (prev #())
  (frees '())
  mark)

(define (make-closure-from-code-builder code)
  (make-closure code 0))

(define (make-closure code s)
  (let* ((raw-code  (array-data (code-builder-code code)))
	 (reqargs   (code-builder-argc code))
	 (opt       (code-builder-optional? code))
	 (free      (code-builder-freec code))
	 (max-stack (code-builder-maxstack code))
	 (src       (code-builder-name code)))
    (let ((v (%closure raw-code 0 reqargs free opt max-stack src)))
      (if (zero? free)
	  v
	  (let ((fv (make-vector free)))
	    (let f ((i 0))
	      (unless (= i free)
		(vector-set! fv i (index s i))
		(f (+ i 1))))
	    (closure-frees-set! v fv)
	  v)))))

(define (make-empty-closure)
  (%closure))

(define (make-display n s)
  (let ((d (%closure))
	(v (make-vector n)))
    (let f ((i 0))
      (unless (= i n)
	(vector-set! v i (index s i))
	(f (+ i 1))))
    (closure-frees-set! d v)
    d))

(define (closure? c)
  (and (vector? c)
       (eq? (vector-ref c 0) '.closure)))

(define (index-closure cl n)
  (let ((fv (closure-frees cl)))
    (vector-ref fv n)))

#;(define-simple-struct procedure '.procedure make-procedure
  name
  body)

#;(define (procedure? p)
  (and (vector? p)
       (eq? (vector-ref p 0) '.procedure)))

(define (box o)
  (vector 'box o))
(define (unbox o)
  (vector-ref o 1))
(define (box? o)
  (and (vector? o)
       (eq? (vector-ref o 0) 'box)))
(define (set-box! b v)
  (vector-set! b 1 v))

(define (push-frame f c pc x s)
  (push x (push pc (push c (push f s)))))
(define (push-let-frame f c s)
  (push c (push f s)))

(define (shift-args-to-top s depth diff)
  (let loop ((i 0))
    (cond ((< i diff)
	   (index-set! (- (+ s diff) i) 0 (index s i))
	   (loop (+ i 1)))
	  (else
	   (+ s diff)))))

(define (shift-args-to-bottom s depth diff)
  (let nxtarg ((i (- depth 1)))
    (unless (< i 0)
      (index-set! s (+ i diff) (index s i))
      (nxtarg (- i 1))))
  (- s diff))

(define (shift-args f m s)
  (do ((i (- m 1) (- i 1)))
      ((< i 0) (+ f m))
    (index-set! (+ f m) i (index s i))))

(define (discard-let-frame f n s)
  (let ((prev (index f 1)))
    (let loop ((i (- n 1)))
      (unless (< i 0)
	(index-set! (+ f n) i  (index s i))
	(loop (- i 1))))
    (+ f n)))

(define (stack->pair-args sp num-args)
  (let loop ([n (- num-args 1)])
    (if (>= n 0)
        (cons (index sp n) (loop (- n 1)) )
        '())))

(define (pair-args->stack s offset args)
  (cond ((null? args) (index-set! s offset '()))
	(else
	 (let loop ((i (- (length args) 1))
		    (args args))
	   (unless (null? args)
	     (index-set! s (+ i offset) (car args))
	     (loop (- i 1) (cdr args)))))))

;; stub
(define-simple-struct valuez '.values #f
  valuez)
(define (make-values lst)
  (if (null? lst)
      (vector '.values #f)
      (let ((v (make-vector (length lst))))
	(let loop ((i 0)
		   (lst lst))
	  (if (null? lst)
	      (vector '.values v)
	      (begin
		(vector-set! v i (car lst))
		(loop (+ i 1) (cdr lst))))))))
(define (values? v)
  (and (vector? v)
       (eq? (vector-ref v 0) '.values)))

(define *debug* #f)
(define vm-debug
  (lambda f
    (if (null? f)
	*debug*
	(set! *debug* (car f)))))

(define vm-debug-step
  (let ((flag #f))
    (lambda f
      (if (null? f)
	  flag
	  (set! flag (car f))))))

(define (stack-trace c)
  (let loop ((c c)
	     (r '()))
    (let ((src (closure-src c))
	  (prev (closure-prev c)))
      (if (zero? (vector-length prev))
	  r
	  (loop prev (cons src r))))))
      

;; VM
;; x = code
;; pc = program counter
;; a = accumulator
;; c = current closure
;; f = frame pointer
;; s = stack pointer
(define (VM x pc a c f s)
  (let-syntax ((next (syntax-rules ()
		       ((_)
			(vector-ref x pc))
		       ((_ n)
			(vector-ref x (+ pc n)))))
	       (skip (syntax-rules ()
		       ((_)
			(begin
			  (set! pc (+ pc 1))
			  pc))
		       ((_ n)
			(begin
			  (set! pc (+ pc n))
			  pc))))
	       (fetch (syntax-rules ()
			((_)
			 (begin
			   (set! pc (+ pc 1))
			   (vector-ref x pc)))))
	       (return (syntax-rules ()
			 ((_)
			  (let ((s f))
			    (VM (index s 0) ;; code
				(index s 1) ;; pc
				a
				(index s 2) ;; closure
				(index s 3) ;; fp
				(- s 4)))))))
    (define (apply-body cl argc s)
      (cond ((procedure? cl)
	     (let ((p (stack->pair-args s argc)))
	       (VM `#(,RET ,HALT) 0 (apply (procedure-body cl) p) c (- s argc) s)))
	    ((closure? cl)
	     (let ((required-length (closure-argc cl)))
	       (cond ((closure-option? cl)
		      (let ((extra-argc (- argc (closure-argc cl))))
			;; last arg is '()
			(cond ((= -1 extra-argc)
			       (let ((sp (shift-args-to-top s argc 1)))
				 (index-set! sp 0 '())
				 (VM (closure-body-code cl)
				     (closure-body-pc cl)
				     cl
				     cl
				     (- sp required-length)
				     sp)))
			      ((>= extra-argc 0)
			       (index-set! s extra-argc (stack->pair-args s (+ extra-argc 1)))
			       (let ((sp (- s extra-argc)))
				 (VM (closure-body-code cl)
				     (closure-body-pc cl)
				     cl
				     cl
				     (- sp required-length)
				     sp)))
			      (else
			       (errorf "closure ~a require ~d argument got ~d" a required-length argc)))))
		     (else
		      (cond ((= argc required-length)
			     (VM (closure-body-code cl)
				 (closure-body-pc cl)
				 cl
				 cl
				 (- s argc)
				 s))
			    (else
			     (errorf "[2]wrang number of arguments for #<closure> (required ~d, got ~d ~a)"
				     required-length argc (closure-src cl))))))))
	    (else
	     (errorf "invalid application ~s" (shorten-object cl)))))
    (define (debug-insn-print)
      (let* ((insn (next))
	     (info (lookup-insn-name (get-insn insn)))
	     (name (car info))
	     (ival (cadr info))
	     (argc (caddr info))
	     (src? (cadddr info)))
	(format #t "Inst: ~s" name)
	(let inst ((i 0))
	  (unless (= i ival)
	    (if (= i 0)
		(display "(")
		(display " "))
	    (display (get-insn-value insn ival i))
	    (inst (+ i 1))))
	(unless (zero? ival)
	  (display ")"))
	(let loop ((i 1))
	  (if (> i argc)
	      (if src?
		  (let ((v (shorten-object (vector-ref x (+ pc i)))))
		    (format #t " ;; ~s"  v)))
	      (begin
		(format #t " ~s" (shorten-object (vector-ref x (+ pc i))))
		(loop (+ i 1)))))
	(newline)))

    (define (refer-local n)
      (index (+ f n 1) 0))

    (define (insn-value1 insn)
      (bitwise-arithmatic-shift insn -8))

    (define (insn-value1-with-mask insn )
      (bitwise-and (bitwise-arithmatic-shift insn -8) #xfff))
    (define (insn-value2 insn)
      (bitwise-arithmatic-shift insn -20))

    (when *debug*
      (format #t "pc:~a/~a~%" pc (vector-length x))
      (debug-insn-print)
      (display "ac:")(dbg-print a)
      ;(display "cl:")(dbg-print c)
      (print "frame:" f)
      (print-stack s)
      (newline)
      (flush))

    (let ((insn (next)))
    (case/unquote (get-insn insn)
      ((HALT) a)
      ((NOP) 
       (VM x (skip) a c f s))
      ((UNDEF) 
       (VM x (skip) 'undef c f s))
      ((FRAME)
       (let ((n (fetch)))
       (VM x (skip) a c f (push-frame f c (- (+ pc n) 1) x s))))
      ((LET_FRAME)
       ;; TODO expand stack
       (let ((n (insn-value1 insn)))
	 (VM x (skip) a c f (push-let-frame f c s))))
      ((POP_LET_FRAME)
       (let* ((n (insn-value1 insn))
	      (s (discard-let-frame f n s)))
	 (VM x (skip) a c f s)))
      ((DISPLAY)
       (let ((n (insn-value1 insn)))
	 (let ((new-c (make-display n s)))
	   (closure-prev-set! new-c c)
	   (VM x (skip) a new-c f (- s n)))))
      ((ENTER)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) a c (- s n) s)))
      ((LEAVE)
       (VM x (skip) a (index f 0) (index f 1) (- f 2)))
      ((DEFINE)
       (let ((const? (insn-value1 insn))
	     (var (fetch)))
	 (or (identifier? var)
	     (error "runtime error: DEFINE instruction requires identifier for its argument but got:" var))
	 (let ((name (id-name var)))
	   ;(namespace-set! name a)
	   (%insert-binding (id-library var) name a)
	   (VM x (skip) name #;'unspecified c f s))))
      ((BOX)
       (let ((n (insn-value1 insn)))
	 (index-set! s n (box (index s n)))
	 (VM x (skip) a c f s)))
      ((UNBOX)
       (VM x (skip) (unbox a) c f s))
      ((CONST)
       (let ((value (fetch)))
	 (VM x (skip) value c f s)))
      ((PUSH)
       (VM x (skip) a c f (push a s)))
      ((CONST_PUSH)
       (let ((value (fetch)))
	 (VM x (skip) value c f (push value s))))
      ((LREF)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (refer-local n) c f s)))
      ((FREF)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (index-closure c n) c f s)))
      ((GREF)
       (let ((var (fetch)))
	 (or (identifier? var)
	     (error (format "runtime error: GREF instruction requires identifier for ist argument but got: ~s~%~s"
			    var (stack-trace c))))
		    
	 (let ((value ;(namespace-ref (id-name var)))
		(find-binding (id-library var) (id-name var))))
	   (or value
	       (error (format "unbound variable ~s, library ~s~% ~s" (id-name var) (library-name (id-library var)) (stack-trace c))))
	   (VM x (skip) value c f s))))
      ((LREF_PUSH)
       (let ((n (insn-value1 insn)))
	 (let ((a (refer-local n)))
	   (VM x (skip) a c f (push a s)))))
      ((FREF_PUSH)
       (let ((n (insn-value1 insn)))
	 (let ((a (index-closure c n)))
	 (VM x (skip) a c f (push a s)))))
      ((GREF_PUSH)
       (let ((var (fetch)))
	 (or (identifier? var)
	     (error (format "runtime error: GREF instruction requires identifier for ist argument but got: ~s~%~s"
			    var (stack-trace c))))
		    
	 (let ((value ;(namespace-ref (id-name var)))
		(find-binding (id-library var) (id-name var))))
	   (or value
	       (error 'vm (format "unbound variable ~s, library ~s~% ~s" (id-name var) (library-name (id-library var)) (stack-trace c))))
	   (VM x (skip) value c f (push value s)))))
      ((LSET)
       (let ((n (insn-value1 insn)))
	 (set-box! (refer-local n) a)
	 (VM x (skip) a c f s)))
      ((FSET)
       (let ((n (insn-value1 insn)))
	 (set-box! (index-closure c n) a)
	 (VM x (skip) a c f s)))
      ((GSET)
       (let ((var (fetch)))
	 (or (identifier? var)
	     (error "runtime error: GPUT instruction requires identifier for ist argument but got:" var))
	 #;(namespace-set! (id-name var) a)
	 (%insert-binding (id-library var) (id-name var) a)
	 (VM x (skip) a c f s)))
      ((TEST)
       (let ((n (fetch)))
	 (if a
	     (VM x (skip) a c f s)
	     (VM x (skip n) a c f s))))
      ((BNEQ)
       (let ((n (fetch))
	     (val (eq? (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNEQV)
       (let ((n (fetch))
	     (val (eqv? (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNNUME)
       (let ((n (fetch))
	     (val (= (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNLE)
       (let ((n (fetch))
	     (val (<= (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNLT)
       (let ((n (fetch))
	     (val (< (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNGE)
       (let ((n (fetch))
	     (val (>= (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNGT)
       (let ((n (fetch))
	     (val (> (index s 0) a)))
	 (if val
	     (VM x (skip) val c f (- s 1))
	     (VM x (skip n) val c f (- s 1)))))
      ((BNNULL)
       (let ((n (fetch))
	     (val (null? a)))
	 (if val
	     (VM x (skip) val c f s)
	     (VM x (skip n) val c f s))))

      ;; set mark on current closure
      ;; then shiftj can look for this mark to jump where.
      ((MARK)
       (closure-mark-set! c f)
       (VM x (skip) a c f s))
      ((SHIFTJ)
       (let ((argc (insn-value1-with-mask insn))
	     (display (insn-value2 insn)))
	 (let loop ((i display)
		    (c c))
	   (if (and (<= i 0)
		    (closure-mark c))
	       (let* ((fp (closure-mark c))
		      (s (shift-args fp argc s)))
		 (VM x (skip) a c fp s))
	       (loop (- i 1) (closure-prev c))))))
      ((JUMP)
       (let ((n (fetch)))
	 (VM x (skip n) a c f s)))
      ((RECEIVE)
       (let ((reqargs (insn-value1-with-mask insn))
	     (optargs (insn-value2 insn)))
	 (or (values? a)
	     (error "receive requires values but got " a))
	 (let* ((v  (valuez-valuez a))
		(vl (vector-length v)))

	   (and (< vl reqargs)
		(error "received fewer values than expected"))
	   (and (and (zero? optargs)
		     (> vl reqargs))
		(error "received more values than expected"))

	   (cond ((zero? optargs)
		  ;; (receive (a b c) ...)
		  (when (> reqargs 0)
			(let loop ((i 0)
				   (sp s))
			  (if (>= i reqargs)
			      '()
			      (loop (+ i 1) (push (vector-ref v i) sp))))))
		 ((zero? reqargs)
		  ;; (receive a ...)
		  (let loop ((ret '())
			     (i 0))
		    (if (>= i vl)
			(push ret s)
			(loop (append ret (list (vector-ref v i)))
			      (+ i 1)))))
		 (else
		  ;; (receive (a b . c) ...)
		  (let loop ((i 0)
			     (sp s)
			     (ret '()))
		    (cond
		     ((< i reqargs) ;; push a, b
		      (loop (+ i 1) (push (vector-ref v i) sp) ret))
		     ((< i vl)
		      (loop (+ i 1) sp (append ret (list (vector-ref v i)))))
		     (else
		      (push ret sp))))))
	 (VM x (skip) a c f (+ s reqargs optargs)))))
      ((CLOSURE)
       (let* ((cb (fetch))
	      (cl (make-closure cb s)))
	 (VM x (skip) cl c f (- s (code-builder-freec cb)))))
      ((CALL)
       (let ((argc (insn-value1 insn)))
	 (apply-body a argc s)))
      ((TAIL_CALL)
       (let ((argc (insn-value1 insn)))
	 (apply-body a argc (shift-args f argc s))))
      ((GREF_CALL)
       (let ((var (fetch))
	     (argc (insn-value1 insn)))
	 (or (identifier? var)
	     (error (format "runtime error: GREF instruction requires identifier for ist argument but got: ~s~%~s"
			    var (stack-trace c))))
	 
	 (let ((value ;(namespace-ref (id-name var)))
		(find-binding (id-library var) (id-name var))))
	   (or value
	       (error 'vm (format "unbound variable ~s, library ~s~% ~s" (id-name var) (library-name (id-library var)) (stack-trace c))))
	   (apply-body value argc s))))
      ((GREF_TAIL_CALL)
       (let ((var (fetch))
	     (argc (insn-value1 insn)))
	 (or (identifier? var)
	     (error (format "runtime error: GREF instruction requires identifier for ist argument but got: ~s~%~s"
			    var (stack-trace c))))
	 
	 (let ((value ;(namespace-ref (id-name var)))
		(find-binding (id-library var) (id-name var))))
	   (or value
	       (error 'vm (format "unbound variable ~s, library ~s~% ~s" (id-name var) (library-name (id-library var)) (stack-trace c))))
	   (apply-body value argc (shift-args f argc s)))))
      ((LOCAL_CALL)
       (let ((argc (insn-value1 insn)))
	 ;; TODO stack expand
	 (VM (closure-body-code a)
	     (closure-body-pc a)
	     a
	     a
	     (- s argc)
	     s)))
      ((LOCAL_TAIL_CALL)
       (let ((argc (insn-value1 insn)))
	 (let ((s (shift-args f argc s)))
	   ;; TODO stack expand
	   (VM (closure-body-code a)
	       (closure-body-pc a)
	       a
	       a
	       (- s argc)
	       s))))
      ((APPLY)
       (let ((args (index s 0)))
	 (cond ((null? args) ;; (apply proc '())
		(VM `#(,CALL ,HALT) 0 a c f (- s 1)))
	       ((values? args) ;; (call-with-values ...)
		(let* ((args (valuez-valuez args))
		       (len  (vector-length args))
		       (shift-len (if (> len 1) (- len 1) 0))
		       (s (shift-args-to-top s 0 shift-len)))
		  (if (and (not (procedure? a))
			   (not (closure-option? a))
			   (not (= len (closure-argc a))))
		      (errorf "values received ~a values then expected" (if (> len (closure-argc a)) "more" "fewer")))
		  (pair-args->stack s 0 args)
		  (VM `#(,(merge-insn1 CALL len) ,HALT) 0 a c f s)))
	       (else
		(let* ((len (length args))
		       (shift-len (if (> len 1) (- len 1) 0))
		       (s (+ s shift-len)))
		  (pair-args->stack s 0 args)
		  (VM `#(,(merge-insn1 CALL len) ,HALT) 0 a c f s))))))
		
      ((RET)
       ;; I don't use arg0... do i still need this?
       (return))
      ;; builtin procedures
      ((EQ)
       (VM x (skip) (eq? (index s 0) a) c f (- s 1)))
      ((EQV)
       (VM x (skip) (eqv? (index s 0) a) c f (- s 1)))
      ((NEG)
       (VM x (skip) (- a) c f (- s 1)))
      ((ADD)
       (VM x (skip) (+ (index s 0) a) c f (- s 1)))
      ((ADDI)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (+ n a) c f s)))
      ((SUB)
       (VM x (skip) (- (index s 0) a) c f (- s 1)))
      ((SUBI)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (- n a) c f s)))
      ((MUL)
       (VM x (skip) (* (index s 0) a) c f (- s 1)))
      ((MULI)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (* n a) c f s)))
      ((DIV)
       (VM x (skip) (/ (index s 0) a) c f (- s 1)))
      ((DIVI)
       (let ((n (insn-value1 insn)))
	 (VM x (skip) (/ n a) c f s)))
      ((NUM_EQ)
       (VM x (skip) (= (index s 0) a) c f (- s 1)))
      ((NUM_LT)
       (VM x (skip) (< (index s 0) a) c f (- s 1)))
      ((NUM_LE)
       (VM x (skip) (<= (index s 0) a) c f (- s 1)))
      ((NUM_GT)
       (VM x (skip) (> (index s 0) a) c f (- s 1)))
      ((NUM_GE)
       (VM x (skip) (>= (index s 0) a) c f (- s 1)))
      ((NULLP)
       (VM x (skip) (null? a) c f s))
      ((CAR)
       (VM x (skip) (car a) c f s))
      ((CDR)
       (VM x (skip) (cdr a) c f s))
      ((CONS)
       (VM x (skip) (cons (index s 0) a) c f (- s 1)))
      ;; vector
      ((VECTOR)
       (let* ((val1 (insn-value1 insn))
	      (n (- val1 1))
	      (v (make-vector val1)))
	 (when (> val1 0)
	   (vector-set! v n a)
	   (let loop ((i 0))
	     (when (< i n)
	       (vector-set! v (- n i 1) (index s i))
	       (loop (+ i 1)))))
	 (VM x (skip) v c f (- s (if (> val1 0) n 0)))))
      ((VECTORP)
       (VM x (skip) (vector? a) c f s))
      ((VEC_LEN)
       (VM x (skip) (vector-length a) c f s))
      ((VEC_REF)
       (VM x (skip) (vector-ref (index s 0) a) c f (- s 1)))
      ((VEC_SET)
       (VM x (skip) (vector-set! (index s 1) (index s 0) a) c f (- s 2)))
      ((LIST)
       (let ((n (- (insn-value1 insn) 1)))
	 (VM x (skip) (let loop ((i 0)
				 (ret (list a)))
			(if (= n i)
			    ret
			    (loop (+ i 1) (cons (index s i)
						ret))))
	     c f (- s n))))
      (else
       (errorf "invalid vm instruction: ~s[~a]" (lookup-insn-name it) it))))))

(define (vm/apply code . args)
  (let* ((code-c `#(,FRAME ,(+ (vector-length
				(if (code-builder? code)
				    (array-data (code-builder-code code))
				    (closure-body-code code))) 5)
		    ,CONST ,args
                    ,PUSH
		    ,@(vector->list (if (code-builder? code)
				    (array-data (code-builder-code code))
				    (closure-body-code code)))
		    ,APPLY
		    ,HALT))
	 (cb (make-code-builder)))
    (code-builder-code-set! cb code-c)
(when (vm-debug-step)
  (vm-dump-code code))
    (let ((cl (make-closure cb 0)))
      (fluid-let ((*stack* (make-vector 1000)))
	(VM code-c 0 cl cl 0 0)))))

;; for debug
(define (print-stack s)
  (display "(")
  (let loop ((i 0))
    (if (= i s)
	(format #t ")~%")
	(let ((o (vector-ref *stack* i)))
	  (let ((v (shorten-object o)))
	    (if (> i 0) (display ", "))
	    (format #t "[~a]:~s" i v)
	    (loop (+ i 1)))))))

(define (shorten-object o)
  (cond ((closure? o)
	 "#<closure #f>")
	((identifier? o)
	 (format "#<identifier ~s#~s>" (id-name o) (library-name (id-library o))))
	((pair? o)
	 (if (and (pair? (car o))
		  (not (zero? (length o))))
	     (let loop ((r '())
			(o o))
	       (if (null? o)
		   (reverse r)
		   (loop (cons (shorten-object (car o)) r)
			 (cdr o))))
	     (let ((s (x->string o)))
	       (if (> (string-length s) 30)
		   (substring s 0 30)
		   s))))
	((box? o)
	 (format "#<box ~a>"
		 (shorten-object (unbox o))))
	((procedure? o)
	 (format "#<subr ~s>" (procedure-name o)))
	((macro? o)
	 (format "#<macro ~s>" (macro-name o)))
	((vector? o)
	 (format "#<code? ~a>" (shorten-object (vector-ref o 0))))
	(else
	 o)))

(define (dbg-print o)
  (define (print-free free)
    (if (vector? free)
	(let ((len (vector-length free)))
	  (let loop ((i 0))
	    (unless (= i len)
	      (format #t "~s " (shorten-object (vector-ref free i)))
	      (loop (+ i 1)))))))
  (cond ((closure? o)
	 (format #t "#<closure>~%")
	 #;(format #t "    :src   ~s~%" (shorten-object (closure-src o)))
	 (format #t "    :mark  ~a~%" (closure-mark o))
	 (format #t "    :frees ")
	 (print-free  (closure-frees o))
	 (let loop ((prev (closure-prev o))
		    (pos 1))
	   (unless (zero? (vector-length prev))
	     (when (closure? prev)
	       (newline)
	       (format #t "    :prev[~a] " pos)
	       (print-free (closure-frees prev))
	       (newline)
	       (format #t "       -> :mark ~a" (closure-mark prev))
	       (loop (closure-prev prev) (+ pos 1)))))
	 (newline))
	((procedure? o)
	 (print (procedure-name o)))
	((box? o)
	 (format #t "~s~%" (shorten-object o)))
	(else
	 (write/ss o)
	 (newline))))

(define *show-code* #f)
;(load "tools/code2c.scm")
(load "tools/code2c2.scm")

(define (library-path->path path)
  (let ((body (path-sans-extension path)))
    (string-map (lambda (c)
		  (cond ((char=? c #\/) #\_)
			(else c)))
		body)))

(define (execute s opt)
  (if (null? opt)
      (let ((c (compile s '()))
	    (cl (%closure)))
	(when *show-code*
	  (vm-dump-code c))
	(VM (array-data (code-builder-code c)) 0 '() cl 0 0))
      (let ((o (string->symbol (car opt))))
	#;(format #t "~,,,,50s~%" s)
	(case o
	  ((p1)
	   (print "pass1 test")
	   (compile-p1 s) 'done)
	  ((p2)
	   (print "pass2 test")
	   (compile-p2 s) 'done)
	  ((p3)
	   (print "pass3 test")
	   (compile-p3 s) 'done)
	  ((d)
	   (set! *debug* #t)
	   (let ((c (compile s '()))
		 (cl (%closure)))
	     (VM (array-data (code-builder-code c)) 0 '() cl 0 0)))
	  ((c)
	   (or (>= (length opt) 2)
	       (error 'execute "option c requires following parameter: libname &output-file"))
	   (vm-no-debug-info #t)
	   (let* ((lib (cadr opt))
		  (out (if (null? (cddr opt))
			   (open-output-file (string-append (library-name->path lib) ".c"))
			   (open-output-file (caddr opt))))
		  (c (compile s '())))
	     #;(code->c (array-data (code-builder-code c))
		      (array-length (code-builder-code c))
		      lib out)
	     (code->c c lib out)
	     (close-output-port out)))
	  ((lc)
	   (or (>= (length opt) 2)
	       (error "option c requires following parameter: &output-dir"))
	   (vm-no-debug-info #t)
	   (let ((dir (cadr opt)))
	     (for-each (lambda (builtin-info)
			 (let ((path (car builtin-info))
			       (name (cadr builtin-info))
			       (import (caddr builtin-info))
			       (can-be-c? (cadddr builtin-info)))
			   (when can-be-c?
			     (let* ((form (construct-library path name import))
				    (outpath (string-append dir "/"
							    (library-path->path path)
							    ".c"))
				    (out (open-output-file outpath))
				    (c (compile form '())))
			       (print (format "generating compiled library (in: ~s, out ~s)"
					      path outpath))
			       (code->c c (format "~s" name) out)
			       (close-output-port out)))))
		       *builtin-libraries*)))
			     
	  (else
	   (error 'execute "invalid option:" o))))))

(define (apply-proc . args)
  (let-syntax 
      ((fluid-let
	   (syntax-rules ()
	     ((fluid-let () be ...)
	      (begin be ...))
	     ((fluid-let ((p0 e0) (p e) ...) be ...)
	      (let ((saved p0))
		(set! p0 e0)
		(receive results (fluid-let ((p e) ...) be ...)
		  (set! p0 saved)
		  (apply values results)))))))
  (let* ([proc (car args)]
         [args (cdr args)]
         [adjusted_args (append
                         (take args (- (length args) 1))
                         (car (drop args (- (length args) 1))))])
    (fluid-let ((*stack* (make-vector 1000)))
      (VM `#(,FRAME
	     7
	     ,CONST ,adjusted_args
	     ,PUSH
	     ,CONST ,proc
	     ,APPLY
	     ,HALT)
	  0
	  '()
	  '()
	  0
	  0)))))

;; make 'user library before add
(define (vm-init)
  (define *compiler-library* '(sagittarius compiler))
  (let ((lib (make-library 'user)))
    (init-compiler)
    (let ((lib (find-library *compiler-library* #f)))
      (if lib
	  lib
	  (let ((clib (make-library *compiler-library*)))
	    (load-library clib 'null)
	    (load-library clib '(sagittarius)))))
    (load-library lib *compiler-library*)
    (%set-library lib)
    (vm-current-library lib)))

(define vm-no-debug-info
  (let ((flag #f))
    (lambda o
      (if (null? o)
	  flag
	  (set! flag (car o))))))

#;(define-macro (add-namespace! p . body)
  (if (null? body)
      `(%insert-binding (vm-current-library) ',p (make-procedure ',p ,p))
      `(%insert-binding (vm-current-library) ',p (make-procedure ',p ,@body))))

(define-macro (add-namespace! name args . proc)
  (let ((lib 'null))
    (receive (reqargs opt?) (parse-args args)
      (let ((proc (if (null? proc)
		      `(make-procedure ',name #f ,reqargs ,opt? ,name)
		      `(make-procedure ',name #f ,reqargs ,opt? ,@proc))))
	  `(%insert-binding ',lib ',name
			    ,proc)))))

;; for debug
(add-namespace! make-closure-from-code-builder (c))
(add-namespace! make-empty-closure ())
(add-namespace! VM (x pc ac cl fp sp))

 ;; library needs to be compare by equal?
(add-namespace! undefined ())
(add-namespace! undefined? (o))
(add-namespace! bitwise-arithmatic-shift (a b))
(add-namespace! bitwise-arithmatic-shift-left (a b))
(add-namespace! bitwise-and (a b))
(add-namespace! bitwise-ior (a b))
;(add-namespace! acons (a b c))
;(add-namespace! vm-libraries ())
;(add-namespace! < (a b . c))
;(add-namespace! <= (a b . c))
;(add-namespace! > (a b . c))
;(add-namespace! >= (a b . c))
;(add-namespace! = (a b . c))
;(add-namespace! -)
;(add-namespace! +)
;(add-namespace! *)
;(add-namespace! /)
(add-namespace! values o (lambda a (make-values a)))
(add-namespace! display (o))
(add-namespace! newline ())
(add-namespace! print o) ;; vm-dump-code
(add-namespace! flush-output-port (p) (lambda (p)
					(flush)))
(add-namespace! current-output-port ())
(add-namespace! format (fmt . o))
(add-namespace! list o)
;(add-namespace! vector? (o))
;(add-namespace! vector o)
(add-namespace! make-vector (n))
;(add-namespace! vector-set! (v i o))
;(add-namespace! vector-ref  (v i))
;(add-namespace! vector-length (v))
;(add-namespace! car (p))
;(add-namespace! cdr (p))
;(add-namespace! cons (a b))
(add-namespace! vector->list (v))
(add-namespace! set-cdr! (p o))
(add-namespace! integer? (o))
(add-namespace! number? (o))
(add-namespace! fixnum? (o))
(add-namespace! exact? (o))
(add-namespace! inexact? (o))
(add-namespace! char? (o))
(add-namespace! boolean? (o))
(add-namespace! pair? (p))
(add-namespace! length (p))
;(add-namespace! null? (p))
;(add-namespace! list o)
(add-namespace! list? (o))
(add-namespace! list->vector (l))
(add-namespace! append o)
(add-namespace! append! o)
(add-namespace! reverse (o))
(add-namespace! apply (proc l1 . rest) apply-proc)
;(add-namespace! eq? (a b))
;(add-namespace! eqv? (a b))
(add-namespace! equal? (a b))
(add-namespace! not (o))
(add-namespace! assq (a b))
(add-namespace! assv (a b))
(add-namespace! memq (a b))
(add-namespace! memv (a b))
(add-namespace! equal-hash (o) hash)
(add-namespace! make-eq-hashtable () make-hash-table)
;; only used for *libraries*
(add-namespace! make-hashtable (a b) (lambda (h e)
				       (make-hash-table 'equal?)))
(add-namespace! hashtable-ref (h k) hash-table-get)
(add-namespace! hashtable-set! (h k v) hash-table-put!)
(add-namespace! hashtable->alist (h) hash-table->alist)
(add-namespace! hashtable-keys (h) hash-table-keys)
(add-namespace! hashtable-values (h) hash-table-values)
(add-namespace! vm/apply (c . a))
(add-namespace! er-rename (a b c))
(add-namespace! identifier? (i))
(add-namespace! symbol? (s))
(add-namespace! gensym ())
(add-namespace! zero? (o))
(add-namespace! string->symbol (o))
(add-namespace! string-length (o))
(add-namespace! symbol->string (o))
(add-namespace! string-append o)
(add-namespace! vm-debug o)
(add-namespace! vm-debug-step o)
(add-namespace! number->string (n))
(add-namespace! x->string (o)) ;; for vm-dump-code
(add-namespace! substring (o)) ;; for vm-dump-code
(add-namespace! error (m . f))
(add-namespace! debug-print (o)
		(lambda (o)
		  (when (vm-debug-step)
		    (print o))))
(add-namespace! vm-init ())
(add-namespace! vm-no-debug-info (o))

(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

(define (notify-cond-expand form mark)
  (let loop ((form form)
	     (r '()))
    (if (null? form)
	(reverse r)
	(match (car form)
	  (('cond-expand . body)
	   (aif (find (lambda (x) (eq? (car x) mark)) body)
		(loop (cdr form) (append (cdr it) r))
		(aif (find (lambda (x) (eq? (car x) 'else)) body)
		     (loop (cdr form) (cons (cdr it) r))
		     (errorf "condition? ~a not found " mark))))
	  (else (loop (cdr form) (cons (car form) r)))))))

(define (construct-library file lib imports)
  (let* ((sexp (file->sexp-list file))
	 (program (notify-cond-expand sexp 'sagittarius))
	 (exports '()))
    (for-each (lambda (sexp)
		(match sexp
		  (('define (name . args) expr ...)
		   (set! exports (cons name exports)))
		  (('define (? symbol? name) expr)
		   (set! exports (cons name exports)))
		  (('define-constant (? symbol? name) expr)
		   (set! exports (cons name exports)))
		  (('define-syntax name expr)
		   (set! exports (cons name exports)))
		  ;; fxxk!!!
		  (('define-enum name . enums)
		   (set! exports (cons name exports))
		   (set! exports (append enums exports)))
		  (else #f))) program)
    (let ((form `(library ,lib 
		     (export ,@(reverse exports))
		     (import ,@imports)
		   ,@program)))
      form)))

(define (load-file file opt lib imports show?)
  (if lib
      (let ((form (construct-library file lib imports)))
	(let ((r (execute form opt)))
	  (when show?
	    (print r)
	    (flush))))
      (with-input-from-file file
	(lambda ()
	  (let loop ((s (read))
		     (ret '()))
	    (if (eof-object? s)
		'()
		(begin 
		  (let ((r (execute s opt)))
		    (when show?
		      (print r)
		      (flush))
		    (loop (read) ret)))))))))

(define (dump-library-symbols lib . search)
  (let ((lib (find-library lib #f)))
    (if (null? search)
	(print (hashtable-keys (library-table lib)))
	(print (memq (car search) (hashtable-keys (library-table lib)))))))

(add-namespace! dump-library-symbols (o . s))
(add-namespace! vm-current-library o)
(add-namespace! find-binding (l n))
(add-namespace! unwrap-syntax (form))

(define *base-lib* "lib/scmlib.scm")
(define *compaux* "compiler-aux.scm")
(define *proc-lib* "lib/proc.scm")
(define *match-lib* "lib/match.scm")
(define *ext-lib* "lib/ext.scm")
(define *vm-lib* "lib/vm.scm")
(define *vm-debug* "lib/debug.scm")
(define *insn* "insn.scm")
;; syntax-rules
(define *struct-lib* "../lib/core/struct.scm")
(define *synrule-lib* "../lib/core/syntax-rules.scm")

(define *builtin-libraries* 
  `((,*base-lib* (core base) (null) #t)
    (,*ext-lib* (sagittarius) (null) #f)
    (,*struct-lib* #f () #f)
    (,*synrule-lib* #f () #f)
    (,*match-lib* (sagittarius compiler match)
		  (null (core base)
			(:only (core syntax-rules) :compile)
			(sagittarius)) #f)
    (,*compaux* (sagittarius compiler util)
		(null (core base) 
		      (:only (core syntax-rules) :compile)
		      (sagittarius)
		      (:only (sagittarius compiler match) :compile))
		#t)
    (,*insn* (sagittarius vm instruction)
	     (null (sagittarius)) #f)
    (,*vm-lib* (sagittarius vm)
	       (null (core base) (sagittarius) (sagittarius vm instruction)) #f)
    (,*vm-debug* (sagittarius vm debug)
		 (null (core base) (sagittarius) (sagittarius vm)
		       (sagittarius vm instruction)) #f)
    (,*proc-lib* (sagittarius compiler procedure) 
		 (null (core base) (sagittarius)
		       #;(core syntax-rules)
		      (sagittarius compiler match)
		      (sagittarius compiler util)
		      (sagittarius vm)
		      (sagittarius vm instruction)) #f)))
    

(define (main args)
  (if (< (length args) 2)
      (display "usage: vm.scm file &option{p1|p2|p3|d|c}
&option:
p1: compile-p1
p2: compile-p2
p3: compile-p3
d:  vm debug (show all instruction and current stack etc.)
c:  compile library file to C
    following arguments are required: libname &output-file
lc: compile builtin libraries
")
  (begin
    (vm-init)
    (for-each (lambda (builtin-info)
		(let ((path (car builtin-info))
		      (name (cadr builtin-info))
		      (import (caddr builtin-info)))
		  (print "loading " path)
		  #;(if (eq? path *proc-lib*)
		      (vm-debug #t))
		  ;; (sagittarius) library's export spec must be #f
		  (let ((lib (find-library '(sagittarius) #f)))
		    (when lib
		      (library-exported-set! lib #f)))
		  (load-file path '() name import #f)))
	      *builtin-libraries*)
    ;(vm-debug-step #t)
    (load-file (cadr args) (cddr args) #f #f #t))))
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End
