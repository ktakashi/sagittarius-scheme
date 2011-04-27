(library (sagittarius compiler)
         (export compile compile-with-* compile-p1 compile-p2 compile-p3
                 ensure-library-name)
         (import null (core base) 
                      (for (core syntax-rules) expand)
                      (core syntax-case)
                      (sagittarius)
                      (sagittarius vm)
                      (sagittarius vm instruction)
                      (sagittarius vm debug)
                      (for (sagittarius compiler match) expand)
                      (sagittarius compiler util)
                      (sagittarius compiler procedure))
(define-syntax
 smatch
 (syntax-rules
  ()
  ((smatch (op arg |...|) clause |...|)
   (let ((x (op arg |...|))) (smatch x clause |...|)))
  ((smatch x) (error 'smatch "invalid form" x))
  ((smatch x (pat e |...|) clause |...|)
   (smatcher
    "base"
    pat
    "done"
    x
    (e |...|)
    (lambda () (smatch x clause |...|))))))
(define-syntax
 smatcher
 (syntax-rules
  (- ___ ? syntax)
  ((smatcher "base" () k arg |...|)
   (smatcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg |...|))
  ((smatcher "base" - k arg |...|)
   (smatcher k (lambda (x sk fk) (sk)) () arg |...|))
  ((smatcher "base" (? pred? p) k arg |...|)
   (smatcher "base" p "predicate" pred? k arg |...|))
  ((smatcher "predicate" code vars pred? k arg |...|)
   (smatcher
    k
    (lambda (x sk fk) (if (pred? x) (code x sk fk) (fk)))
    vars
    arg
    |...|))
  ((smatcher "base" (p1 ___ tailp |...|) k arg |...|)
   (smatcher "base" p1 "ellipses" (tailp |...|) k arg |...|))
  ((smatcher "ellipses" code vars (tailp |...|) k arg |...|)
   (smatcher
    k
    (lambda
     (x sk fk)
     (let
      loop
      ((x x) (result '()))
      (define (match-tail)
        (smatch
         x
         ((tailp |...|)
          (apply
           sk
           (if
            (null? result)
            (map (lambda (ignore) '()) 'vars)
            (apply map list (reverse result)))))
         (- (fk))))
      (if
       (null? x)
       (match-tail)
       (if
        (pair? x)
        (code
         (car x)
         (lambda car-vars (loop (cdr x) (cons car-vars result)))
         match-tail)
        (fk)))))
    vars
    arg
    |...|))
  ((smatcher "base" (p1 . p2) k arg |...|)
   (smatcher "base" p1 "pair" p2 k arg |...|))
  ((smatcher "pair" car-code car-vars p2 k arg |...|)
   (smatcher "base" p2 "pair-done" car-code car-vars k arg |...|))
  ((smatcher
    "pair-done"
    cdr-code
    (cdr-var |...|)
    car-code
    (car-var |...|)
    k
    arg
    |...|)
   (smatcher
    k
    (lambda
     (x sk fk)
     (if
      (pair? x)
      (car-code
       (car x)
       (lambda
        (car-var |...|)
        (cdr-code
         (cdr x)
         (lambda (cdr-var |...|) (sk car-var |...| cdr-var |...|))
         fk))
       fk)
      (fk)))
    (car-var |...| cdr-var |...|)
    arg
    |...|))
  ((smatcher "base" #(p ___) k arg |...|)
   (smatcher "base" (p ___) "vector" k arg |...|))
  ((smatcher "vector" list-code vars k arg |...|)
   (smatcher
    k
    (lambda
     (x sk fk)
     (if (vector? x) (list-code (vector->list x) sk fk) (fk)))
    vars
    arg
    |...|))
  ((smatcher "base" id k arg |...|)
   (smatcher k (lambda (x sk fk) (sk x)) (id) arg |...|))
  ((smatcher "done" code vars x (e |...|) fk)
   (code x (lambda vars e |...|) fk))))
(define-syntax
 imap
 (syntax-rules
  ()
  ((_ proc lis)
   (let
    loop
    ((r '()) (p lis))
    (if (null? p) (reverse r) (loop (cons (proc (car p)) r) (cdr p)))))))

(define-syntax
 $append-map1
 (syntax-rules () ((_ f l) (apply append (imap f l)))))

(define
 uniq
 (lambda
  (lst)
  (let
   loop
   ((lst lst) (ret '()))
   (cond
    ((null? lst) ret)
    (else
     (if
      (memq (car lst) ret)
      (loop (cdr lst) ret)
      (loop (cdr lst) (cons (car lst) ret))))))))

(define
 $for-each1-with-rindex
 (lambda
  (proc lst)
  (let
   loop
   ((i (- (length lst) 1)) (lst lst))
   (cond
    ((null? lst) '())
    (else (proc i (car lst)) (loop (- i 1) (cdr lst)))))))

(define-constant LEXICAL 0)

(define-constant SYNTAX 1)

(define-constant PATTERN 2)

(define pass0 (lambda (form env) form))

(define-constant SMALL_LAMBDA_SIZE 12)

(define-syntax
 generate-dispatch-table
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((_ prefix)
     `(vector
      ,@(map
       (lambda
        (p)
        (string->symbol
         (string-append
          (symbol->string prefix)
          "/"
          (symbol->string (car p)))))
       |.intermediate-tags.|)))))))
(define-syntax iform-tag (syntax-rules () ((_ iform) (vector-ref iform 0))))

(define-syntax
 has-tag?
 (syntax-rules () ((_ iform t) (eqv? t (iform-tag iform)))))

(define-simple-struct
 lvar
 'lvar
 make-lvar
 name
 (initval '())
 (ref-count 0)
 (set-count 0))

(define make-lvar+ (lambda (name) (make-lvar name)))

(define
 lvar?
 (lambda (obj) (and (vector? obj) (eq? (vector-ref obj 0) 'lvar))))

(define
 lvar-ref++!
 (lambda (lvar) (lvar-ref-count-set! lvar (+ (lvar-ref-count lvar) 1))))

(define
 lvar-ref--!
 (lambda (lvar) (lvar-ref-count-set! lvar (- (lvar-ref-count lvar) 1))))

(define
 lvar-set++!
 (lambda (lvar) (lvar-set-count-set! lvar (+ (lvar-set-count lvar) 1))))

(define-simple-struct $undef $UNDEF $undef)

(define-simple-struct $define $DEFINE $define src flags id expr)

(define-simple-struct $lref $LREF #f lvar)

(define $lref (lambda (lvar) (lvar-ref++! lvar) (vector $LREF lvar)))

(define-simple-struct $lset $LSET #f lvar expr)

(define
 $lset
 (lambda (lvar expr) (lvar-set++! lvar) (vector $LSET lvar expr)))

(define-simple-struct $gref $GREF $gref id)

(define-simple-struct $gset $GSET $gset id expr)

(define-simple-struct $const $CONST $const value)

(define $const-nil (let ((x ($const '()))) (lambda () x)))

(define $const-f (let ((x ($const #f))) (lambda () x)))

(define-simple-struct $if $IF $if src test then else)

(define-simple-struct $let $LET $let src type lvars inits body)

(define-simple-struct
 $lambda
 $LAMBDA
 $lambda
 src
 name
 args
 option
 lvars
 body
 flag
 (calls '())
 (free-lvars '()))

(define-simple-struct
 $receive
 $RECEIVE
 $receive
 src
 args
 option
 lvars
 expr
 body)

(define-simple-struct $label $LABEL $label src label body)

(define-simple-struct $seq $SEQ #f body)

(define
 $seq
 (lambda
  (exprs)
  (if
   (and (pair? exprs) (null? (cdr exprs)))
   (car exprs)
   (vector $SEQ exprs))))

(define-simple-struct $call $CALL $call src proc args flag (renv '()))

(define-simple-struct $asm $ASM $asm src insn args)

(define $it (let ((c `#(,$IT))) (lambda () c)))

(define-simple-struct $list $LIST $list src args)

(define-syntax $*-src (syntax-rules () ((_ iform) (vector-ref iform 1))))

(define-syntax $*-args (syntax-rules () ((_ iform) (vector-ref iform 2))))

(define-syntax
 $*-args-set!
 (syntax-rules () ((_ iform val) (vector-set! iform 2 val))))

(define
 iform-count-size-upto
 (lambda
  (iform limit)
  (define
   rec
   (lambda
    (iform cnt)
    (letrec-syntax
     ((sum-items
       (syntax-rules
        (*)
        ((_ cnt) cnt)
        ((_ cnt (* item1) item2 |...|)
         (let
          ((s1 (rec-list item1 cnt)))
          (if (>= s1 limit) limit (sum-items s1 item2 |...|))))
        ((_ cnt item1 item2 |...|)
         (let
          ((s1 (rec item1 cnt)))
          (if (>= s1 limit) limit (sum-items s1 item2 |...|)))))))
     (cond
      ((has-tag? iform $UNDEF) cnt)
      ((has-tag? iform $DEFINE) (sum-items (+ cnt 1) ($define-expr iform)))
      ((or
        (has-tag? iform $LREF)
        (has-tag? iform $GREF)
        (has-tag? iform $CONST))
       (+ cnt 1))
      ((has-tag? iform $LSET) (sum-items (+ cnt 1) ($lset-expr iform)))
      ((has-tag? iform $GSET) (sum-items (+ cnt 1) ($gset-expr iform)))
      ((has-tag? iform $IF)
       (sum-items
        (+ cnt 1)
        ($if-test iform)
        ($if-then iform)
        ($if-else iform)))
      ((has-tag? iform $LET)
       (sum-items (+ cnt 1) (* ($let-inits iform)) ($let-body iform)))
      ((has-tag? iform $LAMBDA) (sum-items (+ cnt 1) ($lambda-body iform)))
      ((has-tag? iform $RECEIVE)
       (sum-items (+ cnt 1) ($receive-expr iform) ($receive-body iform)))
      ((has-tag? iform $LABEL) (sum-items cnt ($label-body iform)))
      ((has-tag? iform $SEQ) (sum-items cnt (* ($seq-body iform))))
      ((has-tag? iform $CALL)
       (sum-items (+ cnt 1) ($call-proc iform) (* ($call-args iform))))
      ((has-tag? iform $ASM) (sum-items (+ cnt 1) (* ($asm-args iform))))
      ((has-tag? iform $IT) cnt)
      ((has-tag? iform $LIST) (sum-items (+ cnt 1) ($*-arg0 iform)))
      (else
       (scheme-error
        'iform-count-size-upto
        "[internal error] iform-count-size-upto: unknown iform tag:"
        (iform-tag iform)))))))
  (define
   rec-list
   (lambda
    (iform-list cnt)
    (cond
     ((null? iform-list) cnt)
     ((>= cnt limit) limit)
     (else (rec-list (cdr iform-list) (rec (car iform-list) cnt))))))
  (rec iform 0)))

(define
 iform-copy
 (lambda
  (iform lv-alist)
  (cond
   ((has-tag? iform $DEFINE)
    ($define
     ($define-src iform)
     ($define-flags iform)
     ($define-id iform)
     (iform-copy ($define-expr iform) lv-alist)))
   ((has-tag? iform $LREF)
    ($lref (iform-copy-lvar ($lref-lvar iform) lv-alist)))
   ((has-tag? iform $LSET)
    ($lset
     (iform-copy-lvar ($lset-lvar iform) lv-alist)
     (iform-copy ($lset-expr iform) lv-alist)))
   ((has-tag? iform $GREF) ($gref ($gref-id iform)))
   ((has-tag? iform $GSET)
    ($gset ($gset-id iform) (iform-copy ($gset-expr iform) lv-alist)))
   ((has-tag? iform $CONST) ($const ($const-value iform)))
   ((has-tag? iform $IF)
    ($if
     ($*-src iform)
     (iform-copy ($if-test iform) lv-alist)
     (iform-copy ($if-then iform) lv-alist)
     (iform-copy ($if-else iform) lv-alist)))
   ((has-tag? iform $LET)
    (receive
     (newlvs newalist)
     (iform-copy-zip-lvs ($let-lvars iform) lv-alist)
     ($let
      ($let-src iform)
      ($let-type iform)
      newlvs
      (let
       ((al (case ($let-type iform) ((let) lv-alist) ((rec) newalist))))
       (imap (lambda (init) (iform-copy init al)) ($let-inits iform)))
      (iform-copy ($let-body iform) newalist))))
   ((has-tag? iform $LAMBDA)
    (receive
     (newlvs newalist)
     (iform-copy-zip-lvs ($lambda-lvars iform) lv-alist)
     ($lambda
      ($lambda-src iform)
      ($lambda-name iform)
      ($lambda-args iform)
      ($lambda-option iform)
      newlvs
      (iform-copy ($lambda-body iform) newalist)
      ($lambda-flag iform))))
   ((has-tag? iform $RECEIVE)
    (receive
     (newlvs newalist)
     (iform-copy-zip-lvs ($lambda-lvars iform) lv-alist)
     ($receive
      ($receive-src iform)
      ($receive-args iform)
      ($receive-optarg iform)
      newlvs
      (iform-copy ($receive-expr iform) lv-alist)
      (iform-copy ($receive-body iform) newalist))))
   ((has-tag? iform $LABEL)
    (cond
     ((assq iform lv-alist) => cdr)
     (else
      (let
       ((newnode ($label ($label-src iform) ($label-label iform) #f)))
       ($label-body-set!
        newnode
        (iform-copy ($label-body iform) (acons iform newnode lv-alist)))
       newnode))))
   ((has-tag? iform $SEQ)
    ($seq (imap (lambda (ifm) (iform-copy ifm lv-alist)) ($seq-body iform))))
   ((has-tag? iform $CALL)
    ($call
     ($call-src iform)
     (iform-copy ($call-proc iform) lv-alist)
     (imap (lambda (arg) (iform-copy arg lv-alist)) ($call-args iform))
     #f))
   ((has-tag? iform $ASM)
    ($asm
     ($asm-src iform)
     ($asm-insn iform)
     (imap (lambda (arg) (iform-copy arg lv-alist)) ($asm-args iform))))
   ((has-tag? iform $LIST)
    ($list
     ($*-src iform)
     (imap (lambda (arg) (iform-copy arg lv-alist)) ($*-args iform))))
   ((has-tag? iform $IT) ($it))
   (else iform))))

(define
 iform-copy-zip-lvs
 (lambda
  (org-lvars lv-alist)
  (let
   ((new-lvars (imap (lambda (lv) (make-lvar (lvar-name lv))) org-lvars)))
   (values
    new-lvars
    (fold-right
     (lambda (a b c) (acons a b c))
     lv-alist
     org-lvars
     new-lvars)))))

(define
 iform-copy-lvar
 (lambda (lvar lv-alist) (cond ((assq lvar lv-alist) => cdr) (else lvar))))

(define
 pp-iform
 (lambda
  (iform)
  (define labels '())
  (define
   indent
   (lambda
    (count)
    (let
     loop
     ((i 0))
     (if (= i count) '() (begin (display #\space) (loop (+ i 1)))))))
  (define nl (lambda (ind) (newline) (indent ind)))
  (define id->string (lambda (id) (format "~a" (id-name id))))
  (define
   lvar->string
   (lambda
    (lvar)
    (format
     "~a[~a;~a]"
     (lvar-name lvar)
     (lvar-ref-count lvar)
     (lvar-set-count lvar))))
  (define
   rec
   (lambda
    (ind iform)
    (cond
     ((has-tag? iform $CONST) (format #t "($const ~s)" ($const-value iform)))
     ((has-tag? iform $UNDEF) (format #t "($const #<undef>)"))
     ((has-tag? iform $LAMBDA)
      (format
       #t
       "($lambda[~a;~a] ~a"
       ($lambda-name iform)
       (length ($lambda-calls iform))
       (map lvar->string ($lambda-lvars iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lambda-body iform))
      (display ")"))
     ((has-tag? iform $RECEIVE)
      (format #t "($receive ~a" (map lvar->string ($receive-lvars iform)))
      (nl (+ ind 4))
      (rec (+ ind 4) ($receive-expr iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($receive-body iform))
      (display ")"))
     ((has-tag? iform $LABEL)
      (cond
       ((assq iform labels) => (lambda (p) (format #t "label#~a" (cdr p))))
       (else
        (let
         ((num (length labels)))
         (set! labels (acons iform num labels))
         (format #t "($label #~a" num)
         (nl (+ ind 2))
         (rec (+ ind 2) ($label-body iform))
         (display ")")))))
     ((has-tag? iform $SEQ)
      (display "($seq")
      (for-each
       (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
       ($seq-body iform))
      (display ")"))
     ((has-tag? iform $LREF)
      (format #t "($lref ~a)" (lvar->string ($lref-lvar iform))))
     ((has-tag? iform $GREF)
      (format #t "($gref ~a)" (id->string ($gref-id iform))))
     ((has-tag? iform $DEFINE)
      (format
       #t
       "($define ~a ~a"
       ($define-flags iform)
       (id->string ($define-id iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($define-expr iform))
      (display ")"))
     ((has-tag? iform $CALL)
      (let
       ((pre
         (cond
          (($call-flag iform) => (lambda (x) (format "($call[~a] " x)))
          (else "($call "))))
       (display pre)
       (rec (+ ind (string-length pre)) ($call-proc iform))
       (for-each
        (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
        ($call-args iform))
       (display ")")))
     ((has-tag? iform $ASM)
      (let
       ((insn ($asm-insn iform)))
       (format #t "($asm ~a" (cons (insn-name (car insn)) (cdr insn))))
      (for-each
       (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
       ($asm-args iform))
      (display ")"))
     ((has-tag? iform $LET)
      (let*
       ((hdr
         (format
          "($let~a ("
          (case ($let-type iform) ((let) "") ((rec) "rec"))))
        (xind (+ ind (string-length hdr)))
        (first #t))
       (display hdr)
       (for-each
        (lambda
         (var init)
         (if first (set! first #f) (nl xind))
         (let
          ((z (format "(~a " (lvar->string var))))
          (display z)
          (rec (+ xind (string-length z)) init)
          (display ")")))
        ($let-lvars iform)
        ($let-inits iform))
       (display ")")
       (nl (+ ind 2))
       (rec (+ ind 2) ($let-body iform))
       (display ")")))
     ((has-tag? iform $IF)
      (display "($if ")
      (rec (+ ind 5) ($if-test iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($if-then iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($if-else iform))
      (display ")"))
     ((has-tag? iform $IT) (display "($it)"))
     ((has-tag? iform $LSET)
      (format #t "($lset ~a" (lvar->string ($lset-lvar iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lset-expr iform))
      (display ")"))
     ((has-tag? iform $GSET)
      (format #t "($gset ~a" (id->string ($gset-id iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($gset-expr iform))
      (display ")"))
     ((has-tag? iform $LIST)
      (format #t "($list ")
      (for-each
       (lambda (elt) (nl (+ ind 2)) (rec (+ ind 2) elt))
       ($list-args iform)))
     (else (scheme-error 'pp-iform "unknown tag:" (iform-tag iform))))))
  (rec 0 iform)
  (newline)))

(define
 variable-name
 (lambda
  (arg)
  (cond
   ((symbol? arg) arg)
   ((identifier? arg) (id-name arg))
   ((lvar? arg) (lvar-name arg))
   (else (scheme-error 'variable-name "variable required but got:" arg)))))

(define
 id->bound-gloc
 (lambda
  (id)
  (let
   ((gloc (find-binding (id-library id) (id-name id) #f)))
   (if (and gloc (gloc-bound? gloc)) gloc #f))))

(define
 global-eq?
 (lambda
  (var sym p1env)
  (and
   (variable? var)
   (let
    ((v (p1env-lookup p1env var LEXICAL)))
    (and (identifier? v) (eq? (id-name v) sym) (null? (id-envs v)))))))

(define
 ensure-library
 (lambda
  (thing name create?)
  (let
   ((mod
     (cond
      ((pair? thing) (find-library thing create?))
      ((library? thing) thing)
      ((symbol? thing) (find-library thing create?))
      (else
       (scheme-error
        'ensure-library
        (format
         "~a requires a library name or a library, but got: ~s"
         name
         thing))))))
   (or
    mod
    (scheme-error
     'ensure-library
     (format "~a: no such library: ~s" name thing))))))

(define-simple-struct
 p1env
 #f
 make-p1env
 library
 frames
 exp-name
 current-proc)

(define
 check-toplevel
 (lambda
  (form p1env)
  (unless
   (p1env-toplevel? p1env)
   (syntax-error
    "the form can appear only in the toplevel:"
    (unwrap-syntax form)))))

(define
 p1env-add-name
 (lambda
  (p1env name)
  (make-p1env
   (p1env-library p1env)
   (p1env-frames p1env)
   name
   (p1env-current-proc p1env))))

(define
 p1env-extend
 (lambda
  (p1env frame type)
  (make-p1env
   (p1env-library p1env)
   (acons type frame (p1env-frames p1env))
   (p1env-exp-name p1env)
   (p1env-current-proc p1env))))

(define
 p1env-extend/name
 (lambda
  (p1env frame type name)
  (make-p1env
   (p1env-library p1env)
   (acons type frame (p1env-frames p1env))
   name
   (p1env-current-proc p1env))))

(define
 p1env-extend/proc
 (lambda
  (p1env frame type proc)
  (make-p1env
   (p1env-library p1env)
   (acons type frame (p1env-frames p1env))
   (p1env-exp-name p1env)
   proc)))

(define
 p1env-sans-name
 (lambda
  (p1env)
  (if
   (p1env-exp-name p1env)
   (make-p1env
    (p1env-library p1env)
    (p1env-frames p1env)
    #f
    (p1env-current-proc p1env))
   p1env)))

(define
 p1env-swap-library
 (lambda
  (p1env library)
  (make-p1env
   library
   (p1env-frames p1env)
   (p1env-exp-name p1env)
   (p1env-current-proc p1env))))

(define
 make-bottom-p1env
 (lambda
  maybe-library
  (if
   (null? maybe-library)
   (make-p1env (vm-current-library) '())
   (make-p1env (car maybe-library) '()))))

(define
 formals->list
 (lambda
  (l)
  (cond
   ((null? l) l)
   ((pair? l) (cons (car l) (formals->list (cdr l))))
   (else (list l)))))

(define
 parse-lambda-args
 (lambda
  (formals)
  (let
   loop
   ((formals formals) (args '()))
   (cond
    ((null? formals) (values (reverse args) (length args) 0))
    ((pair? formals) (loop (cdr formals) (cons (car formals) args)))
    (else (values (reverse (cons formals args)) (length args) 1))))))

(define
 argcount-ok?
 (lambda
  (args reqargs optarg?)
  (let
   ((nargs (length args)))
   (or
    (and (not optarg?) (= nargs reqargs))
    (and optarg? (>= nargs reqargs))))))

(define
 expand-inlined-procedure
 (lambda
  (src iform iargs)
  (let
   ((lvars ($lambda-lvars iform))
    (args
     (adjust-arglist
      ($lambda-args iform)
      ($lambda-option iform)
      iargs
      ($lambda-name iform))))
   (for-each (lambda (lv a) (lvar-initval-set! lv a)) lvars args)
   ($let src 'let lvars args ($lambda-body iform)))))

(define
 adjust-arglist
 (lambda
  (reqargs optarg iargs name)
  (unless
   (argcount-ok? iargs reqargs (> optarg 0))
   (scheme-error
    'adjust-arglist
    (format
     "wrong number of arguments: ~s requires ~a, but got ~a"
     name
     reqargs
     (length iargs))))
  (if
   (zero? optarg)
   iargs
   (receive
    (reqs opts)
    (split-at iargs reqargs)
    (append! reqs (list ($list #f opts)))))))

(define
 parse-lambda-vars
 (lambda
  (vars)
  (let
   loop
   ((vars vars) (args '()) (n 0))
   (smatch
    vars
    (() (values (reverse args) n 0))
    ((x . y) (loop (cdr vars) (cons (car vars) args) (+ n 1)))
    (x (values (reverse (cons x args)) n 1))))))

(define
 pass1/find-symbol-in-lvars
 (lambda
  (symbol lvars)
  (cond
   ((null? lvars) #f)
   ((eq? symbol (lvar-name (car lvars))) (car lvars))
   (else (pass1/find-symbol-in-lvars symbol (cdr lvars))))))

(define
 global-id
 (lambda (id) (make-identifier id '() '(sagittarius compiler))))

(define lambda. (global-id 'lambda))

(define-syntax
 define-pass1-syntax
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- formals library . body)
     (let
      ((lib (ensure-library-name library)))
      (let
       ((name
         (string->symbol
          (string-append "syntax/" (symbol->string (car formals))))))
       `(let
        ((,name (lambda ,(cdr formals) ,@body)))
        (%insert-binding
         ',lib
         ',(car formals)
         (make-syntax ',(car formals) ,name))))))))))
(define
 ensure-identifier
 (lambda
  (sym-or-id p1env)
  (if
   (identifier? sym-or-id)
   sym-or-id
   (make-identifier sym-or-id '() (p1env-library p1env)))))

(define pass1/quote (lambda (obj) ($const (unwrap-syntax obj))))

(define-pass1-syntax
 (quote form p1env)
 :null
 (smatch
  form
  ((- obj) (pass1/quote obj))
  (else (syntax-error "malformed quote:" form))))

(define
 finalize-quasiquote
 (lambda
  (mode arg)
  (cond
   ((eq? mode 'quote) (list 'quote arg))
   ((eq? mode 'unquote) arg)
   ((eq? mode 'unquote-splicing)
    (scheme-error 'quasiquote ",@ in invalid context" arg))
   (else (cons mode arg)))))

(define
 descend-quasiquote
 (lambda
  (x level return)
  (cond
   ((vector? x) (descend-quasiquote-vector x level return))
   ((not (pair? x)) (return 'quote x))
   ((interesting-to-quasiquote? x 'quasiquote)
    (descend-quasiquote-pair x (+ level 1) return))
   ((interesting-to-quasiquote? x 'unquote)
    (cond
     ((= level 0) (return 'unquote (cadr x)))
     (else (descend-quasiquote-pair x (- level 1) return))))
   ((interesting-to-quasiquote? x 'unquote-splicing)
    (cond
     ((= level 0) (return 'unquote-splicing (cadr x)))
     (else (descend-quasiquote-pair x (- level 1) return))))
   (else (descend-quasiquote-pair x level return)))))

(define
 descend-quasiquote-pair
 (lambda
  (x level return)
  (descend-quasiquote
   (car x)
   level
   (lambda
    (car-mode car-arg)
    (descend-quasiquote
     (cdr x)
     level
     (lambda
      (cdr-mode cdr-arg)
      (cond
       ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote)) (return 'quote x))
       ((eq? car-mode 'unquote-splicing)
        (cond
         ((and (eq? cdr-mode 'quote) (null? cdr-arg))
          (return 'unquote car-arg))
         (else
          (return
           'append
           (list car-arg (finalize-quasiquote cdr-mode cdr-arg))))))
       (else
        (return
         'cons
         (list
          (finalize-quasiquote car-mode car-arg)
          (finalize-quasiquote cdr-mode cdr-arg)))))))))))

(define
 descend-quasiquote-vector
 (lambda
  (x level return)
  (descend-quasiquote
   (vector->list x)
   level
   (lambda
    (mode arg)
    (if
     (eq? mode 'quote)
     (return 'quote x)
     (return 'list->vector (list (finalize-quasiquote mode arg))))))))

(define
 interesting-to-quasiquote?
 (lambda
  (x marker)
  (and (pair? x) (variable? (car x)) (eq? (variable-name (car x)) marker))))

(define
 pass1/quasiquote
 (lambda (x level) (descend-quasiquote x level finalize-quasiquote)))

(define-pass1-syntax
 (quasiquote form p1env)
 :null
 (smatch
  form
  ((- obj) (pass1 (pass1/quasiquote (cadr form) 0) p1env))
  (- (syntax-error "malformed quasiquote" form))))

(define
 pass1/define
 (lambda
  (form oform flags module p1env)
  (check-toplevel oform p1env)
  (smatch
   form
   ((- (name . args) body ___)
    (pass1/define
     `(define ,name (,lambda. ,args ,@body))
     oform
     flags
     module
     p1env))
   ((- name expr)
    (unless (variable? name) (syntax-error "malformed define" oform))
    (let
     ((p1env (p1env-add-name p1env (variable-name name))))
     ($define
      oform
      flags
      (make-identifier (unwrap-syntax name) '() module)
      (pass1 (caddr form) (p1env-add-name p1env name)))))
   (- (syntax-error "malformed define" oform)))))

(define-pass1-syntax
 (define form p1env)
 :null
 (pass1/define form form '() (p1env-library p1env) p1env))

(define-pass1-syntax
 (define-constant form p1env)
 :sagittarius
 (pass1/define form form '(const) (p1env-library p1env) p1env))

(define
 pass1/eval-macro-rhs
 (lambda
  (who name expr p1env)
  (let*
   ((transformer (make-toplevel-closure (compile expr p1env)))
    (macro (make-macro-transformer name transformer (p1env-library p1env))))
   macro)))

(define-pass1-syntax
 (syntax-case form p1env)
 :null
 (smatch
  form
  ((- expr (literal ___) rule ___)
   (pass1
    (compile-syntax-case
     (p1env-exp-name p1env)
     expr
     literal
     rule
     (p1env-library p1env)
     (p1env-frames p1env)
     p1env)
    p1env))
  (- (syntax-error "malformed syntax-case" form))))

(define-pass1-syntax
 (syntax form p1env)
 :null
 (smatch
  form
  ((- tmpl) (pass1 (compile-syntax (p1env-exp-name p1env) tmpl p1env) p1env))
  (- (syntax-error "malformed syntax: expected exactly one datum" form))))

(define-pass1-syntax
 (define-syntax form p1env)
 :null
 (check-toplevel form p1env)
 (smatch
  form
  ((- name expr)
   (let
    ((transformer
      (pass1/eval-macro-rhs
       'define-syntax
       (variable-name name)
       expr
       (p1env-add-name p1env (variable-name name)))))
    (%insert-binding (p1env-library p1env) name transformer)
    ($undef)))
  (- (syntax-error "malformed define-syntax" form))))

(define-pass1-syntax
 (let-syntax form p1env)
 :null
 (smatch
  form
  ((- ((name trans-spec) ___) body ___)
   (let*
    ((trans
      (map
       (lambda
        (n x)
        (pass1/eval-macro-rhs
         'let-syntax
         (variable-name n)
         x
         (p1env-add-name p1env (variable-name n))))
       name
       trans-spec))
     (newenv (p1env-extend p1env (%map-cons name trans) SYNTAX)))
    (pass1/body body newenv)))
  (else (syntax-error "malformed let-syntax" form))))

(define-pass1-syntax
 (letrec-syntax form p1env)
 :null
 (smatch
  form
  ((- ((name trans-spec) ___) body ___)
   (let*
    ((newenv (p1env-extend p1env (%map-cons name trans-spec) SYNTAX))
     (trans
      (map
       (lambda
        (n x)
        (pass1/eval-macro-rhs
         'letrec-syntax
         (variable-name n)
         x
         (p1env-add-name newenv (variable-name n))))
       name
       trans-spec)))
    (for-each set-cdr! (cdar (p1env-frames newenv)) trans)
    (pass1/body body newenv)))
  (- (syntax-error "malformed let-syntax" form))))

(define
 er-rename
 (lambda
  (symid p1env dict)
  (unless
   (variable? symid)
   (scheme-error
    'er-macro-transformer
    "rename procrdure requires a symbol or an identifier, but got "
    symid))
  (if
   (symbol? symid)
   (or
    (hashtable-ref dict symid #f)
    (let
     ((var (p1env-lookup p1env symid SYNTAX)))
     (let
      ((id
        (if
         (identifier? var)
         var
         (make-identifier symid (p1env-frames p1env) (p1env-library p1env)))))
      (hashtable-set! dict symid id)
      id)))
   symid)))

(let
 ((lib (ensure-library-name :null)))
 (%insert-binding lib 'er-rename er-rename))
(define-pass1-syntax
 (%macroexpand form p1env)
 :sagittarius
 (smatch
  form
  ((- expr) ($const (%internal-macro-expand expr p1env #f)))
  (- (syntax-error "malformed %macroexpand" form))))

(define-pass1-syntax
 (%macroexpand-1 form p1env)
 :sagittarius
 (smatch
  form
  ((- expr) ($const (%internal-macro-expand expr p1env #t)))
  (- (syntax-error "malformed %macroexpand" form))))

(define
 pass1/lambda
 (lambda
  (form formals body p1env flag)
  (receive
   (vars reqargs opt)
   (parse-lambda-vars formals)
   (let*
    ((this-lvars (map make-lvar+ vars))
     (intform
      ($lambda form (p1env-exp-name p1env) reqargs opt this-lvars #f flag))
     (newenv
      (p1env-extend/proc p1env (%map-cons vars this-lvars) LEXICAL intform)))
    ($lambda-body-set! intform (pass1/body body newenv))
    intform))))

(define-pass1-syntax
 (lambda form p1env)
 :null
 (smatch
  form
  ((- formals . body) (pass1/lambda form formals body p1env #f))
  (- (syntax-error "malformed lambda" form))))

(define-pass1-syntax
 (receive form p1env)
 :sagittarius
 (smatch
  form
  ((- formals expr body ___)
   (receive
    (args reqargs opt)
    (parse-lambda-args formals)
    (let*
     ((lvars (map make-lvar+ args))
      (newenv (p1env-extend p1env (%map-cons args lvars) LEXICAL)))
     ($receive
      form
      reqargs
      opt
      lvars
      (pass1 expr p1env)
      (pass1/body body newenv)))))
  (- (syntax-error "malformed receive" form))))

(define-pass1-syntax
 (let form p1env)
 :null
 (smatch
  form
  ((- () body ___) (pass1/body body p1env))
  ((- ((var expr) ___) body ___)
   (let*
    ((lvars (map make-lvar+ var))
     (newenv (p1env-extend p1env (%map-cons var lvars) LEXICAL)))
    ($let
     form
     'let
     lvars
     (map
      (lambda
       (init lvar)
       (let
        ((iexpr (pass1 init (p1env-add-name p1env (lvar-name lvar)))))
        (lvar-initval-set! lvar iexpr)
        iexpr))
      expr
      lvars)
     (pass1/body body newenv))))
  ((- name ((var expr) ___) body ___)
   (unless (variable? name) (syntax-error "bad name for named let" name))
   (let
    ((lvar (make-lvar name))
     (args (map make-lvar+ var))
     (argenv (p1env-sans-name p1env)))
    (let*
     ((env1 (p1env-extend p1env `((,name unquote lvar)) LEXICAL))
      (env2 (p1env-extend/name env1 (%map-cons var args) LEXICAL name))
      (lmda ($lambda form name (length args) 0 args (pass1/body body env2))))
     (lvar-initval-set! lvar lmda)
     ($let
      form
      'rec
      (list lvar)
      (list lmda)
      ($call
       form
       ($lref lvar)
       (map (lambda (exp) (pass1 exp argenv)) expr))))))
  (- (syntax-error "malformed let:" form))))

(define-pass1-syntax
 (let* form p1env)
 :null
 (smatch
  form
  ((- ((var expr) ___) body ___)
   (let
    loop
    ((vars var) (inits expr) (p1env p1env) (src form))
    (if
     (null? vars)
     (pass1/body body p1env)
     (let*
      ((lv (make-lvar (car vars)))
       (newenv (p1env-extend p1env `((,(car vars) unquote lv)) LEXICAL))
       (iexpr (pass1 (car inits) (p1env-add-name p1env (car vars)))))
      (lvar-initval-set! lv iexpr)
      ($let
       src
       'let
       (list lv)
       (list iexpr)
       (loop (cdr vars) (cdr inits) newenv #f))))))
  (- (syntax-error "malformed let*" form))))

(define-pass1-syntax
 (letrec form p1env)
 :null
 (pass1/letrec form p1env 'letrec))

(define-pass1-syntax
 (letrec* form p1env)
 :null
 (pass1/letrec form p1env 'letrec*))

(define
 pass1/letrec
 (lambda
  (form p1env name)
  (smatch
   form
   ((- () body ___) (pass1/body body p1env))
   ((- ((var expr) ___) body ___)
    (let*
     ((lvars (map make-lvar+ var))
      (newenv (p1env-extend p1env (%map-cons var lvars) LEXICAL)))
     ($let
      form
      'rec
      lvars
      (map
       (lambda
        (lv init)
        (let
         ((iexpr (pass1 init (p1env-add-name newenv (lvar-name lv)))))
         (lvar-initval-set! lv iexpr)
         iexpr))
       lvars
       expr)
      (pass1/body body newenv))))
   (else (syntax-error (format "malformed ~a: ~s" name form))))))

(define-pass1-syntax
 (do form p1env)
 :null
 (smatch
  form
  ((- ((var init . update) ___) (test expr ___) body ___)
   (let*
    ((tmp (make-lvar 'do-proc))
     (args (map make-lvar+ var))
     (newenv (p1env-extend/proc p1env (%map-cons var args) LEXICAL 'do-proc))
     (clo
      ($lambda
       form
       'do-body
       (length var)
       0
       args
       ($if
        #f
        (pass1 test newenv)
        (if
         (null? expr)
         ($it)
         ($seq (imap (lambda (x) (pass1 x newenv)) expr)))
        ($seq
         (list
          (pass1/body body newenv)
          ($call
           form
           ($lref tmp)
           (map
            (lambda
             x
             (smatch
              x
              ((() arg) ($lref arg))
              (((expr) -) (pass1 expr newenv))
              (- (syntax-error "bad update expr in do" form))))
            update
            args)))))
       #f)))
    (lvar-initval-set! tmp clo)
    ($let
     form
     'rec
     (list tmp)
     (list clo)
     ($call
      form
      ($lref tmp)
      (map (lambda (x) (pass1 x (p1env-sans-name p1env))) init)))))
  (- (syntax-error "malformed do" form))))

(define-pass1-syntax
 (if form p1env)
 :null
 (smatch
  form
  ((- test then else)
   ($if
    form
    (pass1 test (p1env-sans-name p1env))
    (pass1 then p1env)
    (pass1 else p1env)))
  ((- test then)
   ($if
    form
    (pass1 test (p1env-sans-name p1env))
    (pass1 then p1env)
    ($undef)))
  (- (syntax-error "malformed if" form))))

(define-pass1-syntax
 (or form p1env)
 :null
 (define
  rec
  (lambda
   (exprs)
   (smatch
    exprs
    (() ($const-f))
    ((expr) (pass1 expr p1env))
    ((expr . more)
     ($if #f (pass1 expr (p1env-sans-name p1env)) ($it) (rec more)))
    (_ (syntax-error "malformed or" form)))))
 (rec (cdr form)))

(define-pass1-syntax
 (and form p1env)
 :null
 (define
  rec
  (lambda
   (exprs)
   (smatch
    exprs
    (() ($const #t))
    ((expr) (pass1 expr p1env))
    ((expr . more)
     ($if #f (pass1 expr (p1env-sans-name p1env)) (rec more) ($it)))
    (- (syntax-error "malformed and" form)))))
 (rec (cdr form)))

(define-pass1-syntax
 (when form p1env)
 :null
 (smatch
  form
  ((- test body ___)
   (let
    ((p1env (p1env-sans-name p1env)))
    ($if
     form
     (pass1 test p1env)
     ($seq (imap (lambda (b) (pass1 b p1env)) body))
     ($undef))))
  (- (syntax-error "malformed when" form))))

(define-pass1-syntax
 (unless form p1env)
 :null
 (smatch
  form
  ((- test body ___)
   (let
    ((p1env (p1env-sans-name p1env)))
    ($if
     form
     (pass1 test p1env)
     ($undef)
     ($seq (imap (lambda (b) (pass1 b p1env)) body)))))
  (- (syntax-error "malformed unless" form))))

(define-pass1-syntax
 (cond form p1env)
 :null
 (define
  process-clauses
  (lambda
   (cls)
   (smatch
    cls
    (() ($undef))
    ((((? (lambda (x) (global-eq? x 'else p1env)) -) exprs ___) . rest)
     (unless
      (null? rest)
      (syntax-error "'else' clause followed by more clauses" form))
     ($seq (imap (lambda (expr) (pass1 expr p1env)) exprs)))
    (((test (? (lambda (x) (global-eq? x '=> p1env)) -) proc) . rest)
     (let
      ((test (pass1 test p1env)) (tmp (make-lvar 'tmp)))
      (lvar-initval-set! tmp test)
      ($let
       (car cls)
       'let
       (list tmp)
       (list test)
       ($if
        (car cls)
        ($lref tmp)
        ($call
         (car cls)
         (pass1 proc (p1env-sans-name p1env))
         (list ($lref tmp)))
        (process-clauses rest)))))
    (((test) . rest)
     ($if
      (car cls)
      (pass1 test (p1env-sans-name p1env))
      ($it)
      (process-clauses rest)))
    (((test exprs ___) . rest)
     ($if
      (car cls)
      (pass1 test (p1env-sans-name p1env))
      ($seq (imap (lambda (expr) (pass1 expr p1env)) exprs))
      (process-clauses rest)))
    (- (syntax-error "bad clause in cond" form)))))
 (smatch
  form
  ((-) (syntax-error "at least one clause is required for cond" form))
  ((- clause ___) (process-clauses clause))
  (else (syntax-error "malformed cond" form))))

(define-pass1-syntax
 (case form p1env)
 :null
 (define
  expand-clauses
  (lambda
   (clauses tmp)
   (let
    loop
    ((clauses clauses))
    (if
     (null? clauses)
     '()
     (if
      (eq? 'else (caar clauses))
      clauses
      (if
       (= 1 (length (caar clauses)))
       (cons
        `((eqv? ',(caaar clauses) ,tmp) ,@(cdar clauses))
        (loop (cdr clauses)))
       (cons
        `((memv ,tmp ',(caar clauses)) ,@(cdar clauses))
        (loop (cdr clauses)))))))))
 (smatch
  form
  ((-) (syntax-error "at least one clause is required for case" form))
  ((- pred clauses ___)
   (let*
    ((tmp (gensym)) (expanded-clauses (expand-clauses clauses tmp)))
    (let
     ((expr `(let ((,tmp ,pred)) (cond ,@expanded-clauses))))
     (pass1 expr p1env))))
  (- (syntax-error "malformed case" form))))

(define-pass1-syntax
 (set! form p1env)
 :null
 (smatch
  form
  ((- name expr)
   (unless (variable? name) (syntax-error "malformed set!" form))
   (let
    ((var (pass1/lookup-head name p1env)))
    (if
     (lvar? var)
     ($lset var (pass1 expr p1env))
     (let
      ((gloc (find-binding (p1env-library p1env) (id-name var) #f)))
      (if
       gloc
       (let
        ((gval (gloc-ref gloc)))
        (cond
         ((macro? gval) (pass1 (call-macro-expander gval form p1env) p1env))
         (else ($gset (ensure-identifier var p1env) (pass1 expr p1env)))))
       ($gset (ensure-identifier var p1env) (pass1 expr p1env)))))))
  (- (syntax-error "malformed set!" form))))

(define-pass1-syntax
 (begin form p1env)
 :null
 ($seq (imap (lambda (expr) (pass1 expr p1env)) (cdr form))))

(define
 import-library
 (lambda
  (to from type etc p1env oform)
  (case
   type
   ((all) (load-library to from))
   ((only) (import-only to from etc))
   ((rename) (import-rename to from etc #f))
   ((prefix)
    (or
     (= (length etc) 1)
     (syntax-error "prefix must have only one symbol:" oform))
    (import-rename to from (car etc) #t))
   ((for)
    (cond
     ((memq 'expand etc)
      (library-transient-set! from #t)
      (load-library to from))
     (else (load-library to from))))
   (else (syntax-error "malformed import spec:" oform)))))

(define
 pass1/import
 (lambda
  (oform form p1env)
  (define
   import-prefix?
   (lambda (x) (case x ((for only except rename prefix) #t) (else #f))))
  (define
   process-clause
   (lambda
    (clause)
    (smatch
     clause
     (((? import-prefix? sym) (? pair? lib) etc ___)
      (import-library
       (p1env-library p1env)
       (ensure-library lib 'import #f)
       sym
       etc
       p1env
       oform))
     (((? keyword? key) (? pair? lib) etc ___)
      (import-library
       (p1env-library p1env)
       (ensure-library lib 'import #f)
       key
       etc
       p1env
       oform))
     (other
      (import-library
       (p1env-library p1env)
       (ensure-library other 'import #f)
       'all
       '()
       p1env
       oform)))))
  (smatch
   form
   ((- clauses ___)
    (let
     loop
     ((clauses clauses))
     (unless
      (null? clauses)
      (process-clause (car clauses))
      (loop (cdr clauses)))
     ($undef))))))

(define
 pass1/export
 (lambda
  (oform export newenv)
  (define (parse-export spec)
    (let
     loop
     ((spec spec) (ex '()) (renames '()))
     (cond
      ((null? spec) (values ex renames))
      ((symbol? (car spec)) (loop (cdr spec) (cons (car spec) ex) renames))
      ((keyword? (car spec))
       (case
        (car spec)
        ((:all) (loop (cdr spec) (cons (car spec) ex) renames))
        (else
         (syntax-error
          (format "unsupported export keyword ~s" (car spec))
          oform))))
      ((and (pair? (car spec)) (eq? (caar spec) 'rename))
       (let
        lp
        ((rename (cdar spec)) (r '()))
        (cond
         ((null? rename) (loop (cdr spec) ex (append r renames)))
         ((and
           (pair? (car rename))
           (symbol? (caar rename))
           (symbol? (cadar rename)))
          (lp (cdr rename) (acons (cadar rename) (caar rename) r)))
         (else
          (syntax-error "malformed rename clause in export spec" oform)))))
      (else
       (syntax-error "unknown object appeared in export spec" (car spec))))))
  (receive
   (exports renames)
   (parse-export (cdr export))
   (library-exported-set! (p1env-library newenv) (cons exports renames)))))

(define-pass1-syntax
 (import form p1env)
 :null
 (check-toplevel form p1env)
 (pass1/import form form p1env))

(define-pass1-syntax
 (library form p1env)
 :null
 (define
  check
  (lambda
   (tag clause name)
   (or
    (eq? (car clause) tag)
    (syntax-error
     (format "malformed ~s clause in library ~s" tag name)
     clause))))
 (check-toplevel form p1env)
 (smatch
  form
  ((- name export import body ___)
   (check 'import import name)
   (check 'export export name)
   (let*
    ((current-lib (ensure-library name 'library #t))
     (newenv (make-bottom-p1env current-lib)))
    (pass1/import form import newenv)
    (pass1/export form export newenv)
    (let
     ((r
       ($seq
        (append (map (lambda (x) (pass1 x newenv)) body) (list ($undef))))))
     r)))
  (- (syntax-error "malformed library" form))))

(define
 pass1/body
 (lambda (exprs p1env) (pass1/body-rec (map list exprs) '() p1env)))

(define
 pass1/body-rec
 (lambda
  (exprs intdefs p1env)
  (smatch
   exprs
   ((((op . args) . src) . rest)
    (or
     (and
      (not (assq op intdefs))
      (let
       ((head (pass1/lookup-head op p1env)))
       (cond
        (head
         (unless
          (list? args)
          (syntax-error
           "proper list required for function application or macro use"
           (caar exprs)))
         (cond
          ((lvar? head) (pass1/body-finish intdefs exprs p1env))
          ((macro? head)
           (pass1/body-macro-expand-rec head exprs intdefs p1env))
          ((syntax? head) (pass1/body-finish intdefs exprs p1env))
          ((global-eq? head 'define p1env)
           (let
            ((def
              (smatch
               args
               (((name . formals) . body)
                `(,name (,lambda. ,formals ,@body) unquote src))
               ((var init) `(,var ,init unquote src))
               (- (syntax-error "malformed internal define" (caar exprs))))))
            (pass1/body-rec rest (cons def intdefs) p1env)))
          ((global-eq? head 'define-syntax p1env)
           (smatch
            args
            ((name expr)
             (let*
              ((newenv (p1env-extend p1env `((,name ,expr)) SYNTAX))
               (trans
                (pass1/eval-macro-rhs
                 'define-syntax
                 (variable-name name)
                 expr
                 (p1env-add-name newenv (variable-name name)))))
              (for-each set-cdr! (cdar (p1env-frames newenv)) (list trans))
              (pass1/body-rec rest intdefs newenv)))
            (-
             (syntax-error "malformed internal define-syntax" (caar exprs)))))
          ((global-eq? head 'begin p1env)
           (pass1/body-rec
            (append (imap (lambda (x) (cons x src)) args) rest)
            intdefs
            p1env))
          ((identifier? head)
           (let
            ((gloc (id->bound-gloc head)))
            (if
             gloc
             (let
              ((gval (gloc-ref gloc)))
              (if
               (macro? gval)
               (pass1/body-macro-expand-rec gval exprs intdefs p1env)
               (pass1/body-finish intdefs exprs p1env)))
             (pass1/body-finish intdefs exprs p1env))))
          (else (scheme-error 'pass1/body "[internal] pass1/body" head))))
        (else #f))))
     (pass1/body-finish intdefs exprs p1env)))
   (- (pass1/body-finish intdefs exprs p1env)))))

(define
 pass1/body-macro-expand-rec
 (lambda
  (mac exprs intdefs p1env)
  (pass1/body-rec
   (acons
    (call-macro-expander mac (caar exprs) p1env)
    (cdar exprs)
    (cdr exprs))
   intdefs
   p1env)))

(define
 pass1/body-finish
 (lambda
  (intdefs exprs p1env)
  (if
   (null? intdefs)
   (pass1/body-rest exprs p1env)
   (let*
    ((intdefs. (reverse intdefs))
     (vars (map car intdefs.))
     (lvars (map make-lvar+ vars))
     (newenv (p1env-extend p1env (%map-cons vars lvars) LEXICAL)))
    ($let
     #f
     'rec
     lvars
     (map
      (lambda (lv def) (pass1/body-init lv def newenv))
      lvars
      (map cdr intdefs.))
     (pass1/body-rest exprs newenv))))))

(define
 pass1/body-init
 (lambda
  (lvar init&src newenv)
  (let
   ((e (p1env-add-name newenv (lvar-name lvar))))
   (let
    ((iexpr (pass1 (car init&src) e)))
    (lvar-initval-set! lvar iexpr)
    iexpr))))

(define
 pass1/body-rest
 (lambda
  (exprs p1env)
  (smatch
   exprs
   (() ($seq '()))
   ((expr&src) (pass1/body-1 expr&src p1env))
   (-
    (let
     ((stmtenv (p1env-sans-name p1env)))
     ($seq
      (let
       loop
       ((exprs exprs) (r '()))
       (if
        (null? (cdr exprs))
        (reverse (cons (pass1/body-1 (car exprs) p1env) r))
        (loop (cdr exprs) (cons (pass1/body-1 (car exprs) stmtenv) r))))))))))

(define
 pass1/body-1
 (lambda
  (expr&src p1env)
  (let ((src (cdr expr&src))) (pass1 (car expr&src) p1env))))

(define
 pass1/call
 (lambda
  (form proc args p1env)
  (cond
   ((null? args) ($call form proc '()))
   (else
    (let
     ((p1env (p1env-sans-name p1env)))
     ($call form proc (imap (lambda (arg) (pass1 arg p1env)) args)))))))

(define
 pass1/lookup-head
 (lambda
  (head p1env)
  (and (variable? head) (p1env-lookup p1env head SYNTAX))))

(define
 pass1
 (lambda
  (form p1env)
  (define
   pass1/global-call
   (lambda
    (id)
    (let*
     ((lib (id-library id)) (gloc (find-binding lib (id-name id) #f)))
     (if
      gloc
      (let
       ((gval (gloc-ref gloc)))
       (cond
        ((macro? gval) (pass1 (call-macro-expander gval form p1env) p1env))
        ((syntax? gval) (call-syntax-handler gval form p1env))
        ((inline? gval) (pass1/expand-inliner id gval))
        (else (pass1/call form ($gref id) (cdr form) p1env))))
      (pass1/call form ($gref id) (cdr form) p1env)))))
  (define
   pass1/expand-inliner
   (lambda
    (name proc)
    (let
     ((inliner (procedure-inliner proc)))
     (cond
      ((integer? inliner)
       (let
        ((nargs (length (cdr form))) (opt? (procedure-optional proc)))
        (unless
         (argcount-ok? (cdr form) (procedure-reqargs proc) opt?)
         (scheme-error
          'pass1/expand-inliner
          (format
           "wrong number of arguments: ~a requires ~a, but got ~a"
           (variable-name name)
           (procedure-reqargs proc)
           nargs)
          form))
        ($asm
         form
         (if opt? `(,inliner ,nargs) `(,inliner))
         (imap (lambda (x) (pass1 x p1env)) (cdr form)))))
      (else
       (let
        ((inlined (inliner form p1env)))
        (if
         (undefined? inlined)
         (pass1/call form ($gref name) (cdr form) p1env)
         inlined)))))))
  (cond
   ((pair? form)
    (unless
     (list? form)
     (scheme-error
      'pass1
      "proper list required for function application or macro use"
      form))
    (cond
     ((pass1/lookup-head (car form) p1env)
      =>
      (lambda
       (obj)
       (cond
        ((identifier? obj) (pass1/global-call obj))
        ((lvar? obj) (pass1/call form ($lref obj) (cdr form) p1env))
        ((syntax? obj) (call-syntax-handler obj form p1env))
        ((macro? obj) (pass1 (call-macro-expander obj form p1env) p1env))
        (else
         (scheme-error
          'pass1
          "[internal] unknown resolution of head:"
          obj)))))
     (else
      (pass1/call
       form
       (pass1 (car form) (p1env-sans-name p1env))
       (cdr form)
       p1env))))
   ((variable? form)
    (let
     ((r (p1env-lookup p1env form LEXICAL)))
     (cond
      ((lvar? r) ($lref r))
      ((identifier? r)
       (let*
        ((lib (id-library r)) (gloc (find-binding lib (id-name r) #f)))
        (if
         gloc
         (let
          ((gval (gloc-ref gloc)))
          (cond
           ((macro? gval) (pass1 (call-macro-expander gval form p1env) p1env))
           (else ($gref r))))
         ($gref r))))
      (else
       (scheme-error
        'pass1
        "[internal] p1env-lookup returned weird obj:"
        r)))))
   (else ($const form)))))

(define
 pass2/rec
 (lambda
  (iform penv tail?)
  ((vector-ref *pass2-dispatch-table* (iform-tag iform)) iform penv tail?)))

(define pass2 (lambda (iform) (pass2/rec iform '() #t)))

(define pass2/$UNDEF (lambda (iform penv tail?) iform))

(define
 pass2/$DEFINE
 (lambda
  (iform penv tail?)
  ($define-expr-set! iform (pass2/rec ($define-expr iform) penv #f))
  iform))

(define
 pass2/$LREF
 (lambda
  (iform penv tail?)
  (let
   ((lvar ($lref-lvar iform)))
   (if
    (zero? (lvar-set-count lvar))
    (let
     ((initval (lvar-initval lvar)))
     (cond
      ((not (vector? initval)) iform)
      ((has-tag? initval $CONST)
       (lvar-ref--! lvar)
       (vector-set! iform 0 $CONST)
       ($const-value-set! iform ($const-value initval))
       iform)
      ((and
        (has-tag? initval $LREF)
        (zero? (lvar-set-count ($lref-lvar initval))))
       (when
        (eq? iform initval)
        (scheme-error
         'pass2/$LREF
         "circular reference appeared in letrec binding"
         (lvar-name lvar)))
       (lvar-ref--! lvar)
       (lvar-ref++! ($lref-lvar initval))
       ($lref-lvar-set! iform ($lref-lvar initval))
       (pass2/$LREF iform penv tail?))
      (else iform)))
    iform))))

(define
 pass2/$LSET
 (lambda
  (iform penv tail?)
  ($lset-expr-set! iform (pass2/rec ($lset-expr iform) penv #f))
  iform))

(define pass2/$GREF (lambda (iform penv tail?) iform))

(define
 pass2/$GSET
 (lambda
  (iform penv tail?)
  ($gset-expr-set! iform (pass2/rec ($gset-expr iform) penv #f))
  iform))

(define pass2/$CONST (lambda (iform penv tail?) iform))

(define
 pass2/$IF
 (lambda
  (iform penv tail?)
  (let
   ((test (pass2/rec ($if-test iform) penv #f)))
   (or
    (and
     (has-tag? test $IF)
     (let
      ((test-then ($if-then test)) (test-else ($if-else test)))
      (cond
       ((has-tag? test-then $IT)
        (receive
         (l0 l1)
         (pass2/label-or-dup (pass2/rec ($if-then iform) penv tail?))
         (pass2/update-if
          iform
          ($if-test test)
          l0
          (pass2/rec ($if #f test-else l1 ($if-else iform)) penv tail?))))
       ((or
         (has-tag? test-else $IT)
         (and (has-tag? test-else $CONST) (not ($const-value test-else))))
        (receive
         (l0 l1)
         (pass2/label-or-dup (pass2/rec ($if-else iform) penv tail?))
         (pass2/update-if
          iform
          ($if-test test)
          (pass2/rec ($if #f test-then ($if-then iform) l0) penv tail?)
          l1)))
       ((and (has-tag? test-then $CONST) (not ($const-value test-then)))
        (receive
         (l0 l1)
         (pass2/label-or-dup (pass2/rec ($if-else iform) penv tail?))
         (pass2/update-if
          iform
          ($if-test test)
          (if (has-tag? l0 $IT) ($const-f) l0)
          (pass2/rec ($if #f test-else ($if-then iform) l1) penv tail?))))
       (else #f))))
    (pass2/update-if
     iform
     test
     (pass2/rec ($if-then iform) penv tail?)
     (pass2/rec ($if-else iform) penv tail?))))))

(define
 pass2/label-or-dup
 (lambda
  (iform)
  (if
   (memv (iform-tag iform) `(,$LREF ,$CONST ,$IT))
   (values iform (iform-copy iform '()))
   (let ((lab ($label #f #f iform))) (values lab lab)))))

(define
 pass2/update-if
 (lambda
  (iform new-test new-then new-else)
  (if
   (eq? new-then new-else)
   ($seq (list new-test new-then))
   (begin
    ($if-test-set! iform new-test)
    ($if-then-set! iform new-then)
    ($if-else-set! iform new-else)
    iform))))

(define
 pass2/$LET
 (lambda
  (iform penv tail?)
  (let
   ((lvars ($let-lvars iform))
    (inits (imap (lambda (init) (pass2/rec init penv #f)) ($let-inits iform)))
    (obody (pass2/rec ($let-body iform) penv tail?)))
   (for-each pass2/optimize-closure lvars inits)
   (receive
    (new-lvars new-inits removed-inits)
    (pass2/remove-unused-lvars lvars inits)
    (cond
     ((null? new-lvars)
      (if
       (null? removed-inits)
       obody
       ($seq (append! removed-inits (list obody)))))
     (else
      ($let-lvars-set! iform new-lvars)
      ($let-inits-set! iform new-inits)
      ($let-body-set! iform obody)
      (unless
       (null? removed-inits)
       (if
        (has-tag? obody $SEQ)
        ($seq-body-set! obody (append! removed-inits ($seq-body obody)))
        ($let-body-set! iform ($seq (append removed-inits (list obody))))))
      iform))))))

(define
 pass2/remove-unused-lvars
 (lambda
  (lvars inits)
  (let
   loop
   ((lvars lvars) (inits inits) (rl '()) (ri '()) (rr '()))
   (cond
    ((null? lvars) (values (reverse rl) (reverse ri) (reverse rr)))
    ((and
      (zero? (lvar-ref-count (car lvars)))
      (zero? (lvar-set-count (car lvars))))
     (loop
      (cdr lvars)
      (cdr inits)
      rl
      ri
      (if
       (memv (iform-tag (car inits)) `(,$CONST ,$LREF ,$LAMBDA))
       rr
       (cons (car inits) rr))))
    (else
     (loop
      (cdr lvars)
      (cdr inits)
      (cons (car lvars) rl)
      (cons (car inits) ri)
      rr))))))

(define
 pass2/optimize-closure
 (lambda
  (lvar lambda-node)
  (when
   (and
    (zero? (lvar-set-count lvar))
    (> (lvar-ref-count lvar) 0)
    (has-tag? lambda-node $LAMBDA))
   (or
    (and
     (= (lvar-ref-count lvar) (length ($lambda-calls lambda-node)))
     (receive
      (locals recs tail-recs)
      (pass2/classify-calls ($lambda-calls lambda-node) lambda-node)
      (and
       (null? recs)
       (pair? locals)
       (or
        (and
         (null? (cdr locals))
         (pass2/local-call-embedder lvar lambda-node (car locals) tail-recs))
        (and
         (null? tail-recs)
         (<
          (iform-count-size-upto lambda-node SMALL_LAMBDA_SIZE)
          SMALL_LAMBDA_SIZE)
         (pass2/local-call-inliner lvar lambda-node locals))))))
    (pass2/local-call-optimizer lvar lambda-node)))))

(define
 pass2/classify-calls
 (lambda
  (call&envs lambda-node)
  (define
   direct-call?
   (lambda
    (env)
    (let
     loop
     ((env env))
     (cond
      ((null? env) #t)
      ((eq? (car env) lambda-node) #t)
      ((eq? ($lambda-flag (car env)) 'dissolved) (loop (cdr env)))
      (else #f)))))
  (let
   loop
   ((call&envs call&envs) (local '()) (rec '()) (trec '()))
   (smatch
    call&envs
    (() (values local rec trec))
    (((call . env) . more)
     (case
      ($call-flag call)
      ((tail-rec)
       (if
        (direct-call? env)
        (loop more local rec (cons call trec))
        (loop more local (cons call rec) trec)))
      ((rec) (loop more local (cons call rec) trec))
      (else (loop more (cons call local) rec trec))))))))

(define
 pass2/local-call-optimizer
 (lambda
  (lvar lambda-node)
  (let
   ((reqargs ($lambda-args lambda-node))
    (optarg ($lambda-option lambda-node))
    (name ($lambda-name lambda-node))
    (calls ($lambda-calls lambda-node)))
   (for-each
    (lambda
     (call)
     ($call-args-set!
      (car call)
      (adjust-arglist reqargs optarg ($call-args (car call)) name))
     ($call-flag-set! (car call) 'local))
    calls)
   ($lambda-calls-set! lambda-node '()))))

(define
 pass2/local-call-embedder
 (lambda
  (lvar lambda-node call rec-calls)
  (let
   ((reqargs ($lambda-args lambda-node))
    (optarg ($lambda-option lambda-node))
    (name ($lambda-name lambda-node)))
   ($call-args-set!
    call
    (adjust-arglist reqargs optarg ($call-args call) name))
   (lvar-ref--! lvar)
   ($call-flag-set! call 'embed)
   ($call-proc-set! call lambda-node)
   ($lambda-flag-set! lambda-node 'dissolved)
   (unless
    (null? rec-calls)
    (let
     ((body ($label ($lambda-src lambda-node) #f ($lambda-body lambda-node))))
     ($lambda-body-set! lambda-node body)
     (for-each
      (lambda
       (jcall)
       (lvar-ref--! lvar)
       ($call-args-set!
        jcall
        (adjust-arglist reqargs optarg ($call-args jcall) name))
       ($call-proc-set! jcall call)
       ($call-flag-set! jcall 'jump))
      rec-calls)
     '())))))

(define
 pass2/local-call-inliner
 (lambda
  (lvar lambda-node calls)
  (define
   inline-it
   (lambda
    (call-node lambda-node)
    (let
     ((inlined
       (expand-inlined-procedure
        ($*-src lambda-node)
        lambda-node
        ($call-args call-node))))
     (vector-set! call-node 0 $SEQ)
     (if
      (has-tag? inlined $SEQ)
      ($seq-body-set! call-node ($seq-body inlined))
      ($seq-body-set! call-node (list inlined))))))
  (lvar-ref-count-set! lvar 0)
  ($lambda-flag-set! lambda-node 'dissolved)
  (let
   loop
   ((calls calls))
   (cond
    ((null? (cdr calls)) (inline-it (car calls) lambda-node))
    (else
     (inline-it (car calls) (iform-copy lambda-node '()))
     (loop (cdr calls)))))))

(define
 pass2/$LAMBDA
 (lambda
  (iform penv tail?)
  ($lambda-body-set!
   iform
   (pass2/rec ($lambda-body iform) (cons iform penv) #t))
  iform))

(define
 pass2/$RECEIVE
 (lambda
  (iform penv tail?)
  ($receive-expr-set! iform (pass2/rec ($receive-expr iform) penv #f))
  ($receive-body-set! iform (pass2/rec ($receive-body iform) penv tail?))
  iform))

(define pass2/$LABEL (lambda (iform penv tail?) iform))

(define
 pass2/$SEQ
 (lambda
  (iform penv tail?)
  (if
   (null? ($seq-body iform))
   iform
   (let
    loop
    ((body ($seq-body iform)) (r '()))
    (cond
     ((null? (cdr body))
      ($seq-body-set!
       iform
       (reverse (cons (pass2/rec (car body) penv tail?) r)))
      iform)
     (else (loop (cdr body) (cons (pass2/rec (car body) penv #f) r))))))))

(define
 pass2/$CALL
 (lambda
  (iform penv tail?)
  (cond
   (($call-flag iform) iform)
   (else
    (let
     ((proc ($call-proc iform)) (args ($call-args iform)))
     ($call-proc-set! iform (pass2/rec proc penv #f))
     (cond
      ((has-tag? proc $LAMBDA)
       (pass2/rec
        (expand-inlined-procedure ($*-src iform) proc args)
        penv
        tail?))
      ((and (has-tag? proc $LREF) (pass2/head-lref proc penv tail?))
       =>
       (lambda
        (result)
        (cond
         ((vector? result)
          ($call-proc-set! iform result)
          (pass2/rec
           (expand-inlined-procedure ($*-src iform) result args)
           penv
           tail?))
         (else
          (let
           ((lambda-node (lvar-initval ($lref-lvar proc))))
           ($call-flag-set! iform result)
           ($lambda-calls-set!
            lambda-node
            (acons iform penv ($lambda-calls lambda-node)))
           ($call-args-set!
            iform
            (imap (lambda (arg) (pass2/rec arg penv #f)) args))
           iform)))))
      (else
       ($call-args-set!
        iform
        (imap (lambda (arg) (pass2/rec arg penv #f)) args))
       iform)))))))

(define
 pass2/head-lref
 (lambda
  (iform penv tail?)
  (let*
   ((lvar ($lref-lvar iform)) (initval (lvar-initval lvar)))
   (and
    (zero? (lvar-set-count lvar))
    (vector? initval)
    (has-tag? initval $LAMBDA)
    (cond
     ((pass2/self-recursing? initval penv) (if tail? 'tail-rec 'rec))
     ((= (lvar-ref-count lvar) 1)
      (lvar-ref--! lvar)
      (lvar-initval-set! lvar ($undef))
      initval)
     (else 'local))))))

(define pass2/self-recursing? (lambda (node penv) (memq node penv)))

(define
 pass2/$ASM
 (lambda
  (iform penv tail?)
  ($asm-args-set!
   iform
   (imap (lambda (arg) (pass2/rec arg penv #f)) ($asm-args iform)))
  iform))

(define pass2/$IT (lambda (iform penv tail?) iform))

(define
 pass2/narg-inliner
 (lambda
  (iform penv tail?)
  ($*-args-set!
   iform
   (imap (lambda (arg) (pass2/rec arg penv #f)) ($*-args iform)))
  iform))

(define pass2/$LIST pass2/narg-inliner)

(define *pass2-dispatch-table* (generate-dispatch-table pass2))

(define make-new-label (lambda src ($label (if (null? src) #f (car src)))))

(define
 normal-context?
 (lambda (ctx) (or (eq? ctx 'norma/bottom) (eq? ctx 'normal/top))))

(define
 stmt-context?
 (lambda (ctx) (or (eq? ctx 'stmt/bottom) (eq? ctx 'stmt/top))))

(define tail-context? (lambda (ctx) (eq? ctx 'tail)))

(define
 bottom-context?
 (lambda
  (ctx)
  (or (eq? ctx 'stmt/bottom) (eq? ctx 'normal/bottom) (eq? ctx 'tail))))

(define
 top-context?
 (lambda (ctx) (or (eq? ctx 'stmt/top) (eq? ctx 'normal/top))))

(define
 normal-context
 (lambda
  (prev-cxt)
  (if (bottom-context? prev-cxt) 'normal/bottom 'normal/top)))

(define
 stmt-context
 (lambda (prev-cxt) (if (bottom-context? prev-cxt) 'stmt/bottom 'stmt/top)))

(define-simple-struct
 renv
 #f
 make-renv
 (locals '())
 (frees '())
 (sets (make-eq-hashtable))
 (can-free '())
 (display 0))

(define
 make-new-renv
 (lambda
  (renv locals free sets can-free add-display?)
  (make-renv
   locals
   free
   (pass3/add-sets (renv-sets renv) sets)
   (if
    (null? can-free)
    (renv-can-free renv)
    (append (renv-can-free renv) (list can-free)))
   (+ (renv-display renv) (if add-display? 1 0)))))

(define
 renv-add-can-free1
 (lambda
  (renv vars)
  (make-renv
   (renv-locals renv)
   (renv-frees renv)
   (renv-sets renv)
   (append (renv-can-free renv) (list vars))
   (renv-display renv))))

(define
 renv-add-can-free2
 (lambda
  (renv vars1 vars2)
  (make-renv
   (renv-locals renv)
   (renv-frees renv)
   (renv-sets renv)
   (append (renv-can-free renv) (list vars1) (list vars2))
   (renv-display renv))))

(define
 renv-copy
 (lambda
  (renv)
  (make-renv
   (renv-locals renv)
   (renv-frees renv)
   (renv-sets renv)
   (renv-can-free renv)
   (renv-display renv))))

(define
 eq-hashtable-copy
 (lambda
  (ht)
  (let
   ((ret (make-eq-hashtable)))
   (hashtable-for-each (lambda (key value) (hashtable-set! ret key value)) ht)
   ret)))

(define
 hashtable-set-true!
 (lambda
  (ht keys)
  (let
   loop
   ((keys keys))
   (cond
    ((null? keys) ht)
    (else (hashtable-set! ht (car keys) #t) (loop (cdr keys)))))))

(define
 pass3/add-sets
 (lambda
  (sets new-sets)
  (if
   (null? new-sets)
   sets
   (hashtable-set-true! (eq-hashtable-copy sets) new-sets))))

(define
 pass3/rec
 (lambda
  (iform cb renv ctx)
  ((vector-ref *pass3-dispatch-table* (iform-tag iform)) iform cb renv ctx)))

(define
 pass3
 (lambda
  (iform cb renv ctx last)
  (let
   ((maxstack (pass3/rec iform cb renv ctx)))
   (code-builder-finish-builder cb last)
   cb)))

(define
 pass3/exists-in-can-frees?
 (lambda
  (lvar can-frees)
  (cond
   ((null? can-frees) #f)
   ((memq lvar (car can-frees)) #t)
   (else (pass3/exists-in-can-frees? lvar (cdr can-frees))))))

(define
 pass3/find-free
 (lambda
  (iform locals renv)
  (define
   rec
   (lambda
    (i l labels-seen)
    (cond
     ((has-tag? i $CONST) '())
     ((has-tag? i $LET)
      (append
       ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($let-inits i))
       (rec ($let-body i) ($let-lvars i) labels-seen)))
     ((has-tag? i $RECEIVE)
      (append
       (rec ($receive-expr i) l labels-seen)
       (rec ($receive-body i) ($receive-lvars i) labels-seen)))
     ((has-tag? i $SEQ)
      ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($seq-body i)))
     ((has-tag? i $LAMBDA)
      (rec ($lambda-body i) ($lambda-lvars i) labels-seen))
     ((has-tag? i $LSET)
      (let
       ((lvar ($lset-lvar i)))
       (if
        (pass3/exists-in-can-frees? lvar (renv-can-free renv))
        (cons lvar (rec ($lset-expr i) l labels-seen))
        (rec ($lset-expr i) l labels-seen))))
     ((has-tag? i $LREF)
      (let
       ((lvar ($lref-lvar i)))
       (cond
        ((memq lvar l) '())
        ((pass3/exists-in-can-frees? lvar (renv-can-free renv)) (list lvar))
        (else '()))))
     ((has-tag? i $GSET) (rec ($gset-expr i) l labels-seen))
     ((has-tag? i $GREF) '())
     ((has-tag? i $IF)
      (append
       (rec ($if-test i) l labels-seen)
       (rec ($if-then i) l labels-seen)
       (rec ($if-else i) l labels-seen)))
     ((has-tag? i $ASM)
      ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($asm-args i)))
     ((has-tag? i $DEFINE) (rec ($define-expr i) l labels-seen))
     ((has-tag? i $CALL)
      (append
       ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($call-args i))
       (rec ($call-proc i) l labels-seen)))
     ((has-tag? i $LABEL)
      (if
       (memq i labels-seen)
       '()
       (rec ($label-body i) l (cons i labels-seen))))
     ((has-tag? i $IT) '())
     ((has-tag? i $UNDEF) '())
     ((has-tag? i $LIST)
      ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($*-args i)))
     (else (scheme-error 'pass3/find-free "unknown iform:" (iform-tag i))))))
  (uniq (rec iform locals '()))))

(define
 pass3/find-sets
 (lambda
  (iform lvars)
  (define
   rec
   (lambda
    (i labels-seen)
    (cond
     ((has-tag? i $CONST) '())
     ((has-tag? i $LET)
      (append
       ($append-map1 (lambda (init) (rec init labels-seen)) ($let-inits i))
       (rec ($let-body i) labels-seen)))
     ((has-tag? i $RECEIVE)
      (append
       (rec ($receive-expr i) labels-seen)
       (rec ($receive-body i) labels-seen)))
     ((has-tag? i $SEQ)
      ($append-map1 (lambda (fm) (rec fm labels-seen)) ($seq-body i)))
     ((has-tag? i $LAMBDA) (rec ($lambda-body i) labels-seen))
     ((has-tag? i $LSET)
      (let
       ((lvar ($lset-lvar i)))
       (append
        (if (memq lvar lvars) (list lvar) '())
        (rec ($lset-expr i) labels-seen))))
     ((has-tag? i $LREF) '())
     ((has-tag? i $GSET) (rec ($gset-expr i) labels-seen))
     ((has-tag? i $GREF) '())
     ((has-tag? i $IF)
      (append
       (rec ($if-test i) labels-seen)
       (rec ($if-then i) labels-seen)
       (rec ($if-else i) labels-seen)))
     ((has-tag? i $ASM)
      ($append-map1 (lambda (arg) (rec arg labels-seen)) ($asm-args i)))
     ((has-tag? i $DEFINE) (rec ($define-expr i) labels-seen))
     ((has-tag? i $CALL)
      (append
       ($append-map1 (lambda (arg) (rec arg labels-seen)) ($call-args i))
       (rec ($call-proc i) labels-seen)))
     ((has-tag? i $LABEL)
      (if
       (memq i labels-seen)
       '()
       (rec ($label-body i) (cons i labels-seen))))
     ((has-tag? i $IT) '())
     ((has-tag? i $UNDEF) '())
     ((has-tag? i $LIST)
      ($append-map1 (lambda (arg) (rec arg labels-seen)) ($*-args i)))
     (else (scheme-error 'pass3/find-sets "unknown iform:" (iform-tag i))))))
  (uniq (rec iform '()))))

(define
 pass3/collect-free
 (lambda
  (cb frees renv)
  (let
   loop
   ((size 0) (reversed-frees (reverse frees)))
   (cond
    ((null? reversed-frees) size)
    (else
     (let
      ((stack-size (pass3/compile-refer (car reversed-frees) cb renv)))
      (cb-emit0! cb PUSH)
      (loop (+ size stack-size) (cdr reversed-frees))))))))

(define
 pass3/symbol-lookup
 (lambda
  (lvar cb renv return-local return-free)
  (let
   ((locals (renv-locals renv)) (frees (renv-frees renv)))
   (let
    next-local
    ((locals locals) (n 0))
    (if
     (null? locals)
     (let
      next-free
      ((free frees) (n 0))
      (cond
       ((null? free)
        (scheme-error
         'pass3/symbol-lookup
         "bug? Unknown lvar:"
         (lvar-name lvar)))
       ((eq? (car free) lvar) (return-free cb n lvar))
       (else (next-free (cdr free) (+ n 1)))))
     (if
      (eq? (car locals) lvar)
      (return-local cb n lvar)
      (next-local (cdr locals) (+ n 1))))))))

(define
 pass3/return-refer-local
 (lambda (cb n lvar) (cb-emit1i! cb LREF n (lvar-name lvar)) 0))

(define
 pass3/return-assign-local
 (lambda (cb n lvar) (cb-emit1i! cb LSET n (lvar-name lvar)) 0))

(define
 pass3/return-refer-free
 (lambda (cb n lvar) (cb-emit1i! cb FREF n (lvar-name lvar)) 0))

(define
 pass3/return-assign-free
 (lambda (cb n lvar) (cb-emit1i! cb FSET n (lvar-name lvar)) 0))

(define
 pass3/compile-refer
 (lambda
  (lvar cb renv)
  (pass3/symbol-lookup
   lvar
   cb
   renv
   pass3/return-refer-local
   pass3/return-refer-free)))

(define
 pass3/compile-assign
 (lambda
  (lvar cb renv)
  (pass3/symbol-lookup
   lvar
   cb
   renv
   pass3/return-assign-local
   pass3/return-assign-free)))

(define
 pass3/make-boxes
 (lambda
  (cb sets vars)
  ($for-each1-with-rindex
   (lambda (index var) (if (memq var sets) (cb-emit1! cb BOX index)))
   vars)))

(define
 pass3/ensure-label
 (lambda
  (cb label-node)
  (or
   ($label-label label-node)
   (let ((lab (make-new-label))) ($label-label-set! label-node lab) lab))))

(define pass3/$UNDEF (lambda (iform cb renv ctx) (cb-emit0! cb UNDEF) 0))

(define
 pass3/$DEFINE
 (lambda
  (iform cb renv ctx)
  (let
   ((d (pass3/rec ($define-expr iform) cb renv 'normal/bottom))
    (f (if (memq 'const ($define-flags iform)) 1 0)))
   (cb-emit1oi! cb DEFINE f ($define-id iform) ($*-src iform))
   d)))

(define
 pass3/$LREF
 (lambda
  (iform cb renv ctx)
  (pass3/compile-refer ($lref-lvar iform) cb renv)
  (when
   (hashtable-ref (renv-sets renv) ($lref-lvar iform) #f)
   (cb-emit0! cb UNBOX))
  0))

(define
 pass3/$LSET
 (lambda
  (iform cb renv ctx)
  (let
   ((val-stack-size
     (pass3/rec ($lset-expr iform) cb renv (normal-context ctx)))
    (var-stack-size (pass3/compile-assign ($lset-lvar iform) cb renv)))
   (+ val-stack-size var-stack-size))))

(define
 pass3/$GREF
 (lambda
  (iform cb renv ctx)
  (let ((id ($gref-id iform))) (cb-emit0oi! cb GREF id id) 0)))

(define
 pass3/$GSET
 (lambda
  (iform cb renv ctx)
  (let
   ((val-stack-size
     (pass3/rec ($gset-expr iform) cb renv (normal-context ctx)))
    (id ($gset-id iform)))
   (cb-emit0oi! cb GSET id id)
   val-stack-size)))

(define
 pass3/$CONST
 (lambda
  (iform cb renv ctx)
  (unless (stmt-context? ctx) (cb-emit0o! cb CONST ($const-value iform)))
  0))

(define
 pass3/$IF
 (lambda
  (iform cb renv ctx)
  (cond
   ((and
     (not (has-tag? ($if-then iform) $IT))
     (not (has-tag? ($if-else iform) $IT))
     (has-tag? ($if-test iform) $ASM)
     (eqv? (car ($asm-insn ($if-test iform))) NOT))
    (pass3/$IF
     ($if
      ($*-src iform)
      (car ($asm-args ($if-test iform)))
      ($if-else iform)
      ($if-then iform))
     cb
     renv
     ctx))
   ((and
     (has-tag? ($if-else iform) $CONST)
     (not ($const-value ($if-else iform))))
    ($if-else-set! iform ($it))
    (pass3/$IF iform cb renv ctx))
   (else (pass3/branch-core iform cb renv ctx)))))

(define
 pass3/branch-core
 (lambda
  (iform cb renv ctx)
  (let
   ((test ($if-test iform)))
   (cond
    ((has-tag? test $ASM)
     (let
      ((code (car ($asm-insn test))))
      (cond
       ((eqv? code NULLP) (pass3/branch-on-arg1 iform BNNULL cb renv ctx))
       ((eqv? code EQ) (pass3/branch-on-arg2 iform BNEQ cb renv ctx))
       ((eqv? code EQV) (pass3/branch-on-arg2 iform BNEQV cb renv ctx))
       ((eqv? code NUM_EQ) (pass3/branch-on-arg2 iform BNNUME cb renv ctx))
       ((eqv? code NUM_LE) (pass3/branch-on-arg2 iform BNLE cb renv ctx))
       ((eqv? code NUM_LT) (pass3/branch-on-arg2 iform BNLT cb renv ctx))
       ((eqv? code NUM_GE) (pass3/branch-on-arg2 iform BNGE cb renv ctx))
       ((eqv? code NUM_GT) (pass3/branch-on-arg2 iform BNGT cb renv ctx))
       (else (pass3/branch-on-false iform cb renv ctx)))))
    ((has-tag? test $CONST)
     (pass3/rec
      (if
       ($const-value test)
       (if (has-tag? ($if-then iform) $IT) test ($if-then iform))
       (if (has-tag? ($if-else iform) $IT) test ($if-else iform)))
      cb
      renv
      ctx))
    (else (pass3/branch-on-false iform cb renv ctx))))))

(define
 pass3/branch-on-arg1
 (lambda
  (iform insn cb renv ctx)
  (let*
   ((args ($asm-args ($if-test iform)))
    (arg1-size (max (pass3/rec (car args) cb renv 'normal/top) 1)))
   (pass3/emit-then-else iform cb insn arg1-size renv ctx))))

(define
 pass3/branch-on-arg2
 (lambda
  (iform insn cb renv ctx)
  (let*
   ((args ($asm-args ($if-test iform)))
    (arg1-size (max (pass3/rec (car args) cb renv (normal-context ctx)) 1)))
   (cb-emit0! cb PUSH)
   (pass3/emit-then-else
    iform
    cb
    insn
    (max (pass3/rec (cadr args) cb renv 'normal/top) arg1-size)
    renv
    ctx))))

(define
 pass3/branch-on-false
 (lambda
  (iform cb renv ctx)
  (let
   ((test-size (pass3/rec ($if-test iform) cb renv (normal-context ctx))))
   (pass3/emit-then-else iform cb TEST test-size renv ctx))))

(define
 pass3/emit-then-else
 (lambda
  (iform cb insn test-size renv ctx)
  (let
   ((end-of-else (make-new-label)) (begin-of-else (make-new-label)))
   (cb-emit0oi! cb insn begin-of-else ($*-src iform))
   (let
    ((then-size (pass3/rec ($if-then iform) cb renv ctx)))
    (cond
     ((has-tag? ($if-else iform) $IT)
      (cb-label-set! cb begin-of-else)
      (+ test-size then-size))
     (else
      (cb-emit0o! cb JUMP end-of-else)
      (cb-label-set! cb begin-of-else)
      (let
       ((else-size (pass3/rec ($if-else iform) cb renv ctx)))
       (cb-label-set! cb end-of-else)
       (+ test-size then-size else-size))))))))

(define
 pass3/$LET
 (lambda
  (iform cb renv ctx)
  (if
   (eq? ($let-type iform) 'rec)
   (pass3/letrec iform cb renv ctx)
   (pass3/let iform cb renv ctx))))

(define
 pass3/letrec
 (lambda
  (iform cb renv ctx)
  (let*
   ((vars ($let-lvars iform))
    (body ($let-body iform))
    (args ($let-inits iform))
    (free
     (append
      ($append-map1
       (lambda
        (i)
        (pass3/find-free
         i
         vars
         (renv-add-can-free2 renv (renv-locals renv) (renv-frees renv))))
       args)
      (pass3/find-free
       body
       vars
       (renv-add-can-free2 renv (renv-frees renv) (renv-locals renv)))))
    (sets
     (append
      vars
      (pass3/find-sets body vars)
      ($append-map1 (lambda (i) (pass3/find-sets i vars)) args)))
    (free-length (length free))
    (nargs (length vars))
    (need-display? (> free-length 0)))
   (cb-emit1i! cb LET_FRAME (+ nargs free-length) ($*-src iform))
   (let
    ((free-size (if (> free-length 0) (pass3/collect-free cb free renv) 0)))
    (when need-display? (cb-emit1! cb DISPLAY free-length))
    (let
     loop
     ((args args))
     (cond
      ((null? args) '())
      (else (cb-emit0! cb UNDEF) (cb-emit0! cb PUSH) (loop (cdr args)))))
    (pass3/make-boxes cb sets vars)
    (if
     (tail-context? ctx)
     (cb-emit1! cb POP_LET_FRAME nargs)
     (cb-emit1! cb ENTER nargs))
    (let*
     ((new-renv (make-new-renv renv vars free sets vars need-display?))
      (assign-size
       (let
        loop
        ((args args) (vars vars) (size 0) (index 0))
        (cond
         ((null? args) size)
         (else
          (let
           ((stack-size (pass3/rec (car args) cb new-renv 'normal/bottom)))
           (cb-emit1i! cb LSET index (lvar-name (car vars)))
           (loop (cdr args) (cdr vars) (+ stack-size size) (+ index 1)))))))
      (body-size (pass3/rec body cb new-renv ctx)))
     (unless (tail-context? ctx) (cb-emit0! cb LEAVE))
     (+ body-size assign-size free-size))))))

(define
 pass3/let
 (lambda
  (iform cb renv ctx)
  (let*
   ((vars ($let-lvars iform))
    (body ($let-body iform))
    (free
     (append
      ($append-map1
       (lambda
        (i)
        (pass3/find-free
         i
         (renv-locals renv)
         (renv-add-can-free2 renv (renv-frees renv) (renv-locals renv))))
       ($let-inits iform))
      (pass3/find-free
       body
       vars
       (renv-add-can-free2 renv (renv-frees renv) (renv-locals renv)))))
    (sets (pass3/find-sets body vars))
    (free-length (length free))
    (nargs (length vars))
    (need-display? (> free-length 0)))
   (cb-emit1i! cb LET_FRAME (+ nargs free-length) ($*-src iform))
   (let
    ((free-size (if (> free-length 0) (pass3/collect-free cb free renv) 0)))
    (when need-display? (cb-emit1! cb DISPLAY free-length))
    (let
     ((args-size
       (pass3/compile-args
        ($let-inits iform)
        cb
        (make-new-renv renv (renv-locals renv) free sets '() need-display?)
        ctx)))
     (pass3/make-boxes cb sets vars)
     (if
      (tail-context? ctx)
      (cb-emit1! cb POP_LET_FRAME nargs)
      (cb-emit1! cb ENTER nargs))
     (let*
      ((new-renv (make-new-renv renv vars free sets vars need-display?))
       (body-size (pass3/rec body cb new-renv ctx)))
      (unless (tail-context? ctx) (cb-emit0! cb LEAVE))
      (+ body-size args-size free-size)))))))

(define
 pass3/$LAMBDA
 (lambda
  (iform cb renv ctx)
  (let*
   ((vars ($lambda-lvars iform))
    (body ($lambda-body iform))
    (free
     (pass3/find-free
      body
      vars
      (renv-add-can-free2 renv (renv-locals renv) (renv-frees renv))))
    (sets (pass3/find-sets body vars))
    (lambda-cb (make-code-builder))
    (frlen (length free))
    (frsiz (if (> frlen 0) (pass3/collect-free cb free renv) 0))
    (nargs (length vars)))
   (pass3/make-boxes lambda-cb sets vars)
   (let*
    ((new-renv (make-new-renv renv vars free sets vars #f))
     (body-size (pass3/rec body lambda-cb new-renv 'tail)))
    (cb-emit0! lambda-cb RET)
    (cb-emit-closure!
     cb
     CLOSURE
     lambda-cb
     ($lambda-name iform)
     nargs
     (> ($lambda-option iform) 0)
     frlen
     (+ body-size frsiz nargs (pass3/frame-size))
     ($lambda-src iform))
    0))))

(define
 pass3/$RECEIVE
 (lambda
  (iform cb renv ctx)
  (let*
   ((vars ($receive-lvars iform))
    (body ($receive-body iform))
    (free
     (append
      (pass3/find-free
       ($receive-expr iform)
       (renv-locals renv)
       (renv-add-can-free2 renv (renv-locals renv) (renv-frees renv)))
      (pass3/find-free
       body
       vars
       (renv-add-can-free2 renv (renv-locals renv) (renv-frees renv)))))
    (sets (pass3/find-sets body vars))
    (nargs (length vars))
    (free-length (length free)))
   (cb-emit1i! cb LET_FRAME (+ nargs free-length) ($*-src iform))
   (let*
    ((free-size (if (> free-length 0) (pass3/collect-free cb free renv) 0))
     (need-display? (> free-length 0)))
    (when need-display? (cb-emit1! cb DISPLAY free-length))
    (let
     ((expr-size
       (pass3/rec
        ($receive-expr iform)
        cb
        (make-new-renv
         renv
         (renv-locals renv)
         free
         (hashtable-keys (renv-sets renv))
         '()
         need-display?)
        (normal-context ctx))))
     (cb-emit2i!
      cb
      RECEIVE
      ($receive-args iform)
      ($receive-option iform)
      ($*-src iform))
     (pass3/make-boxes cb sets vars)
     (if
      (tail-context? ctx)
      (cb-emit1! cb POP_LET_FRAME nargs)
      (cb-emit1! cb ENTER nargs))
     (let*
      ((new-renv (make-new-renv renv vars free sets vars need-display?))
       (body-size (pass3/rec body cb new-renv ctx)))
      (unless (tail-context? ctx) (cb-emit0! cb LEAVE))
      (+ body-size expr-size free-size)))))))

(define
 pass3/$LABEL
 (lambda
  (iform cb renv ctx)
  (let
   ((label ($label-label iform)))
   (cond
    (label (cb-emit0o! cb JUMP label) 0)
    (else
     (cb-label-set! cb (pass3/ensure-label cb iform))
     (pass3/rec ($label-body iform) cb renv ctx))))))

(define
 pass3/$SEQ
 (lambda
  (iform cb renv ctx)
  (let
   ((exprs ($seq-body iform)))
   (cond
    ((null? exprs) 0)
    ((null? (cdr exprs)) (pass3/rec (car exprs) cb renv ctx))
    (else
     (let
      loop
      ((exprs exprs) (depth 0))
      (if
       (null? (cdr exprs))
       (max (pass3/rec (car exprs) cb renv ctx) depth)
       (loop
        (cdr exprs)
        (max (pass3/rec (car exprs) cb renv (stmt-context ctx)) depth)))))))))

(define
 pass3/$CALL
 (lambda
  (iform cb renv ctx)
  (case
   ($call-flag iform)
   ((local) (pass3/local-call iform cb renv ctx))
   ((embed) (pass3/embed-call iform cb renv ctx))
   ((jump) (pass3/jump-call iform cb renv ctx))
   (else
    (if
     (and
      (bottom-context? ctx)
      (has-tag? ($call-proc iform) $LET)
      (all-args-simple? ($call-args iform)))
     (pass3/head-heavy-call iform cb renv ctx)
     (pass3/normal-call iform cb renv ctx))))))

(define
 pass3/local-call
 (lambda
  (iform cb renv ctx)
  (let
   ((end-of-frame (make-new-label)) (tail? (tail-context? ctx)))
   (unless tail? (cb-emit0o! cb FRAME end-of-frame))
   (let*
    ((args-size (pass3/compile-args ($call-args iform) cb renv ctx))
     (proc-size (pass3/rec ($call-proc iform) cb renv 'normal/top))
     (nargs (length ($call-args iform))))
    (if
     tail?
     (cb-emit1i! cb LOCAL_TAIL_CALL nargs ($*-src iform))
     (cb-emit1i! cb LOCAL_CALL nargs ($*-src iform)))
    (unless tail? (cb-label-set! cb end-of-frame))
    (+ args-size proc-size)))))

(define
 pass3/embed-call
 (lambda
  (iform cb renv ctx)
  (let*
   ((proc ($call-proc iform))
    (args ($call-args iform))
    (nargs (length args))
    (label ($lambda-body proc))
    (body ($label-body label))
    (vars ($lambda-lvars proc))
    (free (pass3/find-free body vars renv))
    (sets (pass3/find-sets body vars))
    (frlen (length free)))
   ($call-renv-set! iform (renv-copy renv))
   (cb-emit1i! cb LET_FRAME (+ nargs frlen) ($*-src iform))
   (let*
    ((frsiz (if (> frlen 0) (pass3/collect-free cb free renv) 0))
     (need-display? (> frlen 0)))
    (when need-display? (cb-emit1! cb DISPLAY frlen))
    (let
     ((args-size
       (pass3/compile-args
        args
        cb
        (make-new-renv renv (renv-locals renv) free sets '() need-display?)
        ctx)))
     (if
      (tail-context? ctx)
      (cb-emit1! cb POP_LET_FRAME nargs)
      (cb-emit1! cb ENTER nargs))
     (cb-emit0! cb MARK)
     (cb-label-set! cb (pass3/ensure-label cb label))
     (pass3/make-boxes cb sets vars)
     (let
      ((body-size
        (pass3/rec
         body
         cb
         (make-new-renv renv vars free sets vars need-display?)
         ctx)))
      (unless (tail-context? ctx) (cb-emit0! cb LEAVE))
      (+ nargs body-size frsiz)))))))

(define
 pass3/jump-call
 (lambda
  (iform cb renv ctx)
  (let
   ((label ($lambda-body ($call-proc ($call-proc iform))))
    (nargs (length ($call-args iform))))
   (let
    ((ret
      (pass3/compile-args
       ($call-args iform)
       cb
       (renv-copy renv)
       (normal-context ctx))))
    (cb-emit2!
     cb
     SHIFTJ
     nargs
     (- (renv-display renv) (renv-display ($call-renv ($call-proc iform))) 1))
    (cb-emit0o! cb JUMP (pass3/ensure-label cb label))
    ret))))

(define
 pass3/head-heavy-call
 (lambda
  (iform cb renv ctx)
  (let
   ((end-of-frame (make-new-label)) (tail? (tail-context? ctx)))
   (unless tail? (cb-emit0o! cb FRAME end-of-frame))
   (let*
    ((proc-size (pass3/rec ($call-proc iform) cb renv (normal-context ctx)))
     (args-size (pass3/compile-args ($call-args iform) cb renv 'normal/top))
     (nargs (length ($call-args iform))))
    (if
     tail?
     (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
     (cb-emit1i! cb CALL nargs ($*-src iform)))
    (unless tail? (cb-label-set! cb end-of-frame))
    (+ args-size proc-size)))))

(define
 pass3/normal-call
 (lambda
  (iform cb renv ctx)
  (let
   ((end-of-frame (make-new-label)) (tail? (tail-context? ctx)))
   (unless tail? (cb-emit0o! cb FRAME end-of-frame))
   (let*
    ((args-size (pass3/compile-args ($call-args iform) cb renv ctx))
     (proc-size (pass3/rec ($call-proc iform) cb renv 'normal/top))
     (nargs (length ($call-args iform))))
    (if
     tail?
     (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
     (cb-emit1i! cb CALL nargs ($*-src iform)))
    (unless tail? (cb-label-set! cb end-of-frame))
    (+ args-size proc-size)))))

(define
 all-args-simple?
 (lambda
  (args)
  (cond
   ((null? args) #t)
   ((memv (iform-tag (car args)) `(,$LREF ,$CONST))
    (all-args-simple? (cdr args)))
   (else #f))))

(define
 pass3/$ASM
 (lambda
  (iform cb renv ctx)
  (let
   ((info ($*-src iform)) (insn ($asm-insn iform)) (args ($asm-args iform)))
   (case/unquote
    (car insn)
    ((EQ) (pass3/asm-eq info (car args) (cadr args) cb renv ctx))
    ((EQV) (pass3/asm-eqv info (car args) (cadr args) cb renv ctx))
    ((NUM_EQ) (pass3/asm-numeq info (car args) (cadr args) cb renv ctx))
    ((NUM_LT NUM_LE NUM_GT NUM_GE)
     (pass3/asm-numcmp info (car insn) (car args) (cadr args) cb renv ctx))
    ((ADD) (pass3/asm-add info (car args) (cadr args) cb renv ctx))
    ((SUB) (pass3/asm-sub info (car args) (cadr args) cb renv ctx))
    ((MUL) (pass3/asm-mul info (car args) (cadr args) cb renv ctx))
    ((DIV) (pass3/asm-div info (car args) (cadr args) cb renv ctx))
    (else (pass3/asm-generic cb insn args info renv))))))

(define
 pass3/asm-generic
 (lambda
  (cb insn args info renv)
  (case
   (length args)
   ((0) (pass3/emit-asm! cb insn info) 0)
   ((1)
    (let
     ((d (pass3/rec (car args) cb renv 'normal/top)))
     (pass3/emit-asm! cb insn info)
     d))
   ((2)
    (let
     ((d0 (pass3/rec (car args) cb renv 'normal/top)))
     (cb-emit0! cb PUSH)
     (let
      ((d1 (pass3/rec (cadr args) cb renv 'normal/top)))
      (pass3/emit-asm! cb insn info)
      (max d0 (+ d1 1)))))
   (else
    (let
     loop
     ((args args) (depth 0) (count 0))
     (cond
      ((null? (cdr args))
       (let
        ((d (pass3/rec (car args) cb renv 'normal/top)))
        (pass3/emit-asm! cb insn info)
        (max depth (+ count d))))
      (else
       (let
        ((d (pass3/rec (car args) cb renv 'normal/top)))
        (cb-emit0! cb PUSH)
        (loop (cdr args) (max depth (+ d count)) (+ count 1))))))))))

(define
 pass3/emit-asm!
 (lambda
  (cb insn info)
  (smatch
   insn
   ((code) (cb-emit0i! cb code info))
   ((code arg0) (cb-emit1i! cb code arg0 info))
   ((code arg0 arg1) (cb-emit2i! cb code arg0 arg1 info)))))

(define-syntax
 pass3/builtin-twoargs
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- info code param arg0 arg1)
     (let
      ((d0 (gensym)) (d1 (gensym)))
      `(let
       ((,d0 (pass3/rec ,arg0 cb renv (normal-context ctx))))
       (cb-emit0! cb PUSH)
       (let
        ((,d1 (pass3/rec ,arg1 cb renv 'normal/top)))
        (cb-emit1i! cb ,code ,param ,info)
        (max ,d0 (+ ,d1 1))))))))))
(define-syntax
 pass3/builtin-oneargs
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- info code param arg0)
     (let
      ((d0 (gensym)))
      `(let
       ((,d0 (pass3/rec ,arg0 cb renv (normal-context ctx))))
       (cb-emit1i! cb ,code ,param ,info)
       ,d0)))))))
(define-syntax
 pass3/builtin-nargs
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- info code args) `(%pass3/builtin-nargs cb ,info ,code ,args renv))))))
(define pass3/$IT (lambda (iform cb renv ctx) 0))

(define
 %pass3/builtin-nargs
 (lambda
  (cb info code args renv)
  (if
   (null? args)
   (begin (cb-emit1i! cb code 0 info) 0)
   (let
    loop
    ((as args) (depth 0) (count 0))
    (cond
     ((null? (cdr as))
      (let
       ((d (pass3/rec (car as) cb renv 'normal/top)))
       (cb-emit1i! cb code (length args) info)
       (max (+ d count) depth)))
     (else
      (let
       ((d (pass3/rec (car as) cb renv 'normal/top)))
       (cb-emit0! cb PUSH)
       (loop (cdr as) (max (+ d count) depth) (+ count 1)))))))))

(define
 pass3/$LIST
 (lambda
  (iform cb renv ctx)
  (%pass3/builtin-nargs cb ($*-src iform) LIST ($*-args iform) renv)))

(define
 pass3/asm-eq
 (lambda (info x y cb renv ctx) (pass3/builtin-twoargs info EQ 0 x y)))

(define
 pass3/asm-eqv
 (lambda (info x y cb renv ctx) (pass3/builtin-twoargs info EQV 0 x y)))

(define
 pass3/asm-numeq
 (lambda (info x y cb renv ctx) (pass3/builtin-twoargs info NUM_EQ 0 x y)))

(define
 pass3/asm-numcmp
 (lambda (info code x y cb renv ctx) (pass3/builtin-twoargs info code 0 x y)))

(define
 pass3/asm-add
 (lambda
  (info x y cb renv ctx)
  (or
   (and
    (has-tag? x $CONST)
    (integer-fits-insn-arg? ($const-value x))
    (pass3/builtin-oneargs info ADDI ($const-value x) y))
   (and
    (has-tag? y $CONST)
    (integer-fits-insn-arg? ($const-value y))
    (pass3/builtin-oneargs info ADDI ($const-value y) x))
   (pass3/builtin-twoargs info ADD 0 x y))))

(define
 pass3/asm-sub
 (lambda
  (info x y cb renv ctx)
  (or
   (and
    (has-tag? x $CONST)
    (integer-fits-insn-arg? ($const-value x))
    (pass3/builtin-oneargs info SUBI ($const-value x) y))
   (and
    (has-tag? y $CONST)
    (integer-fits-insn-arg? ($const-value y))
    (pass3/builtin-oneargs info ADDI (- ($const-value y)) x))
   (pass3/builtin-twoargs info SUB 0 x y))))

(define
 pass3/asm-mul
 (lambda (info x y cb renv ctx) (pass3/builtin-twoargs info MUL 0 x y)))

(define
 pass3/asm-div
 (lambda (info x y cb renv ctx) (pass3/builtin-twoargs info DIV 0 x y)))

(define *pass3-dispatch-table* (generate-dispatch-table pass3))

(define
 pass3/compile-args
 (lambda
  (args cb renv ctx)
  (if
   (null? args)
   0
   (let
    ((d (pass3/rec (car args) cb renv (normal-context ctx))))
    (cb-emit0! cb PUSH)
    (let
     loop
     ((args (cdr args)) (depth (+ d 1)) (cnt 1))
     (if
      (null? args)
      depth
      (let
       ((d (pass3/rec (car args) cb renv 'normal/top)))
       (cb-emit0! cb PUSH)
       (loop (cdr args) (max depth (+ d cnt 1)) (+ cnt 1)))))))))

(define
 asm-arg1
 (lambda (form insn x p1env) ($asm form insn (list (pass1 x p1env)))))
(define
 asm-arg2
 (lambda
  (form insn x y p1env)
  ($asm form insn (list (pass1 x p1env) (pass1 y p1env)))))
(define
 gen-inliner-arg2
 (lambda
  (insn)
  (lambda
   (form p1env)
   (smatch
    form
    ((- x y) (asm-arg2 form (list insn) x y p1env))
    (- (undefined))))))
(define
 check-numeric-constant
 (lambda
  (form p1env)
  (if
   (number? form)
   (values form #f)
   (let
    ((f (pass1 form p1env)))
    (if
     (and (has-tag? f $CONST) (number? ($const-value f)))
     (values ($const-value f) f)
     (values #f f))))))
(define
 ensure-inexact-const
 (lambda (numconstval) ($const (inexact numconstval))))
(define-syntax
 define-builtin-inliner
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- name lib proc)
     (let
      ((debug-name
        (string->symbol (string-append "inliner/" (symbol->string name)))))
      `(let
       ((,debug-name ,proc))
       (procedure-inliner-set!
        (find-procedure ',name (ensure-library-name ,lib))
        ,debug-name))))))))
(define-syntax
 define-builtin-inliner-+
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- op insn const)
     `(define-builtin-inliner
      ,op
      :null
      (lambda
       (form cenv)
       (define (fold-+ asm rest)
         (fold
          (lambda
           (arg asm)
           (receive
            (val tree)
            (check-numeric-constant arg cenv)
            ($asm form (list ,insn) (list asm (or tree (,const val))))))
          asm
          rest))
       (let
        inline
        ((args (cdr form)))
        (smatch
         args
         (() (,const 0))
         ((x)
          (receive
           (num tree)
           (check-numeric-constant x cenv)
           (if
            num
            (or tree (,const num))
            ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
         ((x y . more)
          (receive
           (xval xtree)
           (check-numeric-constant x cenv)
           (receive
            (yval ytree)
            (check-numeric-constant y cenv)
            (if
             xval
             (if
              yval
              (inline (cons (,op xval yval) more))
              (fold-+ ytree (cons xval more)))
             (if
              yval
              (fold-+ xtree (cons yval more))
              (fold-+
               ($asm form (list ,insn) `(,xtree ,ytree))
               more)))))))))))))))
(define-syntax
 define-builtin-inliner--
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- op insn const)
     `(define-builtin-inliner
      ,op
      :null
      (lambda
       (form cenv)
       (define (fold-- asm rest)
         (fold
          (lambda
           (arg asm)
           (receive
            (val tree)
            (check-numeric-constant arg cenv)
            ($asm form (list ,insn) (list asm (or tree (,const val))))))
          asm
          rest))
       (let
        inline
        ((args (cdr form)))
        (smatch
         args
         (() (error "procedure requires at least one argument:" form))
         ((x)
          (receive
           (num tree)
           (check-numeric-constant x cenv)
           (if
            num
            (,const (- num))
            ,(if
             (eq? op '-)
             '($asm form `(,NEG) (list tree))
             '($call form ($gref (ensure-identifier '|-.| cenv)) `(,tree))))))
         ((x y . more)
          (receive
           (xval xtree)
           (check-numeric-constant x cenv)
           (receive
            (yval ytree)
            (check-numeric-constant y cenv)
            (if
             xval
             (if
              yval
              (if
               (null? more)
               ($const (,op xval yval))
               (inline (cons (,op xval yval) more)))
              (fold--
               ($asm form (list ,insn) (list (or xtree ($const xval)) ytree))
               more))
             (fold--
              ($asm form (list ,insn) (list xtree (or ytree ($const yval))))
              more))))))))))))))
(define-syntax
 define-builtin-inliner-*
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- op insn const)
     `(define-builtin-inliner
      ,op
      :null
      (lambda
       (form cenv)
       (let
        inline
        ((args (cdr form)))
        (smatch
         args
         (() (,const 1))
         ((x)
          (receive
           (num tree)
           (check-numeric-constant x cenv)
           (if
            (number? num)
            (or tree (,const num))
            ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
         ((x y . more)
          (receive
           (xval xtree)
           (check-numeric-constant x cenv)
           (receive
            (yval ytree)
            (check-numeric-constant y cenv)
            (if
             (and xval yval)
             (inline (cons (,op xval yval) more))
             (fold
              (lambda
               (arg asm)
               ($asm form (list ,insn) (list asm (pass1 arg cenv))))
              ($asm
               form
               (list ,insn)
               (list (or xtree (,const xval)) (or ytree (,const yval))))
              more))))))))))))))
(define-syntax
 define-builtin-inliner-/
 (er-macro-transformer
  (lambda
   (form rename compare)
   (smatch
    form
    ((- op insn const)
     `(define-builtin-inliner
      ,op
      :null
      (lambda
       (form cenv)
       (let
        inline
        ((args (cdr form)))
        (smatch
         args
         (() (error "procedure requires at least one argument:" form))
         ((x)
          (receive
           (num tree)
           (check-numeric-constant x cenv)
           (if
            (number? num)
            ($const (,op num))
            ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
         ((x y . more)
          (receive
           (xval xtree)
           (check-numeric-constant x cenv)
           (receive
            (yval ytree)
            (check-numeric-constant y cenv)
            (if
             (and xval yval)
             (if
              (null? more)
              ($const (,op xval yval))
              (inline (cons (,op xval yval) more)))
             (fold
              (lambda
               (arg asm)
               ($asm form (list ,insn) (list asm (pass1 arg cenv))))
              ($asm
               form
               (list ,insn)
               (list (or xtree (,const xval)) (or ytree (,const yval))))
              more))))))))))))))
(define-builtin-inliner-+ + ADD $const)

(define-builtin-inliner-+ |+.| ADDI ensure-inexact-const)

(define-builtin-inliner-- - SUB $const)

(define-builtin-inliner-- |-.| SUBI ensure-inexact-const)

(define-builtin-inliner-* * MUL $const)

(define-builtin-inliner-* *. MULI ensure-inexact-const)

(define-builtin-inliner-/ / DIV $const)

(define-builtin-inliner-/ /. DIVI ensure-inexact-const)

(define-builtin-inliner = :null (gen-inliner-arg2 NUM_EQ))

(define-builtin-inliner < :null (gen-inliner-arg2 NUM_LT))

(define-builtin-inliner <= :null (gen-inliner-arg2 NUM_LE))

(define-builtin-inliner > :null (gen-inliner-arg2 NUM_GT))

(define-builtin-inliner >= :null (gen-inliner-arg2 NUM_GE))

(define-builtin-inliner
 zero?
 :null
 (lambda
  (form p1env)
  (smatch
   form
   ((- arg) ($asm form `(,NUM_EQ) `(,(pass1 arg p1env) ,($const 0))))
   (- (scheme-error 'zero? "wrong number of arguments" form)))))

(define-builtin-inliner
 vector-ref
 :null
 (lambda
  (form p1env)
  (smatch
   form
   ((- vec ind) (asm-arg2 form `(,VEC_REF) vec ind p1env))
   (- (scheme-error 'vector-ref "wrong number of arguments" form)))))

(define-builtin-inliner
 vector-set!
 :null
 (lambda
  (form p1env)
  (smatch
   form
   ((- vec ind val)
    ($asm
     form
     `(,VEC_SET)
     `(,(pass1 vec p1env) ,(pass1 ind p1env) ,(pass1 val p1env))))
   (- (scheme-error 'vector-set! "wrong number of arguments" form)))))

(define-builtin-inliner
 acons
 :null
 (lambda
  (form p1env)
  (smatch
   form
   ((- a b c)
    ($asm
     form
     `(,CONS)
     `(,($asm #f `(,CONS) `(,(pass1 a p1env) ,(pass1 b p1env)))
      ,(pass1 c p1env))))
   (- (scheme-error 'acons "wrong number of arguments" form)))))

(define
 integer-fits-insn-arg?
 (lambda (obj) (and (integer? obj) (exact? obj) (<= -524287 obj 524287))))

(define
 compile
 (lambda
  (program env)
  (let
   ((env (cond ((vector? env) env) (else (make-bottom-p1env)))))
   (let
    ((p1 (pass1 (pass0 program env) env)))
    (pass3 (pass2 p1) (make-code-builder) (make-renv) 'normal/top HALT)))))

(define
 compile-p1
 (lambda
  (program)
  (let
   ((env (make-bottom-p1env)))
   (pp-iform (pass1 (pass0 program env) env)))))

(define
 compile-p2
 (lambda
  (program)
  (let
   ((env (make-bottom-p1env)))
   (pp-iform (pass2 (pass1 (pass0 program env) env))))))

(define
 compile-p3
 (lambda
  (program)
  (let
   ((env (make-bottom-p1env)))
   (let*
    ((p1 (pass1 (pass0 program env) env))
     (p3 (pass3 (pass2 p1) (make-code-builder) (make-renv) 'normal/top HALT)))
    (vm-dump-code p3)))))

(define init-compiler (lambda () #f))

)
