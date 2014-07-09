#!read-macro=sagittarius/regex
#!read-macro=sagittarius/debug
;; #!debug
(import (rnrs) (text sre)
	(srfi :14)
	(srfi :64)
	(sagittarius)
	(sagittarius control)
	(sagittarius debug)
	(sagittarius regex))
(define (write-to-string o)
  (call-with-string-output-port
   (lambda (out) (write o out))))
(define (re-equal? re1 re2)
  ;; to regard 'any as equal to char-set:full
  (ast-equal? (regex-ast re1) (regex-ast re2)))

(define (ast-equal? ast1 ast2)
  (or (eqv? ast1 ast2)
      (and (string? ast1) (string? ast2)
	   (string=? ast1 ast2))
      (and (pair? ast1) (pair? ast2)
           (ast-equal? (car ast1) (car ast2))
           (ast-equal? (cdr ast1) (cdr ast2)))
      (and (char-set? ast1) (char-set? ast2)
	   (char-set= ast1 ast2))
      (and (symbol? ast1) (eq? ast1 'any)
	   (char-set? ast2) (char-set= ast2 char-set:full))
      (begin #?=ast1 #?=ast2 #f)	; for debugging
     ))

(define regexp? regex-pattern?)

(define-syntax test-sre
  (syntax-rules ()
   ((_ sre re)
    (test-assert (write-to-string 'sre) (re-equal? re (rx sre))))))

(define-syntax test-sre*
  (syntax-rules ()
   ((_ sre re)
    (test-assert (write-to-string sre) (re-equal? re (sre->regex sre))))))

(define-syntax test-sre-empty
  (syntax-rules ()
    ((_ sre)
     (test-assert (write-to-string sre) 
		  (ast-equal? empty-char-class-sre (sre-parse sre))))))

(define-syntax test-invalid-sre
  (syntax-rules ()
    ((_ sre)
     (test-error (write-to-string 'sre) sre-parse-error? (sre-parse 'sre)))))

(define empty-char-class-sre `(register 0 #f ,char-set:empty))

(test-begin "SRE")

(test-group "sre->regex")

(test-assert "sre->regex" (regexp? (sre->regex '(+ #\a))))

(test-group "rx")

(test-assert "rx" (re-equal? #/abc/ (rx "a" "b" "c")))

(test-group "repetion")

(test-sre (* "a") #/(?u:a*)/)

(test-sre (+ "a") #/(?u:a+)/
)
(test-sre (? "abc") #/(?u:(?:abc)?)/)

(test-sre (= 3 "abc") #/(?u:(?:abc){3})/)

(test-sre (>= 3 "abc") #/(?u:(?:abc){3,})/)

(test-sre (** 2 5 "abc") #/(?u:(?:abc){2,5})/)


(test-group "non-greedy repetion") 

(test-sre (*? "a") #/(?u:a*?)/)

(test-sre (+? "a") #/(?u:a+?)/)

(test-sre (?? "abc") #/(?u:(?:abc)??)/)

(test-sre (>=? 3 "abc") #/(?u:(?:abc){3,}?)/)

(test-sre (**? 2 5 "abc") #/(?u:(?:abc){2,5}?)/)


(test-group "atomic clustering & repetion")

(test-sre (?> "a") #/(?>a)/)
;; a bit awkward...
(test-sre (*+ "a") #/(?>(?u:a*))/)

(test-sre (++ "a") #/(?>(?u:a+))/)

(test-sre (?+ "a") #/(?>(?u:a?))/)


(test-group "submatch")

(test-sre
 (: (submatch "a" (submatch "b" (submatch "c"))) (submatch "d"))
 #/(a(b(c)))(d)/)


(test-group "named submatch")

(test-sre (submatch-named name "a") #/(?<name>a)/)


(test-group "back reference")

(test-sre (: (submatch) (backref 1)) #/()\1/)

(test-group "back reference by name")

(test-sre (: (submatch-named name) (backref name)) #/(?<name>)\k<name>/)

(test-sre (: (submatch-named name) (submatch-named name) (backref name))
	  #/(?<name>)(?<name>)\k<name>/)


(test-group "alternation")

(test-sre (|\|| "ab" "cd" "ef") #/ab|cd|ef/)


(test-group "sequence")

(test-sre (: "ab" "cd" "ef") #/abcdef/)


(test-group "wild card")

(test-sre any #/./)


(test-group "beggining/end of string")

(test-sre (: bos eos) #/^$/)


(test-group "character class")

(test-sre ("abc") #/[a-c]/)

(test-sre (/"az") #/[a-z]/)

(for-each
  (lambda (x)
    (let1 re (car x)
      (for-each (lambda (sre) (test-sre* sre re)) (cdr x))))
  '((#/[[:alpha:]]/u alphabetic alpha)
    (#/[[:lower:]]/u lower-case lower)
    (#/[[:upper:]]/u upper-case upper)
    (#/[[:digit:]]/u numeric digit num)
    (#/[[:alnum:]]/u alphanumeric alnum alphanum)
    (#/[[:punct:]]/u punctuation punct)
    (#/[[:graph:]]/u graphic graph)
    (#/[[:blank:]]/u blank)
    (#/[[:space:]]/u whitespace space white)
    (#/[[:print:]]/u printing print)
    (#/[[:cntrl:]]/u control cntrl)
    (#/[[:xdigit:]]/u hex-digit xdigit hex)
    (#/[\x00-\x7f]/ ascii)
    ))

(test-sre digit #/\d/u)

(test-sre space #/\s/u)

(test-sre (or alnum #\_) #/\w/u)

(test-sre nonl #/[^\n]/)

(test-sre word #/\b(?u:\w+)\b/u)


(test-group "character class operation")

(test-sre (~ digit) #/\D/u)

(test-sre (~ space) #/\S/u)

(test-sre (~ (or alnum #\_)) #/\W/u)

(test-sre (w/ascii (& alpha (/"az"))) #/[a-z]/)

(test-sre (w/ascii (- alpha (/"AZ"))) #/[a-z]/)

(test-sre (- ("abc") #\a) #/[bc]/)

(test-sre (- ("abc") "a") #/[bc]/)

(test-sre (~ (/"az")) #/[^a-z]/)

(test-sre (or (~ (- any (/ #\a #\z))) (/"AZ")) #/[a-zA-Z]/)


(test-group "lexical case sensivity")

(test-sre (uncase "ab") #/(?i:ab)/)
;; regexp compiler is not so smart...
;;(test-sre (uncase (~ ("a"))) #/./)

(test-sre (w/nocase ("abc")) #/[a-cA-C]/)

(test-sre (w/nocase (/ "az")) #/[a-zA-Z]/)

(test-sre (w/nocase (/ "acDZ")) #/[a-zA-Z]/)

(test-sre (w/nocase (/ "az") (w/case "abc")) #/[a-zA-Z]abc/)

(test-sre (w/nocase (/"az") (w/case #\a)) #/[a-zA-Z]a/)

(test-sre-empty '(- (/"azAZ") (w/nocase (/"az"))))

(test-sre-empty '(w/ascii (- alpha (uncase lower))))

(test-sre (w/ascii (- alpha (w/nocase lower))) #/[A-Z]/)

(test-sre (w/ascii (- lower (uncase "a"))) #/[b-z]/)

(test-sre (w/ascii (- lower (uncase ("abc")))) #/[d-z]/)


(test-group "lookahead assertion")

(test-sre (?= "abcd") #/(?=abcd)/)
(test-sre (look-ahead "abcd") #/(?=abcd)/)

(test-sre (?! "abcd") #/(?!abcd)/)
(test-sre (neg-look-ahead "abcd") #/(?!abcd)/)

(test-sre nwb #/\B/)


(test-group "lookbehind assertion")

(test-sre (?<= #\a #\b) #/(?<=ab)/)
(test-sre (look-behind #\a #\b) #/(?<=ab)/)

(test-sre (?<! #\a #\b) #/(?<!ab)/)
(test-sre (neg-look-behind #\a #\b) #/(?<!ab)/)


(test-group "conditional pattern")

(test-sre (cond (?=) #\y) #/(?(?=)y)/)

(test-sre (cond (?=) #\y #\n) #/(?(?=)y|n)/)              ;; |

(test-sre (cond (?<=) #\y) #/(?(?<=)y)/)

(test-sre (cond (?<=) #\y #\n) #/(?(?<=)y|n)/)            ;; |

(test-sre (: (submatch) (cond 1 #\y)) #/()(?(1)y)/)

(test-sre (: (submatch) (cond 1 #\y #\n)) #/()(?(1)y|n)/) ;; | 


(test-group "boundary case")

(test-sre (** 0 0 "a") #//)

(test-sre-empty '(** 5 2 "a"))

(test-sre-empty '(or))

(test-sre (~) #/./)


(test-group "dyanamic SRE")

(define ws (rx (+ whitespace)))

(define date (rx (seq (or "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
		      ,ws
		      (or ("123456789")
			  (seq ("12") digit)
			  "30"
			  "31"))))

(test-assert "embeded character" (re-equal? #/abcd/ (rx (: "a" "b" ,#\c "d"))))

(test-assert "embeded char-set" 
	     (re-equal? #/ab[cd]e/ (rx (: "ab" ,(string->char-set "cd") "e"))))

(test-assert "embeded string" (re-equal? #/abcdef/ (rx (: "ab" ,"cde" "f"))))

;; this doesn't work because of optimisation issue...
;; (test-assert "embeded regexp"
;;  (re-equal?
;;   #/(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+(?:[1-9]|[12]\d|30|31)/
;;   date))

(test-assert "lexical case folding does not affect embeded SRE"
	     (re-equal? #/[[:lower:]]/u (rx (w/nocase ,char-set:lower-case))))


(let1 p (rx (submatch "b" (submatch "a")))
  (test-assert "`,' removes submatch info."
	       (re-equal? #/(cba)/ (rx (submatch "c" ,p))))

  (test-assert "`,' retains submatch info."
	       (re-equal? #/(c(b(a)))/ (rx (submatch "c" ,@p))))
  )

(test-group "invalid SRE")

(test-invalid-sre (~ "abc"))

(test-invalid-sre (& ("abc" "abc")))

(test-invalid-sre (- ("abc" "abc")))

(test-invalid-sre (/"abc"))

(test-invalid-sre (/ #\a))


(test-group "Regex to SRE")

(define-syntax test-rx
  (syntax-rules ()
   ((_ sre re)
    (test-equal (write-to-string 'sre) 
		(regex->sre (sre->regex 'sre)) (regex->sre re)))))

(test-rx (?<= #\a #\b) #/(?<=ab)/)
(test-rx (look-behind #\a #\b) #/(?<=ab)/)

(test-rx (?<! #\a #\b) #/(?<!ab)/)
(test-rx (neg-look-behind #\a #\b) #/(?<!ab)/)

(test-rx (cond (?=) #\y) #/(?(?=)y)/)
(test-rx (cond (?=) #\y #\n) #/(?(?=)y|n)/)              ;; |
(test-rx (cond (?<=) #\y) #/(?(?<=)y)/)
(test-rx (cond (?<=) #\y #\n) #/(?(?<=)y|n)/)            ;; |
(test-rx (: (submatch) (cond 1 #\y)) #/()(?(1)y)/)
(test-rx (: (submatch) (cond 1 #\y #\n)) #/()(?(1)y|n)/) ;; | 

(test-rx (w/nocase ("abc")) #/[a-cA-C]/)
(test-rx (w/nocase (/ "az")) #/[a-zA-Z]/)
(test-rx (w/nocase (/ "acDZ")) #/[a-zA-Z]/)
(test-rx (w/nocase (/ "az") (w/case "abc")) #/[a-zA-Z]abc/)
(test-rx (w/nocase (/"az") (w/case #\a)) #/[a-zA-Z]a/)
(test-rx (w/ascii (- alpha (w/nocase lower))) #/[A-Z]/)
(test-rx (w/ascii (- lower (uncase "a"))) #/[b-z]/)
(test-rx (w/ascii (- lower (uncase ("abc")))) #/[d-z]/)

(test-end)
