;;; compiler.scm
;;;
;;; Programmer: ???

;;; general support routines

(define with (lambda (s f) (apply f s)))

(define member?
  (lambda (a s)
    (ormap
     (lambda (b) (eq? a b))
     s)))

(define file->list
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (letrec ((loop
		(lambda ()
		  (let ((ch (read-char port)))
		    (if (eof-object? ch) '()
			(cons ch (loop)))))))
	(let ((s (loop)))
	  (close-input-port port)
	  s)))))

(define make-char-between?
  (lambda (char<=?)
    (lambda (char-from char-to)
      (lambda (char)
	(and (char<=? char-from char)
	     (char<=? char char-to))))))

;;; The scanner recognizes parenthesis and single quote.
;;; It knows to ignore comments up to the end of the current input line,
;;; as well as whitespaces.

(define list->tokens
  (letrec ((st-init
	    (lambda (s)
	      (cond
	       ((null? s) '())
	       ((char=? (car s) #\;) (st-comment s))
	       ((char=? (car s) #\.) `((dot) ,@(st-init (cdr s))))
	       ((char=? (car s) #\') `((single-quote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\`) `((quasiquote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\,) (st-unquote (cdr s)))
	       ((char=? (car s) #\() `((lparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\)) `((rparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\#) (st-hash (cdr s)))
	       ((char=? (car s) #\") (st-string (cdr s) '()))
	       ((char-whitespace? (car s)) (st-init (cdr s)))
	       ((char-symbol? (car s))
		(st-symbol/number (cdr s) (list (car s))))
	       (else (scanner-error "What's this" s)))))
	   (st-unquote
	    (lambda (s)
	      (cond ((null? s) `((,'unquote) ,@(st-init '())))
		    ((char=? (car s) #\@)
		     `((,'unquote-splicing) ,@(st-init (cdr s))))
		    (else `((,'unquote) ,@(st-init s))))))
	   (st-symbol/number
	    (lambda (s chars)
	      (cond ((null? s)
		     `(,(make-symbol/number-token chars) ,@(st-init '())))
		    ((char-symbol? (car s))
		     (st-symbol/number (cdr s) (cons (car s) chars)))
		    ((char-delimiter? (car s))
		     `(,(make-symbol/number-token chars) ,@(st-init s)))
		    (else (scanner-error "At the end of a symbol: " s)))))
	   (st-string
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error "Expecting a \" char to close the string"))
		    ((char=? (car s) #\")
		     `((string ,(list->string (reverse chars)))
		       ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-meta-char (cdr s) chars))
		    (else (st-string (cdr s) (cons (car s) chars))))))
	   (st-meta-char
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting a string meta-char; reached EOF"))
		    ((char=? (car s) #\\) (st-string (cdr s) (cons #\\ chars)))
		    ((char=? (car s) #\") (st-string (cdr s) (cons #\" chars)))
		    ((char-ci=? (car s) #\n)
		     (st-string (cdr s) (cons #\newline chars)))
		    ((char-ci=? (car s) #\r)
		     (st-string (cdr s) (cons #\return chars)))
		    ((char-ci=? (car s) #\t)
		     (st-string (cdr s) (cons #\tab chars)))
		    ((char-ci=? (car s) #\f)
		     (st-string (cdr s) (cons #\page chars)))
		    (else (scanner-error "What kind of a meta-char is " s)))))
	   (st-hash
	    (lambda (s)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting something after #, but reached end"))
		    ((char=? (car s) #\() `((vector) ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-char-1 (cdr s)))
		    ((char-ci=? (car s) #\f)
		     `((boolean #f) ,@(st-init (cdr s))))
		    ((char-ci=? (car s) #\t)
		     `((boolean #t) ,@(st-init (cdr s))))
		    ((char=? (car s) #\;) `((comment) ,@(st-init (cdr s))))
		    (else (scanner-error
			   "Expecting t, f, \\, ( after #, but found" s)))))
	   (st-char-1
	    (lambda (s)
	      (cond ((null? s) (error 'scanner "Must be one char after #\\"))
		    (else (st-char (cdr s) (list (car s)))))))
	   (st-char
	    (lambda (s chars)
	      (cond ((null? s) `((char ,(make-char chars)) ,@(st-init '())))
		    ((char-delimiter? (car s))
		     `((char ,(make-char chars)) ,@(st-init s)))
		    (else (st-char (cdr s) (cons (car s) chars))))))
	   (st-comment
	    (lambda (s)
	      (cond ((null? s) (st-init '()))
		    ((char=? (car s) #\newline) (st-init (cdr s)))
		    (else (st-comment (cdr s)))))))
    (lambda (s)
      (st-init s))))

(define make-symbol/number-token
  (lambda (chars)
    (let* ((string (list->string (reverse chars)))
	   (maybe-number (string->number string)))
      (if (number? maybe-number)
	  `(number ,maybe-number)
	  `(symbol ,(string->symbol (string-downcase string)))))))

(define make-char
  (lambda (chars)
    (cond ((null? chars) (scanner-error "Found #\\ without any char"))
	  ((null? (cdr chars)) (car chars))
	  (else (let* ((string (list->string (reverse chars)))
		       (maybe-number (string->number string 8)))
		  (if (number? maybe-number)
		      (integer->char maybe-number)
		      (cond ((string-ci=? string "return") #\return)
			    ((string-ci=? string "newline") #\newline)
			    ((string-ci=? string "space") #\space)
			    ((string-ci=? string "tab") #\tab)
			    ((string-ci=? string "page") #\page)
			    (else (scanner-error
				   "Can't recognize the following character: "
				   (format "#\\~s" string))))))))))

(define char-alphabetic? ((make-char-between? char-ci<=?) #\a #\z))
(define char-decimal? ((make-char-between? char<=?) #\0 #\9))

(define char-symbol?
  (let ((punc-chars (string->list "!@$%^*-_=+<>./?:")))
    (lambda (char)
      (or (char-alphabetic? char)
	  (char-decimal? char)
	  (ormap 
	   (lambda (punc-char) (char=? punc-char char))
	   punc-chars)))))

(define char-whitespace?
  (lambda (char)
    (char<=? char #\space)))

(define char-delimiter?
  (lambda (char)
    (or (char-whitespace? char)
	(not (char-symbol? char)))))

(define scanner-error
  (lambda (message s)
    (if (null? s)
	(error 'list-tokens message)
	(error 'list-tokens
	       (format "~a: [~s]~a"
		       message
		       (car s)
		       (list->string (cdr s)))))))

(define file->tokens
  (lambda (filename)
    (list->tokens
     (file->list filename))))

(define string->tokens
  (lambda (string)
    (list->tokens
     (string->list string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 1 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; part 1 - READER IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tokens->sexprs
	(lambda (tokens)
		(get-sexprs tokens (lambda(id empty) id))))
		
(define get-sexpr
  (lambda (token ret-sexpr+toks ret-fail)
    (cond 
		;BASE CASE NULL - send fail procedure
		((null? token) (ret-fail))
        
		;BASE CASES (terminals) - send  ret-sexpr+toks
		((or 	(eq? (caar token) 'boolean)
				(eq? (caar token) 'char)
				(eq? (caar token) 'number)
				(eq? (caar token) 'string)
				(eq? (caar token) 'symbol)
				) (ret-sexpr+toks (cadar token) (cdr token)))
        
		;VECTOR CASE  
		((eq? (caar token) 'vector)
           (get-sexprs (cdr token)
                       (lambda (sexprs toks)
                         (if (or (null? toks) (not (eq? (caar toks) 'rparen))) ;if there is no rparen or end-of-input
								(reader-error "No rparen ) found after vector.") ; case we should throw an error
								(ret-sexpr+toks (list->vector sexprs) (cdr toks) ) ; case we should go on                     
						 ))))
						 
		;BEGIN LIST CASE  
		((eq? (caar token) 'lparen)
           (get-sexprs (cdr token)
                       (lambda (sexprs toks)
						 (if (or (null? toks) (not (or (eq? (caar toks) 'dot) (eq? (caar toks) 'rparen)))) ;if there is no rparen or dot or end-of-input
								(reader-error "No rparen ) found after list.") ; case we should throw an error
								(if (eq? (caar toks) 'rparen)
									(ret-sexpr+toks sexprs (cdr toks)) ; case we got list
									(get-sexpr (cdr toks) ; case we got improper list
												(lambda (sexpr2 toks2) 
													(if (or (null? toks2) (not (eq? (caar toks2) 'rparen))) ;success - check for rparen (improper list)
														(reader-error "No rparen ) found after list.")
														(ret-sexpr+toks (append sexprs sexpr2) (cdr toks2))))
												ret-fail)) ;fail procedure - remain the same
						 ))))
						 
		;RIGHT-PAREN / DOT CASE - IDENTIFY END OF VECTOR / LIST
		((or (eq? (caar token) 'dot) (eq? (caar token) 'rparen)) (ret-fail))
		
		
		;SINGLE QUOTE CASE 
		((eq? (caar token) 'single-quote)
			(get-sexpr (cdr token)
                      (lambda (sexpr toks)
                        (ret-sexpr+toks `',sexpr toks))
                      (lambda ()
                        (reader-error "No s-expression given after quote"))))
		
		;OTHER QUOTE TYPES CASE
		((or (eq? (caar token) 'quasiquote) (eq? (caar token) 'unquote) (eq? (caar token) 'unquote-splicing))
			(get-sexpr (cdr token)
                      (lambda (sexpr toks)
                        (ret-sexpr+toks (list (caar token) `,sexpr) toks))
                      (lambda ()
                        (reader-error "No s-expression given after quasiquote"))))
						
		;UN-RECOGNIZED TOKEN CASE
		(else (reader-error "Unrecognized characters."))
	)))
	
(define get-sexprs
  (lambda (toks ret-exps)
    (get-sexpr 
				;tokens input
				toks
               ;success
			   (lambda (sexpr toks)
						(get-sexprs toks
									(lambda (sexprs toks) (ret-exps (cons sexpr sexprs) toks))))
               ;fail (i.e. stop recursion when in get-sexpr procedure
			   (lambda () (ret-exps '() toks)))))
			   

; A generic error procedure based on given message. copy-past from scanner-error			   
(define reader-error
  (lambda (msg)
	(error 'read-error msg)))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; part 2 - PARSER IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	
;;; matcher.scm
;;;
;;; Programmer: Mayer Goldberg, 2012
;;; Editors:-in-chief Tom Peres, Niv Stolarski

(define match
  (letrec ((match
	    (lambda (pat e ret-vals ret-fail)
	      (cond ((and (pair? pat) (pair? e))
		     (match (car pat) (car e)
			    (lambda (vals-car)
			      (match (cdr pat) (cdr e)
				     (lambda (vals-cdr)
				       (ret-vals
					(append vals-car vals-cdr)))
				     ret-fail))
			    ret-fail))
		    ((and (vector? pat) (vector? e)
			  (= (vector-length pat) (vector-length e))
			  (match (vector->list pat) (vector->list e)
				 ret-vals ret-fail)))
		    ((procedure? pat)
		     (let ((v (pat e)))
		       (if v (ret-vals v) (ret-fail))))
		    ((equal? pat e) (ret-vals '()))
		    (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
	     (lambda (vals) (apply ret-with vals))
	     ret-fail))))

(define ?
  (lambda (name . guards)
    (let ((guard?
	   (lambda (e)
	     (andmap 
	      (lambda (g?) (g? e))
	      guards))))
      (lambda (value)
	(if (guard? value)
	    (list value)
	    #f)))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing (lambda (e failure) (failure)))
		   (loop (lambda (s) (if (null? s)
								match-nothing
								(let (	(match-rest (loop (cdr s))) ;v1 e1
										(match-first (car s))); v2 e2
									;body
									(lambda (e failure)
										(match-first e
										(lambda ()
										(match-rest e failure))))))))
			)
    ;letrec body
	(lambda patterns
      (loop patterns))))

;;; Example: A tag-parser for Scheme

(define *void-object* (if #f #f))

(define simple-const?
  (let ((preds (list boolean? char? number? string? vector?)))
    (lambda (e)
      (ormap (lambda (p?) (p? e)) preds))))

(define var?
  (lambda (e)
    (and (symbol? e)
	 (not (reserved-word? e)))))

(define reserved-word?
  (lambda (e)
    (ormap
     (lambda (kw) (eq? e kw))
     *reserved-words*)))
	 
(define not-reserved-word?
	(lambda (e)
		(not (reserved-word? e))))

(define *reserved-words*
  '(and begin cond define do else if lambda
	let let* letrec or quasiquote
	quote set! unquote unquote-splicing))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 2 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
(define parse
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   (? 'c simple-const?)
	   (lambda (c) `(const ,c)))
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))
	  (pattern-rule
	   (? 'v var?)
	   (lambda (v) `(var ,v)))
	  ;; if
	  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit))
	   (lambda (test dit)
	     `(if-3 ,(parse test) ,(parse dit) (const ,*void-object*))))
	  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
	   (lambda (test dit dif)
	     `(if-3 ,(parse test) ,(parse dit) ,(parse dif))))
	  ;; let
	  (pattern-rule
	   `(let ,(? 'bindings list?) . ,(? 'exprs))
	   (lambda (bindings exprs)
	     (if (duplicate-vars? (map car bindings))
				(error 'parse "There is a duplicate arg in the let expression!")
				(parse `((lambda ,(map car bindings) ,(beginify exprs)) ,@(map cadr bindings))))))
	  ;; let*
	  (pattern-rule
	   `(let* () ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (expr exprs)
	     (parse (beginify (cons expr exprs)))))
	  (pattern-rule
	   `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
	   (lambda (var val rest exprs)
	     (parse `(let ((,var ,val))
		       (let* ,rest . ,exprs)))))
	  ;; letrec
	  (pattern-rule
	   `(letrec ,(? 'c))
	   (lambda (c) (parse (expand-letrec `(letrec ,c)))))
	  ;; quaziquote
	  (pattern-rule
	   `(quasiquote ,(? 'c))
	   (lambda (c) `(const ,(expand-qq c))))
	  ;; lambda (without beginify)
	  (pattern-rule
	   `(lambda ,(? 'args) ,(? 'expr))
	   (lambda (args expr)
			(lambda-selector 
				args
				(lambda () `(lambda-simple ,args ,(parse expr)))
				(lambda (s a) `(lambda-opt ,s ,a ,(parse expr)))
				(lambda () `(lambda-variadic ,args ,(parse expr)))
				)))
	   ;; lambda (with beginify)
	  (pattern-rule
	   `(lambda ,(? 'args) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (args expr exprs)
			(lambda-selector 
				args
				(lambda () `(lambda-simple ,args ,(parse (beginify (cons expr exprs)))))
				(lambda (s a) `(lambda-opt ,s ,a ,(parse (beginify (cons expr exprs)))))
				(lambda () `(lambda-variadic ,args ,(parse (beginify (cons expr exprs)))))
				)))
	  ;; MIT Define
	  (pattern-rule
	   `(define (,(? 'v var?) . ,(? 'vars)) ,(? 'exprs))
	   (lambda (v vars exprs) `(define ,(parse v) ,(parse (list 'lambda vars exprs)))))
	  ;; regular Define
	  (pattern-rule
	   `(define ,(? 'v var?) ,(? 'expr))
	   (lambda (v expr) `(define ,(parse v) ,(parse expr))))
	  ;; Begin (Seq)
	  (pattern-rule
	   `(begin . ,(? 'exprs list?))
	   (lambda (exprs) `(seq ,(map parse exprs))))	   
	  ;; OR (empty)
	  (pattern-rule
	   `(or)
	   (lambda () (parse '#f)))
	  ;; OR (1 arg)
	  (pattern-rule
	   `(or ,(? 'expr))
	   (lambda (expr) `,(parse expr)))	
	  ;; OR (2+ arg)
	  (pattern-rule
	   `(or ,(? 'expr) .  ,(? 'exprs))
	   (lambda (expr exprs) `(or ,(map parse (cons expr exprs)))))
	  ;; AND (empty)
	  (pattern-rule
	   `(and)
	   (lambda () (parse '#t)))
	  ;; AND (1 arg)
	  (pattern-rule
	   `(and ,(? 'expr))
	   (lambda (expr) `,(parse expr)))	
	  ;; AND (2+ arg)
	  (pattern-rule
	   `(and ,(? 'expr) .  ,(? 'exprs))
	   (lambda (expr exprs) (parse `(if ,expr (and ,@exprs) #f))))
	  ;; Cond (else rule)
	  (pattern-rule
	   `(cond (else . ,(? 'exprs list?)))
	   (lambda (exprs) (parse `,(beginify exprs))))	   
	   ;; Cond (1 rule)
	  (pattern-rule
	   `(cond ,(? 'expr list?))
	   (lambda (expr) (parse `(if ,(car expr) ,(beginify (cdr expr))))))
	   ;; Cond (2+ rules)
	  (pattern-rule
	   `(cond ,(? 'expr list?) .  ,(? 'exprs))
	   (lambda (expr exprs) (parse `(if ,(car expr) ,(beginify (cdr expr)) (cond ,@exprs)))))	   
	  ;; Application (without args)
	  (pattern-rule
	   `(,(? 'v not-reserved-word?))
	   (lambda (v) `(applic ,(parse v) ())))
	  ;; Application (with args)
	  (pattern-rule
	   `(,(? 'v not-reserved-word?) . ,(? 'args))
	   (lambda (v args) `(applic ,(parse v) ,(map parse args))))
	  )))	  
	  
    (lambda (e)
      (run e
	   (lambda ()
	     e
	     #;(error 'parse
		    (format "I can't recognize this: ~s" e)))))))
			

(define beginify
  (lambda (s)
    (cond 	((null? s) *void-object*)
			((null? (cdr s)) (car s))
			(else `(begin ,@s)))))

;;;;;;;;;;;;; Quaziquote expander ;;;;;;;;;;;;;
	  
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e) (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

;;;;;;;;;;;;; Lambda expander ;;;;;;;;;;;;;

(define lambda-selector
	(lambda (e ret-pro ret-imp ret-sym)
		(cond 	((pair? e) 
					(lambda-selector (cdr e)
						ret-pro
						(lambda (s a) (ret-imp (cons (car e)s) a))
						(lambda () (ret-imp (list (car e))(cdr e)))))
				((null? e)(ret-pro))
				((symbol? e)(ret-sym))
				(else (parser-error "un-recognized lambda.")))))

;;;;;;;;;;;;; letrec expander ;;;;;;;;;;;;;

(define Yn
  (lambda fs
    (let ((ms (map
		  (lambda (fi)
		    (lambda ms
		      (apply fi
			     (map (lambda (mi)
				    (lambda args
				      (apply (apply mi ms) args)))
			       ms))))
		fs)))
      (apply (car ms) ms))))

(define expand-letrec
  (lambda (e)
    (with e
      (lambda (_letrec ribs . exprs)
	(let* ((names `(,(gensym) ,@(map car ribs)))
	       (fs `((lambda ,names ,@exprs)
		     ,@(map (lambda (rib) `(lambda ,names ,(cadr rib)))
			 ribs))))
	  `(Yn ,@fs))))))

;;;;;;;;;;;; Duplication test in a let arguments

(define duplicate-vars?
	(lambda (args)
		(cond	((null? args) #f)
				((member? (car args) (cdr args)) #t)
				(else (duplicate-vars? (cdr args))))))
		
	  
;;;;;;;;;;;; A generic error procedure based on given message. copy-past from scanner-error
			   
(define parser-error
  (lambda (msg)
	(error 'read-error msg)))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 3 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Ass3 - Q1 ;;;;;;;;;;;;;;;;;;;

(define map-run
	(lambda (action params env lst)
		(if (null? lst) 
			'()
			(cons (action (car lst) params env) (map-run action params env (cdr lst))))))

(define pe->lex-pe
	(letrec ((run
		(lambda (pe params env)
			(cond
;				((null? pe) pe) ;;;;;;;;; CHECK!!!!!!
				((not (pair? pe)) pe)
				((eq? (car pe) 'const) pe)
				((eq? (car pe) 'if-3)
					`(if-3 ,(run (cadr pe) params env) ,(run (caddr pe) params env) ,(run (cadddr pe) params env)))
				((eq? (car pe) 'or)
					`(or ,(map-run run params env (cadr pe))))
				((eq? (car pe) 'seq)
					`(seq ,(map-run run params env (cadr pe))))
				((eq? (car pe) 'define)
					`(define ,(run (cadr pe) params env) ,(run (caddr pe) params env)))
				((eq? (car pe) 'applic)
					`(applic ,(run (cadr pe) params env) ,(map-run run params env (caddr pe))))
				((eq? (car pe) 'var)
					(with pe
						(lambda (_ v)
							(search-in-rib 
								v 
								params
								(lambda (min) `(pvar ,v ,min))
								(lambda ()
									(search-in-ribs 
										v 
										env
										(lambda (maj min)
											`(bvar ,v ,maj ,min))
										(lambda ()
											`(fvar ,v))))))))
				((eq? (car pe) 'lambda-simple)
					(with pe
						(lambda (_ argl body)
							`(lambda-simple ,argl ,(run body argl (cons params env))))))
				((eq? (car pe) 'lambda-opt)
					(with pe
						(lambda (_ argl opt body)
							`(lambda-opt ,argl ,opt
								,(run body `(,@argl ,opt) (cons params env))))))
				((eq? (car pe) 'lambda-variadic)
					(with pe
						(lambda (_ argl body)
							`(lambda-variadic ,argl ,(run body (list argl) (cons params env))))))
				))))
		(lambda (pe)
			(run pe '() '()))))

(define search-in-rib
	(lambda (a s ret-min ret-nf)
		(cond 	( (null? s) (ret-nf) )
				( (eq? (car s) a ) (ret-min 0) )
				( else (search-in-rib a (cdr s)
						(lambda(min) (ret-min (+ 1 min)))
						ret-nf)))))

(define search-in-ribs
	(lambda (a env ret-maj+min ret-nf)
			(if (null? env)
				(ret-nf)
				(search-in-rib 
						a 
						(car env)
						(lambda(min) (ret-maj+min 0 min))
						(lambda() (search-in-ribs 
							a 
							(cdr env)
							(lambda (maj min)
								(ret-maj+min (+ 1 maj) min))
							ret-nf))))))
							

;;;;;;;;;;;;;;;;; Ass3 - Q2 ;;;;;;;;;;;;;;;

(define map-run-tc
	(lambda (lst flag)
		(cond 
			((null? lst) '())
			((null? (cdr lst)) (list (run-tc (car lst) flag)))
			(else (cons (run-tc (car lst) #f) (map-run-tc (cdr lst) flag ))))))

(define run-tc
		(lambda (pe tc-flag)
			(cond
				((null? pe) pe)
				((not (pair? pe)) pe)
				((eq? (car pe) 'const) pe)
				((eq? (car pe) 'fvar) pe)
				((eq? (car pe) 'bvar) pe)
				((eq? (car pe) 'pvar) pe)
				((eq? (car pe) 'if-3)
					`(if-3 ,(run-tc (cadr pe) #f) ,(run-tc (caddr pe) tc-flag) ,(run-tc (cadddr pe) tc-flag)))
				((eq? (car pe) 'seq)
					`(seq ,(map-run-tc (cadr pe) tc-flag)))
				((eq? (car pe) 'or)
					`(or ,(map-run-tc (cadr pe) tc-flag)))
				((eq? (car pe) 'define)
					`(define ,(run-tc (cadr pe) #f) ,(run-tc (caddr pe) #f))) ;??? not sure why i changed caddr to #f
				((eq? (car pe) 'lambda-simple)
					`(lambda-simple ,(cadr pe) ,(run-tc (caddr pe) #t)))
				((eq? (car pe) 'lambda-variadic)				
					`(lambda-variadic ,(cadr pe) ,(run-tc (caddr pe) #t))) 
				((eq? (car pe) 'lambda-opt)
					`(lambda-opt ,(cadr pe) ,(caddr pe) ,(run-tc (cadddr pe) #t)))
				;;;;;;;;;;;;;;;;;
				((eq? (car pe) 'applic)
					(if (eq? #t tc-flag)
						`(tc-applic ,(run-tc (cadr pe) #f) ,(run-tc (caddr pe) #f))
						`(applic ,(run-tc (cadr pe) #f) ,(map-run-tc (caddr pe) #f))))
				(else (cons (run-tc (car pe) tc-flag) (run-tc (cdr pe) tc-flag) ))
				
				)))

(define annotate-tc
  (lambda (pe)
    (run-tc pe #f)))
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 4 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code Generator
;;; Purpose: turns the scheme code into a pseudo-assembly code
;;; Input: scheme code
;;; Output: pseudo-assembly code

;; CodeGen main function
(define code-gen
	(lambda (pe env)
		(cond
			((pe-const? pe) (cg-const (cadr pe)))
			((pe-if-3? pe) (cg-if-3 pe env))
			((pe-or? pe) (cg-or (cadr pe) env))
			((pe-seq? pe) (cg-seq (cadr pe) env))
			((pe-pvar? pe) (cg-pvar (cdr pe)))
			((pe-bvar? pe) (cg-bvar (cdr pe)))
			((pe-lambda-simple? pe) (cg-lambda-simple (cdr pe) env))
			((pe-lambda-opt? pe) (cg-lambda-opt (cdr pe) env))
			((pe-lambda-variadic? pe) (cg-lambda-variadic (cdr pe) env))
			((pe-applic? pe) (cg-applic (cdr pe) env))
			((pe-tc-applic? pe) (cg-tc-applic (cdr pe) env))
			(else "")
			)))
			
;;;;;;;;;; sub-CodeGen function ;;;;;;;;;	

(define cg-const
	(lambda (c)
		(let ((const-address (get-const-address c global-const-address)))
			(cond
				((boolean? c) 
					(if (equal? c #t)
						"\tMOV(R0, IMM(14));\n"
						"\tMOV(R0, IMM(12));\n"	))
				((char? c) 
						(string-append "\tMOV(R0,IMM(" (number->string const-address) "));\n"))
				((number? c) 
						(string-append "\tMOV(R0,IMM(" (number->string const-address) "));\n"))
				((string? c) 
						(string-append "\tMOV(R0,IMM(" (number->string const-address) "));\n"))
				((vector? c) )
	;			((quoted? c) )
				))))

(define cg-if-3
	(lambda (pe env)
		(set-unique-tag)
		(let 
			( (unique-tag (number->string current-unique-tag)) )
			(string-append
				"\t/* Test if_" unique-tag " */\n"
				(code-gen (cadr pe) env) ;test (returns in R0 an ADDRESS of scheme object boolean)
				"\tCMP(INDD(R0,1),IMM(0));\n"
				"\tJUMP_EQ(DIF_LABEL_" unique-tag ");\n"
				"\t/* Do-if-true if_" unique-tag " */\n"
				(code-gen (caddr pe) env) ;do-if-true
				"\tJUMP(END_IF_" unique-tag ");\n"
				"\tDIF_LABEL_" unique-tag ":\n"
				"\t/* Do-if-false if_" unique-tag " */\n"
				(code-gen (cadddr pe) env) ;do-if-false
				"\tEND_IF_" unique-tag ":\n"
				""))))

; input (pe) is a list of expressions in or				
(define cg-or
	(lambda (pe env)
		(set-unique-tag)
		(let 
			( (unique-tag (number->string current-unique-tag)) )
			(string-append
				"\t/* BEGIN_OR_" unique-tag " */\n"	
				(map-cg-or-light pe env unique-tag "")
				"\tEND_OR_" unique-tag ":\n"
				""))))
				
(define map-cg-or-light
	(lambda (pe env unique-tag ans)
		(if (null? pe)
			ans
			(cond 
				((not (list? pe)) 
					(string-append
							ans
							"\t/* or_" unique-tag " | Test expression i */\n"	
							(code-gen pe env)
							"\tCMP(INDD(R0,1),IMM(0));\n"
							"\tJUMP_NE(END_OR_" unique-tag ");\n"))
				((list? pe)
					(map-cg-or-light 
						(cdr pe)
						env
						unique-tag 
						(string-append
							ans
							"\t/* or_" unique-tag " | Test expression i */\n"	
							(code-gen (car pe) env)
							"\tCMP(INDD(R0,1),IMM(0));\n"
							"\tJUMP_NE(END_OR_" unique-tag ");\n")))))))
	
(define cg-seq
	(lambda (pe env)
		(set-unique-tag)
		(let 
			( (unique-tag (number->string current-unique-tag)) )
			(string-append
				"\t/* BEGIN_SEQ_" unique-tag " */\n"	
				(map-cg-seq-light pe env unique-tag "")
				""))))

(define map-cg-seq-light
	(lambda (pe env unique-tag ans)
		(cond 
			((null? pe) 
				ans)
			((not (list? pe)) 
				(string-append
						ans
						"\t/* seq_" unique-tag " | expression i */\n"	
						(code-gen pe env)
						))
			((list? pe)
				(map-cg-seq-light 
					(cdr pe)
					env
					unique-tag
					(string-append
						ans
						"\t/* seq_" unique-tag " | expression i */\n"	
						(code-gen (car pe) env)
						))))))
							
(define cg-pvar
	(lambda (pe)
		(string-append
			"\t/* pvar_" (symbol->string (car pe)) " */\n"
			"\tMOV(R0,FPARG(" (number->string (+ 2 (cadr pe))) "));\n")
	))
							
(define cg-bvar
	(lambda (pe)
		(string-append
			"\t/* bvar_" (symbol->string (car pe)) " */\n"
			"\tMOV(R0,FPARG(0));\n"
			"\tMOV(R0,INDD(R0," (number->string (cadr pe)) "));\n"
			"\tMOV(R0,INDD(R0," (number->string (caddr pe)) "));\n")
	))

(define cg-lambda-simple
	(lambda (pe env)
		(set-unique-tag)
		(let 
			((unique-tag (number->string current-unique-tag))
			(params (car pe))
			(body (cadr pe)))
			(string-append
				"\t/* Part A : lambda-simple " unique-tag "*/\n"
				"\tPUSH(IMM(3));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R0),IMM(T_CLOSURE));\n"
				"\tMOV(R1,R0);\n" ;R1[0] points on R0 address (with T_CLOSURE at the moment)
				"\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(INDD(R1,1),R0);\n" ;R1[1] gets the increased-by-one enviornment 
				"\tMOV(R2,IMM(0));\n"
				"\tMOV(R3,IMM(1));\n" 
				"LOOP_" unique-tag ":\n"
				"\tCMP(R2,IMM(" (number->string env) "));\n"
				"\tJUMP_GE(END_LOOP_" unique-tag ");\n"
				"\tMOV(R4,FPARG(0));\n"
				"\tMOV(INDD(R0,R3),INDD(R4,R2));\n"
				"\tADD(R2,IMM(1));\n"
				"\tADD(R3,IMM(1));\n"
				"\tJUMP(LOOP_" unique-tag ");\n"
				"END_LOOP_" unique-tag ":\n"
				"\tMOV(R2,R0);\n"
				"\tPUSH(FPARG(1));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R2),R0);\n"
				"\tMOV(R4,IMM(0));\n"
				"\tMOV(R5,IMM(2));\n"
				"LOOP_PARAMS_" unique-tag ":\n"
				"\tCMP(R4,FPARG(1));\n"
				"\tJUMP_GE(END_LOOP_PARAMS_" unique-tag ");\n"
				"\tMOV(INDD(R0,R4),FPARG(R5));\n"
				"\tADD(R5,IMM(1));\n"
				"\tADD(R4,IMM(1));\n"
				"\tJUMP(LOOP_PARAMS_" unique-tag ");\n"
				"END_LOOP_PARAMS_" unique-tag ":\n"
				"\tMOV(INDD(R1,2),LABEL(L_CLOS_CODE_" unique-tag "));\n"
				"\tMOV(R0,R1);\n"
				"\tJUMP(L_CLOS_EXIT_" unique-tag ");\n"
				"\t/* Part B : lambda-simple " unique-tag "*/\n"
				"L_CLOS_CODE_" unique-tag ":\n"
				"\tPUSH(FP);\n"
				"\tMOV(FP,SP);\n"
				(code-gen body (+ env 1))
				"\tPOP(FP);\n"
				"RETURN;\n"
				"L_CLOS_EXIT_" unique-tag ":\n"
		))))

(define cg-lambda-opt
	(lambda (pe env)
		(set-unique-tag)
		(let 
			((unique-tag (number->string current-unique-tag))
			(params (car pe))
			(rest-param-index (number->string (+ (length (car pe)) 2)))
			(body (caddr pe)))
			(string-append
				"\t/* Part A : lambda-opt " unique-tag "*/\n"
				"\tPUSH(IMM(3));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R0),IMM(T_CLOSURE));\n"
				"\tMOV(R1,R0);\n" ;R1[0] points on R0 address (with T_CLOSURE at the moment)
				"\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(INDD(R1,1),R0);\n" ;R1[1] gets the increased-by-one enviornment 
				"\tMOV(R2,IMM(0));\n"
				"\tMOV(R3,IMM(1));\n" 
				"LOOP_" unique-tag ":\n"
				"\tCMP(R2,IMM(" (number->string env) "));\n"
				"\tJUMP_GE(END_LOOP_" unique-tag ");\n"
				"\tMOV(R4,FPARG(0));\n"
				"\tMOV(INDD(R0,R3),INDD(R4,R2));\n"
				"\tADD(R2,IMM(1));\n"
				"\tADD(R3,IMM(1));\n"
				"\tJUMP(LOOP_" unique-tag ");\n"
				"END_LOOP_" unique-tag ":\n"
				"\tMOV(R2,R0);\n"
				"\tPUSH(FPARG(1));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R2),R0);\n"
				"\tMOV(R4,IMM(0));\n"
				"\tMOV(R5,IMM(2));\n"
				"LOOP_PARAMS_" unique-tag ":\n"
				"\tCMP(R4,FPARG(1));\n"
				"\tJUMP_GE(END_LOOP_PARAMS_" unique-tag ");\n"
				"\tMOV(INDD(R0,R4),FPARG(R5));\n"
				"\tADD(R5,IMM(1));\n"
				"\tADD(R4,IMM(1));\n"
				"\tJUMP(LOOP_PARAMS_" unique-tag ");\n"
				"END_LOOP_PARAMS_" unique-tag ":\n"
				"\tMOV(INDD(R1,2),LABEL(L_CLOS_CODE_" unique-tag "));\n"
				"\tMOV(R0,R1);\n"
				"\tJUMP(L_CLOS_EXIT_" unique-tag ");\n"
				"\t/* Part B : lambda-opt " unique-tag "*/\n"
				"L_CLOS_CODE_" unique-tag ":\n"
				"\tPUSH(FP);\n"
				"\tMOV(FP,SP);\n"
				"\t/* stack adjustment for lambda-opt : make a list (based on pairs) for each FPARG(i) */\n"
				"\tMOV(R7,IMM(FPARG(1)));\n"
				"\tADD(R7,IMM(1));\n"
				"\tPUSH(IMM(11));\n"
				"\tPUSH(FPARG(R7));\n"
				"\tCALL(MAKE_SOB_PAIR);\n"
				"\tDROP(IMM(2));\n"
				"\tSUB(R7,IMM(1));\n"
				;loop - make lisy of T_PAIR
				"LOOP_PARAMS_OPT_" unique-tag ":\n"
				"\tCMP(R7,IMM(" rest-param-index "));\n"	
				"\tJUMP_LT(END_LOOP_PARAMS_OPT_" unique-tag ");\n"
				"\tPUSH(FPARG(R7));\n"
				"\tPUSH(R0);\n"
				"\tCALL(MAKE_SOB_PAIR);\n"
				"\tDROP(IMM(2));\n"
				"\tSUB(R7,IMM(1));\n"
				"\tJUMP(LOOP_PARAMS_OPT_" unique-tag ");\n"
				; end of loop
				"\tEND_LOOP_PARAMS_OPT_" unique-tag ":\n"
				; insert list into FPARG(params + 1)
				"\tMOV(FPARG(" rest-param-index "),R0);\n"
				"\t/* lambda-opt body */\n"
				(code-gen body (+ env 1))
				"\tPOP(FP);\n"
				"\tRETURN;\n"
				"L_CLOS_EXIT_" unique-tag ":\n"
		))))
		
		
(define cg-lambda-variadic
	(lambda (pe env)
		(set-unique-tag)
		(let 
			((unique-tag (number->string current-unique-tag))
			(body (cadr pe)))
			(string-append
				"\t/* Part A : lambda-variadic " unique-tag "*/\n"
				"\tPUSH(IMM(3));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R0),IMM(T_CLOSURE));\n"
				"\tMOV(R1,R0);\n" ;R1[0] points on R0 address (with T_CLOSURE at the moment)
				"\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(INDD(R1,1),R0);\n" ;R1[1] gets the increased-by-one enviornment 
				"\tMOV(R2,IMM(0));\n"
				"\tMOV(R3,IMM(1));\n" 
				"LOOP_" unique-tag ":\n"
				"\tCMP(R2,IMM(" (number->string env) "));\n"
				"\tJUMP_GE(END_LOOP_" unique-tag ");\n"
				"\tMOV(R4,FPARG(0));\n"
				"\tMOV(INDD(R0,R3),INDD(R4,R2));\n"
				"\tADD(R2,IMM(1));\n"
				"\tADD(R3,IMM(1));\n"
				"\tJUMP(LOOP_" unique-tag ");\n"
				"END_LOOP_" unique-tag ":\n"
				"\tMOV(R2,R0);\n"
				"\tPUSH(FPARG(1));\n"
				"\tCALL(MALLOC);\n"
				"\tDROP(IMM(1));\n"
				"\tMOV(IND(R2),R0);\n"
				"\tMOV(R4,IMM(0));\n"
				"\tMOV(R5,IMM(2));\n"
				"LOOP_PARAMS_" unique-tag ":\n"
				"\tCMP(R4,FPARG(1));\n"
				"\tJUMP_GE(END_LOOP_PARAMS_" unique-tag ");\n"
				"\tMOV(INDD(R0,R4),FPARG(R5));\n"
				"\tADD(R5,IMM(1));\n"
				"\tADD(R4,IMM(1));\n"
				"\tJUMP(LOOP_PARAMS_" unique-tag ");\n"
				"END_LOOP_PARAMS_" unique-tag ":\n"
				"\tMOV(INDD(R1,2),LABEL(L_CLOS_CODE_" unique-tag "));\n"
				"\tMOV(R0,R1);\n"
				"\tJUMP(L_CLOS_EXIT_" unique-tag ");\n"
				"\t/* Part B : lambda-variadic " unique-tag "*/\n"
				"L_CLOS_CODE_" unique-tag ":\n"
				"\tPUSH(FP);\n"
				"\tMOV(FP,SP);\n"
				"\t/* stack adjustment for lambda-variadic : make a list (based on pairs) */\n"
				"\tMOV(R8,IMM(FPARG(1)));\n"
				"\tADD(R8,IMM(1));\n"
				"\tPUSH(IMM(11));\n"
				"\tPUSH(FPARG(R8));\n"
				"\tCALL(MAKE_SOB_PAIR);\n"
				"\tDROP(IMM(2));\n"
				"\tSUB(R8,IMM(1));\n"
				;loop - make list of T_PAIR
				"LOOP_PARAMS_VARIADIC_" unique-tag ":\n"
				"\tCMP(R8,IMM(2));\n"	
				"\tJUMP_LT(END_LOOP_PARAMS_VARIADIC_" unique-tag ");\n"
				"\tPUSH(FPARG(R8));\n"
				"\tPUSH(R0);\n"
				"\tCALL(MAKE_SOB_PAIR);\n"
				"\tDROP(IMM(2));\n"
				"\tSUB(R8,IMM(1));\n"
				"\tJUMP(LOOP_PARAMS_VARIADIC_" unique-tag ");\n"
				"\tEND_LOOP_PARAMS_VARIADIC_" unique-tag ":\n"
				"\tMOV(FPARG(2),R0);\n"
				"\t/* lambda-variadic body */\n"
				(code-gen body (+ env 1))
				"\tPOP(FP);\n"
				"\tRETURN;\n"
				"L_CLOS_EXIT_" unique-tag ":\n"
		))))
		
(define cg-applic
	(lambda (pe env)
		(set-unique-tag)
		(let 
			((unique-tag (number->string current-unique-tag))
			(procedure (car pe))
			(operands (cadr pe)))
			(string-append
				"\t/* applic_" unique-tag "*/\n"
				(map-cg-applic operands env unique-tag 0)
				"\t/* pushing number of operands to stack */\n"
				"\tPUSH(IMM(" (number->string (length operands)) "));\n"
				"\t/* generate applic's operator code */\n"
				(code-gen procedure env)
				"\t/* final stage of the procedure */\n"
				"\tCMP(INDD(R0,0),IMM(T_CLOSURE));\n"
				"\tJUMP_NE(ERROR_NST);\n"
				"\tPUSH(INDD(R0,1));\n"
				"\tCALLA(INDD(R0,2));\n"
;				"\tMOV(IND(R1),IMM(2));\n"
;				"\tADD(IND(R1),IMM("(number->string (length operands))"));\n"
				"\tMOV(R6,STARG(0));\n"
				"\tADD(R6,IMM(2));\n"
				"\tDROP(R6);\n"
;				"\tMOV(IND(SP),IND(R1));\n"
				))))
				
(define map-cg-applic
	(lambda (blist env unique-tag index)
		(if (null? blist)
			""
			(string-append
				(map-cg-applic (cdr blist) env unique-tag (+ index 1))
				"\t/* applic_" unique-tag " - B" (number->string (+ index 1)) " */\n"
				(code-gen (car blist) env)
				"\tPUSH(R0);\n"
				)
		)))
		
(define cg-tc-applic
	(lambda (pe env)
		(set-unique-tag)
		(let
			((unique-tag (number->string current-unique-tag))
			(procedure (car pe))
			(operands (cadr pe)))
			(string-append
				"\t/* tc_applic_" unique-tag "*/\n"
				(map-cg-applic operands env unique-tag 0)
				"\t/* pushing number of operands to stack */\n"
				"\tPUSH(IMM(" (number->string (length operands)) "));\n"
				"\t/* generate tc_applic's operator code */\n"
				(code-gen procedure env)
				"\t/* final stage of the procedure */\n"
				"\tCMP(INDD(R0,0),IMM(T_CLOSURE));\n"
				"\tJUMP_NE(ERROR_NST);\n"
				"\tPUSH(FPARG(-1));\n"
				"\tMOV(FP,FPARG(-2));\n"
				
				;;;;;;;;; copy and run down the old frame ;;;;;;;;;;;
				"\t/********************************************/\n"
				
				"\tJUMPA(INDD(R0,2));\n"
			))))
				


; (define cg-fvar
	; (lambda (pe env)
		; (cond
			; ((is-premitive? (car pe)) )
			; )))

			
;;;;;;;;;; isType? functions ;;;;;;;;;

(define pe-const?
	(lambda (pe)
		(if (eq? (car pe) 'const) #t #f)))
		
(define pe-if-3?
	(lambda (pe)
		(if (eq? (car pe) 'if-3) #t #f)))
		
(define pe-or?
	(lambda (pe)
		(if (eq? (car pe) 'or) #t #f)))
		
(define pe-seq?
	(lambda (pe)
		(if (eq? (car pe) 'seq) #t #f)))
		
(define pe-bvar?
	(lambda (pe)
		(if (eq? (car pe) 'bvar) #t #f)))
		
(define pe-pvar?
	(lambda (pe)
		(if (eq? (car pe) 'pvar) #t #f)))
		
(define pe-lambda-simple?
	(lambda (pe)
		(if (eq? (car pe) 'lambda-simple) #t #f)))

(define pe-lambda-opt?
	(lambda (pe)
		(if (eq? (car pe) 'lambda-opt) #t #f)))
		
(define pe-lambda-variadic?
	(lambda (pe)
		(if (eq? (car pe) 'lambda-variadic) #t #f)))

(define pe-applic?
	(lambda (pe)
		(if (eq? (car pe) 'applic) #t #f)))

(define pe-tc-applic?
	(lambda (pe)
		(if (eq? (car pe) 'tc-applic) #t #f)))
		
(define pe-fvar?
	(lambda (pe)
		(if (eq? (car pe) 'fvar) #t #f)))
		
(define is-premitive?
	(lambda (pe)
		(if (or (eq? pe '+) (eq? pe '-) (eq? pe '*) (eq? pe '/))
			#t
			#f)))

			
;;;;;;;;;; constants handle functions ;;;;;;;;;

(define create-consts-table
	(lambda (consts-list)
		; (type (caar consts-list) 
		; (value (cadar consts-list)) 
		; (index (caddar consts-list)) 
		(cond
			((null? consts-list)
				"") ;return empty string
			((eq? 'integer (caar consts-list))
				(string-append
					"\t/* Allocate memory and create the SOB integer: " (number->string (cadar consts-list)) " */\n"
					"\tPUSH(IMM(" (number->string (cadar consts-list)) "));\n"
					"\tCALL(MAKE_SOB_INTEGER);\n"
					"\tDROP(IMM(1));\n"
					(create-consts-table (cdr consts-list))
					))
			((eq? 'char (caar consts-list))
				(string-append
					"\t/* Allocate memory and create the SOB char: " (string (cadar consts-list)) " */\n"
					"\tPUSH(IMM(" (number->string (char->integer (cadar consts-list))) "));\n"
					"\tCALL(MAKE_SOB_CHAR);\n"
					"\tDROP(IMM(1));\n"
					(create-consts-table (cdr consts-list))
					))
			((eq? 'string (caar consts-list))
				(string-append
					"\t/* Allocate memory and create the SOB string: \"" (cadar consts-list) "\" */\n"
					(cg-string-to-chars (cadar consts-list) (- (string-length (cadar consts-list)) 1))
					"\tPUSH(IMM(" (number->string (string-length (cadar consts-list))) "));\n"
					"\tCALL(MAKE_SOB_STRING);\n"
					"\tDROP(IMM(" (number->string (+ 1 (string-length (cadar consts-list)))) "));\n"
					(create-consts-table (cdr consts-list))
					))	
			)
		))

(define cg-string-to-chars
	(lambda (str index)
		(if (eq? index -1)
			""
			(string-append
				(cg-string-to-chars str (- index 1))
				"\tPUSH(" (number->string (char->integer (string-ref str index))) ");\n"			
				))))
		

(define get-consts 
	(lambda (code)
          (tag-types (get-consts-list code '()) '() 16)))

(define get-consts-list 
	(lambda (code ans)
		(cond 
			((or (null? code) (not (list? code))) ans)					; end of exp
			((pe-const? code) 											; single const exp
				(if (or (member? (cadr code) ans) (boolean? (cadr code))) 
						ans ;in case value is duplicate or boolean - don't insert to ans
						(append ans (cdr code))))
            (else (get-consts-list (cdr code) (get-consts-list (car code) ans)))
        )))

(define tag-types 
	(lambda (const-lst ans index)
		(cond 
			((null? const-lst) ans)
			((number? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 
			'integer (car const-lst) index))) (+ index 2)))					
			((char? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 
			'char (car const-lst) index))) (+ index 2)))	
			((string? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 
			'string (car const-lst) index))) (+ index (+ 2 (string-length (car const-lst))))))	
			
			;TODO - check other types
			; ((symbol? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 
			; 'symbol (car const-lst) index))) (+ index 1)))
			((pair? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 'pair (car const-lst) index)) (+ index 3))))			
			((vector? (car const-lst)) (tag-types (cdr const-lst) (append ans (list (list 'vector (car const-lst) index)) (+ index (+ 2 (vector-length (car const-lst)))))))
		)))
		
; assuming every constant will be found in global-const-address
; (type (caar consts-list) 
; (value (cadar consts-list)) 
; (index (caddar consts-list)) 
(define get-const-address
	(lambda (c consts-list)
		(cond 
			((boolean? c) 0) ;address is hard-coded in boolean, therefore return value is fake
			((eq? c (cadar consts-list)) (caddar consts-list)) ;found const! return address
			(else (get-const-address c (cdr consts-list))) ; keep searching			
		)))
		

; A global data structure which contains all constants in the code, according to the format:
; ( type-tag . value . address in memory ) 
; This list is set in the main function compile-scheme-file		
(define global-const-address '())
		
;;;;;;;;;; unique tagging ;;;;;;;;;;;
		
(define current-unique-tag 3342060)

(define set-unique-tag
	(lambda()
		(set! current-unique-tag (+ current-unique-tag 1))
		))

; A global error code
(define error-code
	(lambda()
		(string-append
			"\t/* error_no-such-type */\n"
			"ERROR_NST:\n"
			(cg-string-to-chars "Error: no such type" 18)
			"\tPUSH(IMM(19));\n"
			"\tCALL(MAKE_SOB_STRING);\n"
			"\tDROP(IMM(20));\n"
			"\tPUSH(R0);\n"
			"\tCALL(WRITE_SOB_STRING);\n"
			"return 1;"
		)))
			
		
;;; Q2 - Scheme Compiler
;;; Purpose: takes a scheme code and compiles it
;;; Input: scheme code
;;; Output: .c file

;; write the output assembly file
;; use "user-scheme-code.c" as an output example
(define compile-scheme-file
	(lambda (inputFileName outputFileName)
		(let (	(output (open-output-file outputFileName 'replace))			
				(input (car (map annotate-tc (map pe->lex-pe (map parse (tokens->sexprs (list->tokens 
				(file->list inputFileName))))))))    ) ;;;;add anotate-tc
			(set! global-const-address (get-consts input))
			(display 
				(string-append
					"	
#include <stdio.h>
#include <stdlib.h>
#include \"cisc.h\"

int main()
{
	START_MACHINE;
	
	/* test functions section */
	void print_heap(){
		int i;
		printf(\"\\n\");
		printf(\"printing heap\\n\");
		for (i=ADDR(0); i>=0; i--){
			printf(\"\\t element %d: %d\\n\", i, ADDR(i));
		}
	}
	
	void print_stack(){
        int i;
        printf(\"printing stack, FP: %d SP: %d\\n\", (int)(FP), (int)(SP));
        for(i=SP+5; i>=0; --i){
			if(SP == i){
				printf(\"SP \");
			}
			if(FP == i){
				printf(\"FP \");
			}
			printf(\"\\telement %d: %d \\n\", i, STACK(i));
        }
	}
	
	/* end of test functions section */
	
	JUMP(CONTINUE);

	#include \"scheme.lib\"
	#include \"char.lib\"
	#include \"io.lib\"
	#include \"math.lib\"
	#include \"string.lib\"
	#include \"system.lib\"
	
	"

	(error-code)
"
CONTINUE:
	
	/* Initialize stack with default values */
	
	/* Void */
	MOV(ADDR(10), IMM(T_VOID));
	
	/* Nil (Empty List) */
	MOV(ADDR(11), IMM(T_NIL));
	
	/* False (Boolean) */
	MOV(ADDR(12), IMM(T_BOOL));
	MOV(ADDR(13), IMM(0));
	
	/* True (Boolean) */
	MOV(ADDR(14), IMM(T_BOOL));
	MOV(ADDR(15), IMM(1));
	
	/* Increase address */
	ADD(ADDR(0), IMM(15))\n;
	
	/* create symbol table based on constants */
	"
	
	(create-consts-table (get-consts input))	
	
	"
	
	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);

	/* code generation */
	"
	(code-gen input 0)	
	"
END:
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
		
	STOP_MACHINE;

	return 0;
}")
				output) ;; check write / display
			(close-output-port output))))
			
