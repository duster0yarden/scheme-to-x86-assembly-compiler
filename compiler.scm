(load "./project/pc.scm") ;ass1 loads
(load "./project/qq.scm") ;ass2 loads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 1 Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Setup Vars for logic
(define <digit-0-9>
	(range #\0 #\9))

(define <lowCase-a-z>
	(range #\a #\z))

(define <upperCase-a-z>
	(range #\A #\Z))

(define <digit-a-f>
	(range-ci #\a #\f))

(define <notFollowNaturalpre>
	(new 	
			(*parser (range #\a #\z))
			(*parser (range #\A #\Z))
			(*parser (range #\< #\?)) ;; <,=,>,?
			(*parser (char #\!))
			(*parser (char #\$))
			(*parser (char #\^))
			(*parser (char #\*))
			(*parser (char #\-))
			(*parser (char #\_))
			(*parser (char #\+))
			(*disj 10)
	done))

(define <char-frm-spc>
	(const (lambda (ch) (char<=? #\!  ch) )))

(define <whiteSpace>
	(const (lambda (ch) (char>? #\!  ch) )))

; Function for warrping a string to a value
(define <format-string-to>
	(lambda (str formatedstr)
		(new
			(*parser (word-ci str))
			(*pack (lambda (_) formatedstr))
		done)))

;Comment Section (Inline, Block Sexpression, Block Infix Sexpression)
(define <LineComment>
	(new 
		(*parser (char #\;))
		(*parser <any-char>)
		(*parser (char (integer->char 10)))
		(*parser <end-of-input>)
		(*disj 2)
		*diff
		*star
		(*parser (char (integer->char 10)))
		(*parser <end-of-input>)
		(*disj 2)
		(*caten 3)
	done))

(define <sexprExtComment>
	(new
		(*parser (word "#;"))
		(*delayed (lambda() <sexpr>))
		(*caten 2)
	done))

(define <sexprInfixExtComment>
	(new
		(*parser (word "#;"))
		(*delayed (lambda() <InfixExpression>))
		(*caten 2)
	done))

(define <Comment>
	(new
		(*parser <LineComment>)
		(*parser <sexprInfixExtComment>)
		(*parser <sexprExtComment>)
		(*disj 3)
	done))

; Defining Chars to ignore in parsing
(define <Ignore>
	(new
		(*parser <Comment>)
		(*parser <whiteSpace>)
		(*disj 2)
	done))

;Runing on parser in order to remove ignored char (Space / WhiteSpace / Comments)
(define ^^<wrapped>
  (lambda (<wrapper>)
	(lambda (<p>)
	  (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
		(lambda (_left e _right) e))
	   done))))

(define ^<ignored*> (^^<wrapped> (star <Ignore>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							BOOLEAN Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Boolean>
	(new
	  	(*parser (word-ci "#t"))
	  	(*pack (lambda(_) #t))
	  	(*parser (word-ci "#f"))
	  	(*pack (lambda(_) #f))
	  	(*disj 2)
	 done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							CHAR Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <CharPrefix>
	(new
		(*parser (word "#\\"))
		(*pack (lambda (pre) "#\\"))
	done))

(define <VisibleSimpleChar>
	(new
		(*parser <any-char>)
		(*parser <whiteSpace>)
		*diff
		(*parser <digit-0-9>)
		(*parser <lowCase-a-z>)
		(*parser <upperCase-a-z>)
		(*disj 3)
		*not-followed-by
	done))

(define <NamedChar>
	(new
		(*parser (<format-string-to> "lambda" (integer->char 955)))
		(*parser (<format-string-to> "newline" (integer->char 10)))
		(*parser (<format-string-to> "nul" (integer->char 0)))
		(*parser (<format-string-to> "page" (integer->char 12)))
		(*parser (<format-string-to> "return" (integer->char 13)))
		(*parser (<format-string-to> "space" (integer->char 32)))
		(*parser (<format-string-to> "tab" (integer->char 9)))
		(*disj 7)
		(*parser <digit-0-9>)
		(*parser <lowCase-a-z>)
		(*parser <upperCase-a-z>)
		(*disj 3)
		*not-followed-by
		done))

(define <HexChar>
	(new
		(*parser <digit-0-9>)
		(*parser <digit-a-f>)
		(*disj 2)
	done))

(define <HexUnicodeChar> 
	(new
		(*parser (char #\x))
		(*parser <HexChar>) *plus
		(*caten 2)
		(*pack-with (lambda (start hexch) (integer->char (string->number (list->string hexch) 16))))
	done))

; Order is sensetive, first getting individuals and then SimpleChar
(define <Char>
	(new
		(*parser <CharPrefix>)
		(*parser <HexUnicodeChar>)
		(*parser <NamedChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 2)
		(*pack-with (lambda (pre ch) ch))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							NUMBER Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Natural>
	(new
		(*parser <digit-0-9>)	
		(*parser <digit-0-9>) *star
		(*caten 2)
		(*pack-with
			(lambda (x xs)
				(string->number (list->string `(,x ,@xs)))
			)
		)
		(*parser <notFollowNaturalpre>)
		*not-followed-by
	done))

(define <Integer>
	(new
		(*parser (char-ci #\+))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (sign val) val))
		(*parser (char-ci #\-))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (sign val) (- val)))
		(*parser <Natural>)
		(*disj 3)
	done))

(define <Fraction>
	(new
		(*parser <Integer>)
		(*parser (char #\/))
		(*parser <Natural>)
		(*caten 3)
		(*pack-with (lambda (numer div denomi) (/ numer denomi)))
	done))

; Order sensetive
(define <Number>
	(new
		(*parser <Fraction>)
		(*parser <Integer>)
		(*disj 2)
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							STRING Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Any char except \"
(define <StringLiteralChar>
	(new
	(*parser <any-char>)
	(*parser (char #\\))
	(*parser (char #\"))
	(*disj 2)
	*diff
	done))

;Locked words
(define <StringMetaChar>
	(new
		(*parser (<format-string-to> "\\\\" #\\))
		(*parser (<format-string-to> "\\\"" #\"))
		(*parser (<format-string-to> "\\t" (integer->char 9)))
		(*parser (<format-string-to> "\\f" (integer->char 12)))
		(*parser (<format-string-to> "\\n" (integer->char 10)))
		(*parser (<format-string-to> "\\r" (integer->char 13)))
		(*disj 6)
		(*pack (lambda (meta) (car (string->list (string meta)))))
		done))

;The abaility to creat a char using it HEX value
(define <StringHexChar>
	(new
		(*parser (word-ci "\\x"))
		(*parser <HexChar>) *star
		(*parser (char #\;))
		(*caten 3)
		(*pack-with (lambda (pre hex post) (integer->char (string->number (list->string hex) 16))))
	done))

;Order sensetive
(define <StringChar>
	(new
		(*parser <StringHexChar>)
		(*parser <StringMetaChar>)
		(*parser <StringLiteralChar>)
		
		
		(*disj 3)
	done))

(define <String>
	(new
		(*parser (char #\"))
		(*parser <StringChar>)
		(*parser (char #\"))
		*diff
		*star 
		(*parser (char #\"))
		(*caten 3)
		(*pack-with (lambda (start str end) (list->string str)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							SYMBOL Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <SymbolChar>
	(new
	(*parser <digit-0-9>)
	(*parser <lowCase-a-z>)
	(*parser <upperCase-a-z>)
	(*pack (lambda (ch) (integer->char (+ 32 (char->integer ch)))))
	(*parser (char #\!))
	(*parser (char #\$))
	(*parser (char #\^))
	(*parser (char #\*))
	(*parser (char #\-))
	(*parser (char #\_))
	(*parser (char #\=))
	(*parser (char #\+))
	(*parser (char #\<))
	(*parser (char #\>))
	(*parser (char #\?))
	(*parser (char #\/))
	(*disj 15)
	done))

(define <Symbol>
	(new
		(*parser <SymbolChar>) *plus
		(*pack (lambda (symbol) (string->symbol (list->string symbol)) ))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							PROPERLIST Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ProperList>
	(new
		(*parser (char #\())
		(*parser (char #\space)) *star
		(*delayed (lambda () <sexpr>)) *star
		(*parser (char #\)))
		(*caten 4)
		(*pack-with (lambda (start space expr end) `(,@expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							IMPROPERLIST Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
	(new
		(*parser (char #\())
		(*parser (char #\space)) *star
		(*delayed (lambda () <sexpr>)) *plus
		(*parser (char #\.))
		(*delayed (lambda () <sexpr>))
		(*parser (char #\)))
		(*caten 6)
		(*pack-with (lambda (start space exprs brk expr end) `(,@exprs . ,expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							VECTOR Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Vector>
	(new
		(*parser (char #\#))
		(*parser (char #\())
		(*parser (char #\space)) *star
		(*delayed (lambda () <sexpr>)) *star

		(*parser (char #\)))
		(*caten 5)
		(*pack-with (lambda (id start space expr end) `#(,@expr) ))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							QUOTE Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Quoted>
	(new
		(*parser (char #\'))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (id expr) (list 'quote expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							QUASIQUOTE Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <QuasiQuoted>
	(new
		(*parser (char #\`))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (id expr) (list 'quasiquote expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							UNQUOTE Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Unquoted>
	(new
		(*parser (char #\,))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (id expr) (list 'unquote expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							UNQUOTE AND SPLICE Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <UnquoteAndSpliced>
	(new
		(*parser (word ",@"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (id expr) (list 'unquote-splicing expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							CALL BY NAME Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <CBNameSyntax1>
	(new
		(*parser (char #\@))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (pre expr) expr))
	done))

(define <CBNameSyntax2>
	(new
		(*parser (char #\{))
		(*delayed (lambda () <sexpr>))
		(*parser (char #\}))
		(*caten 3)
		(*pack-with (lambda (pre expr post) expr))
	done))

(define <CBName>
	(new
		(*parser <CBNameSyntax1>)
		(*parser <CBNameSyntax2>)
		(*disj 2)
		(*pack (lambda (expr) (list 'cbname expr)))
	done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							INFIX EXTENSION Section													 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SETUP

;Function that takes the operator that is packed on the right expression and returns the proper expression
(define op
	(lambda (left right) (list (car right) left (cadr right))))

(define printInfix
	(lambda (left right op)
		(if (null? (cdr right))
			(list op left (car right))
			(list op left (printInfix (car right) (cdr right) op) ))))

; Needed to allow operators after a Natural Number
(define <NaturalInfix>
	(new
		(*parser <digit-0-9>)   
		(*parser <digit-0-9>) *star
		(*caten 2)
		(*pack-with
			(lambda (x xs)
				(string->number (list->string `(,x ,@xs)))
			)
		)
		(*parser (range-ci #\a #\z))
		*not-followed-by
	done))

(define <IntegerInfix>
	(new
		(*parser (char-ci #\+))
		(*parser <NaturalInfix>)
		(*caten 2)
		(*pack-with (lambda (sign val) val))
		(*parser (char-ci #\-))
		(*parser <NaturalInfix>)
		(*caten 2)
		(*pack-with (lambda (sign val) (- val)))
		(*parser <NaturalInfix>)
		(*disj 3)
	done))

(define <FractionInfix>
	(new
		(*parser <IntegerInfix>)
		(*parser (char #\/))
		(*parser <NaturalInfix>)
		(*caten 3)
		(*pack-with (lambda (numer div denomi) (/ numer denomi)))
	done))

(define <NumberInfix>
	(new
		(*parser <FractionInfix>)
		(*parser <IntegerInfix>)
		(*disj 2)
	done))

;Start of rules
(define <InfixPrefixExtensionPrefix>
	(new
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
	done))

(define <InfixSymbol>
	(new
		(*parser <SymbolChar>)
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser (char #\*))
		(*parser (char #\*))
		(*parser (char #\*))
		(*caten 2)
		(*parser (char #\^))
		(*parser (char #\/))
		(*disj 6)
		*diff
		*plus
		(*pack (lambda (symbol) (string->symbol (list->string symbol))))
	done))

(define <PowerSymbol>
	(^<ignored*>
		(new
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\*))
		(*caten 2)
		(*disj 2)
	done)))

(define <InfixNeg>
	(^<ignored*>
		(new
		(*parser (char #\-))
		(*delayed (lambda () <InfixFuncallAndVar>))
		(*caten 2)
		(*pack-with (lambda (pre expr) (list '- expr)))
	done)))

(define <InfixArgList>
	(^<ignored*>
		(new
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\,))
		(*delayed (lambda () <InfixExpression>))
		(*caten 2)
		(*pack-with (lambda (comma expr) expr))
		*star
		(*caten 2)
		(*pack-with (lambda (expr1 expr2) (cons expr1 expr2)))
		(*parser <epsilon>)
		(*disj 2)
	done)))

(define <InfixArrayGet>
	(^<ignored*>
		(new
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\[))
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\]))
		(*caten 4)
		(*pack-with (lambda (expr1 bro expr2 brc) (list @expr1 #\[ @expr2 #\])))
	done)))

(define <InfixFuncall>
	(^<ignored*>
		(new
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\())
		(*parser <InfixArgList>)
		(*parser (char #\)))
		(*caten 4)
		(*pack-with (lambda (funcName bro args brc) (list @funcName args)))
	done)))

(define <InfixParen>
	(new
		(*parser (char #\())
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\)))
		(*caten 3)
		(*pack-with (lambda (ob expr cb) expr))
	done))

(define <InfixsexprEscape>
	(^<ignored*>
		(new
		(*parser <InfixPrefixExtensionPrefix>)
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (infixpre sexpr) sexpr))
	done)))

(define <InfixVar>
	(^<ignored*>
		(new
		(*parser <NumberInfix>)
		(*parser <InfixSymbol>)
		(*disj 2)
	done)))

(define <InfixParameter>
	(^<ignored*>
		(new
		(*parser <InfixVar>)
		(*parser <InfixParen>)
		(*parser <InfixNeg>)
		(*parser <InfixsexprEscape>)
		(*disj 4)
	done)))

;Returns proper format if it is a function or is it is a vector
(define funcandVec
	(lambda (name arg)
		(if (equal? (caar arg) #\()
			(cons name (cadar arg))
			(list 'vector-ref name (cadar arg))
		)))

;Handler for Function call and Vector
(define <InfixFuncallAndVar>
	(^<ignored*> 
		(new	(*parser <InfixParameter>)
				(*parser (char #\[))
				(*delayed (lambda () <InfixExpression>))
				(*parser (char #\]))
				(*caten 3)
				(*parser (char #\())
				(*parser <InfixArgList>)
				(*parser (char #\)))
				(*caten 3)
				(*disj 2)
				(*parser <whiteSpace>)
				*star
				(*caten 2)
				*star
				(*caten 2)
				(*pack-with (lambda (name open)
							
								(fold-left funcandVec name open)))
			done)))

(define <InfixPow>
	(^<ignored*>
	(new
		(*parser <InfixFuncallAndVar>)
		(*parser <PowerSymbol>)
		(*parser <InfixFuncallAndVar>)
		(*caten 2)
		(*pack-with (lambda (op expr) expr))
		*star
		(*caten 2)
		(*pack-with (lambda (left right) (if (null? right)
											left
											(printInfix left right 'expt))))
	done)))

(define <InfixMulnDiv>
	(^<ignored*>
	(new
		(*parser <InfixPow>)
		(*parser (char #\*))
		(*pack (lambda (_) '*))
		(*parser (char #\/))
		(*pack (lambda (_) '/))
		(*disj 2) 
		(*parser <InfixPow>)
		(*caten 2)
		*star
		(*caten 2)
		(*pack-with (lambda (left right) (if (null? right)
											left
											(fold-left op left right))))
	done)))

(define <InfixAddnSub>
	(^<ignored*>
	(new
		(*parser <InfixMulnDiv>)
		(*parser (char #\+))
		(*pack (lambda (_) '+))
		(*parser (char #\-))
		(*pack (lambda (_) '-))
		(*disj 2)
		(*parser <InfixMulnDiv>)
		(*caten 2)
		*star
		(*caten 2)
		(*pack-with (lambda (left right) (if (null? right)
											left
											(fold-left op left right))))
	done)))

(define <InfixExpression>
	(new
		(*parser <InfixAddnSub>)
	done))

(define <InfixExtension>
	(new
		(*parser <InfixPrefixExtensionPrefix>)
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with (lambda (pre expr) expr))
	done))

(define <sexpr>
	(^<ignored*>
		(new
		(*parser <Boolean>)
		(*parser <Number>)
		(*parser <Char>)
		(*parser <String>)
		(*parser <Symbol>)
		(*parser <ProperList>)
		(*parser <ImproperList>)
		(*parser <Vector>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <UnquoteAndSpliced>)
		(*parser <Unquoted>)
		(*parser <CBName>)
		(*parser <InfixExtension>)
		(*disj 14)
	done)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 1 End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 2 Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;Predicates;;;;;;;;;;;;;;;;;;;;;;;;
(define is-const? 
	(lambda (expr)
		(or (null? expr ) (vector? expr) (const? expr) (eq? (void) expr))
		)
	)

(define is-var?
	(lambda (expr)
		(and (not(list? expr)) (not(ormap (lambda (x) (equal? x expr)) *reserved-words* )))
		)
	)

(define is-cond?
  (lambda (expr)
	(and 	(list? expr) 
			(eq? 'if (car expr))
		
			(or (=(length expr)4)
				(=(length expr)3))
			)))

(define is-disj?
  (lambda (expr)
	(and 	(list? expr) 
			(eq? 'or (car expr)))))

  
(define is-lambda?
  (lambda (expr)
	(and 	(list? expr) 
			(eq? 'lambda (car expr)))))  ;;;; body can be one or more expressions
			

(define is-define?
  (lambda (expr)
	(and 	(list? expr) 
			(eq? 'define (car expr)))))  ;;; not compatiable with MIT style?

(define is-set?
  (lambda (expr)
	(and 	(list? expr) 
			(eq? 'set! (car expr))
			(=(length expr)3)
			)))
			
			
(define is-app?
  (lambda (expr)
	(and 	(list? expr) 
			(not(ormap (lambda (x) (equal? x (car expr))) *reserved-words* ))
			))
  )


(define is-sequence?
	(lambda (expr)
		(and 	(list? expr)
				(eq? 'begin (car expr)))
		)
	)

(define is-and?
	(lambda (expr)
		(and (list? expr) (eq? 'and (car expr)))
		)
	)

(define is-cond-expr?
	(lambda (expr)
		(and (list? expr) (eq? 'cond (car expr)))
		
		)
	)

(define is-quasi?
	(lambda (expr)
		(and (list? expr) (eq? 'quasiquote (car expr))))
	)
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-let?
	(lambda (expr)
		 (and (list? expr) (equal? (car expr) 'let))
		)
)

(define is-let*?
	(lambda (expr)
		 (and (list? expr) (equal? (car expr) 'let*))
		)
)

(define is-letrec?
	(lambda (expr)
		 (and (list? expr) (equal? (car expr) 'letrec))
		)
)


;;;;;;;;;;;;;;;;;;;;;;;;end new;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Ordered map function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ordered-map
	(lambda (func lst)
		(if (null? lst)
			`()
		(cons (func (car lst)) (ordered-map func (cdr lst)))
		)
	)
)


;;;;;;;;;;;;;;;; Parsing Special expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-cond
	(lambda (expr)
		(if (= (length expr) 3)
			(append (list 'if3) (ordered-map parse (cdr expr)) (list (parse(void))))
		(append (list 'if3) (ordered-map parse (cdr expr))))
	)
)

(define parse-disj
	(lambda (sexpr)
		(cond 
					((null? (cdr sexpr)) 
						(parse '#f))
					((null? (cddr sexpr)) 
						(parse (cadr sexpr)))
					(else 
						(list 'or (map parse (cdr sexpr)))))
	)
)


(define parse-lambda 
	(lambda (expr)
		(if (list? (cadr expr)) (simple-lambda expr)
			(opt-lambda expr))
	)
)


(define parse-define ;
	(lambda (expr)
		(let ((tag (car expr)) (var (cadr expr)) (val (cddr expr)))
		(if (not(pair? var)) 
			(if (= 1 (length val))
				(append (list 'define) (ordered-map parse (cdr expr)))
				(append (list 'define) (parse var) (parse (append (list 'begin) val))))
			;; check if lambda-opt/lambda-simple
			(if (= 1 (length val)) 
				(append (list 'define (parse (car var))  (parse `(lambda ,(cdr var) ,@val ))))
				(append (list 'define (parse (car var))  (parse (append `(lambda ,(cdr var)) val )))))
			)
		)
	)
)

(define parse-assign
	(lambda (expr)
		(append (list 'set) (ordered-map parse (cdr expr)))   ;Maybe map
		)
	)

; TODO: Test
(define parse-apply ;
	(lambda (expr)
		(if (= (length expr) 1) (list 'applic (parse(car expr)) '())
			`(applic ,(parse (car expr)) ,(ordered-map parse (cdr expr)))   ;Maybe map
		)
	)
)

; Not checked
(define parse-sequence ;
	(lambda (expr)
		(if (> (length expr) 1)
			(if (null? (cddr expr))
				(parse (cadr expr))
				`(seq ,(unbegin (cdr expr))))
			`(const , (void)))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; parse macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define parse-and
	(lambda (expr)
		(cond 	((=(length expr) 1) `(const #t))
				((=(length expr) 2) (parse (cadr expr)))
				(else (parse (and->if (cdr expr))))

		)

	)
)

(define parse-cond-expr
	(lambda (expr)
		(cond 	((= (length expr) 2) (if (eq? (cadr expr) 'else) (parse (cdadr expr))
																	(parse `(if ,(caadr expr) ,`(begin ,@(cdadr expr))))))
				(else (parse (cond->if (cdr expr))))
		)

	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; split the let list in to vars and values ;;;;;;;;;;;;;;;;;;;;;;;

(define get-vars 
	(lambda (lst)
  		(map 
  			(lambda (list)
  				(if (var? (car list))
  					(car list)
  					#f
  			))
  		lst)))

(define get-vals 
	(lambda (lst)
  		(map 
  			(lambda (list)
  				(if (val? (cadr list))
  					(cadr list)
  					#f
  				))
  		lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Unbegin a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unbegin lst)
	(cond 	((null? lst) '())
			((and (list? (car lst)) (equal? (caar lst) 'begin))
				(append (unbegin (cdar lst)) (unbegin (cdr lst))))
			(else (cons (parse (car lst)) (unbegin (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Macro of parsing a and exp to an if exp (of 2 or more exp)
(define and->if
  	(lambda (expr)
  		(if (not(list? expr)) '(const #t)
		(if (null? (cdr expr))
			(car expr)
			`(if ,(car expr) ,(and->if (cdr expr)) #f)	   
		)
	)
)
  	)


;Macro of parsing a cond exp to an if exp (of 2 or more exp)
(define cond->if
  	(lambda (expr)
		(if (null? (cdr expr))
			(if (eq? (car (car expr)) 'else)
				`(begin ,@(cdr (car expr)))
				`(if ,(car (car expr)) ,`(begin ,@(cdr (car expr))))
			)
			`(if ,(car (car expr)) ,`(begin ,@(cdr (car expr))) ,(cond->if (cdr expr)))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Let Macro section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define binding->var (lambda (x) (car x)))
(define binding->val (lambda (x) (cadr x)))
(define let-exp->bindings (lambda (x) (cadr x)))
(define let-exp->body (lambda (x) (cddr x)))


(define let->lambda
	(lambda (expr) 
			(let (  (vars (map binding->var (let-exp->bindings expr)))
					(vals (map binding->val (let-exp->bindings expr)))
					(body (let-exp->body expr)))
					(if (= (length body) 1)
						`((lambda ,vars ,(car body) ) ,@vals )
						`((lambda ,vars ,@body) ,@vals ))
			)
		)
)
	   
(define let*-exp->bindings (lambda (x) (cadr x)))

(define let*-exp->body (lambda (x) (cddr x)))


(define let*->let
	(lambda (expr) 
	
		   (let (   (bindings (let*-exp->bindings expr))
					(body (let*-exp->body expr))
				)
				
				(letrec ( (rec (lambda (bin bod)
				
				
									(if ( null? bin)
												`(let ()  ,@bod  )
										
											(if (= 1 (length bin))
												
												`(let  ,bin ,@bod  )
												
												 `(let ( ,(car bin) )  ,(rec (cdr bin) bod) ))
												 ))))
												 
				(rec bindings body)
			)
		)
	)
)

(define letrec-exp->bindings (lambda (x) (cadr x)))

(define letrec-exp->body (lambda (x) (cddr x)))

(define letrec-exp->parameters (lambda (x) (map car x)))

(define bindings->fresh-bindings (lambda (x) (map (lambda (y) (list y #f)) x)))


(define letrec->let
	(lambda (expr)
		(let*
			((bin (letrec-exp->bindings  expr))
			 (para (letrec-exp->parameters bin))
			 (bod (letrec-exp->body expr))	 
			 (freshbin (bindings->fresh-bindings para))
			 
			 (exprset! (map (lambda (b) (cons 'set! b)) bin)))
			 
			`(let ,freshbin ,@(append exprset! (list (list `(lambda ()  ,@bod )))))
			 ))
				
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;endnew;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; special lambda functions to parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define var-of-opt
	(lambda (expr)
		(if (pair? expr)
			(append (list (car expr)) (var-of-opt (cdr expr)))
			'()
		)
	)
	)

(define opt-of-opt
	(lambda (expr)
		(if (not(pair? expr))
			expr
			(opt-of-opt (cdr expr))
		)
	)
	)


(define simple-lambda
	(lambda (expr)
		(let 	((tag (car expr))
				(argu (cadr expr))
				(body (cddr expr)))
		(if (= (length body) 1)
		 (append (list 'lambda-simple (cadr expr)) (ordered-map parse body))
			(list 'lambda-simple argu (parse(append (list 'begin) body))))
		)  
	)	
)

;;Warning: lambda with optional : var-of-opt/opt-of-opt not checked 
(define opt-lambda
	(lambda (expr)
		(let ((tag (car expr)) (arguments (cadr expr)) (body (cdr(cdr expr))))
			(if (pair? arguments)
				(if (= (length expr) 3)
					(append (list 'lambda-opt (var-of-opt arguments) (opt-of-opt arguments)) (ordered-map parse body))
					(list 'lambda-opt (var-of-opt arguments) (opt-of-opt arguments) (parse (append (list 'begin) body))))
				(if (= (length expr) 3)
					(list 'lambda-opt '() arguments (parse (car body)))
					(list 'lambda-opt '() (opt-of-opt arguments) (parse (append (list 'begin) body))))
			)
		)
	)
)



;;;;;;;;;;;;;;;;;;;; Key Words ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *reserved-words*
	'(and begin cond define do else if lambda
		let let* letrec or quasiquote unquote
		unquote-splicing quote set!))


;;;;;;;;;;;;;;;;;;;;;;;; QuasiQoute extantion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define quasi-parse
	(lambda (expr) 
		(parse (expand-qq (cadr expr)))
		)
	)

;;;;;;;;;;;;;;;;;;;;;; Main function to be used ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse
  ;; fill in the definition of the tag parser here
  (lambda (expr)
  	(cond
  		((is-const? expr) (list `const (unquotify expr)))
  		((is-var? expr) (list `var expr))
  		((is-cond? expr) (parse-cond expr))
  		((is-disj? expr) (parse-disj expr))
  		((is-lambda? expr) (parse-lambda expr))
  		((is-define? expr) (parse-define expr))
  		((is-set? expr) (parse-assign expr))
  		((is-app? expr) (parse-apply expr))
  		((is-let? expr) (parse-apply (let->lambda expr)))
  		((is-let*? expr) (parse-apply (let->lambda (let*->let expr))))
  		((is-letrec? expr) (parse-apply (let->lambda (letrec->let expr))))
  		((is-sequence? expr) (parse-sequence expr))
  		((is-and? expr) (parse-and expr))
  		((is-cond-expr? expr) (parse-cond-expr expr))
  		((is-quasi? expr) (quasi-parse expr))
  	)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 2 End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 3 START
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first x) (lambda() (car x)))
(define (rest x) (lambda() (cdr x)))

(define (applic? x) 
	(eqv? ((first x)) 'applic))
	
(define (lambda-simple-of-remove-applic? x)
	(eqv?  ((first ((first ((rest x)))))) 'lambda-simple))
	
(define (no-params? x)
	(null? ((first ((rest ((first ((rest x))))))))))


;=========================remove applic lambda nil start===============================================

(define (helper pe)
	(if (null? pe)
		'()
		(if (not (list? pe))
			pe
			(if (and (applic? pe) (pair? ((rest pe))) (pair? (cadr pe)) (lambda-simple-of-remove-applic? pe) (no-params? pe)) 
				((first (helper (cddadr pe))))
				(cons (helper ((first pe))) (helper ((rest pe))))
			)
		)
	)
)

(define remove-applic-lambda-nil
	(lambda (pe)
		((lambda (x) x) (helper  pe))	;or (helper  pe)
	)
)
;; ;==========================remove applic lambda nil end======================================================

(define (s-occur?  b p)
	(if (or(null? b))
		#f
		(if (or(not (pair? b)))
			#f
			(if (or(and (list? b) (equal? ((first b)) 'set) (equal? (caadr b) 'var) (equal? (cadadr b) p)) )
				#t
				(if (or(and (list?  b) (or (equal? (car b) 'lambda-simple) (equal? (car b) 'lambda-opt))))
					(let ((x (if (equal? (car b) 'lambda-opt)
									'(,(caddr b))
									(cadr b))))
						(if (member p x) 
							#f 
							(s-occur?  (if	(equal? ((first b)) 'lambda-opt)
														(cdddr b) 
														(cddr b))
										p)
						)
					)
					(if (or(and (list? b) (not (null?  b))))
						(or (s-occur?  ((first b)) p) (s-occur?  ((rest b))p))
						(s-occur?  ((rest b))p)
					)
				)
			)
		)
	)
)

(define (g-occur?  b p)
	(if (null? b) 
		#f
		(if (not (list? b))
			#f
			(if (and (list? b) (equal? ((first b)) 'var) (equal? (cadr b) p)) 
				#t
				(if (and (list?  b) (or (equal? ((first b)) 'lambda-simple) (equal? (car b) 'lambda-opt)))
					(let ((x (if (equal? ((first b)) 'lambda-opt)
									'(,(caddr b))
									(cadr b))))
						(if (member p x) 
							#f 
							(g-occur?  (if  (equal? ((first b)) 'lambda-opt)
														(cdddr b) 
														(cddr b))
														p)))
					(if (and (list? b) (equal? ((first b)) 'set))
						(g-occur?  (cddr b) p)
						(if (and (list? b) (not (null?  b)))
							(or (g-occur?  (car b) p) (g-occur?  (cdr b)p))
							(g-occur?  (cdr b) p)
						)
					)
				)
			)
		)
	)
)




(define (boundedParam?  b bound p)
	(if (null? b) 
		#f
		(if (not (list? b))
			#f
			(if (and bound (list? b) (equal? ((first b)) 'var) (equal? (cadr b) p))
				#t
				(if (and (list?  b) (or (equal? (car b) 'lambda-simple) (equal? ((first b)) 'lambda-opt)) )
					(let ((pars (if (equal? (car b) 'lambda-opt)
									'(,(caddr b))
									((first ((rest b)))))))
						(if (member p pars)
							#f
							(boundedParam?  (if	(equal? ((first b)) 'lambda-opt)
														(cdddr b) 
														((rest ((rest b))))) #t p)
						)
					)
					(if (and (list? b) (not (null?  b)))
						(or (boundedParam?  ((first b)) bound p) (boundedParam?  ((rest b)) bound p))
						(boundedParam?  ((rest b)) bound p)
					)
				)
			)
		)
	)
)


(define (occur-rep p b bound)
	(if (or(null? b) )
		'()
		(if (or(not (pair? b)) )
			b
			(if (or(and (list?  b) (or (equal? ((first b)) 'lambda-simple) (equal? (car b) 'lambda-opt))))
				(let ((pars (if (equal? ((first b)) 'lambda-opt)
								'(,(caddr b))
								((first ((rest b))))))) 
					(if (or(member p pars) )
						b
						(if (or(equal? ((first b)) 'lambda-opt))
							`(,((first b)) ,(cadr b) ,(caddr b) ,@(occur-rep p (if	(equal? ((first b)) 'lambda-opt)
													(cdddr b) 
													(cddr b)) #t))
							`(,(car b) ,(cadr b) ,@(occur-rep p (if	(equal? ((first b)) 'lambda-opt)
													(cdddr b) 
													(cddr b)) #t))
						)
					)
				)
				(if (or(and bound (list? b) (equal? (car b) 'var) (equal? (cadr b) p)) )
					`(box-get ,b)
					(if (or(and bound (list? b) (equal? ((first b)) 'set) (equal? (caadr b) 'var) (equal? (cadadr b) p)) )
						`(box-set (var ,p) ,(occur-rep p (caddr b) bound))
						(if (or(list?  b) )
							(map (lambda (exp) (occur-rep p exp bound)) b)
							(if (or(pair?  b) )
								(cons (occur-rep p ((first b)) bound) (occur-rep p ((rest b)) bound))
								(cons ((first b)) (occur-rep p ((rest b)) bound))
							)
						)
					)
				)
			)
		)
	)
)

(define (parametersExamination  b cl parameters)
	(if (or(null? parameters))
		(cons cl b)
		(let* ( (ae 
					(if (or(and (boundedParam?  b #f ((first parameters))) (g-occur?  b ((first parameters))) (s-occur?  b ((first parameters)))))
						(cons #t (occur-rep ((first parameters)) b #t))
						(cons #f b)))
				(new-goof ((rest ae)))
				(atc? ((first ae))))
			(parametersExamination  new-goof 
									(if (or atc?) 
										(cons ((first parameters)) cl) 
										cl)
									((rest parameters)))
		)
	)
)


(define (lambdaExamination e)
	(let* ( (goof (if   (or(equal? ((first e)) 'lambda-opt))
						((rest((rest((rest e))))))
						((rest((rest e)))))) 
			(parameters (if (or(equal? ((first e)) 'lambda-simple))
							((first((rest e))))
							(append ((first((rest e)))) (list ((first((rest((rest e))))))))))
			(bae (parametersExamination  goof '() parameters))
			(parameters-that-chosen (reverse ((first bae))))
			(new-goof ((rest bae)))
			(sl
				(if (or(null? new-goof))
					'phi
					(if (null? ((first new-goof)))
						'phi
						(if (or(equal? ((first ((first new-goof)))) 'seq) )
							`(,((first ((first new-goof)))) (,@(map (lambda (param) `(set (var ,param) (box (var ,param)))) parameters-that-chosen) ,@((first((rest ((first new-goof))))))))
							`(seq (,@(map (lambda (param) `(set (var ,param) (box (var ,param)))) parameters-that-chosen)  ,@new-goof))
						)
					)
				)
			)
		)
		(if (null? parameters-that-chosen)
			e
			(if (or(equal? ((first e)) 'lambda-simple))
				`( ,((first e)) ,parameters ,sl)
				(if (or(equal? ((first e)) 'lambda-opt))
					`( ,((first e)) ,((first((rest e)))) ,((first((rest((rest e))))))  ,sl)
					`( ,((first e)) ,((first((rest e)))) ,sl)
				)
			)
		)
	)
)


(define box-set
	(lambda (e)
		 (if (or(null? e) )
			'()
			(if (or(not (pair? e)) )
				e
				(if (or(and (list? e) (or (equal? ((first e)) 'lambda-simple) (equal? ((first e)) 'lambda-opt))))
					(let ((fle (lambdaExamination e)))
						(if (or (equal? ((first fle)) 'lambda-simple) ) 
							`(,((first fle)) ,((first ((rest fle)))) ,@(box-set ((rest ((rest fle))))))
							`(,((first fle)) ,((first ((rest fle)))) ,((first ((rest ((rest fle)))))) ,@(box-set ((rest((rest((rest fle))))))))
						)
					)
					(cons (box-set ((first e))) (box-set  ((rest e))))
				)
			)
		)
	)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tag-Predicate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-tagged-with
	(lambda (tag)
		(lambda (expr)
			(and (list? expr) (eq? tag (car expr))))
		)
	)

(define is-tagged-with-const? (is-tagged-with 'const))
(define is-tagged-with-var? (is-tagged-with 'var))
(define is-tagged-with-fvar? (is-tagged-with 'fvar))
(define is-tagged-with-pvar? (is-tagged-with 'pvar))
(define is-tagged-with-bvar? (is-tagged-with 'bvar))
(define is-tagged-with-if? (is-tagged-with 'if3))
(define is-tagged-with-or? (is-tagged-with 'or))
(define is-tagged-with-lambda-simple? (is-tagged-with 'lambda-simple))
(define is-tagged-with-lambda-opt? (is-tagged-with 'lambda-opt))
(define is-tagged-with-define? (is-tagged-with 'define))
(define is-tagged-with-set? (is-tagged-with 'set))
(define is-tagged-with-apply? (is-tagged-with 'applic))
;;should i need the applic-tc tag??
(define is-tagged-with-seq? (is-tagged-with 'seq))
(define is-tagged-with-box-set? (is-tagged-with 'box-set))
(define is-tagged-with-box-get? (is-tagged-with 'box-get))
(define is-tagged-with-box? (is-tagged-with 'box))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Annotate lexical scope;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-tagged-with
	(lambda (tag)
		(lambda (expr)
			(and (list? expr) (eq? tag (car expr))))
	)
)

(define expand_env
	(lambda (params env)
		(cons params env))
)

(define is-pvar?
	(lambda (var args) 
		(if (member var args) #t #f)
	)
)

(define get-minor 
	(lambda (args var)
		(- (length args) (length (member var args))))
)

(define get-bound-var
	(lambda (var major env)
		(if (null? env) 
			#f
			(if (is-pvar? var (car env))
				`(bvar ,var ,major ,(get-minor (car env) var))
				(get-bound-var var (+ major 1) (cdr env))
			)
		)
	)
)

(define is-bvar?
	(lambda (var env)
		(if (get-bound-var var 0 env) #t #f)
	)
)

(define lex-address-for-var
	(lambda (var args env)
		(if (is-pvar? var args) `(pvar ,var ,(get-minor args var))
			(if (is-bvar? var env) (get-bound-var var 0 env)
				`(fvar ,var))
		)
	)
)

(define pe->lex-pe-Helper
	(lambda (pe params env)
		(cond
			((null? pe) '())
			((is-tagged-with-const? pe) pe)
			((is-tagged-with-apply? pe) 
				(apply (lambda (_ func args)
							`(applic ,(pe->lex-pe-Helper func params env) ,`(,@(pe->lex-pe-Helper args params env)))) pe))
			((is-tagged-with-lambda-simple? pe)
				(apply (lambda (_ args body)
						`(lambda-simple ,args ,(pe->lex-pe-Helper body args (expand_env params env)))) pe))
			((is-tagged-with-lambda-opt? pe)
				(if (= (length pe) 4)
					(apply (lambda (_ args opt body)
							`(lambda-opt ,`(,@args) ,opt ,(pe->lex-pe-Helper body `(,@args ,opt) (expand_env params env)))) pe)
					(if (= (length pe) 3)
							(apply (lambda (_ opt body)
								`(lambda-opt ,opt ,(pe->lex-pe-Helper body `(,opt) (expand_env params env)))) pe))
				)
			)
			((is-tagged-with-var? pe)
				(apply (lambda (_ var)
							(lex-address-for-var var params env)) pe))
			(else
				(if (list? (car pe))
					(if (null? (cdr pe))
						`(,(pe->lex-pe-Helper (car pe) params env))
						`(,(pe->lex-pe-Helper (car pe) params env) ,@(pe->lex-pe-Helper (cdr pe) params env))
					)
					`(,(car pe) ,@(pe->lex-pe-Helper (cdr pe) params env))
				)
			)
		)
	)
)

(define pe->lex-pe
	(lambda (pe)
		(if (or (null? pe) (not (list? pe))) 
			pe
			(pe->lex-pe-Helper pe '() '()))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Annotate Tail call;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define split-to-last
	(lambda (lst splitFunc)
		(cond
			((null? lst) (splitFunc '() '()))
			((null? (cdr lst))
				(split-to-last 
					(cdr lst) 
					(lambda (first last)
					(splitFunc first (cons (car lst) last)))))
			(else
				(split-to-last 
					(cdr lst) 
					(lambda (first last)
					(splitFunc (cons (car lst) first) last))))
			)
		)
	)


(define tc-Helper
	(lambda (tree tp?)
		(cond
			((or (is-tagged-with-var? tree) (is-tagged-with-bvar? tree) (is-tagged-with-fvar? tree) (is-tagged-with-pvar? tree) 
				(is-tagged-with-const? tree) (is-tagged-with-box-get? tree) (is-tagged-with-box? tree)) tree)
			((or (is-tagged-with-seq? tree) (is-tagged-with-or? tree))
				(apply (lambda (tag exprs) 
					(split-to-last exprs (lambda (firsts last)
						`(,tag (,@(map (lambda (exp) (tc-Helper exp #f)) firsts) ,(tc-Helper (car last) tp?)))))) tree ))
			((or (is-tagged-with-set? tree) (is-tagged-with-define? tree))
				(apply (lambda (tag var expr)
					`(,tag ,var ,(tc-Helper expr #f))) tree))
			((is-tagged-with-box-set? tree) 
				(apply
					(lambda (tag var exprs)
						`(,tag ,var ,(tc-Helper exprs #f))) tree))
			((is-tagged-with-apply? tree)
				(apply (lambda (_ func args)
					`(,(if tp? 'tc-applic 'applic) ,(tc-Helper func #f) ,(map (lambda (arg) (tc-Helper arg #f)) args))) tree))
			((is-tagged-with-if? tree)
				(apply (lambda (_ test dit dif)
					`(if3 ,(tc-Helper test #f) ,(tc-Helper dit tp?) ,(tc-Helper dif tp?))) tree))
			((is-tagged-with-lambda-simple? tree)
				(apply (lambda (_ args body) 
					`(lambda-simple ,args ,(tc-Helper body #t))) tree))
			((is-tagged-with-lambda-opt? tree)
				(if (= (length tree) 3)
					(apply (lambda (_ args body) 
					`(lambda-opt ,args ,(tc-Helper body #t))) tree)
					(if (= (length tree) 4)
						(apply (lambda (tag args opt body)
							`(lambda-opt ,args ,opt ,(tc-Helper body #t))) tree))))
			(else #f)
			)
		)
	)

(define annotate-tc
	(lambda (tree)
		(if (null? tree) 
			'()
			(tc-Helper tree #f)
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   ASS 3 End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;													   Project Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define constant-table (box '()))
(define global-table (box '()))
(define symbol-table (box '()))


(define line-gen2
	(lambda (str . opt-strs)
		(string-append  "\t\t"
						(fold-left string-append str opt-strs)
						"\n")
	))

(define gen-newline 
	(lambda () "\n"))

(define pipeline
	(lambda (s)
		(annotate-tc
			(pe->lex-pe
				(box-set
					(remove-applic-lambda-nil
										(parse s)
					)
				)
			)
		)
	))

(define (file->list in-file)
	(let ((in-port (open-input-file in-file)))
		(letrec ((run (lambda () 
							(let ((ch (read-char in-port)))
								(if (eof-object? ch)
									(begin
										(close-input-port in-port)
										'())
									(cons ch (run)))))))
			(run)
		)
	))

(define (list->sexpr l)
	(letrec ((y   (lambda (s-expression r)
						(if (null? r)
							s-expression
							(<sexpr>	r
										(lambda (s-expr remain)
											(y `(,@s-expression ,s-expr) remain))
										(lambda (_w) (error 'sexpr (format "no-match"))))))))
		(y '() l)
	))

; ============================================================================================================
; ================================================= prologue  ================================================
; ============================================================================================================

(define gen-line1
	(lambda (str . opt-strs)
		(string-append  "\t"
						(fold-left string-append str opt-strs)
						"\n")
	))

(define make-prologue 
	(lambda ()
			(string-append			  (start-code)
										"extern malloc" 
										(gen-newline)
										"section .data"
										(gen-newline)
										"start_of_data:"
										(gen-newline)
										(allocate-const-table) 
										(gen-newline)
										(allocate-global-table)
										(gen-newline)
										(allocate-symbol-table)
										(gen-newline)
										(gen-newline)
										"global main"(gen-newline)
										"section .text"(gen-newline)
										"main:"(gen-newline)
										"push qword 0\n"
										"push qword 0\n"
										"push end\n"
										"push rbp\n"
										"mov rbp, rsp\n"
										"mov rdi , 1\n"
										"call malloc\n"
										"mov qword [start_of_data], rax\n"

										(gen-newline)
										(runtime-support)
										(gen-newline)
										
					)))

(define start-code
	(lambda ()
			(string-append
					"%include \"./project/scheme.s\""
					(gen-newline)
					(gen-newline)
					
	)))

(define allocate-const-table
	(lambda ()
				(let* (	 (reprs (make-repr-const (unbox constant-table))) ;returns list of (T_xxxxx y)
							;(											   )
							(new-reprs (cddddr reprs))
							(table-size (length reprs))
							(index (box 7)))
					(string-append
							  ";build constant table "(gen-newline)
							  
							  
							  "Lcons1:"(gen-newline) ;void nil false true
							  "\tdq SOB_VOID"(gen-newline)
							  "Lcons2:"(gen-newline)
							  "\tdq SOB_NIL"(gen-newline)
							  "Lcons3:"(gen-newline)
							  "\tdq SOB_FALSE"(gen-newline)
							  "Lcons5:"(gen-newline)
							  "\tdq SOB_TRUE"(gen-newline)
							 
							 
							(fold-left (lambda (rest curr)
								(let
									((curr-index (unbox index)))
									;(set-box! index (+ curr-index 2))
									(string-append rest 
										"Lcons" (number->string curr-index)":" 
										(gen-newline)  ""
										(cond ((equal? (car curr) 'T_INTEGER)
																								(set-box! index (+ curr-index 2))
																								(string-append 
																								
																								
																								"\tdq MAKE_LITERAL(T_INTEGER," (number->string(cadr curr)) ")"(gen-newline)																								))
								((equal? (car curr) 'T_PAIR)
										(set-box! index (+ curr-index 3))
										(string-append 
												"\tdq MAKE_LITERAL_PAIR (Lcons"(number->string (cadr curr ))",Lcons"(number->string(caddr curr))")"
												(gen-newline)
												)
												)
									((equal? (car curr) 'T_CHAR)
																								(set-box! index (+ curr-index 2))
																								(string-append 
																							   
																								"\tdq MAKE_LITERAL(T_CHAR," (number->string(cadr curr)) ")"(gen-newline)
																								
																								))
									(   (equal? (car curr) 'T_VECTOR)
														(if (equal? (cdr curr) '(0))
															(begin
																(set-box! index  (+ curr-index 1))
																(string-append 
																							   
																								"\tdq MAKE_LITERAL(T_VECTOR,0)"(gen-newline)
																								
																								)
															
															)
																							(begin
																								(set-box! index (+ curr-index (+(cadr curr)2)))
																								(string-append 
																							   
																								"\tMAKE_LITERAL_VECTOR " (fold-left (lambda (rest c)
																																(string-append rest "Lcons" 
																																(number->string c)
																																(if (last? (cddr curr) c )
																																	" "
																																	", "
																																	)))
																														"" (cddr curr))
																														
																								
																								(gen-newline)
																								
																								)
																							))
									)
									((equal? (car curr) 'T_STRING)
																								(set-box! index (+ curr-index (+(cadr curr)2)))
																								(string-append 
																							   
																								"\tMAKE_LITERAL_STRING "  "\"" (fold-left (lambda (rest c)
																																(string-append rest  
																																(make-string 1 (integer->char c))
																																))
																														"" (cddr curr))
																								"\""
																								(gen-newline)
																								
																								))
									((equal? (car curr) 'T_FRACTION)
																								(set-box! index (+ curr-index 3))
																								(string-append 
																							   
																								"\tdq MAKE_LITERAL_FRACTION ("(number->string (cadr curr ))","(number->string(caddr curr))")"
																								
																								(gen-newline)
																								
																								))
									((equal? (car curr) 'T_SYMBOL)
																								(set-box! index (+ curr-index 3))
																						(string-append 
																							   
																							   
																								
																								
																								
																								
																								"\tdq MAKE_LITERAL_SYMBOL(Lcons"(number->string (cadr curr ))")"
																								
																								(gen-newline)
																								
																						)
																					)
																									
																						
																					)(gen-newline)
												)
											)
										)
													""
													new-reprs)
							
							
					)
	)))
(define (last? lst x)
	(if (equal? (cdr lst) '())
		(equal? (car lst) x)
		(last? (cdr lst) x)))

(define allocate-global-table 
	(lambda ()
				(let* (	 (reprs (make-repr-global (unbox global-table)))
							(global-size (length reprs))
							(index (box (+ 1 (length(unbox constant-table))))))
					(string-append 
							(fold-left (lambda (rest curr)
															(let ((curr-index (unbox index)))
																	(set-box! index (+ curr-index 1))
																	(string-append rest 
																								 "Lprim" (number->string curr-index)":"
																								 (gen-newline)
																								  "\tdq 0,0"(gen-newline)
																								 
																								
																	)
																	))
													""
													reprs))
	)))

(define allocate-symbol-table
	(lambda ()
				(let* ((reprs (make-repr-symbol-table (unbox symbol-table)))
							(symbol-size (length reprs))
							(index (box 1)))
					(string-append 
							
							(line-gen2 "head:" )
							
							
							
							; R13 <- symbol-table pointer
							
							
							(fold-left (lambda (rest curr)
															(let ((curr-index (unbox index)))
																	(set-box! index (+ curr-index 1))
																	(string-append rest 
																					(if (= curr 0)
																						(string-append "Lsym"(number->string curr-index)":\n"
																						"\tdq MAKE_LITERAL_PAIR(Lcons2,Lcons2)\n")
																					
																						(string-append "Lsym"(number->string curr-index)":\n"
																						(line-gen2 "\tdq MAKE_LITERAL_PAIR(Lcons"(number->string curr)",Lsym"(number->string (add1 curr-index)) ")"  )))
																						
																						)
																	))
													""
													reprs))
	)))

(define make-repr-symbol-table
	(lambda (symbol-table)
			(if (null? symbol-table)
					(list 0 0)
					(if (null? (cdr symbol-table))
							(list (cadr (caddar symbol-table)) 0)
							(append (list (cadr (caddar symbol-table)) (caadr symbol-table))
											(make-repr-symbol-table (cdr symbol-table)))))))

; ============================================================================================================
; ================================================= epligoue  ================================================
; ============================================================================================================

(define make-epilogue
	(lambda ()
			(string-append
					(gen-newline)
					(line-gen2 "jmp end") 
					(gen-newline)
					
					(gen-line1 "; ERRORS")
					(gen-errors) 
					(gen-newline)
					(gen-line1 "end:")
					(line-gen2 "pop r14")  
					(gen-newline)
					(gen-line1 "mov rax, 0")
					(gen-line1 "call(exit)")
					)))

(define gen-errors
	(lambda ()
			(string-append
					(gen-line1 "L_error_apply_non_clos:")
					(line-gen2 "mov rax ,1")
					(line-gen2 "int 0x80")
					
					(gen-line1 "L_wrong_arity:")
					(line-gen2 "mov rax ,1")
					(line-gen2 "int 0x80")
					
					(gen-line1 "L_incorrect_type:")
					(line-gen2 "mov rax ,1")
					(line-gen2 "int 0x80")
					
					(gen-line1 "L_division_by_zero:")
					(line-gen2 "mov rax ,1")
					(line-gen2 "int 0x80")
			)))

; ============================================================================================================
; ================================================= Constant  ================================================
; ============================================================================================================


(define make-constant-table
	(lambda (parsed-exprs)
			(let* ((constant-list (flatmap (lambda (pe) (construct-constant-list '() pe)) parsed-exprs))
						(constant-list (remove-duplicates constant-list))
						(partial-constant-table (remove-duplicates (make-constant constant-list)))
						(constant-table (append init-table partial-constant-table)))
					(fix-address (assign-address constant-table 1)))))

(define make-constant
	(lambda (const-lst)
			(if (null? const-lst)
					'()
					(let ((curr (car const-lst)))
						(cond	   ((integer? curr) `(,(make-integer-const curr) ,@(make-constant (cdr const-lst))))
									((char? curr) `(,(make-char-const curr) ,@(make-constant (cdr const-lst))))
									((symbol? curr) `(,@(make-symbol-const curr) ,@(make-constant (cdr const-lst))))
									((pair? curr) `(,@(make-pair-const curr) ,@(make-constant (cdr const-lst))))
									((string? curr) `(,(make-string-const curr) ,@(make-constant (cdr const-lst))))
									((vector? curr) `(,@(make-vector-const curr) ,@(make-constant (cdr const-lst))))
									((rational? curr) `(,(make-fraction-const curr) ,@(make-constant (cdr const-lst))))
									(else (make-constant (cdr const-lst))))))))

(define make-integer-const
	(lambda (int)
			`(undef ,int (T_INTEGER ,int))))

(define make-char-const
	(lambda (char)
			(let ((char-ascii (char->integer char)))
					`(undef ,char (T_CHAR ,char-ascii)))))

(define make-symbol-const
	(lambda (symbol)
			(let* ((symbol-str (symbol->string symbol))
						(make-string (make-string-const symbol-str))
						(make-symbol `(undef ,symbol (T_SYMBOL ,symbol-str))))
			`(,make-string ,make-symbol))))

(define make-pair-const
	(lambda (pair)
			(let* ((make-car (make-constant (list (car pair))))
						(make-cdr (make-constant (list (cdr pair))))
						(fix-cdr (if (null? (cdr pair)) '() (cdr pair)))
						(make-pair `(undef ,pair (T_PAIR ,(car pair) ,fix-cdr))))
					`(,@make-cdr ,@make-car ,make-pair))))

(define make-string-const
	(lambda (str)
			(let* ((str-lst (string->list str))
						(str-lst-ascii (map char->integer str-lst)))
					`(undef ,str (T_STRING ,(length str-lst) ,@str-lst-ascii)))))

(define make-vector-const
	(lambda (vec)
			(let* ((vec-lst (vector->list vec))
						(make-vec-elements (flatmap (lambda (el) (make-constant (list el))) vec-lst))
						(make-vector `(undef ,vec (T_VECTOR ,(length vec-lst) ,@vec-lst))))
					`(,@make-vec-elements ,make-vector))))

(define make-fraction-const
	(lambda (frac)
			(let (  (numer (numerator frac))
					(denom (denominator frac)))
			`(undef ,frac (T_FRACTION ,numer ,denom)))))

(define construct-constant-list
	(lambda (const-list pe)
			(if (null? pe)
					const-list
					(let ((first (car pe)))
							(if (list? first)
									(append (construct-constant-list const-list first) 
													(construct-constant-list const-list (cdr pe)))
									(if (and (eq? first 'const) (not-atomic? (cadr pe)))
									`(,@const-list ,(cadr pe))
									(construct-constant-list const-list (cdr pe)))
		)))))


(define init-table
	`((undef ,(void) (T_VOID))
		(undef () (T_NIL))
		(undef #f (T_BOOL 0))
		(undef #t (T_BOOL 1))))

(define assign-address
	(lambda (const-lst addr)
			(if (null? const-lst)
					'()
					(let* ((curr (car const-lst))
								(new-curr (cons addr (cdr curr)))
								(next-addr (+ (length (caddr curr)) addr)))
							(cons new-curr (assign-address (cdr const-lst) next-addr))))))


(define fix-address
	(lambda (const-table)
			(fold-left 
							(lambda (rest curr)
									(let* ((repr (caddr curr))
												(tag (car repr)))
											(cond ((eq? tag 'T_PAIR) 
														(let ((new `(,(car curr) ,(cadr curr) ,(cons 'T_PAIR (map (lambda (c) (lookup-address c rest)) (cdr repr))))))
															`(,@rest ,new)))
														((eq? tag 'T_VECTOR) 
														(let ((new `(,(car curr) ,(cadr curr) ,(cons 'T_VECTOR (cons (length (cddr repr)) 
																					(map (lambda (c) (lookup-address c rest)) (cddr repr)))))))
															`(,@rest ,new)))
														((eq? tag 'T_SYMBOL) 
															(let ((new `(,(car curr) ,(cadr curr) ,(cons 'T_SYMBOL (list (lookup-address (cadr repr) rest))))))
															`(,@rest ,new)))
														(else `(,@rest ,curr)))))
								'()
							const-table)))


(define lookup-address
	(lambda (c const-table)
			(if (null? const-table)
					(error 'lookup-address (format "Error: table is null! lookup: ~s" c))
					(let ((curr (car const-table)))
							(if (equal? c (cadr curr))
									(car curr)
									(lookup-address c (cdr const-table)))))))
							
(define not-atomic?
	(lambda (val)
			(not (or (null? val) (eq? val *void-object*) (boolean? val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;												  const end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ============================================================================================================
; ================================================= Global  ==================================================
; ============================================================================================================

(define make-global-table
	(lambda (parsed-exprs const-table)
			(let* ((global-list (flatmap (lambda (pe) (construct-fvars-list '() pe)) parsed-exprs))
						(global-list (remove-duplicates global-list))
						(global-table (make-global global-list))
						(global-table (append (primitive-table) global-table)) 
						(addr (+ 1 (length (make-repr-const const-table))))) ; end of constant table
					(assign-address global-table addr))
			))

(define construct-fvars-list
	(lambda (fvars-list pe)
			(if (null? pe)
					fvars-list
					(let ((first (car pe)))
							(if (list? first)
									(append (construct-fvars-list fvars-list first) 
													(construct-fvars-list fvars-list (cdr pe)))
									(if (and (eq? first 'fvar) (not (member (cadr pe) (run-time-support-lst))))
									`(,@fvars-list ,(cadr pe))
									(construct-fvars-list fvars-list (cdr pe)))
		)))))

(define run-time-support-lst
	(lambda () 
			'(append apply < = > + / * - boolean? car cdr char->integer 
					char? cons denominator eq? integer? integer->char list
					make-string make-vector map not null? number? numerator
					pair? procedure? rational? remainder set-car! set-cdr!
					string-length string-ref string-set! string->symbol string?
					symbol? symbol->string vector vector-length vector-ref
					vector-set! vector? zero? ; our functions below
					foldr foldl reverse length andmap plus-bin minus-bin mul-bin div-bin gt-bin lt-bin equal-bin)))

(define primitive-table
	(lambda ()
			(map (lambda (var)
							`(undef ,var (-1)))
					(run-time-support-lst))))

(define make-global
	(lambda (global-lst)
			(map (lambda (fvar)
							`(undef ,fvar (-1)))
						global-lst)))

; ============================================================================================================
; ================================================= SYMBOL TABLE  ============================================
; ============================================================================================================

(define make-symbol-table
	(lambda (const-table global-table)
			(let* ((symbol-table (filter (lambda (triple) (symbol? (cadr triple))) const-table))
						(addr (+ 1 (length (make-repr-const const-table)) (length (make-repr-global global-table))))) ; end of global table
					(assign-address symbol-table addr))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remove-duplicates
	(lambda (e)
		(if (null? e) '()
			(cons (car e) (remove-duplicates (filter (lambda (x) (not (equal? x (car e)))) (cdr e)))))))
											
(define *void-object* (void))

(define T_VOID 		1)
(define T_NIL 		2)
(define T_INTEGER	3)

(define T_FRACTION  4)

(define T_BOOL 		5)
(define T_CHAR 		6)

(define T_STRING	7)
(define T_SYMBOL 	8)

(define T_CLOSURE 	9)

(define T_PAIR		10)
(define T_VECTOR 	11)



(define make-repr-global
	(lambda (global-table)
			(flatmap (lambda (el) (caddr el)) global-table)))

(define make-repr-const
	(lambda (const-table)
		(flatmap cddr const-table)))
							   
(define flatmap
	(lambda (f lst)
			(fold-right append '() (map f lst)))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define full-cycle
	(lambda (sexpr)
			(annotate-tc
					(pe->lex-pe
							(box-set
									(remove-applic-lambda-nil
											
													(parse sexpr)))))))

(define file->string
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run (lambda ()
					(let ((ch (read-char in-port)))
					(if (eof-object? ch)
						(begin (close-input-port in-port) '())
						(cons ch (run)))))))
	(list->string (run))))))

(define string->sexprs
	(lambda (content)
			(let ((content-lst (string->list content)))
					(letrec ((run (lambda (sexprs rest)
															(if (null? rest)
																	sexprs
																	(<sexpr> rest
																						(lambda (sexpr remaining)
																							(run `(,@sexprs ,sexpr) remaining))
																						(lambda (_w) (error 'sexpr (format "no-match"))))))))
							(run '() content-lst)))))
							
(define compile-scheme-file
	(lambda (in out)
		(let* (
				(input-content (file->string in))
				(primitives-content (file->string "./project/primitives.scm"))
				(content (string-append primitives-content "\n" input-content))
				(sexprs (string->sexprs content))
				(parsed-exprs (map full-cycle sexprs))

				($constant-table (make-constant-table parsed-exprs))
				($global-table (make-global-table parsed-exprs $constant-table))
				($symbol-table (make-symbol-table $constant-table $global-table)))
				
				(set-box! constant-table $constant-table)
				(set-box! global-table $global-table)
				(set-box! symbol-table $symbol-table)
				
				(let (  (prologue (make-prologue))
						(code-generation (fold-left (lambda (rest curr)
															(string-append  rest
																			(gen-newline)
																			
																			(code-gen curr 0)
																			
																			
																			))
																			""
																			parsed-exprs))
								(epilogue (make-epilogue))
								)
							

							(string->file (string-append	prologue
															(gen-newline)
															 "; START_CODE_GEN:\n"
															code-generation 
															"; END_CODE_GEN:\n"
																			(line-gen2 "cmp rax, Lcons1")
																			(line-gen2 "je end")
																			(line-gen2 "push qword [rax]")
																			(line-gen2 "call write_sob_if_not_void")
																			(line-gen2 "add rsp, 1*8")
															epilogue
															
															
															)
									out))
			
		)
	))

(define string->file
	(lambda (str out-file)
			(if (file-exists? out-file)
					(begin (delete-file out-file) (string->file str out-file))
					(let* ((p (open-output-file out-file))
								(v (display str p)))
							(close-output-port p)
							v))))

(define code-gen
	(lambda (pe maj)
		(let ((tag (car pe)))
			(cond ((eq? tag 'const)
						(string-append
							(gen-newline)
							(line-gen2 "; const-code-gen begin")
							(const-code-generetion pe maj)
							(line-gen2 "; const-code-gen end")
							(gen-newline)))
					((eq? tag 'if3)
									(string-append 
											(gen-newline)
											(line-gen2 "; if code-gen begin")
											(code-gen-if pe maj)
											(line-gen2 "; if code-gen end")
											(gen-newline)))
											
					((eq? tag 'or)
									(string-append 
											(gen-newline)
											(line-gen2 "; or code-gen begin")
											(code-gen-or pe maj)
											(line-gen2 "; or code-gen end")
											))
											
					((eq? tag 'pvar)
									(string-append 
											(gen-newline)
											(line-gen2 "; pvar code-gen begin")
											(code-gen-pvar pe maj)
											(line-gen2 "; pvar code-gen end")
											(gen-newline)))
											
					((eq? tag 'set)
									(string-append 
											(gen-newline)
											(line-gen2 "; set code-gen begin")
											(code-gen-set pe maj)
											(line-gen2 "; set code-gen end")
											(gen-newline)))
											
					((eq? tag 'bvar)
									(string-append 
											(gen-newline)
											(line-gen2 "; bvar code-gen begin")
											(code-gen-bvar pe maj)
											(line-gen2 "; bvar code-gen end")
											(gen-newline)))
					((eq? tag 'fvar)
									(string-append 
											(gen-newline)
											(line-gen2 "; fvar code-gen begin")
											(code-gen-fvar pe maj)
											(line-gen2 "; fvar code-gen end")
											(gen-newline)))
					((eq? tag 'applic)
									(string-append 
											(gen-newline)
											(line-gen2 "; applic code-gen begin")
											(code-gen-applic pe maj)
											(line-gen2 "; applic code-gen end")
											 (gen-newline)))
					((eq? tag 'box-get)
									(string-append 
											(gen-newline)
											(line-gen2 "; bpx-get code-gen begin")
											(code-gen-box-get pe maj)
											(line-gen2 "; box-get code-gen end")
											 (gen-newline)))
					((eq? tag 'box-set)
									(string-append 
											(gen-newline)
											(line-gen2 "; box-set code-gen begin")
											(code-gen-box-set pe maj)
											(line-gen2 "; box-set code-gen end")
											 (gen-newline)))
					((eq? tag 'box)
									(string-append 
											(gen-newline)
											(line-gen2 "; box code-gen begin")
											(code-gen-box pe maj)
											(line-gen2 "; box code-gen end")
											(gen-newline)))
											 
					((eq? tag 'define)
									(string-append 
											(gen-newline)
											(line-gen2 "; define code-gen begin")
											(code-gen-define pe maj)
											(line-gen2 "; define code-gen end")
											 (gen-newline)))
											 
					((eq? tag 'lambda-simple)
									(string-append 
											(gen-newline)
											(line-gen2 "; lambda-simple code-gen begin")
											(code-gen-lambda-simple pe maj)
											(line-gen2 "; lambda-simple code-gen end")
											(gen-newline)))
											
					((eq? tag 'tc-applic)
									(string-append 
											(gen-newline)
											(line-gen2 "; tc-applic code-gen begin")
											(code-gen-applic pe maj)
											(line-gen2 "; tc-applic code-gen end")
											 (gen-newline)))
					((eq? tag 'lambda-opt)
									(string-append 
											(gen-newline)
											(line-gen2 "; lambda-opt code-gen begin")
											(code-gen-lambda-opt pe maj)
											(line-gen2 "; lambda-opt code-gen end")
											 (gen-newline)))
					((eq? tag 'seq)
									(string-append 
											(gen-newline)
											(line-gen2 "; seq code-gen begin")
											(code-gen-seq pe maj)
											(line-gen2 "; seq code-gen end")
											 (gen-newline)))
											
			(else  
				'error))
		)
	))

(define code-gen-seq
	(lambda (pe maj)
			(let* ( (exprs (cadr pe)))
					(string-append
									(fold-left
											(lambda (rest curr)
															(string-append rest
																						(code-gen curr maj)))
											""
											exprs)))))

(define label-index (box 0))

(define fetch-label-and-inc
	(lambda ()
			(let ((curr-index (unbox label-index)))
					(begin (set-box! label-index (add1 curr-index))
								(number->string curr-index)))))

(define const-code-generetion
	(lambda (pe maj)
		(let	((const-address (lookup-address (cadr pe) (unbox constant-table))))
			(string-append(line-gen2 "mov rax, Lcons" (number->string const-address))
			)
		)
	))

(define code-gen-if
	(lambda (pe maj)
		(let (  (test (cadr pe))
				(dit (caddr pe))
				(dif (cadddr pe))
				(label-if3-else (string-append "L_if3_else_" (fetch-label-and-inc)))
				(label-if3-exit (string-append "L_if3_exit_" (fetch-label-and-inc))))
			(string-append
				(code-gen test maj)
				(line-gen2 "cmp rax, Lcons3")
				(line-gen2 "je " label-if3-else)
				(code-gen dit maj)
				(line-gen2 "jmp " label-if3-exit)
				(gen-line1 label-if3-else ":")
				(code-gen dif maj)
				(gen-line1 label-if3-exit ":")
			)
		)
	))

(define code-gen-or
	(lambda (pe maj)
		(let*   ((exprs (cadr pe))
				(exprs-without-last (reverse (cdr (reverse exprs))))
				(last-expr (car (last-pair exprs)))
				(label-or-exit (string-append "L_or_exit_" (fetch-label-and-inc))))
			(string-append
					(fold-left
						(lambda (rest curr)
							(string-append rest
											(code-gen curr maj)
											(line-gen2 "cmp rax, Lcons3")
											(line-gen2 "jne " label-or-exit)))
											""
											exprs-without-last)
									(code-gen last-expr maj)
									(gen-line1 label-or-exit ":")
					)
				)
	))

(define code-gen-pvar
	(lambda (pe maj)
			(let ((minor (caddr pe)))
					(string-append
							(line-gen2 "mov rax ,qword [rbp+(4+" (number->string minor)")*8] "))
					)))
					
(define code-gen-set
	(lambda (pe maj)
			(let* (	 (var (cadr pe))
						(var-tag (car var))
						(expr (caddr pe)))
					(string-append
							(code-gen expr maj)
							(line-gen2 "mov rax, qword [rax]")
							
							(cond ((eq? var-tag 'pvar)
											(string-append
													(line-gen2 "mov qword [rbp+(4+" (number->string (caddr var)) ")*8], rax"))) 
									
									((eq? var-tag 'bvar)
									(string-append
											(line-gen2 "mov rbx, qword [rbp+2*8] ;env")
											(line-gen2 "mov rax, qword [rbx+" (number->string (caddr var)) "*8] ; env[maj]")
											(line-gen2 "mov qword [rbx+" (number->string (cadddr var))"*8], rax ;env[maj[min]")))
													  
									((eq? var-tag 'fvar)
										(line-gen2 "mov qword [Lprim" (number->string (lookup-address (cadr var) (unbox global-table))) "],rax"))
								)
							(line-gen2 "mov rax, Lcons1")
					))))
					
(define code-gen-bvar
	(lambda (pe maj)
			(let (	  (major (caddr pe))
						(minor (cadddr pe)))
			(string-append
			(line-gen2 "mov rax, env ;env")
			(line-gen2 "mov rax, qword [rax+" (number->string major) "*8]  ; env[maj]")
			(line-gen2 "mov rax, qword [rax+" (number->string minor) "*8]  ; env[maj][min]")
	))))

(define code-gen-fvar
	(lambda (pe maj)
			(let* ((var (cadr pe))
						(fvar-address (lookup-address var (unbox global-table))))
					(line-gen2 "mov rax,qword [Lprim" (number->string fvar-address)"]" ))
	))
	
(define code-gen-applic
	(lambda (pe maj)
			(let* (  (proc (cadr pe))
					(args (caddr pe))
					(reverse-args (reverse args))
					(label-error "L_error_apply_non_clos"))
					(string-append
					(line-gen2 "; application ")
							(line-gen2 "push qword Lcons2")
							(fold-right
									(lambda (curr rest)
											(string-append
													rest
														(code-gen curr maj)
														
														(line-gen2 "push qword rax")
														
													
													
													))
										"" args)
							(line-gen2 "mov rax , " (number->string (length args)))
							(line-gen2 "push qword rax" )
							(code-gen proc maj)
							(line-gen2 "mov rax, qword[rax]")
							(line-gen2 "mov rbx, rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax ,T_CLOSURE")
							(line-gen2 "jne " label-error )
							(line-gen2 "mov rax, rbx")
							(line-gen2 "CLOSURE_ENV rbx")
							(line-gen2 "push qword rbx")
							(line-gen2 "CLOSURE_CODE rax")
							(line-gen2 "call rax")
							(line-gen2 "mov rbx, rax")
							(line-gen2 "add rsp, 8") ;env
							(line-gen2 "pop rcx")  ;arg_count
							(line-gen2 "add rcx, 1")
							(line-gen2 "mov rax, 8")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "mul rcx")
							(line-gen2 "mov rcx, rax")
							(line-gen2 "mov rax, rbx")
							(line-gen2 "add rsp, rcx")
							
					))))
   
(define code-gen-tc-applic
	(lambda (pe maj)
			(let* (	 (proc (cadr pe))
						(args (caddr pe))
						(label-error "L_error_apply_non_clos")
						(label-smash-stack-loop (string-append "L_smash_stack_loop_" (fetch-label-and-inc)))
						(label-smash-stack-loop-exit (string-append "L_smash_stack_loop_exit_" (fetch-label-and-inc)))
						(label-check-another-nil (string-append "L_check_another_nil_" (fetch-label-and-inc)))
						
						(numberOf (+ 4 (length args)))
						
						)
					(string-append
							(line-gen2 "; tail call application ")
							(line-gen2 "push qword Lcons2")
							(fold-right
									(lambda (curr rest)
											(string-append
													rest
													(code-gen curr maj)
													(line-gen2 "push qword rax")
													))
									"" args)
							
							(line-gen2 "mov rax , " (number->string (length args)))
							(line-gen2 "push qword rax" )
							(code-gen proc maj)
							(line-gen2 "mov rax, qword[rax]")
							(line-gen2 "mov rbx, rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax ,T_CLOSURE")
							(line-gen2 "jne " label-error )
							
							(line-gen2 "mov rax, rbx")
							;(line-gen2 "mov rbx, rax")
							(line-gen2 "mov r15, rax")
							(line-gen2 "CLOSURE_ENV rbx")
							(line-gen2 "push qword rbx")
							
							
							; tc-applic
							
							(line-gen2 "mov r10, qword[rbp+8] ; push ret")
							(line-gen2 "push r10 ; // push ret")
							(line-gen2 "mov r14, old_rbp ")
							(line-gen2 "mov r13, rsp")

							
							(line-gen2 "mov r8, rbp ")
							(line-gen2 "mov r9, r8 ")
							(line-gen2 "sub r8, 8 ")
							(line-gen2 "mov r12, arg_count")
							(line-gen2 "add r12, 5")
							(line-gen2 "mov rax, 8")
							(line-gen2 "mul r12")
							(line-gen2 "add r9, rax")
							
							(help! numberOf)
							
							(line-gen2 "mov rsp, r13 ")
							(line-gen2 "mov rbp, r14 ") ;old_rbp
							(line-gen2 "CLOSURE_CODE r15 ")
							(line-gen2 "jmp r15 ")
					))))
					
(define help!
	(lambda(i)
		(if (= i 0)
			""
			(string-append
				
				(line-gen2 "mov rcx, qword[r8] ")
				(line-gen2 "mov qword[r9], rcx")
				(line-gen2 "sub r8, 8 ")
				(line-gen2 "sub r9, 8 ")
				(help! (- i 1))))
							
		))

(define code-gen-box-get
	(lambda (pe maj)
			(let*   (	 (var (cadr pe))
						(var-tag (car var))
					)
					(string-append
							(cond ((eq? var-tag 'pvar)
											(string-append
													(line-gen2 "mov rax, qword [rbp+(4+" (number->string (caddr var)) ")*8]"))) 
									((eq? var-tag 'bvar)
										(string-append
											(line-gen2 "mov rax, qword [rbp+2*8] ;env")
											(line-gen2 "mov rax, qword [rax+" (number->string (caddr var)) "*8] ; env[maj]")
											(line-gen2 "mov rax, qword [rax+" (number->string (cadddr var)) "*8] ; env[maj][min]")))
									((eq? var-tag 'fvar) 
										(line-gen2 "mov rax, qword [Lprim" (number->string (lookup-address (cadr var) (unbox global-table))) "] ; take the box pointer"))
								)
							(line-gen2 "mov rax, qword [rax] ; unbox")
					))))
					
(define code-gen-box-set
	(lambda (pe maj)
			(let* (	 (var (cadr pe))
						(var-tag (car var))
						(expr (caddr pe)))
					(string-append
							(code-gen expr maj)
							
							(cond ((eq? var-tag 'pvar)
											(string-append
													(line-gen2 "mov qword [rbp+(4+" (number->string (caddr var)) ")*8] ,rax"))) 
									((eq? var-tag 'bvar)
									(string-append
											(line-gen2 "mov rbx, qword [rbp+2*8] ;env")
											(line-gen2 "mov rbx, qword [rbx+" (number->string (caddr var)) "*8] ; env[maj]")
											(line-gen2 "mov rbx, qword [rbx+" (number->string (cadddr var)) "*8] ; env[maj][min]")
											(line-gen2 "mov qword [rbx], rax")))
									((eq? var-tag 'fvar)
										(line-gen2 "mov rbx, qword [Lprim"(number->string (lookup-address (cadr var) (unbox global-table))) "] ;take pointer from global table")
									
										(line-gen2 "mov qword [rbx], rax"))
								)
							(line-gen2 "mov rax, Lcons1")
					))))
					
(define code-gen-box
	(lambda (pe maj)
			(let* (	 (var (cadr pe))
						(var-tag (car var)))
					(string-append
							(code-gen var maj)
							(line-gen2 "mov rbx, rax; // save code-gen ret")
							(line-gen2 "push rbx")
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							(line-gen2 "mov qword [rax], rbx")
					))))
					
(define code-gen-define
	(lambda (pe maj)
			(let* (	 (fvar (cadr pe))
						(var (cadr fvar))
						(expr (caddr pe))
						(fvar-address (lookup-address var (unbox global-table))))
					(string-append
							(code-gen expr maj)
							(line-gen2 "mov qword [Lprim"(number->string fvar-address)"] ,rax")
							(line-gen2 "mov rax, Lcons1")
					))))
		
(define code-gen-lambda-opt
		(lambda (pe maj)
			(let ((args (cadr pe))
			  (opt (caddr pe))
			  (body (cadddr pe))
			  (label-copy-prev-env-loop (string-append "L_loop_copy_prev_env_" (fetch-label-and-inc)))
			  (label-copy-prev-env-exit-loop (string-append "L_exit_loop_copy_prev_env_" (fetch-label-and-inc)))
			  (label-copy-params-loop (string-append "L_loop_copy_params_" (fetch-label-and-inc)))
			  (label-copy-params-exit-loop (string-append "L_exit_loop_copy_params_" (fetch-label-and-inc)))
			  (label-closure_code (string-append "L_closure_code_" (fetch-label-and-inc)))
			  (label-closure_exit (string-append "L_closure_opt_exit_" (fetch-label-and-inc)))
			  (label-wrong-arity "L_wrong_arity")
			  (label-raise-stack (string-append "L_raise_stack_" (fetch-label-and-inc)))
			  (label-make-opt-list (string-append "L_make_opt_list_" (fetch-label-and-inc)))
			  (label-make-opt-list-exit (string-append "L_make_opt_list_exit_" (fetch-label-and-inc)))
			  (label-fix-stack-with-opt-list (string-append "L_fix_stack_with_opt_list_" (fetch-label-and-inc)))
			  (label-fix-stack-with-opt-list-exit (string-append "L_fix_stack_with_opt_list_exit_" (fetch-label-and-inc)))
			  (label-new-closure-code (string-append "L_new_closure_code_" (fetch-label-and-inc))))

			(string-append
				(line-gen2 "mov rdi, "(number->string (add1 maj)) "")
				(line-gen2 "mov rax, 8")
				(line-gen2 "mul rdi")
				(line-gen2 "mov rdi, rax")
				(line-gen2 "call malloc")
				(line-gen2 "mov rcx, rax")
				(line-gen2 "mov rbx, env")

				(line-gen2 "; copy the address of the previous env (vectors) ;")
				(line-gen2 "mov rdi, 0 ;// i=0")
				(line-gen2 "mov rsi, 1 ;// j=1")
				(gen-line1 label-copy-prev-env-loop ":")
				(line-gen2 "cmp rdi, "(number->string maj)"  ;//i<maj")
				(line-gen2 "je " label-copy-prev-env-exit-loop )
				(line-gen2 "mov r9, qword [rbx + rdi*8]")
				(line-gen2 "mov qword [rcx + rsi*8], r9")
				(line-gen2 "inc rdi ;// i++")
				(line-gen2 "inc rsi ;// j++")
				(line-gen2 "jmp " label-copy-prev-env-loop "")
				(gen-line1 label-copy-prev-env-exit-loop ":")
				(line-gen2 "; end copy ;")
				; malloc(rdx)
				(line-gen2 "mov r14, 0")
				(line-gen2 "cmp r14,"(number->string maj))
				(line-gen2 "je " label-copy-params-exit-loop)
				(line-gen2 "push rcx")
				(line-gen2 "mov rdi, arg_count")
				(line-gen2 "mov rax, 8")
				(line-gen2 "mul rdi")
				(line-gen2 "mov rdi, rax")
				(line-gen2 "call malloc")
				(line-gen2 "pop rcx")
				(line-gen2 "mov qword [rcx], rax;")
				; end of malloc(rdx)

				(line-gen2 "; copy params of the previous lambda to the new env ;")
				(line-gen2 "mov rdi, 0 ;// i=0")
				(line-gen2 "mov r8, qword [rcx]; ;// r8 = rcx[0]")
				(gen-line1 label-copy-params-loop ":")
				(line-gen2 "cmp rdi, arg_count; ;// i<rdx")
				(line-gen2 "jg " label-copy-params-exit-loop)
				(line-gen2 "mov r9, An(rdi)")
				(line-gen2 "mov qword [r8 + rdi*8], r9")
				(line-gen2 "inc rdi")
				(line-gen2 "jmp " label-copy-params-loop)
				(gen-line1 label-copy-params-exit-loop ":")
				(line-gen2 "; end copy ;")


				(line-gen2 "; build closure object ;")
				(line-gen2 "push rcx")
				(line-gen2 "mov rdi, 16")
				(line-gen2 "call malloc")
				(line-gen2 "pop rcx;")
				(line-gen2 "MAKE_LITERAL_CLOSURE rax, rcx, "label-closure_code)
				
				(line-gen2 "jmp " label-closure_exit)
				
				(gen-line1 label-closure_code ":")
				;CLOSURE CODE
				(line-gen2 "push rbp")
				(line-gen2 "mov rbp, rsp")
				(line-gen2 "mov rbx, Lcons2")
				(line-gen2 "cmp arg_count, "(number->string (length args)))
				(line-gen2 "je " label-make-opt-list-exit)
				;(line-gen2 "jl " label-wrong-arity)

				
				(line-gen2 "mov rcx, arg_count")
				(gen-line1 label-make-opt-list ":")
				(line-gen2 "cmp rcx, " (number->string (length args)))
				(line-gen2 "je "label-make-opt-list-exit)
				(line-gen2 "mov r10, rcx")
				(line-gen2 "dec r10")
				(line-gen2 "mov r9, An(r10)")
				(line-gen2 "push qword rcx")
				(line-gen2 "mov rdi, 16")
				(line-gen2 "call malloc")
				(line-gen2 "MAKE_MALLOC_LITERAL_PAIR rax, r9, rbx")
				(line-gen2 "mov rbx, rax")
				(line-gen2 "pop qword rcx")
				(line-gen2 "dec rcx")
				(line-gen2 "jmp "label-make-opt-list)
				(gen-line1 label-make-opt-list-exit ":")	;;; rbx,rax holds the start of the list
				(line-gen2 "mov r13," (number->string (length args)))
				(line-gen2 "mov An(r13),rbx")
				(code-gen body (add1 maj))
				(line-gen2 "leave")
				(line-gen2 "ret")
				(gen-line1 label-closure_exit ":")
			))))

(define code-gen-lambda-simple
	(lambda (pe maj)
		(let (	  (args (cadr pe))
							(body (caddr pe))
			  (label-copy-prev-env-loop (string-append "L_loop_copy_prev_env_" (fetch-label-and-inc)))
			  (label-copy-prev-env-exit-loop (string-append "L_exit_loop_copy_prev_env_" (fetch-label-and-inc)))
			  (label-copy-params-loop (string-append "L_loop_copy_params_" (fetch-label-and-inc)))
			  (label-copy-params-exit-loop (string-append "L_exit_loop_copy_params_" (fetch-label-and-inc)))
			  (label-closure_code (string-append "L_closure_code_" (fetch-label-and-inc)))
			  (label-closure_exit (string-append "L_closure_exit_" (fetch-label-and-inc)))
			  (label-wrong-arity "L_wrong_arity"))
			(string-append
				(line-gen2 "mov rdi, "(number->string (add1 maj)) "")
				(line-gen2 "mov rax, 8")
				(line-gen2 "mul rdi")
				(line-gen2 "mov rdi, rax")
				(line-gen2 "call malloc")
				(line-gen2 "mov rcx, rax")
				(line-gen2 "mov rbx, env")

				(line-gen2 "; copy the address of the previous env (vectors) ;")
				(line-gen2 "mov rdi, 0 ;// i=0")
				(line-gen2 "mov rsi, 1 ;// j=1")
				(gen-line1 label-copy-prev-env-loop ":")
				(line-gen2 "cmp rdi, "(number->string maj)"  ;//i<maj")
				(line-gen2 "je " label-copy-prev-env-exit-loop )
				(line-gen2 "mov r9, qword [rbx + rdi*8]")
				(line-gen2 "mov qword [rcx + rsi*8], r9")
				(line-gen2 "inc rdi ;// i++")
				(line-gen2 "inc rsi ;// j++")
				(line-gen2 "jmp " label-copy-prev-env-loop "")
				(gen-line1 label-copy-prev-env-exit-loop ":")
				(line-gen2 "; end copy ;")
				; malloc(rdx)
				(line-gen2 "mov r14, 0")
				(line-gen2 "cmp r14,"(number->string maj))
				(line-gen2 "je " label-copy-params-exit-loop)
				(line-gen2 "push rcx")
				(line-gen2 "mov rdi, arg_count")
				(line-gen2 "mov rax, 8")
				(line-gen2 "mul rdi")
				(line-gen2 "mov rdi, rax")
				(line-gen2 "call malloc")
				(line-gen2 "pop rcx")
				(line-gen2 "mov qword [rcx], rax;")
				; end of malloc(rdx)

				(line-gen2 "; copy params of the previous lambda to the new env ;")
				(line-gen2 "mov rdi, 0 ;// i=0")
				(line-gen2 "mov r8, qword [rcx]; ;// r8 = rcx[0]")
				(gen-line1 label-copy-params-loop ":")
				(line-gen2 "cmp rdi, arg_count; ;// i<rdx")
				(line-gen2 "jg " label-copy-params-exit-loop)
				(line-gen2 "mov r9, An(rdi)")
				(line-gen2 "mov qword [r8 + rdi*8], r9")
				(line-gen2 "inc rdi")
				(line-gen2 "jmp " label-copy-params-loop)
				(gen-line1 label-copy-params-exit-loop ":")
				(line-gen2 "; end copy ;")


				(line-gen2 "; build closure object ;")
				(line-gen2 "push rcx")
				(line-gen2 "mov rdi, 16")
				(line-gen2 "call malloc")
				(line-gen2 "pop rcx;")
				(line-gen2 "MAKE_LITERAL_CLOSURE rax, rcx, "label-closure_code)
				
				(line-gen2 "jmp " label-closure_exit)
				(gen-line1 label-closure_code ":")
				;CLOSURE CODE
				(line-gen2 "push rbp")
				(line-gen2 "mov rbp, rsp")
				(code-gen body (add1 maj))
				(line-gen2 "leave")
								(line-gen2 "ret")
				(gen-line1 label-closure_exit ":")
			))))
			
; ========================================================================================================
; ============================================ primitives  ===============================================
; ========================================================================================================

(define runtime-support
	(lambda ()
		(string-append
			(code-gen-car)
			(code-gen-cdr)
			(code-gen-cons)
			(code-gen-plus-bin)
			(code-gen-minus-bin)
			(code-gen-mul-bin)
			(code-gen-boolean?)
			(code-gen-char?)
			(code-gen-integer?)
			(code-gen-pair?)
			(code-gen-procedure?)
			(code-gen-string?)
			(code-gen-symbol?)
			(code-gen-vector?)
			(code-gen-char->integer)
			(code-gen-integer->char)
			(code-gen-string-length)
			(code-gen-string-ref)
			(code-gen-string-set!)
			(code-gen-vector-length)
			(code-gen-vector-ref)
			(code-gen-vector-set!)
			(code-gen-div-bin)
			(code-gen-set-car!)
			(code-gen-set-cdr!)
			(code-gen-numerator)
			(code-gen-denominator)
			(code-gen-remainder)
			(code-gen-fraction?)
			(code-gen-addr-eq)
			(code-gen-val-eq) 
			(code-gen-gt-bin)
			(code-gen-lt-bin)
			(code-gen-equal-bin)
			(code-gen-make-string)
			(code-gen-make-vector)
			(code-gen-vector) 
			(code-gen-apply) ;TODO - not working
			(code-gen-symbol->string)
			(code-gen-string->symbol) ;TODO
			(code-gen->)	
			
                    (code-gen-<)
                    (code-gen-=)
		)))
		
(define code-gen-string->symbol
	(lambda ()
		(let 	(       (label-make-string->symbol-clos "L_make_stringtosymbol_clos_")
				(label-string->symbol-body "L_make_stringtosymbol_body")
				
				(label-length-params-loop "L_length_params_loop")
				(label-length-params-loop-exit "L_length_params_loop_exit")
				(label-CDR-loop "L_CDR_loop")
				(label-CDR-loop-exit "L_CDR_loop_exit")
				(label-push-args "L_push_args")
				(label-push-args-exit "L_push_args_exit")
				(label-move-rest-stack-loop "L_move_rest_stack_loop")
				(label-move-rest-stack-loop-exit "L_move_rest_stack_loop_exit"))

					(string-append
							(line-gen2 "jmp " label-make-string->symbol-clos )
							(gen-line1 label-string->symbol-body ":")
                                                        (line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")

							
							(line-gen2 "mov r8, A0")
							(line-gen2 "mov r8, qword[r8]")
							(line-gen2 "STRING_ELEMENTS r8")
							(line-gen2 "mov r10, qword[head]")
							(line-gen2 "mov r13, r10")
							
							
							(line-gen2 "STSLOOP:")
							(line-gen2 "CAR r10")
							(line-gen2 "cmp r10, Lcons2")
							(line-gen2 "je NEWSYMCREATION")
							
							(line-gen2 "mov r9, r13")
							(line-gen2 "CDR r13")
							(line-gen2 "CAR r9")
							
							(line-gen2 "mov r9, qword[r9]")
							
							(line-gen2 "STRING_ELEMENTS r9")
							(line-gen2 "cmp r8, r9")
							(line-gen2 "je SYMISFOUND")
							
							(line-gen2 "mov r10,r13")
							(line-gen2 "mov r10, qword[r10]")
							(line-gen2 "jmp STSLOOP")
							
							(line-gen2 "NEWSYMCREATION:")
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "mov r8, A0")
							(line-gen2 "mov r14, head")
							(line-gen2 "MAKE_MALLOC_LITERAL_PAIR rax, r8, r14")
							(line-gen2 "mov qword[head], rax")
							;(line-gen2 "mov rax, qword[rax]")
							;(line-gen2 "CAR rax")
							

							;(line-gen2 "jmp STSEND")
							
							(line-gen2 "SYMISFOUND:")
							
							(line-gen2 "mov rdi,8")
							(line-gen2 "call malloc")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "sub rbx, start_of_data")
							(line-gen2 "mov qword[rax],rbx")
							(line-gen2 "shl qword[rax],4")
							(line-gen2 "or qword[rax],T_SYMBOL")
							
							(line-gen2 "STSEND:")
							(line-gen2 "leave")
							(line-gen2 "ret ")
							
							
							(gen-line1 label-make-string->symbol-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-string->symbol-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'string->symbol (unbox global-table))) "] , rax" )))))
							
(define code-gen-=
	(lambda ()
		(let 	(       (label-make-=-clos "L_make_EQ_clos_")
				(label-=-body "L_make_EQ_body")
				
				(label-length-params-loop "L_length_params_loop")
				(label-length-params-loop-exit "L_length_params_loop_exit")
				(label-CDR-loop "L_CDR_loop")
				(label-CDR-loop-exit "L_CDR_loop_exit")
				(label-push-args "L_push_args")
				(label-push-args-exit "L_push_args_exit")
				(label-move-rest-stack-loop "L_move_rest_stack_loop")
				(label-move-rest-stack-loop-exit "L_move_rest_stack_loop_exit"))

					(string-append
							(line-gen2 "jmp " label-make-=-clos )
							(gen-line1 label-=-body ":")
                                                        (line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")

							
							(line-gen2 "mov r8, arg_count")  ; r8 - saves the return address
							(line-gen2 "cmp r8, 1")	;disposse of apply env
							(line-gen2 "je retEQtrue")  ;disposse of arg_count
							
							(line-gen2 "sub r8, 2")
							(line-gen2 "mov r10, 0")  ; r9 - saves the proc
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "EQL:")
							(line-gen2 "TYPE r11")
							(line-gen2 "TYPE r12")
							(line-gen2 "cmp r11, T_FRACTION")
							
							(line-gen2 "je ArgIsFracEQ")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "DATA r11")
							(line-gen2 "mov r13, 1")
							
							
							(line-gen2 "jmp SecArgCheckEQ")
							
							(line-gen2 "ArgIsFracEQ:")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "mov r13, r11")
							(line-gen2 "DATA_UPPER r11")
							(line-gen2 "DATA_LOWER r13")
							
							
							(line-gen2 "SecArgCheckEQ: ")
							(line-gen2 "cmp r12, T_FRACTION")
							
							(line-gen2 "je Arg2IsFracEQ")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "DATA r12")
							(line-gen2 "mov r14, 1")
							(line-gen2 "jmp CCNOMEQ")
							
							
							(line-gen2 "Arg2IsFracEQ:")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "mov r14, r12")
							
							(line-gen2 "DATA_UPPER r12")
							(line-gen2 "DATA_LOWER r14")
							
							(line-gen2 "CCNOMEQ:")
							
							
							(line-gen2 "mov rax, r13")
							
							(line-gen2 "mul r12")
							
							(line-gen2 "mov r12, rax")
							(line-gen2 "mov rax, r14")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")
							(line-gen2 "cmp r11, r12")
							
							(line-gen2 "jne retEQfalse")
							
							(line-gen2 "cmp r10, r8")
							
							
							(line-gen2 "je retEQtrue" )
							(line-gen2 "inc r10")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "jmp EQL")
							
							
							
							
							
							(line-gen2 "retEQfalse:")
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp EQend")
							
							(line-gen2 "retEQtrue:")
							(line-gen2 "mov rax, Lcons5")
							
							(line-gen2 "EQend:")
							(line-gen2 "leave")
							(line-gen2 "ret ")
							
							
							(gen-line1 label-make-=-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-=-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address '= (unbox global-table))) "] , rax" )))))
		
(define code-gen-<
	(lambda ()
		(let 	(       (label-make-<-clos "L_make_LT_clos_")
				(label-<-body "L_make_LT_body")
				
				(label-length-params-loop "L_length_params_loop")
				(label-length-params-loop-exit "L_length_params_loop_exit")
				(label-CDR-loop "L_CDR_loop")
				(label-CDR-loop-exit "L_CDR_loop_exit")
				(label-push-args "L_push_args")
				(label-push-args-exit "L_push_args_exit")
				(label-move-rest-stack-loop "L_move_rest_stack_loop")
				(label-move-rest-stack-loop-exit "L_move_rest_stack_loop_exit"))

					(string-append
							(line-gen2 "jmp " label-make-<-clos )
							(gen-line1 label-<-body ":")
                                                        (line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")

							
							(line-gen2 "mov r8, arg_count")  ; r8 - saves the return address
							(line-gen2 "cmp r8, 1")	;disposse of apply env
							(line-gen2 "je retLTtrue")  ;disposse of arg_count
							
							(line-gen2 "sub r8, 2")
							(line-gen2 "mov r10, 0")  ; r9 - saves the proc
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "LTL:")
							(line-gen2 "TYPE r11")
							(line-gen2 "TYPE r12")
							(line-gen2 "cmp r11, T_FRACTION")
							
							(line-gen2 "je ArgIsFracLT")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "DATA r11")
							(line-gen2 "mov r13, 1")
							
							
							(line-gen2 "jmp SecArgCheckLT")
							
							(line-gen2 "ArgIsFracLT:")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "mov r13, r11")
							(line-gen2 "DATA_UPPER r11")
							(line-gen2 "DATA_LOWER r13")
							
							
							(line-gen2 "SecArgCheckLT: ")
							(line-gen2 "cmp r12, T_FRACTION")
							
							(line-gen2 "je Arg2IsFracLT")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "DATA r12")
							(line-gen2 "mov r14, 1")
							(line-gen2 "jmp CCNOMLT")
							
							
							(line-gen2 "Arg2IsFracLT:")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "mov r14, r12")
							
							(line-gen2 "DATA_UPPER r12")
							(line-gen2 "DATA_LOWER r14")
							
							(line-gen2 "CCNOMLT:")
							
							
							(line-gen2 "mov rax, r13")
							
							(line-gen2 "mul r12")
							
							(line-gen2 "mov r12, rax")
							(line-gen2 "mov rax, r14")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")
							(line-gen2 "cmp r11, r12")
							
							(line-gen2 "jge retLTfalse")
							
							(line-gen2 "cmp r10, r8")
							
							
							(line-gen2 "je retLTtrue" )
							(line-gen2 "inc r10")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "jmp LTL")
							
							
							
							
							
							(line-gen2 "retLTfalse:")
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp LTend")
							
							(line-gen2 "retLTtrue:")
							(line-gen2 "mov rax, Lcons5")
							
							(line-gen2 "LTend:")
							(line-gen2 "leave")
							(line-gen2 "ret ")
							
							
							(gen-line1 label-make-<-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-<-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address '< (unbox global-table))) "] , rax" )))))

(define code-gen->
	(lambda ()
		(let 	(       (label-make->-clos "L_make_GT_clos_")
				(label->-body "L_make_GT_body")
				
				(label-length-params-loop "L_length_params_loop")
				(label-length-params-loop-exit "L_length_params_loop_exit")
				(label-CDR-loop "L_CDR_loop")
				(label-CDR-loop-exit "L_CDR_loop_exit")
				(label-push-args "L_push_args")
				(label-push-args-exit "L_push_args_exit")
				(label-move-rest-stack-loop "L_move_rest_stack_loop")
				(label-move-rest-stack-loop-exit "L_move_rest_stack_loop_exit"))

					(string-append
							(line-gen2 "jmp " label-make->-clos )
							(gen-line1 label->-body ":")
                                                        (line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")

							
							(line-gen2 "mov r8, arg_count")  ; r8 - saves the return address
							(line-gen2 "cmp r8, 1")	;disposse of apply env
							(line-gen2 "je retGTtrue")  ;disposse of arg_count
							
							(line-gen2 "sub r8, 2")
							(line-gen2 "mov r10, 0")  ; r9 - saves the proc
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "GTL:")
							(line-gen2 "TYPE r11")
							(line-gen2 "TYPE r12")
							(line-gen2 "cmp r11, T_FRACTION")
							
							(line-gen2 "je ArgIsFracGT")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "DATA r11")
							(line-gen2 "mov r13, 1")
							
							
							(line-gen2 "jmp SecArgCheckGT")
							
							(line-gen2 "ArgIsFracGT:")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "mov r13, r11")
							(line-gen2 "DATA_UPPER r11")
							(line-gen2 "DATA_LOWER r13")
							
							
							(line-gen2 "SecArgCheckGT: ")
							(line-gen2 "cmp r12, T_FRACTION")
							
							(line-gen2 "je Arg2IsFracGT")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "DATA r12")
							(line-gen2 "mov r14, 1")
							(line-gen2 "jmp CCNOMGT")
							
							
							(line-gen2 "Arg2IsFracGT:")
							
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "mov r14, r12")
							
							(line-gen2 "DATA_UPPER r12")
							(line-gen2 "DATA_LOWER r14")
							
							(line-gen2 "CCNOMGT:")
							
							
							(line-gen2 "mov rax, r13")
							
							(line-gen2 "mul r12")
							
							(line-gen2 "mov r12, rax")
							(line-gen2 "mov rax, r14")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")
							(line-gen2 "cmp r11, r12")
							
							(line-gen2 "jle retGTfalse")
							
							(line-gen2 "cmp r10, r8")
							
							
							(line-gen2 "je retGTtrue" )
							(line-gen2 "inc r10")
							
							(line-gen2 "mov r11, qword [rsp+ 4*8 + r10*8]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [rsp+ 5*8 + r10*8]")
							
							(line-gen2 "mov r11, qword [r11]") ; r10 - saves the list
							(line-gen2 "mov r12, qword [r12]")
							
							(line-gen2 "jmp GTL")
							
							
							
							
							
							(line-gen2 "retGTfalse:")
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp GTend")
							
							(line-gen2 "retGTtrue:")
							(line-gen2 "mov rax, Lcons5")
							
							(line-gen2 "GTend:")
							(line-gen2 "leave")
							(line-gen2 "ret ")
							
							
							(gen-line1 label-make->-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label->-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address '> (unbox global-table))) "] , rax" )))))
		
		
(define code-gen-apply
	(lambda ()
		(let 	((label-make-apply-clos "L_make_apply_clos_")
				(label-apply-body "L_make_apply_body")
				(label-length-params-loop "L_length_params_loop")
				(label-length-params-loop-exit "L_length_params_loop_exit")
				(label-CDR-loop "L_CDR_loop")
				(label-CDR-loop-exit "L_CDR_loop_exit")
				(label-push-args "L_push_args")
				(label-push-args-exit "L_push_args_exit")
				(label-move-rest-stack-loop "L_move_rest_stack_loop")
				(label-move-rest-stack-loop-exit "L_move_rest_stack_loop_exit"))

					(string-append
							(line-gen2 "jmp " label-make-apply-clos )
							(gen-line1 label-apply-body ":")


							;(line-gen2 "cmp arg_count, 2")
							;(line-gen2 "jne L_wrong_arity")
							(line-gen2 "pop r8")  ; r8 - saves the return address
							(line-gen2 "pop r9")	;disposse of apply env
							(line-gen2 "pop r9")  ;disposse of arg_count
							(gen-line1 "TEST_01:")
							(line-gen2 "cmp r9, 2")
							(line-gen2 "pop r9")  ; r9 - saves the proc
							(line-gen2 "pop r10") ; r10 - saves the list
							(line-gen2 "mov rdi,0")
							(line-gen2 "mov r11, r10")
							(line-gen2 "mov r12, r9")
							(line-gen2 "mov r12, qword [r12]")
							(line-gen2 "TYPE r12")
							(line-gen2 "cmp r12, T_CLOSURE")
							(line-gen2 "jne L_incorrect_type")
							(gen-line1 label-length-params-loop ":")
							(line-gen2 "cmp r11, Lcons2")
							(line-gen2 "je " label-length-params-loop-exit)
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "CDR r11")
							(line-gen2 "inc rdi")
							(line-gen2 "jmp " label-length-params-loop)
							(gen-line1 label-length-params-loop-exit ":")
							(line-gen2 "mov rsi, rdi")
							(gen-line1 label-push-args ":")
							(line-gen2 "cmp rsi, 0")
							(line-gen2 "je " label-push-args-exit)
							(line-gen2 "sub rsi, 1")
							(line-gen2 "mov rcx, rsi")
							(line-gen2 "mov r11, r10")
							(gen-line1 label-CDR-loop ":")
							(line-gen2 "cmp rcx, 0")
							(line-gen2 "je " label-CDR-loop-exit)
							(line-gen2 "mov r11, qword [r11]") 
							(line-gen2 "CDR r11")
							(line-gen2 "sub rcx, 1")
							(line-gen2 "jmp " label-CDR-loop)
							(gen-line1 label-CDR-loop-exit ":")
							(line-gen2 "mov r11, qword [r11]")
							(line-gen2 "CAR r11")
							(line-gen2 "push r11")
							(line-gen2 "jmp " label-push-args)
							(gen-line1 label-push-args-exit ":")
							(line-gen2 "push rdi")
							(line-gen2 "mov r9, qword [r9]")
							(line-gen2 "mov r12, r9")
							(line-gen2 "CLOSURE_ENV r12")
							(line-gen2 "push r12")
							(line-gen2 "push r8")
							(line-gen2 "CLOSURE_CODE r9")
							(line-gen2 "jmp r9")
							(gen-line1 label-make-apply-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-apply-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'apply (unbox global-table))) "] , rax" )))))











(define code-gen-vector
	(lambda ()
			(let ((label-vector-clos "L_vector_clos")
						(label-vector-body "L_vector_body")
						(label-vector-push-args-loop "L_vector_push_args_loop")
						(label-vector-push-args-exit "L_vector_push_args_loop_exit"))
					(string-append
							(line-gen2 "jmp " label-vector-clos )
							(gen-line1 label-vector-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "mov rbx, arg_count ; // n")
							(line-gen2 "mov rcx, 3*8 ; // index")
							
							(line-gen2 "mov rax, rbx ; // index")
							(line-gen2 "mov r15, 8 ; // index")
							(line-gen2 "mul r15 ; // index")
							(line-gen2 "add rcx, rax ; // index")
							
							(gen-line1 label-vector-push-args-loop ":")
							(line-gen2 "cmp rbx, 0 ")
							(line-gen2 "je " label-vector-push-args-exit )
							(line-gen2 "mov rdx, qword[rbp+rcx] ")
							(line-gen2 "push rdx ")
							(line-gen2 "sub rcx, 8 ")
							(line-gen2 "dec rbx ")
							(line-gen2 "jmp " label-vector-push-args-loop )
							
							
							(gen-line1 label-vector-push-args-exit ":")
							(line-gen2 "mov rbx, arg_count ")
							(line-gen2 "push rbx ")
							(line-gen2 "call MAKE_VECTOR")
							
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-vector-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-vector-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'vector (unbox global-table))) "] , rax" )

							
							
							))))

(define code-gen-make-vector
	(lambda ()
			(let ((label-make-vector-clos "L_make_vector_clos")
						(label-make-vector-body "L_make_vector_body")
						(label-make-vector-check-one-arg "L_make_vector_check_one_arg")
						(label-make-vector-take-n "L_make_vector_take_n")
						(label-make-vector-loop "L_make_vector_loop")
						(label-make-vector-loop-exit "L_make_vector_loop_exit"))
					(string-append
							(line-gen2 "jmp " label-make-vector-clos )
							(gen-line1 label-make-vector-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne make_vec_con" )
							(line-gen2 "mov rcx, Lcons7" )
							
							(line-gen2 "jmp make_vec_con2" )
							(line-gen2 "make_vec_con:" )
							(line-gen2 "mov rcx, A1 ; // obj")
							
							;(line-gen2 "cmp rcx, Lcons ; // obj")
							(line-gen2 "make_vec_con2:" )
							(line-gen2 "mov rbx, A0 ; // n")
							(line-gen2 "mov rbx, qword[rbx] ; // n")
							(line-gen2 "mov r10, rbx ; // n")
							(line-gen2 "TYPE rbx")
							
							(line-gen2 "cmp rbx, T_INTEGER ")
							(line-gen2 "JNE L_incorrect_type;")
							(line-gen2 "DATA r10 ")
							;r10 =n, rcx = Lconsx
							
							; loop
							(line-gen2 "push rcx ")
							(line-gen2 "push r10 ")
							(line-gen2 "call MAKE_SOB_VECTOR")
							
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-vector-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-make-vector-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'make-vector (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-make-string
	(lambda ()
			(let ((label-make-string-clos "L_make_string_clos")
						(label-make-string-body "L_make_string_body")
						(label-make-string-check-one-arg "L_make_string_check_one_arg")
						(label-make-string-take-n "L_make_string_take_n")
						(label-make-string-loop "L_make_string_loop")
						(label-make-string-loop-exit "L_make_string_loop_exit"))
					(string-append
							(line-gen2 "jmp " label-make-string-clos )
							(gen-line1 label-make-string-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count, 2")
							(line-gen2 "jne " label-make-string-check-one-arg)
							
							
							(line-gen2 "mov rax ,A1")
							(line-gen2 "mov rax ,qword [rax] ") 
							(line-gen2 "mov rbx ,rax")
							
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_CHAR")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "DATA rbx")
							
							
							(line-gen2 "jmp " label-make-string-take-n )
							
							(gen-line1 label-make-string-check-one-arg ":")
							(line-gen2 "cmp arg_count, 1")
							
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, 0 ; // ascii 0 - default value")
							
							(gen-line1 label-make-string-take-n ":")
							(line-gen2 "mov rdx, A0 ; // n")
							(line-gen2 "mov rdx, qword[rdx] ; // n")
							(line-gen2 "mov r10, rdx")
							(line-gen2 "TYPE rdx")
							(line-gen2 "cmp rdx, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "DATA r10")
							
							; loop
							(line-gen2 "mov r11, 0 ; // counter") ;
							(gen-line1 label-make-string-loop ":")
							(line-gen2 "cmp r10, r11")
							(line-gen2 "je " label-make-string-loop-exit )
							(line-gen2 "push rbx")
							(line-gen2 "inc r11 ")
							(line-gen2 "jmp " label-make-string-loop )
							(gen-line1 label-make-string-loop-exit ":")
							(line-gen2 "push r10")
							(line-gen2 "call MAKE_SOB_STRING")
							
							(line-gen2 "leave")
							(line-gen2 "ret;")
							
							(gen-line1 label-make-string-clos ":")
						  
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-make-string-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'make-string (unbox global-table))) "] , rax" )

							
							))))
					
(define code-gen-equal-bin
	(lambda ()
			(let (	  
						(label-make-equal-bin-clos "L_make_equal_bin_clos")
						(label-equal-bin-body "L_make_equal_bin_body")
						(label-take-first-fraction-parts "L_take_first_fraction_parts_equal")
						(label-check-second-fraction "L_check_second_fraction_equal")
						(label-take-second-fraction-parts "L_take_second_fraction_parts_equal")
						(label-equal-fraction "L_equal_fraction")
						(label-ans-is-true "L_ans_is_true_equal_bin")
						(label-end-equal-bin "L_end_equal_bin")
						)
					(string-append
						   
							(line-gen2 "jmp " label-make-equal-bin-clos)
							(gen-line1 label-equal-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rbx, A0") 
							(line-gen2 "mov rbx, qword[rbx]")
												
							
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov rax, rbx")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax,T_FRACTION")
							(line-gen2 "je " label-take-first-fraction-parts )
							
							(line-gen2 "cmp rax, T_INTEGER;")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx ")
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction )
							
							(gen-line1 label-take-first-fraction-parts ":")
							
							(line-gen2 "mov r10, rbx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rbx ")
							
							(line-gen2 "mov rdx , rbx ")
							(line-gen2 "mov rsi , r10")
							
							(gen-line1 label-check-second-fraction ":")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx, T_FRACTION ")
							(line-gen2 "je " label-take-second-fraction-parts )
							(line-gen2 "cmp rcx, T_INTEGER ")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							(line-gen2 "DATA rcx")
							
							(line-gen2 "mov r15, rcx")
							(line-gen2 "mov r14, 1 ") 
							(line-gen2 "jmp " label-equal-fraction )
							
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov r10, rcx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rcx ")
							
							
							(line-gen2 "mov r15, rcx ")
							(line-gen2 "mov r14, r10 ")
							
							(gen-line1 label-equal-fraction ":")
							; compute R11
							(line-gen2 "mov rax, rdx")
							
							(line-gen2 "mul r14")
							(line-gen2 "mov r11, rax")
							; compute R8
							(line-gen2 "mov rax, rsi")
							(line-gen2 "mul r15 ")
							(line-gen2 "mov r8, rax")
							
							; compare R11,R8
							(line-gen2 "cmp r11, r8 ")
							(line-gen2 "je " label-ans-is-true )
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-end-equal-bin)
							
							(gen-line1 label-ans-is-true ":")
							(line-gen2 "mov rax, Lcons5 ")
							
							(gen-line1 label-end-equal-bin ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-equal-bin-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-equal-bin-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'equal-bin (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-lt-bin
	(lambda ()
			(let (	  
						
						(label-make-lt-bin-clos "L_make_lt_bin_clos")
						(label-lt-bin-body "L_make_lt_bin_body")
						(label-take-first-fraction-parts "L_take_first_fraction_parts_lt")
						(label-check-second-fraction "L_check_second_fraction_lt")
						(label-take-second-fraction-parts "L_take_second_fraction_parts_lt")
						(label-lt-fraction "L_lt_fraction")
						(label-ans-is-true "L_ans_is_true_lt_bin")
						(label-end-lt-bin "L_end_lt_bin")
						)
					(string-append
							
							(line-gen2 "jmp " label-make-lt-bin-clos)
							(gen-line1 label-lt-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rbx, A0") 
							(line-gen2 "mov rbx, qword[rbx]")
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov rax, rbx")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax,T_FRACTION")
							(line-gen2 "je " label-take-first-fraction-parts )
							
							(line-gen2 "cmp rax, T_INTEGER;")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx ") 
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction )
							
							(gen-line1 label-take-first-fraction-parts ":")
							
							(line-gen2 "mov r10, rbx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rbx ")
							
							(line-gen2 "mov rdx , rbx ")
							(line-gen2 "mov rsi , r10")
							
							(gen-line1 label-check-second-fraction ":")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx, T_FRACTION ")
							(line-gen2 "je " label-take-second-fraction-parts )
							(line-gen2 "cmp rcx, T_INTEGER ")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							(line-gen2 "DATA rcx")
							
							(line-gen2 "mov r15, rcx")
							(line-gen2 "mov r14, 1 ") 
							(line-gen2 "jmp " label-lt-fraction )
							
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov r10, rcx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rcx ")
							
							
							(line-gen2 "mov r15, rcx ")
							(line-gen2 "mov r14, r10 ")
							
							(gen-line1 label-lt-fraction ":")
							; compute R11
							(line-gen2 "mov rax, rdx")
							
							(line-gen2 "mul r14")
							(line-gen2 "mov r11, rax")
							; compute R8
							(line-gen2 "mov rax, rsi")
							(line-gen2 "mul r15 ")
							(line-gen2 "mov r8, rax")
							
							; compare R11,R8
							(line-gen2 "cmp r11, r8 ")
							(line-gen2 "jl " label-ans-is-true )
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-end-lt-bin)
							
							(gen-line1 label-ans-is-true ":")
							(line-gen2 "mov rax, Lcons5 ")
							
							(gen-line1 label-end-lt-bin ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-lt-bin-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-lt-bin-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'lt-bin (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-gt-bin
	(lambda ()
			(let (	  (label-make-gt-bin-clos "L_make_gt_bin_clos")
						(label-gt-bin-body "L_make_gt_bin_body")
						(label-take-first-fraction-parts "L_take_first_fraction_parts_gt")
						(label-check-second-fraction "L_check_second_fraction_gt")
						(label-take-second-fraction-parts "L_take_second_fraction_parts_gt")
						(label-gt-fraction "L_gt_fraction")
						(label-ans-is-true "L_ans_is_true_gt_bin")
						(label-end-gt-bin "L_end_gt_bin"))
					(string-append
							
							(line-gen2 "jmp " label-make-gt-bin-clos)
							(gen-line1 label-gt-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rbx, A0") 
							(line-gen2 "mov rbx, qword[rbx]")
												
							
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov rax, rbx")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax,T_FRACTION")
							(line-gen2 "je " label-take-first-fraction-parts )
							
							(line-gen2 "cmp rax, T_INTEGER;")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx ")
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction )
							
							(gen-line1 label-take-first-fraction-parts ":")
							
							(line-gen2 "mov r10, rbx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rbx ")
							
							(line-gen2 "mov rdx , rbx ")
							(line-gen2 "mov rsi , r10")
							
							(gen-line1 label-check-second-fraction ":")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx, T_FRACTION ")
							(line-gen2 "je " label-take-second-fraction-parts )
							(line-gen2 "cmp rcx, T_INTEGER ")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							(line-gen2 "DATA rcx")
							
							(line-gen2 "mov r15, rcx")
							(line-gen2 "mov r14, 1 ") 
							(line-gen2 "jmp " label-gt-fraction )
							
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1") 
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov r10, rcx ")
							(line-gen2 "Fraction_LOWER r10 ")
							(line-gen2 "Fraction_UPPER rcx ")
							
							
							(line-gen2 "mov r15, rcx ")
							(line-gen2 "mov r14, r10 ")
							
							(gen-line1 label-gt-fraction ":")
							; compute r11
							(line-gen2 "mov rax, rdx")
							
							(line-gen2 "mul r14")
							(line-gen2 "mov r11, rax")
							; compute r8
							(line-gen2 "mov rax, rsi")
							(line-gen2 "mul r15 ")
							(line-gen2 "mov r8, rax")
							
							; compare r11,r8
							(line-gen2 "cmp r11, r8 ")
							(line-gen2 "jg " label-ans-is-true )
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-end-gt-bin)
							
							(gen-line1 label-ans-is-true ":")
							(line-gen2 "mov rax, Lcons5 ")
							
							(gen-line1 label-end-gt-bin ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-gt-bin-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-gt-bin-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'gt-bin (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-val-eq
	(lambda ()
			(let (	  
						
						(label-make-val-eq-clos "L_make_val_eq_clos")
						(label-val-eq-body "L_make_val_eq_body")
						(label-val-eq-exit "L_val_eq_exit")
						)
					(string-append
							(line-gen2 "jmp " label-make-val-eq-clos )
							(gen-line1 label-val-eq-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword[rbx]")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword[rcx]")
							
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "cmp rbx, rcx ")
							(line-gen2 "jne " label-val-eq-exit )
							(line-gen2 "mov rax, Lcons5 ")
							
							(gen-line1 label-val-eq-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-val-eq-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-val-eq-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'val-eq? (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-addr-eq
	(lambda ()
			(let (	  (label-make-addr-eq-clos "L_make_addr_eq_clos")
						(label-addr-eq-body "L_make_addr_eq_body")
						(label-addr-eq-exit "L_addr_eq_exit"))
					(string-append
							(line-gen2 "jmp " label-make-addr-eq-clos )
							(gen-line1 label-addr-eq-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rcx, A1")
							
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "cmp rbx, rcx ")
							(line-gen2 "jne " label-addr-eq-exit )
							(line-gen2 "mov rax, Lcons5")
							
							(gen-line1 label-addr-eq-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-addr-eq-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-addr-eq-body)
							(gen-newline)
						   ; (line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'addr-eq? (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-remainder
	(lambda ()
			(let (	  (label-make-remainder-clos "L_make_remainder_clos_")
						(label-remainder-body "L_make_remainder_body"))
					(string-append
							(line-gen2 "jmp " label-make-remainder-clos )
							(gen-line1 label-remainder-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
						
							(line-gen2 "mov rax ,A1")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rcx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							;rbx = arg0, rcx = arg1
							
							
							
							(line-gen2 "DATA rbx")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov rdx, 0")
							
							(line-gen2 "mov rax, rbx")
							(line-gen2 "cqo")
							(line-gen2 "idiv rcx")
							;rdx has the remainder
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rdx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rdx")
							(line-gen2 "mov qword[rax], rdx")
							(line-gen2 "shl qword[rax], 4 ")
							(line-gen2 "or qword[rax], T_INTEGER ")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-remainder-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-remainder-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'remainder (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-denominator
	(lambda ()
			(let (	  
						(label-make-denominator-clos "L_make_denominator_clos_")
						(label-denominator-body "L_make_denominator_body")
						(label-fraction-denominator "L_fraction_denominator")
						(label-denominator-exit "L_denominator_exit")
						)
					(string-append
							(line-gen2 "jmp " label-make-denominator-clos )
							(gen-line1 label-denominator-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_FRACTION")
							(line-gen2 "je " label-fraction-denominator )
							
							
							(line-gen2 "mov rax ,rbx")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							
							
							(line-gen2 "mov qword [rax], 1")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							(line-gen2 "jmp " label-denominator-exit )
							
							
							
							(gen-line1 label-fraction-denominator ":")
							(line-gen2 "Fraction_LOWER rbx")
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							
							
							(line-gen2 "mov qword [rax], rbx")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							
							(gen-line1 label-denominator-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-denominator-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-denominator-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'denominator (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-numerator
	(lambda ()
			(let (	  (label-make-numerator-clos "L_make_numerator_clos_")
						(label-numerator-body "L_make_numerator_body")
						(label-fraction-numerator "L_fraction_numerator")
						(label-numerator-exit "L_numerator_exit"))
					(string-append
							(line-gen2 "jmp " label-make-numerator-clos )
							(gen-line1 label-numerator-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_FRACTION")
							(line-gen2 "je " label-fraction-numerator )
							
							
							(line-gen2 "mov rax ,rbx")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "jmp " label-numerator-exit )
							
							
							
							(gen-line1 label-fraction-numerator ":")
							(line-gen2 "Fraction_UPPER rbx")
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							
							
							(line-gen2 "mov qword [rax], rbx")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							
							(gen-line1 label-numerator-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-numerator-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-numerator-body)
							(gen-newline)
						   ; (line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'numerator (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-vector-set!
	(lambda ()
			(let ((label-make-vector-set!-clos "L_make_vector_set_clos_")
						(label-vector-set!-body "L_make_vector_set_body"))
					(string-append
							(line-gen2 "jmp " label-make-vector-set!-clos )
							(gen-line1 label-vector-set!-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,3 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_VECTOR")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rax ,A1")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rcx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							
							(line-gen2 "DATA rcx")
							
							(line-gen2 "mov rax, rcx ")
							(line-gen2 "mov rsi, 8 ")
							
							(line-gen2 "mul rsi")
							
							(line-gen2 "mov rcx, rax ")
							(line-gen2 "VECTOR_ELEMENTS rbx")
							(line-gen2 "add rbx, rcx")
							(line-gen2 "mov rdx ,A2")
							(line-gen2 "and qword [rbx], 0x0000000000000000")
							(line-gen2 "or qword [rbx], rdx")
							(line-gen2 "mov rax, Lcons1") 
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-vector-set!-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-vector-set!-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'vector-set! (unbox global-table))) "] , rax" )
							))))

(define code-gen-vector-ref
	(lambda ()
			(let (	  (label-make-vector-ref-clos "L_make_vector_ref_clos_")
						(label-vector-ref-body "L_make_vector_ref_body"))
					(string-append
							(line-gen2 "jmp " label-make-vector-ref-clos)
							(gen-line1 label-vector-ref-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_VECTOR")
							(line-gen2 "jne L_incorrect_type")
							
							
							(line-gen2 "mov rcx ,A1")
							(line-gen2 "mov rcx ,qword [rcx]")
							(line-gen2 "mov rdx ,rcx")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							
							
							
							(line-gen2 "DATA rdx")
							
							(line-gen2 "mov rax, rdx ")
							(line-gen2 "mov rsi, 8 ")
							(line-gen2 "mul rsi")
							(line-gen2 "mov rdx, rax ")
							
							(line-gen2 "VECTOR_ELEMENTS rbx")
							
							(line-gen2 "add rbx, rdx")
							
							(line-gen2 "mov rax, rbx")
							(line-gen2 "mov rax, qword[rbx]")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-vector-ref-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-vector-ref-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]")
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'vector-ref (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-vector-length
	(lambda ()
			(let (	  (label-make-vector-length-clos "L_make_vector_length_clos_")
						(label-vector-length-body "L_make_vector_length_body"))
					(string-append
							(line-gen2 "jmp " label-make-vector-length-clos )
							(gen-line1 label-vector-length-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_VECTOR")
							(line-gen2 "jne L_incorrect_type")
							
							
							(line-gen2 "VECTOR_LENGTH rbx")
							
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							(line-gen2 "mov qword [rax], rbx")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-vector-length-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-vector-length-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'vector-length (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-string-set!
	(lambda ()
			(let (	  (label-make-string-set!-clos "L_make_string_set_clos_")
						(label-string-set!-body "L_make_string_set_body")
						)
					(string-append
							(line-gen2 "jmp " label-make-string-set!-clos )
							(gen-line1 label-string-set!-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,3 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_STRING")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rax ,A1")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rcx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							
						   
							
							(line-gen2 "mov rax ,A2")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rdx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_CHAR")
							(line-gen2 "jne L_incorrect_type")
							
							
						   
							
							(line-gen2 "STRING_ELEMENTS rbx")
							
							(line-gen2 "DATA rcx")
							;(line-gen2 "sub rcx,1")
							(line-gen2 "add rbx, rcx")
							
							(line-gen2 "and byte[rbx],0x0")
							(line-gen2 "or byte[rbx],dl")
							(line-gen2 "mov rax, Lcons1")
							
							
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-string-set!-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							;;; MAKE_LITERAL_CLOSURE target, env, code
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-string-set!-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'string-set! (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-string-ref
	(lambda ()
			(let ((label-make-string-ref-clos "L_make_string_ref_clos_")
						(label-string-ref-body "L_make_string_ref_body"))
					(string-append
							(line-gen2 "jmp " label-make-string-ref-clos)
							(gen-line1 label-string-ref-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_STRING")
							(line-gen2 "jne L_incorrect_type")
							
							
							(line-gen2 "mov rcx ,A1")
							(line-gen2 "mov rcx ,qword [rcx]")
							(line-gen2 "mov rdx ,rcx")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx, T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "STRING_ELEMENTS rbx")
							
							(line-gen2 "DATA rdx")
							
							(line-gen2 "add rbx, rdx")
							
							
							
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							
							(line-gen2 "call malloc")
							
							(line-gen2 "pop rbx")
							
							
							(line-gen2 "mov rbx, qword[rbx]")
							
							(line-gen2 "mov qword[rax], rbx")
							(line-gen2 "shl qword[rax], 4")
							(line-gen2 "or qword[rax], T_CHAR")
							
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-string-ref-clos ":")
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							;;; MAKE_LITERAL_CLOSURE target, env, code
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-string-ref-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'string-ref (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-string-length
	(lambda ()
			(let (	  (label-make-string-length-clos "L_make_string_length_clos_")
						(label-string-length-body "L_make_string_length_body")
						)
					(string-append
							(line-gen2 "jmp " label-make-string-length-clos )
							(gen-line1 label-string-length-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
						   
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rax")
							(line-gen2 "cmp rax, T_STRING")
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "STRING_LENGTH rbx")
							
							(line-gen2 "mov rdi, 8")
							(line-gen2 "push rbx")
							(line-gen2 "call malloc")
							(line-gen2 "pop rbx")
							(line-gen2 "mov qword[rax], rbx")
							(line-gen2 "shl qword[rax], 4")
							(line-gen2 "or qword[rax], T_INTEGER")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-string-length-clos ":")
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							;;; MAKE_LITERAL_CLOSURE target, env, code
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-string-length-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'string-length (unbox global-table))) "] , rax" )

							
							))))

(define code-gen-car
	(lambda ()
			(let (	  (label-make-car-clos "L_make_car_clos_")
						(label-car-body "L_make_car_body"))
				(string-append
							(line-gen2 "jmp " label-make-car-clos )
							(gen-line1 label-car-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx, T_PAIR")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "CAR rax")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-car-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							;;; MAKE_LITERAL_CLOSURE target, env, code
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-car-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'car (unbox global-table))) "] , rax" )
							
							))))
							
(define code-gen-cdr
	(lambda ()
			(let (	  (label-make-cdr-clos "L_make_cdr_clos_")
						(label-cdr-body "L_make_cdr_body"))
				(string-append
							(line-gen2 "jmp " label-make-cdr-clos )
							(gen-line1 label-cdr-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count ,1 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rax ,A0")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx, T_PAIR")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "CDR rax")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-cdr-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							;;; MAKE_LITERAL_CLOSURE target, env, code
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-cdr-body)
							(gen-newline)
							
						   ; (line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'cdr (unbox global-table))) "] , rax" )
							
							))))

(define code-gen-cons
	(lambda ()
			(let ((label-make-cons-clos "L_make_cons_clos")
						(label-cons-body "L_make_cons_body"))
					(string-append
							(line-gen2 "jmp " label-make-cons-clos )
							(gen-line1 label-cons-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							
							
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "mov rbx ,A0 ; get car")
							(line-gen2 "mov rcx ,A1 ; get cdr")
							(line-gen2 "MAKE_MALLOC_LITERAL_PAIR rax, rbx, rcx")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-cons-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-cons-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'cons (unbox global-table))) "] , rax" )))))

(define code-gen-plus-bin
	(lambda ()
			(let ((label-make-plus-bin-clos "L_make_plus_bin_clos")
						(label-plus-bin-body "L_make_plus_bin_body")
						(label-take-first-fraction-parts "L_take_first_fraction_parts_plus")
						(label-check-second-fraction "L_check_second_fraction_plus")
						(label-take-second-fraction-parts "L_take_second_fraction_parts_plus")
						(label-add-fraction "L_add_fraction")
						(label-result-is-integer "L_result_is_integer_plus")
						(label-plus-bin-end "L_plus_bin_end"))
					(string-append

							(line-gen2 "jmp " label-make-plus-bin-clos)
							(gen-line1 label-plus-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 2")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rbx, qword [rbx]") 
							(line-gen2 "mov rcx, qword [rcx]") 
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_FRACTION")
							
							(line-gen2 "je " label-take-first-fraction-parts)
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx") 
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction)
							
							(gen-line1 label-take-first-fraction-parts ":")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_UPPER rbx")
							(line-gen2 "mov rdx, rbx")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_LOWER rbx")
							(line-gen2 "mov rsi, rbx")
							(gen-line1 label-check-second-fraction ":")
							
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx ,T_FRACTION")
							(line-gen2 "je " label-take-second-fraction-parts)
							(line-gen2 "cmp rcx ,T_INTEGER")
							
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov r8, rcx") 
							(line-gen2 "mov r9, 1") 
							(line-gen2 "jmp " label-add-fraction)
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_UPPER rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_LOWER rcx")
							(line-gen2 "mov r9, rcx")
							
							(gen-line1 label-add-fraction ":")
							; compute denominators gcd
							(line-gen2 "mov r10, rsi") 
							(line-gen2 "mov rax, rsi")
							(line-gen2 "push rdx")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r11, rax") 
							(line-gen2 "mov rsi, r10")
							; compute numerator
							
							(line-gen2 "mov rax, rdx")
							(line-gen2 "push rdx")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov rdx, rax")
							
							
							(line-gen2 "mov rax, r8")
							(line-gen2 "push rdx")
							(line-gen2 "mul rsi")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r8, rax")
							
							(line-gen2 "add rdx, r8")
							(line-gen2 "mov r12, rdx") 
							
							(line-gen2 "push r11")
							(line-gen2 "push r12")
							(line-gen2 "call gcd")
							(line-gen2 "pop r12")
							(line-gen2 "pop r11")


							(line-gen2 "mov r14, r11")
							(line-gen2 "cmp r11,0")
							(line-gen2 "jg Positive_numerator_plus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")

							(gen-line1 "Positive_numerator_plus_bin:")
							(line-gen2 "push rdx")
							(line-gen2 "mov r15, rax")
							(line-gen2 "mov rax, r11")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "mov r11, rax")
							(line-gen2 "pop rdx")

							(line-gen2 "mov r14, 0")
							(line-gen2 "jg CheckSecond_plus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")


							(gen-line1 "CheckSecond_plus_bin:")

							(line-gen2 "mov r14, r12")
							(line-gen2 "cmp r12,0")
							(line-gen2 "jg Positive_denumerator_plus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")

							(gen-line1 "Positive_denumerator_plus_bin:")
							(line-gen2 "mov rax, r15")
							(line-gen2 "mov rax, r12")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r12, rax")

							(line-gen2 "mov r14, 0")
							(line-gen2 "jg continue_Plus_binary_plus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")

							(gen-line1 "continue_Plus_binary_plus_bin:")
							
							; check if denominator = 1 (integer)
							(line-gen2 "cmp r11, 1")
							(line-gen2 "je " label-result-is-integer )
							(line-gen2 "push r11") ; denominator 
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							(line-gen2 "pop r11") ; numerator
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 34")
							(line-gen2 "shl r11, 4")
							(line-gen2 "or qword [rax], r11")
							
							(line-gen2 "or qword [rax], T_FRACTION")
							
							(line-gen2 "jmp " label-plus-bin-end )
							(gen-line1 label-result-is-integer ":")
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							; end
							(gen-line1 label-plus-bin-end ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-plus-bin-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-plus-bin-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'plus-bin (unbox global-table))) "] , rax" )
							))))

(define code-gen-minus-bin
	(lambda ()
			(let ((label-make-plus-bin-clos "L_make_minus_bin_clos")
						(label-plus-bin-body "L_make_minus_bin_body")
						(label-take-first-fraction-parts "L_take_first_fraction_parts_minus")
						(label-check-second-fraction "L_check_second_fraction_minus")
						(label-take-second-fraction-parts "L_take_second_fraction_parts_minus")
						(label-add-fraction "L_sub_fraction")
						(label-result-is-integer "L_result_is_integer_minus")
						(label-plus-bin-end "L_minus_bin_end"))
					(string-append

							(line-gen2 "jmp " label-make-plus-bin-clos)
							(gen-line1 label-plus-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 2")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rbx, qword [rbx]") 
							(line-gen2 "mov rcx, qword [rcx]") 
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_FRACTION")
							
							(line-gen2 "je " label-take-first-fraction-parts)
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx") 
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction)
							
							(gen-line1 label-take-first-fraction-parts ":")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_UPPER rbx")
							(line-gen2 "mov rdx, rbx")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_LOWER rbx")
							(line-gen2 "mov rsi, rbx")
							(gen-line1 label-check-second-fraction ":")
							
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx ,T_FRACTION")
							(line-gen2 "je " label-take-second-fraction-parts)
							(line-gen2 "cmp rcx ,T_INTEGER")
							
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov r8, rcx") 
							(line-gen2 "mov r9, 1") 
							(line-gen2 "jmp " label-add-fraction)
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_UPPER rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_LOWER rcx")
							(line-gen2 "mov r9, rcx")
							
							(gen-line1 label-add-fraction ":")
							; compute denominators gcd
							(line-gen2 "mov r10, rsi")
							(line-gen2 "mov rax, rsi")
							(line-gen2 "push rdx")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r11, rax")
							(line-gen2 "mov rsi, r10")
							; compute numerator
							
							(line-gen2 "mov rax, rdx")
							(line-gen2 "push rdx")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov rdx, rax")
							
							
							(line-gen2 "mov rax, r8")
							(line-gen2 "push rdx")
							(line-gen2 "mul rsi")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r8, rax")
							
							(line-gen2 "sub rdx, r8")
							(line-gen2 "mov r12, rdx")
							; reducing. 
							(line-gen2 "push r11")
							(line-gen2 "push r12")
							
							(line-gen2 "call gcd") ; Rax <- gcd(r11, r12)
							(line-gen2 "pop r12")
							(line-gen2 "pop r11")
							
							(line-gen2 "mov r14, r11")
							(line-gen2 "cmp r11,0")
							(line-gen2 "jg Positive_numerator_minus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")

							(gen-line1 "Positive_numerator_minus_bin:")
							(line-gen2 "push rdx")
							(line-gen2 "mov r15, rax")
							(line-gen2 "mov rax, r11")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "mov r11, rax")
							(line-gen2 "pop rdx")

							(line-gen2 "mov r14, 0")
							(line-gen2 "jg CheckSecond_minus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")


							(gen-line1 "CheckSecond_minus_bin:")

							(line-gen2 "mov r14, r12")
							(line-gen2 "cmp r12,0")
							(line-gen2 "jg Positive_denumerator_minus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")

							(gen-line1 "Positive_denumerator_minus_bin:")
							(line-gen2 "mov rax, r15")
							(line-gen2 "mov rax, r12")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r12, rax")

							(line-gen2 "mov r14, 0")
							(line-gen2 "jg continue_minus_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")

							(gen-line1 "continue_minus_bin:")
							
							; check if denominator = 1 (integer)
							(line-gen2 "cmp r11, 1")
							(line-gen2 "je " label-result-is-integer )
							(line-gen2 "push r11") ; denominator 
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							(line-gen2 "pop r11") ; numerator
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 34")
							(line-gen2 "shl r11, 4")
							(line-gen2 "or qword [rax], r11")
							
							(line-gen2 "or qword [rax], T_FRACTION")
							
							(line-gen2 "jmp " label-plus-bin-end )
							(gen-line1 label-result-is-integer ":")
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							; end
							(gen-line1 label-plus-bin-end ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-plus-bin-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-plus-bin-body)
							(gen-newline)
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'minus-bin (unbox global-table))) "] , rax" )
							))))
							
(define code-gen-set-car!
	(lambda ()
			(let ((label-make-set-car!-clos "L_make_set_car_clos")
						(label-set-car!-body "L_make_set_car_body")
						(label-set-car!-exit "L_set_car_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-set-car!-clos)
							(gen-line1 label-set-car!-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax, A0 ; lst")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx, T_PAIR")
							
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rcx, A1 ; param")
							(line-gen2 "mov rcx, qword [rcx]")
							
							(line-gen2 "CAR rax")
							(line-gen2 "mov qword [rax], rcx")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-set-car!-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-set-car!-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'set-car! (unbox global-table))) "] , rax" )))))
							
(define code-gen-set-cdr!
	(lambda ()
			(let (	  (label-make-set-cdr!-clos "L_make_set_cdr_clos")
						(label-set-cdr!-body "L_make_set_cdr_body")
						(label-set-cdr!-exit "L_set_cdr_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-set-cdr!-clos)
							(gen-line1 label-set-cdr!-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count ,2 ;get argcount")
							(line-gen2 "jne L_wrong_arity")
							
							(line-gen2 "mov rax, A0 ; lst")
							(line-gen2 "mov rax ,qword [rax]")
							(line-gen2 "mov rbx ,rax")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx, T_PAIR")
							
							(line-gen2 "jne L_incorrect_type")
							
							(line-gen2 "mov rcx, A1 ; param")
							(line-gen2 "mov rcx, qword [rcx]")
							
							(line-gen2 "CDR rax")
							(line-gen2 "mov qword [rax], rcx")
							
							(line-gen2 "leave")
							(line-gen2 "ret")
							
							(gen-line1 label-make-set-cdr!-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-set-cdr!-body)
							(gen-newline)
							
							;(line-gen2 "mov  rax , qword [rax]" )
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'set-cdr! (unbox global-table))) "] , rax" )))))
							
(define code-gen-mul-bin
	(lambda ()
			(let	((label-make-mul-bin-clos "L_make_mul_bin_clos")
					(label-mul-bin-body "L_make_mul_bin_body")
					(label-take-first-fraction-parts "L_take_first_fraction_parts_mul")
					(label-check-second-fraction "L_check_second_fraction_mul")
					(label-take-second-fraction-parts "L_take_second_fraction_parts_mul")
					(label-mul-fraction "L_mul_fraction")
					(label-result-is-integer "L_result_is_integer_mul")
					(label-mul-bin-end "L_mul_bin_end"))
						(string-append
							(line-gen2 "jmp " label-make-mul-bin-clos)
							(gen-line1 label-mul-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 2")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rbx, qword [rbx]") 
							(line-gen2 "mov rcx, qword [rcx]") 
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_FRACTION")
							(line-gen2 "je " label-take-first-fraction-parts)
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx") 
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction)
							(gen-line1 label-take-first-fraction-parts ":")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_UPPER rbx")
							(line-gen2 "mov rdx, rbx")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_LOWER rbx")
							(line-gen2 "mov rsi, rbx")
							(gen-line1 label-check-second-fraction ":")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx ,T_FRACTION")
							(line-gen2 "je " label-take-second-fraction-parts)
							(line-gen2 "cmp rcx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov r9, 1") 
							(line-gen2 "jmp " label-mul-fraction)
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_UPPER rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_LOWER rcx")
							(line-gen2 "mov r9, rcx")
							(gen-line1 label-mul-fraction ":")
							(line-gen2 "mov r10, rdx")
							(line-gen2 "mov rax, r8")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "mul r10")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r12, rax") ;r12 = mul numerators
							; compute numerator
							(line-gen2 "mov rax, rsi")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r11, rax") ;r11 = mul denominator
							; reducing.
							(line-gen2 "push r11")
							(line-gen2 "push r12")
							(line-gen2 "call gcd") ; Rax <- gcd(r11, r12)
							(line-gen2 "pop r12")
							(line-gen2 "pop r11")
							(line-gen2 "mov r14, r11")
							(line-gen2 "cmp r11,0")
							(line-gen2 "jg Positive_numerator_mul_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")
							(gen-line1 "Positive_numerator_mul_bin:")
							(line-gen2 "push rdx")
							(line-gen2 "mov r15, rax")
							(line-gen2 "mov rax, r11")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "mov r11, rax")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r14, 0")
							(line-gen2 "jg CheckSecond_mul_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r11")
							(line-gen2 "mov r11, rax")
							(gen-line1 "CheckSecond_mul_bin:")
							(line-gen2 "mov r14, r12")
							(line-gen2 "cmp r12,0")
							(line-gen2 "jg Positive_denumerator_mul_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")
							(gen-line1 "Positive_denumerator_mul_bin:")
							(line-gen2 "mov rax, r15")
							(line-gen2 "mov rax, r12")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "div r15")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r12, rax")
							(line-gen2 "mov r14, 0")
							(line-gen2 "jg continue_mul_bin")
							(line-gen2 "mov rax, -1")
							(line-gen2 "mul r12")
							(line-gen2 "mov r12, rax")
							(gen-line1 "continue_mul_bin:")
							; check if denominator = 1 (integer)
							(line-gen2 "cmp r11, 1")
							(line-gen2 "je " label-result-is-integer )
							(line-gen2 "push r11") ; denominator 
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							(line-gen2 "pop r11") ; numerator
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 34")
							(line-gen2 "shl r11, 4")
							(line-gen2 "or qword [rax], r11")
							(line-gen2 "or qword [rax], T_FRACTION")
							(line-gen2 "jmp " label-mul-bin-end )
							(gen-line1 label-result-is-integer ":")
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							; end
							(gen-line1 label-mul-bin-end ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-mul-bin-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-mul-bin-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'mul-bin (unbox global-table))) "] , rax" )
							))))

(define code-gen-div-bin
	(lambda ()
			(let 	((label-make-div-bin-clos "L_make_div_bin_clos")
					(label-div-bin-body "L_make_div_bin_body")
					(label-take-first-fraction-parts "L_take_first_fraction_parts_div")
					(label-check-second-fraction "L_check_second_fraction_div")
					(label-take-second-fraction-parts "L_take_second_fraction_parts_div")
					(label-div-fraction "L_div_fraction")
					(label-result-is-integer "L_result_is_integer_div")
					(label-div-bin-end "L_div_bin_end"))
						(string-append
							; rdx & rsi - first number parts
							; r8 & r9 - second number parts
							(line-gen2 "jmp " label-make-div-bin-clos)
							(gen-line1 label-div-bin-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 2")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rbx, qword [rbx]") 
							(line-gen2 "mov rcx, qword [rcx]") 
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_FRACTION")
							(line-gen2 "je " label-take-first-fraction-parts)
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "DATA rbx")
							(line-gen2 "mov rdx, rbx")
							(line-gen2 "mov rsi, 1") 
							(line-gen2 "jmp " label-check-second-fraction)
							(gen-line1 label-take-first-fraction-parts ":")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_UPPER rbx")
							(line-gen2 "mov rdx, rbx")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "Fraction_LOWER rbx")
							(line-gen2 "mov rsi, rbx")
							(gen-line1 label-check-second-fraction ":")
							(line-gen2 "TYPE rcx")
							(line-gen2 "cmp rcx ,T_FRACTION")
							(line-gen2 "je " label-take-second-fraction-parts)
							(line-gen2 "cmp rcx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov r9, 1") 
							(line-gen2 "jmp " label-div-fraction)
							(gen-line1 label-take-second-fraction-parts ":")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_UPPER rcx")
							(line-gen2 "mov r8, rcx")
							(line-gen2 "mov rcx, A1")
							(line-gen2 "mov rcx, qword [rcx]")
							(line-gen2 "Fraction_LOWER rcx")
							(line-gen2 "mov r9, rcx")
							(gen-line1 label-div-fraction ":")
							; compute denominators gcd
							(line-gen2 "cmp rdx, 0")
							(line-gen2 "je L_division_by_zero")
							(line-gen2 "mov rax, r8")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "mul rsi")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r12, rax") ;r12 = div answer denominator
							; compute numerator
							(line-gen2 "mov rax, rdx")
							(line-gen2 "push rdx")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "mul r9")
							(line-gen2 "pop rdx")
							(line-gen2 "mov r11, rax") ;r11 = div answer numirator
							;reducing. 
							(line-gen2 "push r11")
							(line-gen2 "push r12")
							(line-gen2 "call gcd") ; Rax <- gcd(r11, r12)
							(line-gen2 "mov r15, rax")   ;Common divider
							(line-gen2 "pop r12")
							(line-gen2 "pop r11")
							(line-gen2 "cmp r12,0")
							(line-gen2 "jg L_div_sign_fix")
							(line-gen2 "neg r11")
							(line-gen2 "neg r12")
							; (line-gen2 "cqo")
							; (line-gen2 "imul r11")
							; (line-gen2 "mov r11, rax")
							; (line-gen2 "mov rax, -1")
							; (line-gen2 "cqo")
							; (line-gen2 "imul r12")
							; (line-gen2 "mov r12, rax")


							(gen-line1 "L_div_sign_fix:")
							(line-gen2 "mov rax, r11")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "cqo")
							(line-gen2 "idiv r15")
							(line-gen2 "mov r11, rax")
							(line-gen2 "mov rax, r12")
							(line-gen2 "mov rdx, 0")
							(line-gen2 "cqo")
							(line-gen2 "idiv r15")
							(line-gen2 "mov r12, rax")
							(gen-line1 "continue_div_bin:")
							(line-gen2 "xchg r12, r11")
							; check if denominator = 1 (integer)
							(line-gen2 "cmp r11, 1")
							(line-gen2 "je " label-result-is-integer )
							(line-gen2 "push r11") ; denominator 
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; numerator
							(line-gen2 "pop r11") ; denominator 
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 34")
							(line-gen2 "shl r11, 4")
							(line-gen2 "or qword [rax], r11")
							(line-gen2 "or qword [rax], T_FRACTION")
							(line-gen2 "jmp " label-div-bin-end )
							(gen-line1 label-result-is-integer ":")
							(line-gen2 "push r12") ; numerator
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop r12") ; denominator 
							(line-gen2 "mov qword [rax] , r12")
							(line-gen2 "shl qword [rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							; end
							(gen-line1 label-div-bin-end ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-div-bin-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-div-bin-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'div-bin (unbox global-table))) "] , rax" )))))

(define code-gen-boolean?
	(lambda ()
			(let	((label-make-boolean?-clos "L_make_is_boolean_clos")
					(label-boolean?-body "L_make_is_boolean_body")
					(label-boolean?-exit "L_is_boolean_exit")
					(label-boolean?-answer-is-true "L_is_boolean_ans_is_true")) 
						(string-append
							(line-gen2 "jmp " label-make-boolean?-clos )
							(gen-line1 label-boolean?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_BOOL")
							(line-gen2 "je " label-boolean?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-boolean?-exit )
							(gen-line1 label-boolean?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-boolean?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-boolean?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-boolean?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'boolean? (unbox global-table))) "] , rax" )
							))))

(define code-gen-char?
	(lambda ()
			(let 	((label-char?-answer-is-true "L_is_char_ans_is_true")
					(label-make-char?-clos "L_make_is_char_clos")
					(label-char?-body "L_make_is_char_body")
					(label-char?-exit "L_is_char_exit")) 
						(string-append
							(line-gen2 "jmp " label-make-char?-clos )
							(gen-line1 label-char?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_CHAR")
							(line-gen2 "je " label-char?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-char?-exit )
							(gen-line1 label-char?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-char?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-char?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-char?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'char? (unbox global-table))) "] , rax" )
							))))

(define code-gen-integer?
	(lambda ()
			(let 	((label-integer?-answer-is-true "L_is_integer_ans_is_true")	
					(label-make-integer?-clos "L_make_is_integer_clos")
					(label-integer?-body "L_make_is_integer_body")
					(label-integer?-exit "L_is_integer_exit")) 
						(string-append
							(line-gen2 "jmp " label-make-integer?-clos )
							(gen-line1 label-integer?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "je " label-integer?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-integer?-exit )
							(gen-line1 label-integer?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-integer?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-integer?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-integer?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'integer? (unbox global-table))) "] , rax" )
							))))

(define code-gen-pair?
	(lambda ()
			(let 	((label-pair?-answer-is-true "L_is_pair_ans_is_true")
					(label-make-pair?-clos "L_make_is_pair_clos")
					(label-pair?-body "L_make_is_pair_body")
					(label-pair?-exit "L_is_pair_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-pair?-clos )
							(gen-line1 label-pair?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_PAIR")
							(line-gen2 "je " label-pair?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-pair?-exit )
							(gen-line1 label-pair?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-pair?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-pair?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-pair?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'pair? (unbox global-table))) "] , rax" )
							
							
							
							))))

(define code-gen-procedure?
	(lambda ()
			(let 	((label-procedure?-answer-is-true "L_is_procedure_ans_is_true")
					(label-make-procedure?-clos "L_make_is_procedure_clos")
					(label-procedure?-body "L_make_is_procedure_body")
					(label-procedure?-exit "L_is_procedure_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-procedure?-clos )
							(gen-line1 label-procedure?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")						
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_CLOSURE")
							(line-gen2 "je " label-procedure?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-procedure?-exit )
							(gen-line1 label-procedure?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-procedure?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-procedure?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-procedure?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'procedure? (unbox global-table))) "] , rax" )
							))))

(define code-gen-string?
	(lambda ()
				(let 	((label-string?-answer-is-true "L_is_string_ans_is_true")
						(label-make-string?-clos "L_make_is_string_clos")
						(label-string?-body "L_make_is_string_body")
						(label-string?-exit "L_is_string_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-string?-clos )
							(gen-line1 label-string?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_STRING")
							(line-gen2 "je " label-string?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-string?-exit )
							(gen-line1 label-string?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-string?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-string?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-string?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'string? (unbox global-table))) "] , rax" )
							))))

(define code-gen-symbol?
	(lambda ()
			(let 	((label-symbol?-answer-is-true "L_is_symbol_ans_is_true")
					(label-make-symbol?-clos "L_make_is_symbol_clos")
					(label-symbol?-body "L_make_is_symbol_body")
					(label-symbol?-exit "L_is_symbol_exit")) 
						(string-append
							(line-gen2 "jmp " label-make-symbol?-clos )
							(gen-line1 label-symbol?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_SYMBOL")
							(line-gen2 "je " label-symbol?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-symbol?-exit )
							(gen-line1 label-symbol?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-symbol?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-symbol?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-symbol?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'symbol? (unbox global-table))) "] , rax" )
							))))

(define code-gen-vector?
	(lambda ()
			(let 	((label-vector?-answer-is-true "L_is_vector_ans_is_true")
					(label-make-vector?-clos "L_make_is_vector_clos")
					(label-vector?-body "L_make_is_vector_body")
					(label-vector?-exit "L_is_vector_exit")) 
						(string-append
							(line-gen2 "jmp " label-make-vector?-clos )
							(gen-line1 label-vector?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_VECTOR")
							(line-gen2 "je " label-vector?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-vector?-exit )
							(gen-line1 label-vector?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-vector?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-vector?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-vector?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'vector? (unbox global-table))) "] , rax" )
							))))

(define code-gen-fraction?
	(lambda ()
			(let 	((label-fraction?-answer-is-true "L_is_fraction_ans_is_true")
					(label-make-fraction?-clos "L_make_is_fraction_clos")
					(label-fraction?-body "L_make_is_fraction_body")
					(label-fraction?-exit "L_is_fraction_exit")) 
					(string-append
							(line-gen2 "jmp " label-make-fraction?-clos )
							(gen-line1 label-fraction?-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_FRACTION")
							(line-gen2 "je " label-fraction?-answer-is-true)
							(line-gen2 "mov rax, Lcons3")
							(line-gen2 "jmp " label-fraction?-exit )
							(gen-line1 label-fraction?-answer-is-true ":")
							(line-gen2 "mov rax, Lcons5")
							(gen-line1 label-fraction?-exit ":")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-fraction?-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-fraction?-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'fraction? (unbox global-table))) "] , rax" )
							))))

(define code-gen-char->integer
	(lambda ()
			(let 	((label-char->integer-clos "L_char_to_integer_clos")
					(label-char->integer-body "L_char_to_integer_body"))
						(string-append
							(line-gen2 "jmp " label-char->integer-clos )
							(gen-line1 label-char->integer-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "mov rcx, rbx")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_CHAR")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "push rcx")
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop rcx")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov qword[rax], rcx")
							(line-gen2 "shl qword[rax], 4")
							(line-gen2 "or qword [rax], T_INTEGER")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-char->integer-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-char->integer-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'char->integer (unbox global-table))) "] , rax" )
							))))

(define code-gen-integer->char
	(lambda ()
			(let 	((label-integer->char-clos "L_integer_to_char_clos")
					(label-integer->char-body "L_integer_to_char_body"))
						(string-append
							(line-gen2 "jmp " label-integer->char-clos )
							(gen-line1 label-integer->char-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword [rbx]")
							(line-gen2 "mov rcx, rbx")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx ,T_INTEGER")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "push rcx")
							(line-gen2 "mov rdi, 8")
							(line-gen2 "call malloc")
							(line-gen2 "pop rcx")
							(line-gen2 "DATA rcx")
							(line-gen2 "mov qword[rax], rcx")
							(line-gen2 "shl qword[rax], 4")
							(line-gen2 "or qword [rax], T_CHAR")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-integer->char-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-integer->char-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'integer->char (unbox global-table))) "] , rax" )
							))))

(define code-gen-symbol->string
	(lambda ()
			(let ((label-make-symbol->string-clos "L_make_symbol_to_string_clos_")
				 (label-symbol->string-body "L_make_symbol_to_string_body"))
					(string-append
							(line-gen2 "jmp " label-make-symbol->string-clos )
							(gen-line1 label-symbol->string-body ":")
							(line-gen2 "push rbp")
							(line-gen2 "mov rbp, rsp")
							(line-gen2 "cmp arg_count, 1")
							(line-gen2 "jne L_wrong_arity")
							(line-gen2 "mov rbx, A0")
							(line-gen2 "mov rbx, qword[rbx]")
							(line-gen2 "mov rcx, rbx")
							(line-gen2 "TYPE rbx")
							(line-gen2 "cmp rbx, T_SYMBOL")
							(line-gen2 "jne L_incorrect_type")
							(line-gen2 "DATA rcx")
							(line-gen2 "add rcx, start_of_data")
							(line-gen2 "mov rax, rcx")
							(line-gen2 "leave")
							(line-gen2 "ret")
							(gen-line1 label-make-symbol->string-clos ":")
							(line-gen2 "mov rdi, 16")
							(line-gen2 "call malloc")
							(line-gen2 "MAKE_LITERAL_CLOSURE rax, start_of_data, " label-symbol->string-body)
							(gen-newline)
							(line-gen2 "mov qword [Lprim" (number->string (lookup-address 'symbol->string (unbox global-table))) "] , rax" )
							))))