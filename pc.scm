;;; pc.scm
;;; A simple implementation of parsing combinators
;;;
;;; Programmer: Mayer Goldberg, 2017

(define with (lambda (s f) (apply f s)))

(define *marker-length* 8)

(define <end-of-input>
  (lambda (s ret-match ret-none)
    (if (null? s)
	(ret-match '() '())
	(ret-none '()))))

(define const
  (lambda (pred?)
    (lambda (s ret-match ret-none)
      (cond ((null? s) (ret-none '()))
	    ((pred? (car s)) (ret-match (car s) (cdr s)))
	    (else (ret-none '()))))))

(define <epsilon>
  (lambda (s ret-match ret-none)
    (ret-match '() s)))

(define <fail>
  (lambda (s ret-match ret-none)
    (ret-none '())))

(define caten
  (let ((binary-caten
	 (lambda (<p1> <p2>)
	   (lambda (s ret-match ret-none)
	     (<p1> s
		 (lambda (e1 s)
		   (<p2> s
		       (lambda (e2 s)
			 (ret-match (cons e1 e2) s))
		       ret-none))
		 ret-none)))))
    (lambda ps
      (fold-right binary-caten <epsilon> ps))))

(define disj
  (let ((binary-disj
	 (lambda (<p1> <p2>)
	   (lambda (s ret-match ret-none)
	     (<p1> s
		 ret-match
		 (lambda (w)
		   (<p2> s
		       ret-match
		       ret-none)))))))
    (lambda ps
      (fold-right binary-disj <fail> ps))))

(define delayed
  (lambda (thunk)
    (lambda (s ret-match ret-none)
      ((thunk) s ret-match ret-none))))

(define star
  (lambda (p)
    (disj (pack-with (caten p (delayed (lambda () (star p))))
		     cons)
	  <epsilon>)))

(define plus
  (lambda (p)
    (pack-with (caten p (star p))
	       cons)))

(define times
  (lambda (<p> n)
    (if (zero? n)
	<epsilon>
	(pack-with
	 (caten <p> (times <p> (- n 1)))
	 cons))))

(define at-least
  (lambda (<p> n)
    (new (*parser <p>)
	 (*times n)
	 (*parser <p>)
	 *star
	 (*caten 2)
	 (*pack-with
	  (lambda (e^4 e^*)
	    `(,@e^4 ,@e^*)))
	 done)))

(define pack
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (f e) s)) ret-none))))

(define pack-with
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (apply f e) s)) ret-none))))

(define diff
  (lambda (<p1> <p2>)
    (lambda (s ret-match ret-none)
      (<p1> s
	  (lambda (e w)
	    (<p2> s (lambda _ (ret-none '()))
		(lambda (w1) (ret-match e w))))
	  ret-none))))

(define maybe
  (lambda (p)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s) (ret-match `(#t ,e) s))
	 (lambda (w) (ret-match `(#f #f) s))))))

(define only-if
  (lambda (p pred?)
    (lambda (s ret-match ret-none)
      (p s
	 (lambda (e s)
	   (if (pred? e)
	       (ret-match e s)
	       (ret-none '())))
	 ret-none))))

(define otherwise
  (lambda (p message)
    (lambda (s ret-match ret-none)
      (p s
	 ret-match
	 (let ((marker
		(format "-->[~a]"
		  (list->string
		   (list-head s *marker-length*)))))
	   (lambda (w) (ret-none `(,@w ,message ,marker))))))))

;;;

(define ^char
  (lambda (char=?)
    (lambda (character)
      (const
       (lambda (ch)
	 (char=? ch character))))))

(define char (^char char=?))

(define char-ci (^char char-ci=?))

(define ^word
  (lambda (char)
    (lambda (word)
      (apply caten (map char (string->list word))))))

(define word (^word char))

(define word-ci (^word char-ci))

(define ^word-suffixes
  (lambda (char)
    (letrec ((loop
	      (lambda (s)
		(if (null? s)
		    <epsilon>
		    (maybe
		     (caten (char (car s))
			    (loop (cdr s))))))))
      (lambda (suffix)
	(loop (string->list suffix))))))

(define word-suffixes (^word-suffixes char))

(define word-suffixes-ci (^word-suffixes char-ci))

(define ^word+suffixes
  (lambda (word-suffixes)
    (lambda (prefix suffix)
      (caten (word prefix)
	     (word-suffixes suffix)))))

(define word+suffixes (^word+suffixes word-suffixes))

(define word+suffixes-ci (^word+suffixes word-suffixes-ci))

(define ^one-of
  (lambda (char)
    (lambda (word)
      (apply disj (map char (string->list word))))))

(define one-of (^one-of char))

(define one-of-ci (^one-of char-ci))

(define ^range
  (lambda (char<=?)
    (lambda (char-from char-to)
      (const
       (lambda (ch)
	 (and (char<=? char-from ch)
	      (char<=? ch char-to)))))))

(define range (^range char<=?))

(define range-ci (^range char-ci<=?))

(define <any-char> (const (lambda (ch) #t)))

(define <any> <any-char>)

;;; <expr> {<sep> <expr>}*
(define ^<separated-exprs>
  (lambda (<expr> <sep>)
    (new (*parser <expr>)
	 
	 (*parser <sep>)
	 (*parser <expr>)
	 (*caten 2)
	 (*pack-with (lambda (_sep expr) expr))
	 *star
	 
	 (*caten 2)
	 (*pack-with cons)
	 done)))

;;;

(define continue
  (lambda (ds cs)
    (with cs
      (lambda (c . cs)
	(c ds cs)))))

(define new
  (lambda cs
    (continue '() cs)))

(define done
  (lambda (ds cs)
    (with ds
      (lambda (parser . ds)
	(if (null? ds)
	    parser
	    (error 'done
		   (format "The parser stack still contains ~a parsers!"
		     (length ds))))))))

(define *parser
  (lambda (p)
    (lambda (ds cs)
      (continue `(,p . ,ds) cs))))

(define unary
  (lambda (f-unary)
    (lambda (ds cs)
      (with ds
	(lambda (d . ds)
	  (continue `(,(f-unary d) . ,ds) cs))))))

(define *delayed
  (lambda (thunk)
    (lambda (ds cs)
      (continue `(,(delayed thunk) . ,ds) cs))))

(define binary
  (lambda (f-binary)
    (lambda (ds cs)
      (with ds
	(lambda (d2 d1 . ds)
	  (continue `(,(f-binary d1 d2) . ,ds) cs))))))

(define *dup
  (lambda (ds cs)
    (with ds
      (lambda (d1 . ds)
	(continue `(,d1 ,d1 . ,ds) cs)))))

(define *swap
  (lambda (ds cs)
    (with ds
      (lambda (d1 d2 . ds)
	(continue `(,d2 ,d1 . ,ds) cs)))))

(define *star (unary star))

(define *plus (unary plus))

(define *diff (binary diff))

(define *pack (lambda (f) (unary (lambda (p) (pack p f)))))

(define *pack-with (lambda (f) (unary (lambda (p) (pack-with p f)))))

(define *only-if (lambda (pred?) (unary (lambda (p) (only-if p pred?)))))

(define split-list
  (lambda (s n ret-s1+s2)
    (if (zero? n)
	(ret-s1+s2 '() s)
	(split-list (cdr s) (- n 1)
		    (lambda (s1 s2)
		      (ret-s1+s2 (cons (car s) s1) s2))))))

(define nary
  (lambda (f-n-ary n)
    (lambda (ds cs)
      (split-list ds n
       (lambda (sgra ds)
	 (continue
	  `(,(apply f-n-ary (reverse sgra)) . ,ds) cs))))))

(define *caten (lambda (n) (nary caten n)))

(define *disj (lambda (n) (nary disj n)))

(define *maybe (unary maybe))

(define *otherwise
  (lambda (string)
    (unary
     (lambda (p)
       (otherwise p string)))))

(define *times
  (lambda (n)
    (unary
     (lambda (<p>)
       (times <p> n)))))

(define *at-least
  (lambda (n)
    (unary
     (lambda (<p>)
       (at-least <p> n)))))

(define followed-by
  (lambda (<p1> <p2>)
    (lambda (s ret-match+rest ret-none)
      (<p1> s
	    (lambda (e s)
	      (<p2> s
		    (lambda (_e _s)
		      (ret-match+rest e s))
		    ret-none))
	    ret-none))))

(define not-followed-by
  (lambda (<p1> <p2>)
    (new (*parser <p1>)
	 (*parser <p2>) *maybe
	 (*caten 2)
	 (*pack-with
	  (lambda (e1 ?e2)
	    (with ?e2
	      (lambda (found-e2? _)
		`(,e1 ,found-e2?)))))
	 (*only-if
	  (lambda (e1+found-e2?)
	    (with e1+found-e2?
	      (lambda (_ found-e2?)
		(not found-e2?)))))
	 (*pack-with
	  (lambda (e1 _) e1))
	 done)))

(define *followed-by (binary followed-by))

(define *not-followed-by (binary not-followed-by))

(define *transformer
  (lambda (^<p>)
    (unary (lambda (<p>) (^<p> <p>)))))

;;; 

(define test-string
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s)
	      `((match ,e)
		(remaining ,(list->string s))))
	    (lambda (w) `(failed with report: ,@w)))))

(define test
  (lambda (parser s)
    (parser s
	    (lambda (e s)
	      `((match ,e)
		(remaining ,s)))
	    (lambda (w) `(failed with report: ,@w)))))

;;;

(define file->string
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
		(lambda ()
		  (let ((e (read-char input)))
		    (if (eof-object? e)
			(begin
			  (close-input-port input)
			  '())
			(cons e (run)))))))
	(list->string (run))))))

(define read-stdin-to
  (lambda (end-of-input)
    (let ((end-of-input-list (string->list end-of-input)))
      (letrec ((state-init
		(lambda (seen)
		  (let ((ch (read-char)))
		    (cond ((eof-object? ch)
			   (error 'read-stdin-to
			     (format "Marker ~a not reached"
			       end-of-input)))
			  ((char=? ch (car end-of-input-list))
			   (state-seen seen `(,ch) (cdr end-of-input-list)))
			  (else (state-init `(,ch ,@seen)))))))
	       (state-seen
		(lambda (seen-before seen-now end-of-input-list-rest)
		  (if (null? end-of-input-list-rest)
		      (list->string
		       (reverse seen-before))
		      (let ((ch (read-char)))
			(cond ((eof-object? ch)
			       (format "Marker ~a not reached"
				 end-of-input))
			      ((char=? ch (car end-of-input-list-rest))
			       (state-seen seen-before
					   `(,ch ,@seen-now)
					   (cdr end-of-input-list-rest)))
			      (else (state-init
				     `(,ch ,@seen-now ,@seen-before)))))))))
	(state-init '())))))

;;; end-of-input
