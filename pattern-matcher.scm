;;; pattern-matcher.scm
;;; The pattern-matching package
;;;
;;; Programmer: Mayer Goldberg, 2016

(define match
  (letrec ((match
	    (lambda (pat e ret-dict ret-no-match)
	      (cond ((and (pair? pat) (pair? e))
		     (match (car pat) (car e)
			    (lambda (car-dict)
			      (match (cdr pat) (cdr e)
				     (lambda (cdr-dict)
				       (ret-dict
					(append car-dict cdr-dict)))
				     ret-no-match))
			    ret-no-match))
		    ((and (vector? pat) (vector? e)
			  (= (vector-length pat)
			     (vector-length e)))
		     (match
		      (vector->list pat)
		      (vector->list e)
		      ret-dict
		      ret-no-match))
		    ;; match with unification
		    ((procedure? pat)
		     (if (pat e)
			 (ret-dict `(,e))
			 (ret-no-match)))
		    ((or (and (char? pat) (char? e) (char=? pat e))
			 (and (string? pat)
			      (string? e)
			      (string=? pat e))
			 (and (symbol? pat) (symbol? e) (eq? pat e))
			 (and (number? pat) (number? e) (= pat e))
			 (eq? pat e))
		     (ret-dict '()))
		    (else (ret-no-match))))))
    (lambda (pat e ret-match ret-no-match)
      (match pat e
	     (lambda (dict) (apply ret-match dict))
	     ret-no-match))))

(define ?
  (lambda (name . guards)
    (lambda (e)
      (andmap
       (lambda (pred?)
	 (pred? e))
       guards))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing
	    (lambda (e failure)
	      (failure)))
	   (loop
	    (lambda (s)
	      (if (null? s)
		  match-nothing
		  (let ((match-rest
			 (loop (cdr s)))
			(match-first (car s)))
		    (lambda (e failure)
		      (match-first e
		       (lambda ()
			 (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))

;;; end of input

