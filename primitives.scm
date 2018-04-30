
;;;#| WORKING ... |#


(define list
	(lambda x x))

(define caar
	(lambda (lst)
		(car (car lst))))

(define cddr
	(lambda (lst)
		(cdr (cdr lst))))

(define cadr
	(lambda (lst)
		(car (cdr lst))))

(define cdar
	(lambda (lst)
		(cdr (car lst))))

(define not
	(lambda (x)
		(if x #f #t)))



(define eq?
    (lambda(x y)
        (cond ( (and (symbol? x) (symbol? y)) (val-eq? x y))
	(else (addr-eq? x y)))))

(define number?
	(lambda (x)
		(or (integer? x) (fraction? x))))

(define rational? number?)
			
(define zero?
	(lambda (num)
		(equal-bin num 0)))

 

(define null?
	(lambda (x)
		(val-eq? x '())))

(define andmap 
	(lambda (func lst)
		(cond ((null? lst) #t)
			  ((func (car lst)) (andmap func (cdr lst)))
			  (else #f))))




;;;#| Not Working with tc-applic...    All functions that contain fold(l|r) or apply|#



(define foldr
	(lambda (func init lst)
	  (if (null? lst)
	      init
	      (func (car lst) (foldr func init (cdr lst))))))

(define foldl
	(lambda (func init lst)
	  (if (null? lst)
	      init
	      (foldl func (func init (car lst)) (cdr lst)))))

(define +
	(lambda numbers
		(foldl plus-bin 0 numbers)))

    
(define -
	(lambda numbers
		(if (equal-bin (length numbers) 1)
			(minus-bin 0 (car numbers))
			(foldl minus-bin (car numbers) (cdr numbers)))))

(define *
	(lambda numbers
			(foldl mul-bin 1 numbers)))

(define /
	(lambda numbers
		(if (equal-bin (length numbers) 1)
			(div-bin 1 (car numbers))
			(foldl div-bin (car numbers) (cdr numbers)))))


;;;#| Not Working no matter what ...|#


(define append-bin
	(lambda (l m)
		(if (null? l) m
			(cons (car l) (append-bin (cdr l) m)))

		)

	)

(define append
	(lambda lists
		(foldl append-bin '() lists)))













(define map
  ((lambda (y) 
     ((lambda (map1) 
	((lambda (maplist) 
	   (lambda (f . s) 
	     (maplist f s))) 
	 (y (lambda (maplist) 
	      (lambda (f s) 
		(if (null? (car s)) '() 
		    (cons (apply f (map1 car s)) 
			  (maplist f (map1 cdr s))))))))) 
      (y (lambda (map1) 
	   (lambda (f s) 
	     (if (null? s) '() 
		 (cons (f (car s)) 
		       (map1 f (cdr s))))))))) 
   (lambda (f) 
     ((lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))
      (lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))))))
