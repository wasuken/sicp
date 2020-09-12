(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(define (make-rat n d)
  (let ((g (gcd n d)))
	(cons (/ n g) (/ d g))))

;; (define (cons x y)
;;   (define (dispatch m)
;; 	(cond ((= m 0) x)
;; 		  ((= m 1) y)
;; 		  (else (error "Argument not 0 or 1 -- CONS" m))))
;;   dispatch)

;; (define (car z) (z 0))

;; (define (cdr z) (z 1))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
				(make-interval (/ 1.0 (upper-bound y))
							   (/ 1.0 (lower-bound y)))))

(define (list-ref items n)
  (if (= n 0)
	(car items)
	(list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
	0
	(+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
	(if (null? a)
	  count
	  (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
	list2
	(cons (car list1) (append (cdr list1) list2))))

(define (scale-list items factor)
  (if (null? items)
	'()
	(cons (* (car items) factor)
		  (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
	'()
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
				 (count-leaves (cdr x))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(acumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	'()
	(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares n)
  (acumulate cons
			 0
			 (filter even?
					 (map fib
						  (enumerate-interval 0 n)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap
				(lambda (i)
				  (map (lambda (j) (list i j))
					   (enumerate-interval 1 (- i 1))))
				(enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
	(list '())
	(flatmap (lambda (x)
			   (map (lambda (p) (cons x p))
					(permutations (remove x s))))
			 s)))

(define (remove remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
	(below painter2 painter2)))

(define (wave4 (flipped-pairs wave)))

(define (right-split painter n)
  (if (= n 0)
	painter
	(let ((smaller (right-split painter (- n 1))))
	  (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
	painter
	(let ((up (up-split painter (- n 1)))
		  (right (right-split painter (- n 1))))
	  (let ((top-left (beside up up))
			(bottom-right (below right right))
			(corner (corner-split painter (- n 1))))
		(beside (below))))))
