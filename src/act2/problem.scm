;;; 2-17
(define (length items)
  (if (null? items)
	0
	(+ 1 (length (cdr items)))))

;;; 2-18
(define (last-pair xs)
  (cond ((<= (length xs) 1) xs)
		(else (last-pair (cdr xs)))))

;;; 2-19
(define (reverse xs)
  (cond ((<= (length xs) 1) xs)
		(else (append (reverse (cdr xs)) (list (car xs))))))

;;; 2-20
;;; util
(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (= (remainder n 2) 1))
(define (odd-priority lst)
  (if (not (null? lst))
	(if (odd? (car lst))
	  (cons (car lst) (odd-priority (cdr lst)))
	  (odd-priority (cdr lst)))
	'()))
(define (even-priority lst)
  (if (not (null? lst))
	(if (even? (car lst))
	  (cons (car lst) (even-priority (cdr lst)))
	  (even-priority (cdr lst)))
	'()))
;;; main
(define (same-priority fst . lst)
  (if (odd? fst)
	(cons fst (odd-priority lst))
	(cons fst (even-priority lst))))

;;; 2-21
(define (square-list-a items)
  (if (null? items)
	'()
	(cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map (lambda (x) (square x)) items))

;;; 2-22
;;; 値が新しいものから先頭に追加されていくので逆順になっている。
(define (square-list-c items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons (square (car things))
				  answer))))
  (iter items '()))

;;; consの特性をわかってない?
;;; consは|value|-|pointer|として処理する感じになってて、
;;; (cons 1 '(2 3))と実行すると'(1 2 3)となるが、
;;; 逆に (cons '(2 3) 1)とすると'((2 3) . 1)、|(2 3)というvalue|-|pointer| -> |1というvalue|-|nil|
;;; として処理されるためこうなる。
;;; ちなみに'(1 2 3 4 5 6)とは(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 '()))))))の略である。
(define (square-list-d items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons answer
				  (square (car things))))))
  (iter items '()))

;;; 2-23
(define (for-each f lst)
  (if (null? lst)
	(cons (f (car lst))
		  (for-each f (cdr lst)))
	'())
  '())

;;; 2-27
(define (deep-reverse lst)
  (if (null? lst)
	lst
	(let ((fst (car lst)))
	  (append (deep-reverse (cdr lst))
			  (if (list? fst)
				(list (deep-reverse fst))
				(list fst))))))

;;; 2-28
