;; (load "./utils/list.scm")
(use math.mt-random)
(define mt (make <mersenne-twister> :seed (sys-time)))

(define (test x y)
  (if (= x 0)
	0
	y))
;;; 階乗
(define (factorial x)
  (cond ((>= x 1)
		 (* x (factorial (- x 1))))
		(#t 1)))

;;; フィボナッチ数
(define (fib x)
  (cond
   ((<= x 1) x)
   (#t (+ (fib (- x 1))
		  (fib (- x 2))))))

(define (fib2 x)
  (fib-iter 1 0 x))

(define (fib-iter a b count)
  (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))

;;; 両替
(define (count-change x)
  (cond ((<= x 0) x)
		((>= x 50) (+ 1 (count-change (- x 50))))
		((>= x 25) (+ 1 (count-change (- x 25))))
		((>= x 10) (+ 1 (count-change (- x 10))))
		((>= x 5) (+ 1 (count-change (- x 5))))
		((>= x 1) (+ 1 (count-change (- x 1))))))
;;; 両替、これらが正解らしい・・・。
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount
					 (- kinds-of-coins 1))
				 (cc (- amount
						(first-denomination kinds-of-coins))
					 kinds-of-coins)))))

(define (count-change-2 amount)
  (cc amount 5))

(define (expt x n)
  (cond ((<= n 0) 1)
		(else (* x (expt x (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (gcd a b)
  (if (= b 0)
	a
	(gcd b (remainder a b))))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
	0
	(+ (term a)
	   (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
	  midpoint
	  (let ((test-value (f midpoint)))
		(cond ((positive? test-value)
			   (search f neg-point midpoint))
			  ((negative? test-value)
			   (search f midpoint pos-point))
			  (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
		   (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))

(define (cube x) (* x x x))

(define (append-2 list1 list2)
  (cond ((null? list1)
		 list2)
		(else
		 (cons (car list1) (append-2 (cdr list1) list2)))))

(define (scale-list items factor)
  (if (null? items)
	'()
	(cons (* (car items) factor)
		  (scale-list (cdr items) factor))))

(define (count-leave x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leave (car x))
				 (count-leave (cdr x))))))

(define (memq item x)
  (cond ((null? x) false)
		((eq? item (car x)) x)
		(else (memq item (cdr x)))))


(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m))
					m))
		(else
		 (remainder (* base (expmod base (- exp 1) m))
					m))))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (mt-random-integer mt (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

(define (start-prime-test n start-time)
  (if (prime? n)
	(report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (pi-sum a b)
  (if (a > b)
	0
	(+ (/ 1.0 (* a (+ a 2))) (pi-sum a 4) b)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	 dx))
