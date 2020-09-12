;;; 長さを取得する。
(define (length xs)
  (cond ((null? xs) 0)
		(else (+ 1 (length (cdr xs))))))
