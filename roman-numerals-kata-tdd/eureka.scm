(use-modules (srfi srfi-1))
(define rest cdr)

;;; ``Romani ite domum''
;;; numerals conversion arabic<->roman,
;;; take 1: the "eureka" method (before the workshop).

(define (arabic->roman n)
  (let convert ([romans '("M" "CM" "D" "CD" "C" "XC" "L"
			  "XL" "X" "IX" "V" "IV" "I")]
		[arabs '(1000 900 500 400 100 90 50
			 40 10 9 5 4 1)]
		[n n])
    (cond [(= n 0)
	   ""] ;; that's why Romans didn't invent the notion of zero!
	  [(< n (first arabs))
	   (convert (rest romans) (rest arabs) n)]
	  [else
	   (string-append (first romans)
			  (convert romans arabs (- n (first arabs))))])))


(define (roman->arabic r)
  (let convert ([arabs '(1000 900 500 400 100 90 50
			 40 10 9 5 4 1)]
		[romans '("M" "CM" "D" "CD" "C" "XC" "L"
			  "XL" "X" "IX" "V" "IV" "I")]
		[r r])
    (cond [(string=? "" r)
	   0]
	  [(not (string-prefix? (first romans) r))
	   (convert (rest arabs) (rest romans) r)]
	  [else
	   (+ (first arabs)
	      (convert arabs 
		       romans
		       (substring r (string-length (first romans)))))])))

;;; test the thing:
(equal? (iota 4000)
	(map (lambda (x) (roman->arabic (arabic->roman x)))
	     (iota 4000)))


