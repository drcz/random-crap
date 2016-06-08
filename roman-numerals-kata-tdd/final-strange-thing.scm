(use-modules (ice-9 match) (srfi srfi-1))

(define (digit p #;-th #;from #;the #;right #;of n)
  (modulo (floor (/ n (expt 10 p))) 10))

(define (number-of-digits n)
  (inexact->exact (ceiling (/ (log (1+ n)) (log 10)))))

(define (mk-romans-of-order m)
  (define (build x y z) 
    (map (lambda (xs) (apply string-append xs))
	 `(("") (,x) (,x ,x) (,x ,x ,x) (,x ,y) (,y) (,y ,x)
	   (,y ,x ,x) (,y ,x ,x ,x) (,x ,z))))
  (match m
    [0 (build "I" "V" "X")] ;; units
    [1 (build "X" "L" "C")] ;; tens
    [2 (build "C" "D" "M")] ;; hundreds
    [3 (build "M" "-" "-")] ;; thousands
    ))

(define (arabic->roman n)
  (if (< n 1)
      ""
      (let* ([m (- (number-of-digits n) 1)]
	     [a-digit (digit m #;of n)]
	     [a-string (list-ref (mk-romans-of-order m) a-digit)]
	     [remaining-n (- n (* (expt 10 m) a-digit))])
	(string-append a-string (arabic->roman remaining-n)))))

;;; boom done.