(use-modules (ice-9 match)
	     (srfi srfi-1))

;;; Roman Numerals Kata 2016.06.08,
;;; by victor&drcz Scheme Team
;;; -- to be read along with kata1-test.scm

;;; we started with not passing the (do-test):
(define (arabic->roman n) "I")

;;; then extended it so that it passes "all green":
(define (arabic->roman n)
  (match n
    [1 "I"]
    [3 "III"]
    [9 "IX"]
    [1066 "MLXVI"]
    [1989 "MCMLXXXIX"]
    [_ "wat?"]))

;;; then we went on to generalize the first 3 cases;
;;; [why not another lookup trick?]

(define (arabic->roman n)
  (cond	[(< n 10)
	 (list-ref '("" "I" "II" "III" "IV" "V" "VI"
		     "VII" "VIII" "IX")
		   n)]
	[else
	 (match n
	   [1066 "MLXVI"]
	   [1989 "MCMLXXXIX"]
	   [_ "wat?"])]))

;;; it passed, so we decided to extend the method to tens,
;;; hundreds and thousands -- of course, one at a time.
;;; at the same time we also extended the test [cf (do-test-2)]:

(define (tens-of n) (if (< 100) (floor (/ n 10)) 'GO-AWAY))
(define (hundreds-of n) (if (< n 1000) (floor (/ n 100)) 'GO-AWAY))
(define (thousands-of n) (if (< n 10000) (floor (/ n 1000)) 'GO-AWAY))
;;; this GO-AWAY thing was not the best thing to do,
;;; but who cares, it should go away while refactoring anyway...

(define (arabic->roman n)
  (cond	[(< n 10)
	 (list-ref '("" "I" "II" "III" "IV" "V" "VI"
		     "VII" "VIII" "IX")
		   n)]
	[(< n 100)
	 (let* ((tens-digit (tens-of n))
		(tens-str (list-ref '("" "X" "XX" "XXX" "XL" "L" "LX"
				      "LXX" "LXXX" "XC")
			       tens-digit))
		(remaining-n (- n (* 10 tens-digit))))
	   (string-append tens-str (arabic->roman remaining-n)))]
	[(< n 1000)
	 (let* ((hundreds-digit (hundreds-of n))
		(hundreds-str (list-ref '("" "C" "CC" "CCC" "CD" "D" "DC"
					  "DCC" "DCCC" "CM")
					hundreds-digit))
		(remaining-n (- n (* 100 hundreds-digit))))
	   (string-append hundreds-str (arabic->roman remaining-n)))]
	[(< n 4000)
	 (let* ((thousands-digit (thousands-of n))
		(thousands-str (list-ref '("" "M" "MM" "MMM")
					thousands-digit))
		(remaining-n (- n (* 1000 thousands-digit))))
	   (string-append thousands-str (arabic->roman remaining-n)))]))

;;; at this point we had a working thing!
;;; next, we attempted refactoring (DRYing the code), but ran out of time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; so, here's what we would do:
;;; first, make all the cases look similar.

(define (units-of n) n)

(define (arabic->roman n)
  (cond	[(< n 1)
	 ""]
	[(< n 10)
	 (let* ((units-digit (units-of n))
		(units-str (list-ref '("" "I" "II" "III" "IV" "V" "VI"
				       "VII" "VIII" "IX")
				     units-digit))
		(remaining-n (- n (* 1 units-digit))))
	   (string-append units-str (arabic->roman remaining-n)))]
	[(< n 100)
	 (let* ((tens-digit (tens-of n))
		(tens-str (list-ref '("" "X" "XX" "XXX" "XL" "L" "LX"
				      "LXX" "LXXX" "XC")
			       tens-digit))
		(remaining-n (- n (* 10 tens-digit))))
	   (string-append tens-str (arabic->roman remaining-n)))]
	[(< n 1000)
	 (let* ((hundreds-digit (hundreds-of n))
		(hundreds-str (list-ref '("" "C" "CC" "CCC" "CD" "D" "DC"
					  "DCC" "DCCC" "CM")
					hundreds-digit))
		(remaining-n (- n (* 100 hundreds-digit))))
	   (string-append hundreds-str (arabic->roman remaining-n)))]
	[(< n 10000) ;; actually there are no roman numerals >3999...
	 (let* ((thousands-digit (thousands-of n))
		(thousands-str (list-ref '("" "M" "MM" "MMM" "--" "--" "--"
					   "--" "--" "--")
					 thousands-digit))
		(remaining-n (- n (* 1000 thousands-digit))))
	   (string-append thousands-str (arabic->roman remaining-n)))]))

;;; it passes, so we're ready for DRYing:

;;; generalize all this (***s-of n) nonsense:
(define (digit p #;-th #;from #;the #;right #;of n)
  ;;; starting from 0 as we're both C programmers
  (modulo (floor (/ n (expt 10 p))) 10))

;;; after replacing "(tens-of n)" to "(digit 2 #;of n)" etc.
;;; it passes, so we now extract the repeating pattern...

;; engineers love logarithms, right?
(define (number-of-digits n)
  (inexact->exact (ceiling (/ (log (1+ n)) (log 10)))))

(define (mk-romans-of-order m)
  ;;; no idea how to name this thing, but I believe you get what it does:
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
	     ;;; ^-- a subtraction, because it reads better
	     ;;; than "least power of 10 larger than n".
	     [a-digit (digit m #;of n)]
	     [a-string (list-ref (mk-romans-of-order m) a-digit)]
	     [remaining-n (- n (* (expt 10 m) a-digit))])
	(string-append a-string (arabic->roman remaining-n)))))

;;; that's it... what a strange program! where should we get from here?
;;; [btw we moved to (do-test-3)].

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

