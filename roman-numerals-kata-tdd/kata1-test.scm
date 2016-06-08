(include "test-macros.scm")
(include "kata1.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-test) ;; the original "seed" test we got.
  (and [assert-eq? (arabic->roman 1) "I"]
       [assert-eq? (arabic->roman 3) "III"]
       [assert-eq? (arabic->roman 9) "IX"]
       [assert-eq? (arabic->roman 1066) "MLXVI"]
       [assert-eq? (arabic->roman 1989) "MCMLXXXIX"]))

(do-test)


(define (do-test-2) ;;; extended while refactoring.
  (and [assert-eq? (arabic->roman 1) "I"]
       [assert-eq? (arabic->roman 3) "III"]
       [assert-eq? (arabic->roman 4) "IV"]
       [assert-eq? (arabic->roman 9) "IX"]
       [assert-eq? (arabic->roman 91) "XCI"]
       [assert-eq? (arabic->roman 99) "XCIX"]
       [assert-eq? (arabic->roman 999) "CMXCIX"]
       [assert-eq? (hundreds-of 913) 9]
       [assert-eq? (hundreds-of 1713) 'GO-AWAY] ;; don't do that!
       [assert-eq? (thousands-of 3715) 3]
       [assert-eq? (arabic->roman 1066) "MLXVI"]
       [assert-eq? (arabic->roman 1989) "MCMLXXXIX"]))

(do-test-2)

(define (do-test-3)
  (and [assert-eq? (digit 0 987) 7]
       [assert-eq? (digit 1 987) 8]
       [assert-eq? (digit 2 987) 9]
       [assert-eq? (number-of-digits 1) 1]
       [assert-eq? (number-of-digits 11) 2]
       [assert-eq? (number-of-digits 99) 2]
       [assert-eq? (number-of-digits 999) 3]
       [assert-eq? (mk-romans-of-order 0)
		   '("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")]
       [assert-eq? (mk-romans-of-order 2)
		   '("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM")]
       [do-test-2]))

(do-test-3)

