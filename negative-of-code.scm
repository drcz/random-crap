;;; the following is a 12x12 scheme solution to:
;;; http://codegolf.stackexchange.com/questions/42017/print-a-negative-of-your-code
;;; (using λ instead of lambda is almost like cheating)

 (let((d(λ(x
)(display x)
)) )(map(λ(s
)(map (λ(x)(
d(if(=(car s
)x)"*"" ")))
(iota(1+ 11)
))(d "\n"))'
((0 )(9)(2)(
5 )(10)(7)(8
)(4)(3)(1)( 
11)(6) )));;
