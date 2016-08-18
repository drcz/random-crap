;;; the following is a 12x12 solution to
;;; http://codegolf.stackexchange.com/questions/42017/print-a-negative-of-your-code

 (let((d(lambda(x
)(display x)
)) )(map(lambda(s
)(map (lambda(x)(
d(if(=(car s
)x)"*"" ")))
(iota(1+ 11)
))(d "\n"))'
((0 )(9)(2)(
5 )(10)(7)(8
)(4)(3)(1)( 
11)(6) )));;
