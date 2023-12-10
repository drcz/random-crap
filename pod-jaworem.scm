;;; an ugly-but-hopefully-useful scanner-and-parser for s-expressions with
;;; comments, roughly preserving their spatial properties (rows & columns)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; we propose to first tokenize source string and then perform
;;; all the necessary parsing/counting/matching/reconstruction on
;;; such representation. but what constitutes a ``good'' token?
;;; surely it must be as granular as atoms in sexps, BUT if we want
;;; to keep track of multi-line comments and strings (and we do!),
;;; it must also be no larger than what fits into single row of text.
;;; a proper tokenization must allow easy computation of expressions'
;;; width (in columns) and height (in rows), and it should allow to
;;; recognize matching parentheses, parent expression for given atom
;;; and commented #;expressions, ideally before we even parse the
;;; chosen tokens-span...

;;; nb since srfi-200 is still in draft mode we use the old match syntax.

(use-modules (grand scheme))
(define empty? null?)

(define (index-for #;first-element-satisfying property? #;in
                   #;or-otherwise-just-for-the-end-of xs)
  (let index ((i 0) (xs xs))
    (match xs
      (() i) ;; nb
      (((? property?) . _) i)
      ((_ . xs*) (index (1+ i) xs*)))))

 (e.g. (index-for (is _ equal? 'q) '(a s d q w e)) ===> 3)
 (e.g. (index-for (is _ equal? 'x) '(a s d q w e)) ===> 6)

(define (repeat e n #;times) (map (lambda _ e) (iota n))) ;; :)

 (e.g. (repeat 'tora! 3) ===> (tora! tora! tora!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; positions within a string we'll represent as (index col row) triplets

;;; we often compute position N chars further:
(define (position+ #;by N #;characters #;on (index col row))
  `(,(+ index N) ,col ,(+ row N)))

;;; ...or ``newline and carriage return'':
(define (position+nl #;on (index col row))
  `(,(+ index 1) ,(+ col 1) 0))

 (e.g. (position+ 3 '(0 0 0)) ===> (3 0 3))
 (e.g. (position+nl '(3 0 3)) ===> (4 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scanners are more-or-less RTN nodes, expressed ``functionally''
;;; as [char] x Position -> [char] x [token] x Position

(define (nl-scanner chars position)
  (values (drop chars 1)
          `(((NEWLINE "\n") ,position))
          (position+nl position)))

(define (space-scanner chars position)
  (values (drop chars 1) '() (position+ 1 position)))

(define ((one-token-scanner string tag) chars position)
  (let* ((len (string-length string))
         (chars* (drop chars len))
         (position* (position+ len position))
         (token `((,tag ,string) ,position)))
    (values chars* `(,token) position*)))

 (e.g. ((one-token-scanner "pop" 'JAZZ) (string->list "popeye") '(3 0 3))
       ===> (#\e #\y #\e)
            (((JAZZ "pop") (3 0 3)))
            (6 0 6))

(define (atom-scanner chars position)
  (let* ((len (index-for (is _ member? (string->list "();\"\n\t "))
                         chars))
         (atom (list->string (take chars len)))
         (token `((ATOM ,atom) ,position))
         (chars* (drop chars len))
         (position* (position+ len position)))
    (values chars* `(,token) position*)))

(define (oneline-comment-scanner chars position)
  (let* ((len (index-for (is _ equal? #\newline) chars))
         (comment (list->string (take chars len)))
         (position* (position+ len position))
         (chars** (drop-upto (1+ len) chars))
         (position** (if (empty? chars**)
                         position*
                         (position+nl position*)))
         (token `((ONELINE-COMMENT ,comment) ,position))
         (token* `((NEWLINE "\n") ,position*))
         (tokens** (if (empty? chars**) `(,token) `(,token ,token*))))
    (values chars** tokens** position**)))

 (e.g. (oneline-comment-scanner (string->list ";;; we all knew
42") '(3 0 3)) ===> (#\4 #\2)
                    (((ONELINE-COMMENT ";;; we all knew") (3 0 3))
                     ((NEWLINE "\n") (18 0 18)))
                    (19 1 0))

 (e.g. (oneline-comment-scanner (string->list ";; whops!") '(0 0 0))
       ===> ()
            (((ONELINE-COMMENT ";; whops!") (0 0 0)))
            (9 0 9))
       

(define (string-content-scanner chars position)
  (let scan ((chars chars)
             (tokens '())
             (current '())
             (position position))
    (match chars
      ((#\\ c . chars*)
       (scan chars* tokens `(,@current #\\ ,c) position))
      ((#\newline . chars*)
       (let* ((string (list->string current))
              (token `((STRING-CONTENT ,string) ,position))
              (position* (position+ (length current) position))
              (token* `((NEWLINE "\n") ,position*))
              (position** (position+nl position*))
              (tokens* `(,@tokens ,token ,token*)))
         (scan chars* tokens* '() position**)))
      ((#\" . chars*)
       (let* ((string (list->string current))
              (token `((STRING-CONTENT ,string) ,position))
              (position* (position+ (length current) position)))
         (values chars `(,@tokens ,token) position*)))
      ((c . chars*)
       (scan chars* tokens `(,@current ,c) position)))))

 (e.g. (string-content-scanner (string->list "blah blah
new line\\\" -- not a real closing\"") '(0 0 0))
      ===> (#\")
      (((STRING-CONTENT "blah blah") (0 0 0))
       ((NEWLINE "\n") (9 0 9))
       ((STRING-CONTENT "new line\\\" -- not a real closing") (10 1 0)))
      (42 1 32))

(define (comment-content-scanner chars position)
  (let scan ((chars chars)
             (tokens '())
             (current '())
             (position position))
    (match chars
      ((#\newline . chars*)
       (let* ((comment (list->string current))
              (token `((COMMENT-CONTENT ,comment) ,position))
              (position* (position+ (length current) position))
              (token* `((NEWLINE "\n") ,position*))
              (position** (position+nl position*))
              (tokens* `(,@tokens ,token ,token*)))
         (scan chars* tokens* '() position**)))
      ((#\| #\# . chars*)
       (let* ((comment (list->string current))
              (token `((COMMENT-CONTENT ,comment) ,position))
              (position* (position+ (length current) position)))
         (values chars `(,@tokens ,token) position*)))
      ((c . chars*)
       (scan chars* tokens `(,@current ,c) position)))))


(define ((comp-scanner scanner* scanner**) chars position)
  (let* ((chars* tokens* position* (scanner* chars position))
         (chars** tokens** position** (scanner** chars* position*)))
    (values chars** `(,@tokens* ,@tokens**) position**)))

(define string-scanner
  (comp-scanner (one-token-scanner "\"" 'STRING-OPEN)
                (comp-scanner string-content-scanner
                              (one-token-scanner "\"" 'STRING-CLOSE))))

(define comment-scanner
  (comp-scanner (one-token-scanner "#|" 'COMMENT-OPEN)
                (comp-scanner comment-content-scanner
                              (one-token-scanner "|#" 'COMMENT-CLOSE))))

 (e.g. (string-scanner (string->list "\"blah blah
new line\\\" -- not a real closing\" tbc") '(0 0 0))
      ===> (#\space #\t #\b #\c)
      (((STRING-OPEN "\"") (0 0 0))
       ((STRING-CONTENT "blah blah") (1 0 1))
       ((NEWLINE "\n") (10 0 10))
       ((STRING-CONTENT "new line\\\" -- not a real closing") (11 1 0))
       ((STRING-CLOSE "\"") (43 1 32)))
      (44 1 33))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which scanner to use depends on the first visible characters...

(define (scanner-for chars)
  (match chars
    ((#\newline . _) nl-scanner)
    ((#\space . _) space-scanner)
    ((#\tab . _) (one-token-scanner "\t" 'TAB))

    ((#\' . _) (one-token-scanner "'" 'QUOTE))
    ((#\` . _) (one-token-scanner "`" 'QUASIQUOTE))
    ((#\, #\@ . _) (one-token-scanner ",@" 'UNQUOTE-SPLITING))
    ((#\, . _) (one-token-scanner "," 'UNQUOTE))

    ((#\( . _) (one-token-scanner "(" 'LPAR))
    ((#\) . _) (one-token-scanner ")" 'RPAR))
    ((#\. . _) (one-token-scanner "." 'DOT))

    ((#\# #\\ c . _) (one-token-scanner (list->string `(#\# #\\ ,c))
                                        'CHAR))
    ((#\" . _) string-scanner)

    ((#\# #\; . _) (one-token-scanner "#;" 'COMMENT-EXPR))

    ((#\; . _) oneline-comment-scanner)
    ((#\# #\| . _) comment-scanner)

    ;;; TODO {moustache symbols perhaps?}
    (_ atom-scanner)))

;;; then the tokenization is straightforward:
(define (tokenized string)
  (let scan ((chars (string->list string))
             (tokens '())
             (position '(0 0 0)))
    (if (empty? chars)
        tokens
        (let* ((scanner (scanner-for chars))
               (chars* tokens* position* (scanner chars position)))
          (scan chars* `(,@tokens ,@tokens*) position*)))))

(e.g. (tokenized "(* 7
                     #;(f x) 8) ;; boo")
      ===> (((LPAR "(") (0 0 0))
            ((ATOM "*") (1 0 1))
            ((ATOM "7") (3 0 3))
            ((NEWLINE "\n") (4 0 4))
            ((COMMENT-EXPR "#;") (26 1 21))
            ((LPAR "(") (28 1 23))
            ((ATOM "f") (29 1 24))
            ((ATOM "x") (31 1 26))
            ((RPAR ")") (32 1 27))
            ((ATOM "8") (34 1 29))
            ((RPAR ")") (35 1 30))
            ((ONELINE-COMMENT ";; boo") (37 1 32))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; given that we can obviously reconstruct the original...

(define (reconstructed tokens)
  (match tokens
    (() "")
    ((((_ s) (p c r)) . tokens*)
     (let* ((len (string-length s))
            (end-pos (+ p len))
            (offset (match tokens*
                      (() 0)
                      (((_ (p* c* r*)) . _) (- p* end-pos))))
            (white (list->string (repeat #\space offset))))
       (string-append s white (reconstructed tokens*))))))

 (e.g. (reconstructed (tokenized "(f x   \"whoa\" #|whatever|#) ;; boom"))
       ===> "(f x   \"whoa\" #|whatever|#) ;; boom")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ...or parse [the first] expression...

(define (parsed-expression tokens)
  (match tokens
    (() #f) ;; sure?

    (((('ATOM s) _) . tokens*)
     (values (parsed-atom s) tokens*))
    (((('CHAR s) _) . tokens*)
     (values (string-ref s 2) tokens*))
    (((('STRING-OPEN s) _) . tokens*)
     (parsed-string tokens*))

    (((('QUOTE _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (values `(quote ,e) tokens**)))
    (((('QUASIQUOTE _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (values `(,'quasiquote ,e) tokens**)))
    (((('UNQUOTE _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (values `(,'unquote ,e) tokens**)))
    (((('UNQUOTE-SPLITING _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (values `(,'unquote-spliting ,e) tokens**)))

    (((('COMMENT-EXPR _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (parsed-expression tokens**)))

    (((('LPAR _) _) . tokens*)
     (parsed-tail tokens*))

    ((_ . tokens*)
     (parsed-expression tokens*))))

(define (parsed-atom str)
  (match str
    ("#t" #t) ("#true" #t)
    ("#f" #f) ("#false" #f)
    (_ (match (string->number str)
         (#f (string->symbol str))
         (num num)))))

(define (parsed-string tokens)
  (match tokens
    (((('STRING-CLOSE _) _) . tokens*)
     (values "" tokens*))
    (((('STRING-CONTENT s) _) . tokens*)
     (let ((s* tokens** (parsed-string tokens*)))
       (values (string-append s s*) tokens**)))
    (((('NEWLINE s) _) . tokens*)
     (let ((s* tokens** (parsed-string tokens*)))
       (values (string-append s s*) tokens**)))
    ((_ . tokens*)
     (parsed-string tokens*))))

(define (parsed-tail tokens)
  (match tokens
    (((('COMMENT-EXPR _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (parsed-tail tokens**)))
    (((((? (is _ member? '(COMMENT-OPEN
                           COMMENT-CLOSE
                           COMMENT-CONTENT
                           NEWLINE
                           TAB))) _) _) . tokens*)
       (parsed-tail tokens*))
    (((('DOT _) _) . tokens*)
     (let ((e tokens** (parsed-expression tokens*)))
       (match tokens**
         (((('RPAR _) _) . tokens***)
          (values e tokens***)))))
    (((('RPAR _) _) . tokens*)
     (values '() tokens*))
    (_ (let* ((head tokens* (parsed-expression tokens))
              (tail tokens** (parsed-tail tokens*)))
         (values `(,head . ,tail) tokens**)))))

 (e.g. (parsed-expression
        (tokenized "`(j-23 ,(+ 2 3) #;(- 3 2) . improperrr)   42"))
       ===> `(j-23 (unquote (+ 2 3)) . improperrr) ;;; the expression
            (((ATOM "42") (42 0 42)))) ;;; tokens left

 (e.g. (parsed-expression (tokenized "
(define (! #;int n) ; -> int
  (if (= n 0)
     #|base case|# 1 ; must be 1, because 0 would break
     (* n (! (- n 1 #|one|#)))))"))
       ===> (define (! n) (if (= n 0) 1 (* n (! (- n 1)))))
            ()) ;;; no tokens left, right?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ...or find matching parentheses and parent expression's span:

(define (token-span from-token to-token #;in tokens)
  (let* ((from-index (index-for (is _ equal? from-token) tokens))
         (to-index  (index-for (is _ equal? to-token) tokens))
         (width (- to-index from-index -1)))
    (take-upto width (drop-upto from-index tokens))))

(define (matching-rpar tokens)
  (let counting-pars ((counter 1)
                      (tokens tokens))
    (match tokens
      (() #f) ;; mismatch!
      (((('RPAR s) pos) . tokens*) (if (= counter 1) 
                                       `((RPAR ,s) ,pos)
                                       (counting-pars (- counter 1)
                                                      tokens*)))
      (((('LPAR s) pos) . tokens*) (counting-pars (+ counter 1) tokens*))
      ((_ . tokens*) (counting-pars counter tokens*)))))

(define (matching-lpar tokens)
  (let counting-pars ((counter 1)
                      (tokens tokens))
    (match tokens
      (() #f) ;; mismatch!
      (((('LPAR s) pos) . tokens*) (if (= counter 1) 
                                       `((LPAR ,s) ,pos)
                                       (counting-pars (- counter 1)
                                                      tokens*)))
      (((('RPAR s) pos) . tokens*) (counting-pars (+ counter 1) tokens*))
      ((_ . tokens*) (counting-pars counter tokens*)))))


(define (neighbourhood #;for token #;in tokens)
  (let* ((index (index-for (is _ equal? token) tokens))
         (west-of (reverse (take tokens index)))
         (east-of (drop tokens (1+ index))))
    (match token
      ((('LPAR _) pos) (and-let* ((rpar (matching-rpar east-of)))
                         (token-span token rpar tokens)))
      ((('RPAR _) pos) (and-let* ((lpar (matching-lpar west-of)))
                         (token-span lpar token tokens)))
      ((_ pos) (and-let* ((lpar (matching-lpar west-of))
                          (rpar (matching-rpar east-of)))
                 (token-span lpar rpar tokens))))))


 (e.g. (reconstructed 
        (neighbourhood '((ATOM "g") (6 0 6)) (tokenized "(f x (g y) z)")))
       ===> "(g y)")

 (e.g. (reconstructed
        (neighbourhood '((ATOM "z") (11 0 11))
                       (tokenized "(f x (g y) z)")))
       ===> "(f x (g y) z)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finally, if i got it right, the whole point of writing this program
;;; was to establish size and position of given source fragment, e.g.
;;; a subexpression.

(define (boundaries tokens)
  (let* ((tokens (filter (lambda (((type _) _)) (not (eq? type 'NEWLINE)))
                         tokens))
         (cols (map (lambda ((type (index col row))) col) tokens))
         (rows-lft (map (lambda ((type (index col row))) row) tokens))
         (rows-rght (map (lambda (((type str) (index col row)))
                           (+ row (string-length str))) tokens))

         (top (apply min cols))       (bottom (apply max cols))
         (left (apply min rows-lft))  (right (apply max rows-rght))
         (height (- bottom top -1))   (width (- right left -1)))

    `((top    . ,top)
      (bottom . ,bottom)
      (left   . ,left)
      (right  . ,right)
      (height . ,height)
      (width  . ,width))))

 (e.g. (boundaries (tokenized "(1 2
3 4
5 6)")) ===> ((top . 0)    (bottom . 2)
              (left . 0)   (right . 4)
              (height . 3) (width . 5)))

 (e.g. (boundaries (neighbourhood '((ATOM "g") (6 0 6))
                                  (tokenized "(f x (g y) z)")))
       ===> ((top . 0)    (bottom . 0)
             (left . 5)   (right . 10)
             (height . 1) (width . 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pod jaworem, pod zielonym...
;;; pod jaworem, pod zielonym hej, łorze Hanka
;;; siwym koniem hej, łorze Hanka
;;; siwym koniem.
;;; Jesce skibki nie zorała...
;;; jesce skibki nie zoarała hej, kie jom mama
;;; zawołała hej, kie jom mama
;;; zawołała (...)
