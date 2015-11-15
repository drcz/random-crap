(cond-expand
 (gambit (include "match.scm"))
 (guile (use-modules (ice-9 match))))

(define (repl msg topenv)
  (let ((error (lambda (msg) (repl msg topenv))))
    (begin
      (display msg)
      (newline)
      (display ">") 
      (match (read)
	(('halt) (exit))
	(('show-topenv)
	 (begin
	   (map (lambda (slt)
		  (display " ")
		  (display (car slt))
		  (display " <- ")
		  (display (cdr slt))
		  (newline))
		topenv)
	   (repl "--------------------" topenv)))
	(('! sym exp)
	 (let ((val (d2-eval exp '() topenv error)))
	   (repl `(remember ,sym as ,val)
		 `((,sym . ,val) . ,(env-del sym topenv)))))
	(anything
	 (repl (d2-eval anything '() topenv error) topenv))))))

(define (env-del sym env)
  (cond ((null? env) '())
	((eq? (caar env) sym) (cdr env))
	(else (cons (car env) (env-del sym (cdr env))))))	

(define (d2-eval expr env topenv error)
  (let evl ((expr expr))
    (match expr
      ('T 'T)
      (() '())
      ((? number? n) n)
      ((? symbol? s)
       (if (member s '(car cdr cons atom? num? = < + - * / % read disp))
	   s
	   (let ((v (assoc s (append env topenv)))) ;;; !!!
	     (if v 
		 (cdr v)
		 (error `(unbound symbol ,s !))))))
      (('closure a b env) expr)
      (('quote x) x)
      (('^ a b) `(closure ,a ,b ,env)) ;; !!!
      (('if p c a) (if (null? (evl p)) (evl a) (evl c)))     	
      ((rator . rands)
       (match (map evl expr)
	 (('read) (read))
	 (('disp x) (begin (display x) (newline) x))
	 (('car e) (car e))
	 (('cdr e) (cdr e))
	 (('cons e1 e2) (cons e1 e2))
	 (('+ e1 e2) (+ e1 e2))
	 (('- e1 e2) (- e1 e2))
	 (('* e1 e2) (* e1 e2))
	 (('/ e1 e2) (/ e1 e2))
	 (('% e1 e2) (modulo e1 e2))
	 (('= e1 e2) (if (equal? e1 e2) 'T '()))
	 (('< e1 e2) (if (< e1 e2) 'T '()))
	 (('atom? e) (if (pair? e) '() 'T))
	 (('num? e) (if (number? e) 'T '()))
	 ((('closure a b c) . args) (d2-eval b (append (map cons a args) c) topenv error)) ;;; !!!
	 (otherwise (error `(Application error in ,expr))))))))

(repl 'READY. '())
