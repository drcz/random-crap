;;; experiments in environment-free LISP/Scheme stepper
;;; with 3 colors TODO, PEND and DONE. go away.
;;; ----------------------------------------------------

(use-modules (grand scheme))

(define *top* '())

(define primop-semantics
  `((+ . ,+) (- . ,-) (* . ,*)
    (eq? . ,eq?) (pair? . ,pair?) (number? . ,number?) (symbol? . ,symbol?)
    (cons . ,cons) (car . ,car) (cdr . ,cdr) (list . ,list)))

(define (meaning-of x) (assoc-ref primop-semantics x))
(e.g. (equal? (meaning-of '+) +))
(e.g. (not (meaning-of 'life)))

(define (primop? x) (and (meaning-of x) #t))
(e.g. (primop? '*))
(e.g. (not (primop? 'your-mom)))

(define (self-evaluating? x)
  (or (number? x)
      (boolean? x)
      (primop? x)))

(define (reduced expr)
  (match expr
    (('TODO (? self-evaluating? e)) `(DONE ,e))
    (('TODO (? symbol? s)) `(TODO ,(assoc-ref *top* s))) ;;; no other way, no?
    (('TODO ('quote e)) `(DONE ,e))
    (('TODO ('if p c a)) `(PEND (if (TODO ,p) ,c ,a)))
    (('TODO ('lambda as b)) `(DONE (lambda ,as ,b))) ;; lol
    (('TODO (rator . rands)) `(PEND (,(reduced `(TODO ,rator))
                                    . ,(map (lambda (e) `(TODO ,e)) rands))))
    (('PEND ('if ('TODO p) c a)) `(PEND (if ,(reduced `(TODO ,p)) ,c ,a)))
    (('PEND ('if ('PEND p) c a)) `(PEND (if ,(reduced `(PEND ,p)) ,c ,a)))
    (('PEND ('if ('DONE '#f) c a)) `(TODO ,a))
    (('PEND ('if ('DONE _) c a)) `(TODO ,c))
    (('PEND (('TODO r) . rs))
     `(PEND (,(reduced `(TODO ,r)) . ,rs))) ;; hmm...
    (('PEND (('PEND r) . rs))
     `(PEND (,(reduced `(PEND ,r)) . ,rs)))
    (('PEND (('DONE r) . rs))
     (let seek ((rs rs)
                (ls '()))
       (match rs
         (() (applied r (map cadr ls)))
         ((('TODO e) . rs*)
          `(PEND ((DONE ,r) ,@ls ,(reduced (car rs)) ,@rs*)))
         ((('PEND e) . rs*)
          `(PEND ((DONE ,r) ,@ls ,(reduced (car rs)) ,@rs*)))
         ((('DONE e) . rs*)
          (seek rs* `(,@ls (DONE ,e))))
         (_ (pretty-print `(COMBINATION ANOMALY ,@ls ,@rs)))))) ;; dbg
    (('DONE e) expr)
    (_ (pretty-print `(REDUCED no match for ,expr))))) ;; dbg

(define (applied rator rands)
  (match rator
    ((? primop? p) `(DONE ,(apply (meaning-of p) rands)))
    (('lambda as b) `(TODO ,(substituted rands #;for as #;in b)))
    (_ (pretty-print `(APPLIED no match for ,rator))))) ;; dbg


(define (substituted vals #;for syms #;in expr)
  (let ((bnd (map cons syms vals)))
    (let sub ((expr expr)
              (bdn bnd))
      (match expr
        ((? self-evaluating?) expr)
        (('quote e) expr)
        ((? symbol? s) (match (assoc-ref bnd s) ;; !!!
                         (#f s)
                         (v `(quote ,v))))
        (('if e e* e**) `(if ,(sub e bnd) ,(sub e* bnd) ,(sub e** bnd)))
        (('lambda as b) (let ((bnd* (filter (lambda ((k . v)) (not (member k as))) bnd)))
                     `(lambda ,as ,(sub b bnd*))))
        ((rator . rands) (map (lambda (e) (sub e bnd)) expr))))))


(define (draw ce) ;; just nvm
  (match ce
    (('TODO e) e)
    (('DONE e) e)
    (('PEND es) (map draw es))
    (_ ce)))

(define (id x) x)
(define (quiet x) "")

(define (sraczka expr limit prettified)
  "taka to była sraczka dziwaczka"
  (let* ((expr* (reduced expr))
         (pretty (prettified expr))
         (pretty* (prettified expr*)))
    (if (not (equal? pretty pretty*)) (pretty-print pretty))
    (if (or (= limit 0) (equal? expr expr*))
        (begin
          (pretty-print pretty*) ;; pff.
          expr*)
        (sraczka expr* (- limit 1) prettified))))



(e.g. (reduced '(TODO 23)) ===> (DONE 23))
(e.g. (sraczka '(TODO (+ (* 7 8) 3)) 10 id) ===> (DONE 59))
(e.g. (sraczka '(TODO (+ (* 7 8) (- 10 3))) 100 draw) ===> (DONE 63))

(e.g. (sraczka '(TODO (eq? 2 3)) 5 id) ===> (DONE #f))
(e.g. (sraczka '(TODO (eq? (eq? 2 3) #f)) 10 draw) ===> (DONE #t))

(e.g. (sraczka '(TODO (list (* 7 8) #t 'quote '(a b c))) 10 draw)
      ===> (DONE (56 #t quote (a b c))))

(e.g. (sraczka '(TODO (if (eq? 2 3) 'boo 'woo)) 10 id) ===> (DONE woo))
(e.g. (sraczka '(TODO (if (eq? (+ 2 1) 3) 'boo 'woo)) 10 id) ===> (DONE boo))
(e.g. (sraczka '(TODO (if (eq? (+ 2 1) 3) (* 7 8) 'woo)) 33 id) ===> (DONE 56))

(e.g. (sraczka '(TODO ((lambda (x) (* x x)) (+ 2 1))) 100 id)
      ===> (DONE 9))
(e.g. (sraczka '(TODO (((lambda (x) (lambda (y) (* x y))) 7) 8)) 100 id)
      ===> (DONE 56))
(e.g. (sraczka '(TODO ((((lambda (x) (lambda (y) (lambda (z) (+ z (* x y))))) 7) 8) 2)) 100 id)
      ===> (DONE 58))
(e.g. (sraczka '(TODO ((((lambda (x) (lambda (y) (lambda (z) (+ z (* x y))))) 7) 8) 2)) 100 id)
      ===> (DONE 58))
(e.g. (sraczka '(TODO ((lambda (x y) (* x y)) 2 3)) 100 draw)
      ===> (DONE 6))

;;; lol
(define *top*
  '(
    (! . (lambda (n) (if (eq? n 0) 1 (* n (! (- n 1))))))
    (apd . (lambda (xs ys) (if (eq? xs '()) ys (cons (car xs) (apd (cdr xs) ys)))))
    (fld . (lambda (op e xs) (if (eq? xs '()) e (op (car xs) (fld op e (cdr xs))))))
    (map . (lambda (f xs) (fld (lambda (h t) (cons (f h) t)) '() xs)))
    (apd* . (lambda (xs ys) (fld cons ys xs)))
    ))

(e.g. (sraczka '(TODO (! 5)) 100 draw) ===> (DONE 120))
(e.g. (sraczka '(TODO (apd '() '(a s d))) 12 draw) ===> (DONE (a s d)))
(e.g. (sraczka '(TODO (apd '(q w e) '(a s d))) 100 draw) ===> (DONE (q w e a s d)))
(e.g. (sraczka '(TODO (apd* '() '(a s d))) 100 draw) ===> (DONE (a s d)))
(e.g. (sraczka '(TODO (apd* '(q w e) '(a s d))) 100 draw)
      ===> (DONE (q w e a s d)))
(e.g. (sraczka '(TODO (map (lambda (x) (cons x x)) '(o ^ -))) 200 draw)
      ===> (DONE ((o . o) (^ . ^) (- . -))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getting darker

(define *top*
  '((primops . '(+ - * cons car cdr list eq? number? symbol? pair?))

    (append . (lambda (xs ys)
                (if (eq? xs '()) ys (cons (car xs) (append (cdr xs) ys)))))

    (map . (lambda (f xs)
             (if (eq? xs '()) '() (cons (f (car xs)) (map f (cdr xs))))))
         
    (member? . (lambda (x xs)
                 (if (eq? xs '()) #f
                     (if (eq? (car xs) x) #t
                         (member? x (cdr xs))))))

    (zip . (lambda (xs ys)
             (if (eq? xs '())
                 '()
                 (cons (cons (car xs) (car ys))
                       (zip (cdr xs) (cdr ys))))))

    (lookup . (lambda (sym top)
                (if (eq? top '())
                    #f
                    (if (eq? (car (car top)) sym)
                        (cdr (car top))
                        (lookup sym (cdr top))))))

    (self-evaluating? . (lambda (x)
                          (if (member? x '(#t #f)) #t
                              (if (number? x) #t
                                  (if (pair? x)
                                      (eq? (car x) '_closure)
                                      #f)))))

    (evlis . (lambda (es env top) (map (lambda (e) (eval e env top)) es)))

    (eval . (lambda (expr env top)              
             (if (self-evaluating? expr) expr
               (if (symbol? expr)
                   ((lambda (lu) (if lu lu #;else (lookup expr top)))
                    (lookup expr env))
                (if (eq? (car expr) 'quote) (car (cdr expr))
                 (if (eq? (car expr) 'if)
                     (if (eval (car (cdr expr)) env top)
                         (eval (car (cdr (cdr expr))) env top)
                         (eval (car (cdr (cdr (cdr expr)))) env top))
                   (if (eq? (car expr) 'lambda)
                       (list '_closure (car (cdr expr))
                             (car (cdr (cdr expr)))
                             env)
                     (apply (eval (car expr) env top)
                            (evlis (cdr expr) env top)
                            top))))))))

    (top-eval . (lambda (expr top) (eval expr (zip primops primops) top))) ;; :D

    (apply . (lambda (rator rands top)
               (if (symbol? rator)
                   (apply-primop rator rands)
                   ((lambda (args body env vals)
                      (eval body (append (zip args vals) env) top))
                    (car (cdr rator)) ;; body
                    (car (cdr (cdr rator))) ;; args
                    (car (cdr (cdr (cdr rator)))) ;; env 
                    rands)))) ;; vals

    (apply-primop . (lambda (rator rands)
                      (if (eq? rator '+) (+ (car rands) (car (cdr rands)))
                       (if (eq? rator '-) (- (car rands) (car (cdr rands)))
                        (if (eq? rator '*) (* (car rands) (car (cdr rands)))
                         (if (eq? rator 'cons) (cons (car rands)
                                                     (car (cdr rands)))
                          (if (eq? rator 'car) (car (car rands))
                           (if (eq? rator 'cdr) (cdr (car rands))
                            (if (eq? rator 'eq?) (eq? (car rands)
                                                      (car (cdr rands)))
                             (if (eq? rator 'number?) (number? (car rands))
                              (if (eq? rator 'symbol?) (symbol? (car rands))
                               (if (eq? rator 'pair?) (pair? (car rands))
                                (if (eq? rator 'list) rands
                                 (list 'ERROR 'unknown 'operand rator))))))))))))))

    (define-form? . (lambda (e)
                      (if (pair? e)
                          (eq? (car e) 'define)
                          #f)))

    (run-program . (lambda (p top)
                     (if (eq? p '())
                         '()
                         (if (define-form? (car p))
                             (run-define (car p) (cdr p) top)
                             (run-expression (car p) (cdr p) top)))))

    (run-define . (lambda (d p top)
                    ((lambda (sym val)
                       (cons (list 'DEFINED sym)
                             (run-program p (cons (cons sym val) top))))
                     (car (cdr d))
                     (top-eval (car (cdr (cdr d))) top))))

    (run-expression . (lambda (e p top)
                        (cons (top-eval e top)
                              (run-program p top))))
))

(e.g. (sraczka '(TODO primops) 3 draw)
      ===> (DONE (+ - * cons car cdr list eq? number? symbol? pair?)))

(e.g. (sraczka '(TODO (append '(q w e) '(1 2 3))) 666 draw)
      ===>  (DONE (q w e 1 2 3)))

(e.g. (sraczka '(TODO (map (lambda (x) (cons x x)) '(o ^ -))) 666 quiet)
      ===> (DONE ((o . o) (^ . ^) (- . -))))

(e.g. (sraczka '(TODO (member? 'q '(a s d q w e))) 666 draw)
      ===> (DONE #t))
(e.g. (sraczka '(TODO (zip '(a b c) '(1 2 3))) 666 draw)
      ===> (DONE ((a . 1) (b . 2) (c . 3))))
(e.g. (sraczka '(TODO (lookup 'y '((y . 42) (x . 23)))) 666 draw)
      ===> (DONE 42))
(e.g. (sraczka '(TODO (self-evaluating? 31)) 66 draw)
      ===> (DONE #t))
(e.g. (sraczka '(TODO (self-evaluating? #t)) 66 draw)
      ===> (DONE #t))
(e.g. (sraczka '(TODO (self-evaluating? '+)) 666 quiet)
      ===> (DONE #f))
(e.g. (sraczka '(TODO (self-evaluating? '(+ 2 3))) 666 quiet)
      ===> (DONE #f)) ;; nb!
(e.g. (sraczka '(TODO (self-evaluating? (+ 2 3))) 666 quiet)
      ===> (DONE #t))

(e.g. (sraczka '(TODO (top-eval '(+ 2 3) '())) 997 quiet)
      ===> (DONE 5))
(e.g. (sraczka '(TODO (top-eval '(+ (* 7 8) 3) '())) 1250 quiet)
      ===> (DONE 59))

(e.g. (sraczka '(TODO (top-eval '((lambda (x) (* x x)) (+ 2 3)) '())) 6666 quiet)
      ===> (DONE 25))
(e.g. (sraczka '(TODO (top-eval '(((lambda (x) (lambda (y) (* y x))) 2) 3) '())) 6666 quiet)
      ===> (DONE 6))

(e.g. (sraczka '(TODO (run-expression '(cons 2 23) '() '())) 6666 quiet)

(e.g. (sraczka
       '(TODO (run-program
               '((define sq (lambda (x) (* x x)))
                 (sq (+ 2 3))
                 (define map (lambda (f xs)
                               (if (eq? xs '())
                                   '()
                                   (cons (f (car xs)) (map f (cdr xs))))))
                 (define numz '(1 2 3 4 5))
                 (map sq numz))
               '())) 99999 quiet)
      ===> (DONE ((DEFINED sq)
                  25
                  (DEFINED map)
                  (DEFINED numz)
                  (1 4 9 16 25))))

;;; no ładnie jego mać.
