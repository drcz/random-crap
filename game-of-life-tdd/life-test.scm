;;; hehe.
(use-modules (ice-9 pretty-print))
(include "life.scm")
;#(define (show x) x) ; no new buffer in geiser...
(define (show x) (pretty-print x) x) ; one for "standalone" run.


(define (test-worlds-content mk-empty-world state-at set-state)
  (let* ((world (mk-empty-world))
	 ;;; todo: rather ask of some random position?
	 (assert1 (eq? (state-at world 0 0) 'dead))
	 (world (set-state world 0 0 'alive))
	 ;;; same here, in order to broaden the coverage...
	 (assert2 (eq? (state-at world 0 0) 'alive))
	 (assert3 (eq? (state-at world 0 1) 'dead)))
    (cond ((not assert1) '(asser1 failed))
	  ((not assert2) '(assert2 failed))
	  ((not assert3) '(assert3 failed))
	  (else 'OK))))

(show (test-worlds-content empty-world state-at update-state))


;;; "worlds comprehension" -- for tests only... ;;;;;;;;;;;;;;;;;;
;;; but test first, of course...
(define (test-wolrds-equiv worlds-equiv)
  (if (and
       (worlds-equiv '((0 0) (1 2) (3 1))
		     '((3 1) (0 0) (1 2)))
       (not (worlds-equiv '((0 1) (1 2) (3 1))
			  '((3 1) (0 0) (1 2)))))
      'OK
      'FAIL))

(define (worlds-equiv w1 w2)
  (and (fold-right (lambda (xy tl)
		     (and (member xy w2) tl))
		   #t
		   w1)
 (fold-right (lambda (xy tl)
		     (and (member xy w1) tl))
		   #t
		   w2)))

(show (test-wolrds-equiv worlds-equiv))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define (test-adjectancy adj?)
  (if
   (fold-right (lambda (((x0 y0 x1 y1) res) rest)
		 (and (equal? (adj? x0 y0 x1 y1) res)
		      rest))
	       #t
	       `[ ((0 0 1 1) #t)
		  ((0 0 1 0) #t)
		  ((1 1 0 0) #t)
		  ((0 0 2 0) #f)
		  ((0 0 2 2) #f)
		  ((3 1 1 3) #f)
		  ((0 0 0 0) #f)
		  ((-2 1 -2 2) #t)
		  ((-2 1 -3 3) #f) ])
   'OK
   'FAIL))
#;(show (test-adjectancy adjectant?))


(define (test-adjectant-coordinates adjectant-coordinates)
  (let* ((x 2)
	 (y 3)
	 (test (adjectant-coordinates x y))
	 (expected '[(1 2) (2 2) (3 2)
		     (1 3)       (3 3)
		     (1 4) (2 4) (3 4)]))
    (if (worlds-equiv test expected)
	'OK
	'FAIL)))

(show (test-adjectant-coordinates adjectant-coordinates))


(define (test-put-pattern empty-world put-pattern)
  (let* ((world (empty-world))
	 (world (put-pattern world
			     0 0 
			     `[(X X X)
			       (X _ _)]))
	 (expected '((0 0) (1 0) (2 0) (0 1))))
    (if (worlds-equiv expected world)
	'OK
	'FAIL)))

(show (test-put-pattern empty-world put-pattern))


(define (test-neighbours empty-world put-pattern number-of-alive-neighbours)
  (let ((world (put-pattern (empty-world)
			    0 0
			    `[(_ X)
			      (X _)])))
    (if (= (number-of-alive-neighbours world 0 0) 2)
	'OK
	'FAIL)))
       
(show (test-neighbours empty-world put-pattern number-of-alive-neighbours))


(define (test-new-state empty-world put-pattern new-state)
  (let* ((world1 (put-pattern (empty-world)
			      0 0
			      `[(X _ X)
				(_ _ X)
				(_ _ _)]))

	 (world2 (put-pattern (empty-world)
			      0 0
			      `[(X _ X)
				(_ X X)
				(_ _ _)]))

	 (world3 (put-pattern (empty-world)
			      0 0
			      `[(X _ X)
				(_ X X)
				(_ X _)]))

	 (world4 (put-pattern (empty-world)
			      0 0
			      `[(X _ X)
				(_ _ X)
				(_ X _)]))

	 (suite `((,world1 alive)
		  (,world2 alive)
		  (,world3 dead)
		  (,world4 dead)))

	 (assertion
	  (fold-right (lambda ((world outcome) rest)
			(and (eq? outcome
				  (new-state world 1 1))
			     rest))
		      #t
		      suite)))				 
    (if assertion
	'OK
	'FAIL)))

(show (test-new-state empty-world put-pattern new-state-at))


(define (test-next-generation empty-world
			      put-pattern
			      next-generation)

  (let (;;; BLINKER:
	(before1 (put-pattern (empty-world)
			      0 0
			      `[(_ X _)
				(_ X _)
				(_ X _)]))
	;;; this is after 1 step...
	(after1 (put-pattern (empty-world)
			      0 0
			      `[(_ _ _)
				(X X X)
				(_ _ _)]))
	;; GLIDER:
	(before2 (put-pattern (empty-world)
			      0 0
			      `[(_ _ X)
				(X _ X)
				(_ X X)]))
	;;; and this is after 2 steps!
	(after2 (put-pattern (empty-world)
			     1 0
			     `[(_ X _)
			       (_ _ X)
			       (X X X)])))
    (if (and (worlds-equiv (next-generation before1)
			   after1)
	     (worlds-equiv (next-generation (next-generation before2))
			   after2))
	'OK
	'FAIL)))

(show (test-next-generation empty-world put-pattern next-generation))

;;; boom done.
