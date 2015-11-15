(use-modules (srfi srfi-1)
	     (ice-9 nice-9)
	     (ice-9 match))

(define (error msg)
  (display msg)
  (newline)
  #f)
	      

;; we start with the world; it's something which answers the following questions:
;; 1) what is the state (dead/alive) of cell under given coordinate <x,y> [The Content]
;; 2) what are the coordinates of neighbours of cell under given coordinate [The Topology]

(define (empty-world) '()) ;; hehe.

(define (update-state world x y state)
  (cond ((eq? state 'alive)
	 (if (eq? (state-at world x y) 'dead)
	     `((,x ,y) . ,world)
	     world))
	((eq? state 'dead)
	 (delete `(,x ,y) world))
	(else (error `(unknown state ,state)))))

(define (state-at world x y)
  (match world
    (() 'dead) ;; empty world is totally dead.
    (((x0 y0) . world) (if (and (= x0 x) (= y0 y))
			   'alive
			   (state-at world x y)))))

#;(define (adjectant? x0 y0 x1 y1)
  (let* ((dx (abs (- x0 x1)))
	 (dy (abs (- y0 y1))))
    (and (< dx 2)
	 (< dy 2)
	 (or (= dx 1)
	     (= dy 1)))))
;;;;; hmm turns out we rather need a list of adjectant coordinates...


;;; this "standard one" assumes we're on [infinite] plane...
(define (adjectant-coordinates x y)
  `([,(- x 1) ,(- y 1)]
    [,x       ,(- y 1)]
    [,(+ x 1) ,(- y 1)]
    [,(- x 1)      ,y ]   
    [,(+ x 1)      ,y ]
    [,(- x 1) ,(+ y 1)]
    [,x       ,(+ y 1)]
    [,(+ x 1) ,(+ y 1)]))

;;; ...and here one can generate surface of torus of selected radii.
#;(define (mk-adjectant-coordinates-on-torus r1 r2)
  (lambda (x y)
  `([,(modulo (- x 1) r1) ,(modulo (- y 1) r2)]
    [,(modulo      x  r1) ,(modulo (- y 1) r2)]
    [,(modulo (+ x 1) r1) ,(modulo (- y 1) r2)]
    [,(modulo (- x 1) r1) ,(modulo      y  r2)]   
    [,(modulo (+ x 1) r1) ,(modulo      y  r2)]
    [,(modulo (- x 1) r1) ,(modulo (+ y 1) r2)]
    [,(modulo      x  r1) ,(modulo (+ y 1) r2)]
    [,(modulo (+ x 1) r1) ,(modulo (+ y 1) r2)])))
;; (you will also [if not mostly] need a toroid version of state-at).


;; we would also like to put the whole pattern of n rows x m cols,
;; denoting alive cells with 'X and dead ones with '_:
(define (put-pattern world start-x start-y pattern)
  (let rowloop ((world world)
		(pattern pattern)
		(x start-x)
		(y start-y))
    (if (null? pattern)
	world
	(let colloop ((world world)
		      (row (car pattern))
		      (x x))
	  (if (null? row)
	      (rowloop world
		       (cdr pattern)
		       start-x
		       (+ y 1))
	      (colloop (update-state world
				     x y
				     (if (eq? (car row) 'X)
					 'alive
					 'dead))
		       (cdr row)
		       (+ x 1)))))))


;; now all we care about are the states of neighbours [surely they are british];
;; in particular, we care about number of alive cells, and nothing more.

#;(define (number-of-alive-neighbours world x y)
  (let ((alive-neighbours (filter (lambda ((x0 y0))
				    (adjectant? x y x0 y0))
				  world)))
    ;;; this is [not] the best way to do this, hehe.
    (length alive-neighbours)))

;; having adjectant-coordinates (which we'll need anyway) we can do better:
(define (number-of-alive-neighbours world x y)
  (let* ((neighbour-coordinates (adjectant-coordinates x y))
	 (neighbours-mask (map (lambda ((x0 y0))
				 (if (eq? (state-at world x0 y0) 'alive)
				     1
				     0))
			       neighbour-coordinates)))
    (fold-right + 0 neighbours-mask)))
	 

;;; and now...

; Any live cell with fewer than two live neighbours dies.
; Any live cell with two or three live neighbours lives on.
; Any live cell with more than three live neighbours dies.
; Any dead cell with exactly three live neighbours becomes a live cell.

(define (new-state-at world x y)
  (let ((my-state (state-at world x y))
	(alive-count (number-of-alive-neighbours world x y)))
    (if (eq? my-state 'dead)
	(if (= alive-count 3)
	    'alive
	    'dead)
	(if (or (= alive-count 2)
		(= alive-count 3))
	    'alive
	    'dead))))

;; so all we're left to do is to compute the whole step:
;; we "only" need to check the coordinates of:
;; (a) all the alive cells
;; (b) all neighbours of alive cells.

(define (next-generation world)
  (let ((coords-to-check
	 (delete-duplicates 
	  (append-map (lambda ((x y))
			`((,x ,y) . ,(adjectant-coordinates x y)))
		      world))))
    (fold-right (lambda ((x y) rest)
		  (if (eq? (new-state-at world x y) 'alive)
		      `((,x ,y) . ,rest)
		      rest))
		'()
		coords-to-check)))

;;; the end.



		       
		      
	 
   
