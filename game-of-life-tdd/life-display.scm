;;; just a display loop... and press any key to quit.
(use-modules (ncurses curses))
(include "life.scm")

;;; start with ``B-heptomino''...
(define (init-world)
  (put-pattern (empty-world)
	       66 33
	       '[(_ X _ _)
		 (X X X _)
		 (X _ X X)]))


(define (disp-area scr x y w h world)
  (map (lambda (dy)
	 (map (lambda (dx)	
		(addstr scr
			(if (eq? (state-at world (+ x dx) (+ y dy)) 'alive)
			    "0"
			    " ")
			#:y dy
			#:x dx))
	      (iota w)))
       (iota h)))


(let* ((scr (initscr))
       (sizes (getmaxyx scr))
       (h (car sizes))
       (w (cadr sizes)))
  (halfdelay! 1)
  (let loop ((world (init-world))
	     (x 0)
	     (y 0))
    (clear scr)
    (disp-area scr x y w h world)
    (refresh scr)
    (if (getch scr) ;; TODO perhaps add some moving of x,y?
	(begin
	  (endwin)
	  (quit))
	(loop (next-generation world) x y))))
