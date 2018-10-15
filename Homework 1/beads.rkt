#lang racket

(define racket-factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (racket-factorial
               (- n 1))))))

(let ([args (vector->list(current-command-line-arguments))])
    (if (not (equal? (length args) 2))
        (displayln "Usage: racket bracelet.rkt <Num_beads> <Num_colors>")
       	(let ([b (string->number(car args))] [c (string->number(car(cdr args)))])        
		(begin0  
		  (display "The number of bracelets with the given parameters is ")
		  (display (/ (racket-factorial b) (* (racket-factorial (abs (- b c)))(racket-factorial c))))
          )
          )
        )
    )