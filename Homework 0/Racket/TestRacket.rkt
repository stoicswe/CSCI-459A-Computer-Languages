#lang racket

(let ([args (current-command-line-arguments)])
    (if (< (vector-length args) 1)
        (displayln "No arguments given")
        (begin0
          (displayln "Your arguments are:")
          (map (lambda (i a) (printf "~a: ~s\n" i a))
               (stream->list (in-range (vector-length args)))
               (vector->list args))
          )
        )
    )