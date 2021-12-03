#lang racket/base

(require racket/file racket/list)

(define diagnostics
  (map
   string->list
   (file->lines "dec3.txt")))

(define (part1 diags)
  (define c-init (map (lambda (x) 0) (car diags)))
  (define n-diagnostics (length diags))
  (define count-ones
    (for/fold ([count-ones c-init])
              ([d diags])
      (map (lambda (i c)
             (+ (if (equal? #\1 i) 1 0)
                c))
           d count-ones)))
  (displayln count-ones)
  (for/fold ([gamma 0]
             [epsilon 0]
             [mult 1]
             #:result (* gamma epsilon))
            ([c (reverse count-ones)])
    (displayln (list gamma epsilon mult n-diagnostics c))
    (if (> c (- n-diagnostics c))
        (values (+ gamma mult) epsilon (* 2 mult))
        (values gamma (+ epsilon mult) (* 2 mult)))))
