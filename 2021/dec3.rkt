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

(define (part2 diags)
  (define (most-and-least-common lod idx)
    (define-values (lod1 lod0)
      (partition (lambda (d)
                   (equal? #\1 (list-ref d idx)))
                 lod))
    (if (< (length lod1) (length lod0))
        (values lod0 lod1)
        (values lod1 lod0)))
  (define-values (oxygen throw-away)
    (for/fold ([most-common-or-1 diags]
               [least-common-or-0 diags])
              ([idx (length (car diags))])
      #:break (= (length most-common-or-1) 1)
      (most-and-least-common most-common-or-1 idx)))
  (displayln oxygen)
  (define-values (throw-away2 co2)
    (for/fold ([most-common-or-1 diags]
               [least-common-or-0 diags])
              ([idx (length (car diags))])
      #:break (= (length least-common-or-0) 1)
      (most-and-least-common least-common-or-0 idx)))
  (displayln co2)
  (for/fold ([o2-decimal 0]
             [co2-decimal 0]
             [mult 1]
             #:result (* o2-decimal co2-decimal))
            ([o-bit (reverse (car oxygen))]
             [c-bit (reverse (car co2))])
    (values (+ o2-decimal (if (equal? o-bit #\1) mult 0))
            (+ co2-decimal (if (equal? c-bit #\1) mult 0))
            (* 2 mult))))
