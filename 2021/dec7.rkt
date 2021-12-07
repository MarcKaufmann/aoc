#lang racket/base

(require racket/list
         racket/string)

(define crabs-data
  (call-with-input-file "dec7.txt"
    (lambda (in)
      (sort
       (map
        string->number
        (string-split
         (read-line in) ","))
       <))))

(define (part1 cbs)
  (define l (length cbs))
  (define minpos
    (list-ref cbs (quotient l 2)))
  (for/sum ([cb cbs])
    (abs (- cb minpos))))

(define test-data
  (list 16 1 2 0 4 2 7 1 2 14))

(define (fuel-cost cb posn)
  (quotient (* (abs (- cb posn)) (add1 (abs (- cb posn))))
            2))

(define (part2 cbs)
  (define (full-fuel-cost posn)
    (for/sum ([cb cbs])
      (fuel-cost cb posn)))
  (apply min
         (map
          full-fuel-cost
          (range (add1 (last cbs))))))
