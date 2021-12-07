#lang racket/base

(require racket/list
         racket/string
         math/statistics)

(define crabs-data
  (call-with-input-file "dec7.txt"
    (lambda (in)
      (sort
       (map
        string->number
        (string-split
         (read-line in) ","))
       <))))

; Lesson 0: work out solution on paper, I almost missed that we just should get the median.
(define (part1 cbs)
  (define l (length cbs))
  (define minpos
    (list-ref cbs (quotient l 2)))
  (for/sum ([cb cbs])
    (abs (- cb minpos))))

(define test-data
  (list 16 1 2 0 4 2 7 1 2 14))

;; Lesson 1: I got this function wrong, because I had not broken it out and checked it did the rigth thing
(define (fuel-cost cb posn)
  (quotient (* (abs (- cb posn)) (add1 (abs (- cb posn))))
            2))

;; Lesson 2: I wasted time coming up with a clever solution, then went for this inefficient garbage anyway
;; Start with simple and correct, then go faster.
(define (part2 cbs)
  (define (full-fuel-cost posn)
    (for/sum ([cb cbs])
      (fuel-cost cb posn)))
  (apply min
         (map
          full-fuel-cost
          (range (add1 (last cbs))))))

;; What is the fast way to get the least-square average?
;; ... I could just have taken the average of the crabs. I almost realized it, then got confused how to compute a linear regression. Now realized that the average is the one that minimizes least squares. Wow. That was slow.
;; Lesson 3: work on piece of paper first with baby example.
(define (part2b cbs)
  (define (full-fuel-cost posn)
    (for/sum ([cb cbs])
      (fuel-cost cb posn)))
  (define avg (mean cbs))
  (min (full-fuel-cost (floor avg)) (full-fuel-cost (ceiling avg))))
