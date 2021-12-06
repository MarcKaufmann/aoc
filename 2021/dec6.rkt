#lang racket/base

(require racket/file
         racket/list
         racket/string)

(define initial-fish
  (map
   string->number
   (string-split
    (call-with-input-file "dec6.txt"
      read-line)
    ",")))

(define fishes
  (for/hash ([i 9])
    (values i (count (lambda (j) (= j i)) initial-fish))))

(define (population fishes final-day)
  (for/fold ([fs fishes]
             #:result (apply + (hash-values fs)))
            ([day final-day])
    (for/hash ([i 9])
      (values i
              (cond [(= i 8) (hash-ref fs 0)]
                    [(= i 6) (+ (hash-ref fs 0)
                                (hash-ref fs 7))]
                    [else
                     (hash-ref fs (add1 i))])))))

(define (part1)
  (population fishes 80))

(define (part2)
  (population fishes 256))
