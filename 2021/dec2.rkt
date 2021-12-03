#lang racket

(define input-data
  (map
   string-split
   (file->lines "dec2.txt")))

(define (position-change data)
  (for/fold ([depth 0]
             [horizontal 0]
             [aim 0])
            ([move data])
    (define delta (string->number (cadr move)))
    (case (string->symbol (car move))
      ['up (values depth horizontal (- aim delta))]
      ['down (values depth horizontal (+ aim delta))]
      ['forward (values (+ depth (* aim delta)) (+ horizontal delta) aim)])))
