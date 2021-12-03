#lang racket

(require 2htdp/batch-io)

(define input
  (map string->number (read-lines "dec1.txt")))

(define (count-increasing in)
  (apply + (map (lambda (x y)
                  (if (< x y) 1 0))
                (reverse (cdr (reverse in)))
                (cdr in))))

(define (count-increasing-windows in)
  (define rin (reverse in))
  (define l1 (reverse (cddr rin)))
  (define l2 (cdr (reverse (cdr rin))))
  (define l3 (cddr in))
  #;(displayln (list l1 l2 l3))
  (count-increasing
   (map (lambda (x y z) (+ x y z))
        l1 l2 l3)))
