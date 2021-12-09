#lang racket/base

(require racket/file
         racket/string)

(define (string->vector s)
  (list->vector
   (map
    (compose (lambda (x) (- x 48)) char->integer)
    (string->list s))))

(define cave-map
  (list->vector
   (map
    string->vector
    (file->lines "dec9.txt"))))

(define (part1 cave-map)
  (define (loc i j)
    (vector-ref (vector-ref cave-map i) j))
  (define w (vector-length (vector-ref cave-map 1)))
  (define h (vector-length cave-map))
  (define (is-low-point? i j)
    (< (loc i j) (min (if (> i 0) (loc (sub1 i) j) 10)
                      (if (< i (sub1 h)) (loc (add1 i) j) 10)
                      (if (> j 0) (loc i (sub1 j)) 10)
                      (if (< j (sub1 w)) (loc i (add1 j)) 10))))
  (for*/sum ([i h]
             [j w])
    (if (is-low-point? i j)
        (add1 (loc i j))
        0)))

; Make a new map of basin-labels for the cave-map
; Initially set to nothing (or -1). Go through all. If not yet in a basin, start a new basin there and then fill the basin by walking to all neighbours and labeling them to, while counting the size of the basin. Then repeat (after storing size of basin).
