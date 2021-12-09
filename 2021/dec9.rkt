#lang racket/base

(require racket/file
         racket/list
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

(define (part2 cave-map)
  (define (loc i j)
    (vector-ref (vector-ref cave-map i) j))
  (define w (vector-length (vector-ref cave-map 1)))
  (define h (vector-length cave-map))
  (define (is-low-point? i j)
    (< (loc i j) (min (if (> i 0) (loc (sub1 i) j) 10)
                      (if (< i (sub1 h)) (loc (add1 i) j) 10)
                      (if (> j 0) (loc i (sub1 j)) 10)
                      (if (< j (sub1 w)) (loc i (add1 j)) 10))))
  (define low-points
    (for*/fold ([low-points '()])
               ([i h]
                [j w])
      (if (is-low-point? i j)
          (cons (list i j) low-points)
          low-points)))
  ;(displayln low-points)
  (define (in-map? ij)
    (define-values (i j) (values (car ij) (cadr ij)))
    ;(displayln (list "i: in-map" i))
    (and (> i -1) (> j -1) (< i h) (< j w)))
  (define (in-basin? ij)
    (< (loc (car ij) (cadr ij)) 9))
  (define (visited? ij)
    (vector-ref (vector-ref cave-map-visited (car ij)) (cadr ij)))
  (define (neighbours ij)
    ;(displayln (list "ij: " ij))
    (define-values (i j) (values (car ij) (cadr ij)))
    ;(displayln (list "i: " i))
    (filter (lambda (ij)
              (and (in-map? ij) (in-basin? ij) (not (visited? ij))))
            (list (list (sub1 i) j)
                  (list (add1 i) j)
                  (list i (sub1 j))
                  (list i (add1 j)))))

  (define cave-map-visited
    (for/vector ([i h])
      (make-vector w #f)))
  (define (visit! ij)
    ;(displayln (list "ns: " ns))
    (define-values (i j) (values (car ij) (cadr ij)))
    (vector-set! (vector-ref cave-map-visited i) j  #t))
  (define (get-basin ij)
    ; returns the size of the basin starting at (i, j) and changes cave-map-visited
    (define (visit-basin next-visit size)
      ;(displayln (list "next-visit: " next-visit "size: " size))
      (cond [(null? next-visit) size]
            [else
             (define ng (neighbours (car next-visit)))
             ;(displayln (list "neigh: " ng "visiting ngs of " (car next-visit)))
             (visit! (car next-visit))
             (visit-basin (remove-duplicates
                           (append (cdr next-visit) ng))
                          (add1 size))]))
    (visit-basin (list ij) 0))
  (apply *
         (take
          (sort (for/list ([ij low-points])
    ;(displayln "=======")
                  (get-basin ij))
                >)
          3)))

; Make a new map of basin-labels for the cave-map
; Initially set to nothing (or -1). Go through all. If not yet in a basin, start a new basin there and then fill the basin by walking to all neighbours and labeling them to, while counting the size of the basin. Then repeat (after storing size of basin).
