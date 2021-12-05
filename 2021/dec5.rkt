#lang racket/base

(require racket/file
         racket/list
         racket/string)

(struct posn (x y)
  #:transparent)

(struct vent (start end)
  #:transparent)

(define (to-point point-string)
  (apply posn (map (compose string->number string-trim)
                   (string-split point-string ","))))

(define vent-data
  (map
   (lambda(l)
     (define points
       (string-split l "->"))
     (vent (to-point (car points))
           (to-point (cadr points))))
   (file->lines "dec5.txt")))

;; Idea: Create all the points in a list that are covered, sort the list, get the uniques, that way I know how many are double.

(define (delta-x v)
  (- (posn-x (vent-end v)) (posn-x (vent-start v))))

(define (delta-y v)
  (- (posn-y (vent-end v)) (posn-y (vent-start v))))

(define (get-all-vent-points v)
  (define dx (delta-x v))
  (define dy (delta-y v))
  (define start-x (posn-x (vent-start v)))
  (define start-y (posn-y (vent-start v)))
  (define steps (max (abs dx) (abs dy)))
  (cond [(> steps 0)
         (define step-x (quotient dx steps)) ;; Lesson: remember quotient
         (define step-y (quotient dy steps))
         (for/list ([i (add1 steps)])
           (posn (+ start-x (* i step-x))
                 (+ start-y (* i step-y))))]
        [else
         (list (vent-start v))]))

(define (collect-duplicates l)
  (define (collect-sorted lop dupes)
    (cond [(or (empty? lop) (empty? (cdr lop))) dupes]
          [(equal? (car lop) (cadr lop))
           (collect-sorted (cdr lop) (cons (car lop) dupes))]
          [else
           (collect-sorted (cdr lop) dupes)]))
  (define lsort
    (sort l
          (lambda (p1 p2)
            (or (< (posn-x p1) (posn-x p2))
                (and (= (posn-x p1) (posn-x p2))
                     (< (posn-y p1) (posn-y p2)))))))
  (collect-sorted lsort '()))

(define (vertical? v)
  (= (posn-x (vent-start v))
     (posn-x (vent-end v))))

(define (horizontal? v)
  (= (posn-y (vent-start v))
     (posn-y (vent-end v))))

(define (part1 vent-data)
  (define all-vent-points/multiples
    (flatten
     (for/list ([v vent-data]
                #:when (or (vertical? v) (horizontal? v)))
       (get-all-vent-points v))))
  (length (remove-duplicates (collect-duplicates all-vent-points/multiples ))))

(define (part2 vent-data)
  (define all-vent-points/multiples
    (flatten
     (for/list ([v vent-data])
       (get-all-vent-points v))))
  (length (remove-duplicates (collect-duplicates all-vent-points/multiples ))))
