#lang racket/base

(require racket/list
         racket/match
         racket/string)

(struct posn (x y)
  #:transparent)

(define (dots-and-folds [path "dec13-test.txt"])
  (call-with-input-file path
    (lambda (in)
      (define dots
        (for/list ([r (in-lines in)]
                   #:break (string=? r ""))
          (match (string-split r ",")
            [(list x y) (posn (string->number x) (string->number y))])))
      (define folds
        (for/list ([r (in-lines in)])
          (match (string-split (caddr (string-split r " ")) "=")
            [(list axis n) (list (string->symbol axis) (string->number n))])))
      (values dots folds))))

(define-values (test-dots test-folds)
  (dots-and-folds))

(define-values (dots folds)
  (dots-and-folds "dec13.txt"))

(define (reflect-x xf d)
  (define x (posn-x d))
  (define y (posn-y d))
  (if (> x xf)
      (posn (- xf (- x xf))
            y)
      d))
(define (reflect-y yf d)
  (define x (posn-x d))
  (define y (posn-y d))
  (if (> y yf)
      (posn x
            (- yf (- y yf)))
      d))
(define (reflect fld)
  (lambda (d)
    (match fld
      [(list 'x x) (reflect-x x d)]
      [(list 'y y) (reflect-y y d)])))

(define (part1 dots folds)
  (define first-fold (car folds))
  (length (remove-duplicates (map (reflect first-fold) dots))))

(define (pp dots folds)
  (define y-folds (filter (lambda (fld)
                            (equal? (car fld) 'y))
                          folds))
  (define x-folds (filter (lambda (fld)
                            (equal? (car fld) 'x))
                          folds))
  (define xmax (apply min (map cadr x-folds)))
  (define ymax (apply min (map cadr y-folds)))
  (define posns (for/hash ([p dots])
                  (values p #t)))
  (for ([j ymax])
    (displayln "")
    (for ([i xmax])
      (if (hash-ref posns (posn i j) #f)
          (display "#")
          (display ".")))))

(define (part2 dots folds)
  (for/fold ([ds dots])
            ([fld folds])
    (remove-duplicates (map (reflect fld) ds))))

; Read off the answer from:
(pp (part2 dots folds) folds)
