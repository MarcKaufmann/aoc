#lang racket

(require racket/list
         racket/file
         racket/string)

(struct entry (signals output)
  #:transparent)

(define display-data
  (map
   (λ (l)
     (define input
       (string-split l "|"))
     (entry
      (string-split (car input))
      (string-split (cadr input))))
   (file->lines
    "dec8.txt")))

(define (is-1478? digit-signals)
  (let ([l (string-length digit-signals)])
    (ormap = (list l l l l) '(2 3 4 7))))

(define (part1 display-entries)
  (length
   (filter is-1478?
           (flatten
            (map entry-output display-entries)))))

(define all-signals
  (string->list "abcdefg"))

(define (signals-for-digit-with-length l ss)
  (string->list
   (findf (lambda (x)
            (= l (string-length x)))
          ss)))

(define (deduce-signals ss)
  (define all-char-occurrences
    (string->list (string-join ss "")))
  (define 2-signals
    (signals-for-digit-with-length 2 ss))
  (define 3-signals
    (signals-for-digit-with-length 3 ss))
  (define 4-signals
    (signals-for-digit-with-length 4 ss))
  (define 8-signals
    (signals-for-digit-with-length 7 ss))
  (for/hash ([c all-signals])
    (define freq
      (count (lambda (s)
               (char=? s c))
             all-char-occurrences))
    (cond [(= freq 4) (values c #\e)]
          [(= freq 9) (values c #\f)]
          [(= freq 6) (values c #\b)]
          [(and (= freq 8)
                (member c 2-signals))
           (values c #\c)]
          [(= freq 8) (values c #\a)]
          [(and (= freq 7)
                (member c 4-signals))
           (values c #\d)]
          [(= freq 7) (values c #\g)])))

(define (signals->digit ss signal-hash)
  (define actual-signals
    (list->string
     (sort
      (map
       (lambda (s)
         (hash-ref signal-hash s))
       (string->list ss))
      char-ci<?)))
  (displayln (format "match: initial ~a; actual ~a" ss actual-signals))
  (match actual-signals
    ["abcefg" 0]
    ["cf" 1]
    ["acdeg" 2]
    ["acdfg" 3]
    ["bcdf" 4]
    ["abdfg" 5]
    ["abdefg" 6]
    ["acf" 7]
    ["abcdefg" 8]
    ["abcdfg" 9]))

(define (part2 display-entries)
  (for/sum ([e display-entries])
    (displayln e)
    (define signal-hash
      (deduce-signals (entry-signals e)))
    (displayln signal-hash)
    (define display-number
      (for/sum ([mult '(1000 100 10 1)]
                [s (entry-output e)])
        (* mult (signals->digit s signal-hash))))
    display-number))

(define test
  (take display-data 2))

(define test-output
  (list "cdfeb" "fcadb" "cdfeb" "cdbaf"))

(define test-signals
  '("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"))

(define test-entry
  (entry test-signals test-output))
