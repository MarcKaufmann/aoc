#lang racket

(define-values (temp rules)
  (call-with-input-file "dec14.txt"
    (lambda (in)
      (define temp (read-line in))
      (read-line in)
      (define rules
        (for/hash ([l (in-lines in)])
          (define insert-rule
            (string-split l " -> "))
          (values (car insert-rule) (cadr insert-rule))))
      (values temp rules))))

(define (next-step temp rules)
  (for/fold ([new-temp (substring temp 0 1)])
            ([i (sub1 (string-length temp))])
    (define p (substring temp i (+ i 2)))
    (string-append new-temp (hash-ref rules p) (substring p 1 2))))

(define (part1 temp rules [step 10])
  (define final-temp
    (for/fold ([current-temp temp])
              ([i step])
      (next-step current-temp rules)))
  (define frequencies
      (for/fold ([freq (hash)])
                ([c (in-string final-temp)])
        (hash-set freq (string c)
                  (add1 (hash-ref freq (string c) 0)))))
  (define sorted-letters
    (sort
     (hash->list frequencies)
     (lambda (p1 p2)
       (> (cdr p1) (cdr p2)))))
  (- (cdr (first sorted-letters)) (cdr (last sorted-letters))))

(define (next-step/fast freq rules)
  (define (inc-hash h np p)
    (hash-set h np (+ (hash-ref freq p 0)
                      (hash-ref h np 0))))
  (for/fold ([h (hash)])
            ([pair+next-pairs (in-hash-pairs rules)])
    (define-values (pair next-pairs)
      (values (car pair+next-pairs) (cdr pair+next-pairs)))
    (inc-hash (inc-hash h (car next-pairs) pair)
              (cadr next-pairs) pair)))

(define (pair-frequencies temp)
  (for/fold ([freq (hash)])
            ([i (sub1 (string-length temp))])
    (let ([pair (substring temp i (+ i 2))])
      (hash-set freq pair
                (add1 (hash-ref freq pair 0))))))

(define (transformed-rules rules)
  (for/hash ([pair+insert (in-hash-pairs rules)])
    (define-values (pair insert)
      (values (car pair+insert) (cdr pair+insert)))
    (values pair (list (string-append (substring pair 0 1) insert)
                       (string-append insert (substring pair 1 2))))))

(define (part2 temp rules [step 10])
  (define first-letter (substring temp 0 1))
  (define last-letter (substring temp
                                 (sub1 (string-length temp))))
  (define pair-freqs
    (for/fold ([freq (pair-frequencies temp)])
              ([i step])
      (next-step/fast freq (transformed-rules rules))))
  (define (hash+ h k inc)
    (hash-set h k (+ (hash-ref h k 0) inc)))
  (displayln pair-freqs)
  (define double-count-freqs
    (for/fold ([letter-freqs (hash first-letter 1 last-letter 1)])
              ([ps (in-hash-keys pair-freqs)])
      (define first-letter (substring ps 0 1))
      (define second-letter (substring ps 1 2))
      (hash+ (hash+ letter-freqs first-letter (hash-ref pair-freqs ps 0))
             second-letter
             (hash-ref pair-freqs ps 0))))
  (displayln (list "double: " double-count-freqs))
  (define sorted-letters
    (sort
     (hash->list double-count-freqs)
     (lambda (p1 p2)
       (> (cdr p1) (cdr p2)))))
  (quotient (- (cdr (first sorted-letters)) (cdr (last sorted-letters))) 2))
