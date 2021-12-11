#lang racket/base

(require racket/hash)

(define octos
  (call-with-input-file "dec11.txt"
    (lambda (in)
      (for/fold
          ([os (hash)])
          ([l (in-lines in)]
           [i (in-naturals)])
        (hash-union os
                    (for/hash ([c (in-string l)]
                               [j (in-naturals)])
                      (values (list i j) (string->number (string c)))))))))

(define d 10)

(define (neighbors loc)
  (define-values (i j)
    (values (car loc) (cadr loc)))
  (define potential-ngs
    (for*/list ([dy '(-1 0 1)]
                [dx '(-1 0 1)]
                #:when (not (and (= 0 dy) (= 0 dx))))
      (list (+ i dy) (+ j dx))))
  (filter (lambda (ij)
            (define-values (i j)
              (values (car ij) (cadr ij)))
            (and (>= i 0) (< i d) (>= j 0) (< j d)))
          potential-ngs))

(define (next-step octos fls)
  (define (reset-energy os fs)
    (for/fold ([os os])
              ([f fs])
      (hash-set os f 0)))
  (define (flash-em os inc-energy flashes)
    ;(displayln (list "os: " os " inc-energy: " inc-energy "   "))
    (cond [(null? inc-energy)
           (values (reset-energy os flashes)
                   (+ fls (length flashes)))]
          ;[(< steps 0) (list os inc-energy steps)]
          [else
           (define loc (car inc-energy))
           (define o (hash-ref os loc #f))
           (if (and (>= o 9) (not (member loc flashes)))
               (flash-em os
                         (append (cdr inc-energy)
                                 (neighbors loc))
                         (cons loc flashes))
               (flash-em (hash-set os loc (add1 o))
                         (cdr inc-energy)
                         flashes))]))
  (flash-em octos (hash-keys octos) '() ))

(define (show-octos octos)
  (for ([i 10])
    (for ([j 10])
      (display (hash-ref octos (list i j))))
    (displayln "")))

(define (part1 octos steps)
  (define-values (os fl)
    (for/fold ([os octos]
               [flashes 0])
              ([i steps])
      (next-step os flashes)))
  (show-octos os)
  fl)

(define (part2 octos steps)
  (for/fold ([octos octos]
             [flashes 0]
             [new-flashes 0]
             [j 0])
            ([i steps])
    #:break (= 100 new-flashes)
    (define-values (os fs) (next-step octos flashes))
    (values os fs (- fs flashes) (add1 j))))
