#lang racket/base

(require racket/hash
         racket/list
         racket/match
         racket/string)

(define (cave-system [path "dec12-test.txt"])
  (define (add-directed-edge cs n1 n2)
    (hash-set cs n1
              (cons n2 (hash-ref cs n1 '() ))))
  (call-with-input-file path
    (lambda (in)
      (for/fold ([cs (hash)])
                ([edge (in-lines in)])
        (match (string-split edge "-")
          [(list node1 node2) (add-directed-edge
                               (add-directed-edge cs node1 node2)
                               node2 node1)])))))

(define (full-path? p)
  (string=? "end" (car p)))

(define (valid-path? p)
  (cond [(empty? p) #t]
        [else
         (if (string=? (car p) (string-upcase (car p)))
             (valid-path? (cdr p))
             (and (not (member (car p) (cdr p)))
                  (valid-path? (cdr p))))]))

(define (add-paths p pps cps cs)
  (define tail (car p))
  (define next-caves
    (hash-ref cs tail '() ))
  (define next-paths
    (filter valid-path?
            (map (lambda (next-cave)
                   (cons next-cave p))
                 next-caves)))
  (values
   (append (filter (compose not full-path?) next-paths) pps)
   (append (filter full-path? next-paths) cps)))

(define (part1 cs #:s [s 0])
  ; #:s exists to test in case of infinite loop
  (for/fold ([partial-paths '(("start"))]
             [complete-paths '()]
             #:result complete-paths)
            ([i (if (zero? s) (in-naturals) (range s))]
             #:break (empty? partial-paths))
    (for/fold ([new-partial-paths '()]
               [new-complete-paths complete-paths])
              ([p partial-paths])
      (add-paths p new-partial-paths new-complete-paths cs))))

(define (unique-new-small-cave-name cs)
  (string-join (map string-downcase (hash-keys cs)) ""))

(define result1 (length (part1 (cave-system "dec12.txt"))))

(define (small-cave? c)
  (and (string=? c (string-downcase c))
       (not (string=? c "end"))
       (not (string=? c "start"))))


(define (rename-unique-to u c ps)
  (for/list ([p ps])
    (map (lambda (n)
           (if (string=? n u)
               c
               n))
         p)))

(define (part2 cs #:s [s 0])
  (define unique-small (unique-new-small-cave-name cs))
  (for/fold ([all-paths '()]
             #:result (remove-duplicates all-paths))
            ([small-cave (filter small-cave? (hash-keys cs))])
    (define cs-with-edge-from-unique-cave
      (hash-set cs unique-small (hash-ref cs small-cave)))
    (define new-cs (for/fold ([new-cs cs-with-edge-from-unique-cave])
                             ([source (in-list (hash-ref cs small-cave))])
                     (hash-set new-cs source (cons unique-small
                                                   (hash-ref new-cs source)))))
    (append (rename-unique-to unique-small small-cave (part1 new-cs))
            all-paths)))
