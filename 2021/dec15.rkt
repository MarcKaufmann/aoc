#lang racket/base

(require racket/hash)

(struct posn (x y)
  #:transparent)

(struct path (final parent dist)
  #:transparent)

(define (cavern [path "dec15.txt"])
  (call-with-input-file path
    (lambda (in)
      (for/fold ([cvn (hash)])
                ([r (in-lines in)]
                 [i (in-naturals)])
        (hash-union cvn
                    (for/hash ([c (in-string r)]
                               [j (in-naturals)])
                      (values (posn i j)
                              (string->number (string c)))))))))

(define (cavern-width cvn)
  (add1 (apply max (map (lambda (ij)
                    (posn-y ij))
                  (hash-keys cvn)))))

(define (cavern-length cvn)
  (add1 (apply max (map (lambda (ij)
                          (posn-x ij))
                        (hash-keys cvn)))))

(define (pp cvn)
  (for ([i (cavern-length cvn)])
    (for ([j (cavern-width cvn)])
      (display (hash-ref cvn (posn i j))))
    (displayln "")))

(define (add-reachable-paths-from p n reach cvn)
  (define (new-path ngb r)
    (cond [(not (hash-ref cvn ngb #f))
           r]
          [else
           (define dist-to-ngb
             (+ (hash-ref cvn ngb) (path-dist p)))
           (define new-path
             (path ngb
                   (path-final p)
                   dist-to-ngb))
           (define new-r
             (hash-set r
                     (path-dist new-path)
                     (cons new-path
                           (hash-ref r dist-to-ngb '() ))))
           ;(displayln (format "New path for ~a: path = ~a" ngb new-path))
           ;(displayln (format "New reachable (inside add): ~a" new-r))
           new-r]))
  (define source-posn
    (path-final p))
  (define neighbor-posns
    (map (lambda (dx dy)
           (posn (+ (posn-x source-posn) dx)
                 (+ (posn-y source-posn) dy)))
         '(0  0 1 -1)
         '(1 -1 0  0)))
  (for/fold ([r reach])
            ([ngb neighbor-posns])
    (new-path ngb r)))


(define (add-shortest-path p n shortest-paths)
  (hash-set shortest-paths
            (path-final p)
            p))

;(define shortest-paths
;  (hash (posn 0 0) (path (posn 0 0) '() 0 )))

(define (part2-cavern cvn)
  (define w (cavern-width cvn))
  (define l (cavern-length cvn))
  (for*/hash ([i (* 5 l)]
             [j (* 5 w)])
    (define new-value1
      (+
       (quotient i l)
       (quotient j w)
       (hash-ref
        cvn
        (posn (remainder i l)
              (remainder j w)))))
    (define new-value2
      (if (< new-value1 10)
          new-value1
          (- new-value1 9)))
    (values (posn i j) new-value2)))


(define (part1 cvn)
  (define w (cavern-width cvn))
  (define l (cavern-length cvn))
  (define-values (reachable-in-n shortest-paths)
    (for/fold ([reachable-in-n (hash 0 (list (path (posn 0 0) '() 0)))]
               [shortest-paths (hash)])
              ([n (* 9 w l)]
               #:break (member (posn (sub1 l) (sub1 w))
                               (hash-keys shortest-paths)))
      (if (not (hash-ref reachable-in-n n #f))
          (values reachable-in-n shortest-paths)
          (for/fold ([reachable reachable-in-n]
                     [shortest shortest-paths])
                    ([p (hash-ref reachable-in-n n '())])
            ;(displayln (list "reachable: " reachable-in-n " shortest-paths: " shortest-paths))
            ;(displayln (format "path: ~a" p))
            (cond [(hash-ref shortest (path-final p) #f)
                   ; There was a shorter path added already, so skip adding
                   (values reachable shortest)]
                  [else
                   ;(displayln "Got here!!")
                   (define-values (r s)
                     (values (add-reachable-paths-from p n reachable cvn)
                             (add-shortest-path p n shortest)))
                   ;(displayln (format "new reachable: ~a\n new shortes: ~a" r s))
                   (values r s)])))))
  (hash-ref shortest-paths (posn (sub1 l) (sub1 w))))

;; DO NOT RUN: Takes 2 minutes
(define (part2 cvn)
  (part1 (part2-cavern (cavern))))
