#lang racket/base

(require racket/list
         racket/match
         racket/string)

(struct posn (x y z)
  #:transparent)

(define scanners
  (reverse
   (call-with-input-file "dec19.txt"
     (lambda (in)
       (define raw
         (for/list ([r (in-lines in)]
                    #:when (not (string=? "" r)))
           (cond [(regexp-match "scanner" r) "new scanner"]
                 [else
                  (apply posn (map string->number (string-split r ",")))])))

       (for/fold ([scanners '()]
                  [raw raw]
                  #:result scanners)
                 ([i (in-naturals)]
                  #:break (empty? raw))
         (define idx
           (index-of (cdr raw) "new scanner"))
         (define new-scanner
           (if idx
               (take (cdr raw) idx)
               (cdr raw)))
         (values (cons new-scanner scanners)
                 (if idx
                     (drop (cdr raw) idx)
                     '())))))))

(define ((rotate rot-axis) dir)
  ; Which direction does dir point after being rotate n times 90 degrees around rot-axis?
  ; rot-axis: 'x, 'y, or 'z
  ; dir: "+x", "+x", "+y", "-y", "+z", "-z"
  (define rot-x
    (hash "+z" "+y"
          "+y" "-z"
          "-z" "-y"
          "-y" "+z"))
  (define rot-y
    (hash "+z" "+x"
          "+x" "-z"
          "-z" "-x"
          "-x" "+z"))
  (define rot-z
    (hash "+x" "+y"
          "+y" "-x"
          "-x" "-y"
          "-y" "+x"))
  (case rot-axis
    [(x) (hash-ref rot-x dir dir)]
    [(y) (hash-ref rot-y dir dir)]
    [(z) (hash-ref rot-z dir dir)]))

(define (rotate-n rot-axis n)
  (apply compose (make-list (remainder n 4) (rotate rot-axis))))

(define all-rotations
  (remove-duplicates
   (for*/list ([ix (range 4)]
               [iy (range 4)]
               [iz (range 4)])
     (define iso
       (compose (rotate-n 'x ix) (rotate-n 'y iy) (rotate-n 'z iz)))
     (list (iso "+x") (iso "+y") (iso "+z")))))

(define ((get-val dir) p)
  (match dir
    ["+x" (posn-x p)]
    ["-x" (- (posn-x p))]
    ["+y" (posn-y p)]
    ["-y" (- (posn-y p))]
    ["+z" (posn-z p)]
    ["-z" (- (posn-z p))]))

(define (rotate-posn p rot)
  ; rot is ("+y" "-z" "-x") for instance
  ; means to create from (posn x y z) the new (posn y -z -x)
  (posn ((get-val (car rot)) p)
        ((get-val (cadr rot)) p)
        ((get-val (caddr rot)) p)))

(define (rotate-scanner sc rot)
  (for/list ([p sc])
    (rotate-posn p rot)))

(define (all-rotations-scanner sc)
  (for/list ([rot all-rotations])
    (rotate-scanner sc rot)))

(define (-posn p1 p2)
  (posn (- (posn-x p1) (posn-x p2))
        (- (posn-y p1) (posn-y p2))
        (- (posn-z p1) (posn-z p2))))

(define (+posn p1 p2)
  ; FIXME: 30 minutes debugging because I copy-pasted -posn to +posn and didn't realize it...
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))
        (+ (posn-z p1) (posn-z p2))))

(define ((in-region-around? origin) p)
  ;(displayln (format "in-region with origin: ~a and p: ~a" origin p))
  ; FIXME: I forgot abs value and chased bug for 20 minutes... Lesson: test case
  (define p0 (-posn p origin))
  (and (<= (abs (posn-x p0)) 1000)
       (<= (abs (posn-y p0)) 1000)
       (<= (abs (posn-z p0)) 1000)))

(define (keep-in-region-around p sc)
  (filter (in-region-around? p) sc))

(define (check-scanners-overlap offset sc1 sc2)
  (define overlap-sc1
    (keep-in-region-around offset sc1))
  (define overlap-sc2
    (keep-in-region-around (posn 0 0 0)
                           (for/list ([p sc2])
                             (+posn offset p))))
  (and (>= (length overlap-sc1) 12)
       (= (length overlap-sc1) (length overlap-sc2))
       (= (length overlap-sc1)
          (length (remove-duplicates (append overlap-sc1 overlap-sc2))))
       offset))

(define (compare-scanners/orientation sc1 sc2)
  (define offset
    (for*/or ([p1 sc1]
              [p2 sc2])
      (define offset (-posn p1 p2))
      (check-scanners-overlap offset sc1 sc2)))
  ;(displayln (format "Offset result: ~a " offset))
  ;(displayln "")
  (if offset
      (values sc2 offset)
      (values #f #f)))

(define (compare-scanners sc1 sc2)
  (define (new-origin o1 offset)
    (+posn o1 offset))
  (define sc1/oriented
    (hash-ref sc1 'oriented))
  (define origin1
    (hash-ref sc1 'origin))
  (define-values (matched-sc offset)
    (for/fold ([matched-sc #f]
               [offset #f])
              ([sc2/oriented (hash-ref sc2 'rotations)]
               #:break offset)
      (compare-scanners/orientation sc1/oriented sc2/oriented)))
  (if offset
      (hash 'origin (new-origin origin1 offset)
            'oriented matched-sc)
      #f))

(define (orient scanners)
  (define scanners/rot
    (for/list ([scanner (in-list scanners)])
      (hash 'rotations
            (all-rotations-scanner scanner))))

  (define scanners/init
    (list (hash 'oriented (car scanners)
                'origin (posn 0 0 0))))
  (define oriented
    (for/fold ([sc/compared '()]
             [sc/oriented scanners/init]
             [sc/not-oriented (cdr scanners/rot)]
             #:result (append sc/compared sc/oriented))
            ([_ (in-naturals)]
             #:break (or (empty? sc/not-oriented) (empty? sc/oriented)))
    (displayln "Comparing the next one...")
    (displayln (format "length of compared: ~a oriented: ~a to orient: ~a" (length sc/compared) (length sc/oriented) (length sc/not-oriented)))

    (define next-sc-to-compare (car sc/oriented))

    (define-values (sco scn)
      (for/fold ([sco '()]
                 [scn '()])
                ([sc sc/not-oriented])
        (define res
          (compare-scanners next-sc-to-compare sc))
        (cond [res
               (values (cons res sco) scn)]
              [else
               (values sco (cons sc scn))])))
    (values (cons next-sc-to-compare sc/compared)
            (append (cdr sc/oriented) sco)
            scn)))
  oriented)

(define (part1 scanners)
  (define oriented (orient scanners))
  (remove-duplicates
   (flatten
   (for/list ([sco oriented])
    (for/list ([p (hash-ref sco 'oriented)])
      (+posn p (hash-ref sco 'origin)))))))

(define (manhattan-distances scanners)
  (for*/list ([sc1 scanners]
              [sc2 scanners])
    (define diff (-posn sc1 sc2))
    (+ (abs (posn-x diff)) (abs (posn-y diff)) (abs (posn-z diff)))))

(define (part2 scanners)
  (define oriented (orient scanners))
  (apply max
         (manhattan-distances
          (for/list ([sco oriented])
            (hash-ref sco 'origin)))))
