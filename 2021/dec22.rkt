#lang racket/base

(require racket/list
         racket/string)

(struct interval (low high)
  #:transparent)

(struct cuboid (rx ry rz [status #:mutable])
  #:transparent)

(define (->coords s)
  (define nums
    (string-split (cadr (string-split s "=")) ".."))
  (values (string->number (car nums))
          (string->number (cadr nums))))

(define (string->cuboid s)
  (define status+coords
    (string-split s " "))
  (define status
    (if (string=? (car status+coords) "on") 1 0))
  (define coords (string-split (cadr status+coords) ","))
  (define-values (x X) (->coords (car coords)))
  (define-values (y Y) (->coords (cadr coords)))
  (define-values (z Z) (->coords (caddr coords)))
  (unless (and (<= x X) (<= y Y) (<= z Z))
    (error "Expected input to be in increasing order"))
  (cuboid (interval x X) (interval y Y) (interval z Z) status))

(define cuboids
  (call-with-input-file "dec22.txt"
    (lambda (in)
      (for/list ([r (in-lines in)])
        (string->cuboid r)))))

(define (interval-length r)
  (add1 (- (interval-high r) (interval-low r))))

(define (cuboid-volume c)
  (* (interval-length (cuboid-rx c))
     (interval-length (cuboid-ry c))
     (interval-length (cuboid-rz c))))

(define (count-cubes-on loc)
  ; Assumes loc contains non-overlapping cubes only
  (for/sum ([c loc]
            #:when (= (cuboid-status c) 1))
    (cuboid-volume c)))

(define (intersect-range? r1 r2)
  (and (not (> (interval-low r1) (interval-high r2)))
       (not (> (interval-low r2) (interval-high r1)))))

(define ((intersect-range/xyz? get-range) c1 c2)
  (intersect-range? (get-range c1) (get-range c2)))

(define intersect-x?
  (intersect-range/xyz? cuboid-rx))
(define intersect-y?
  (intersect-range/xyz? cuboid-ry))
(define intersect-z?
  (intersect-range/xyz? cuboid-rz))

(define (intersect? c1 c2)
  (and (intersect-x? c1 c2)
       (intersect-y? c1 c2)
       (intersect-z? c1 c2)))

(define (break-up-x c1 c2)
  (interval-break-up (cuboid-rx c1) (cuboid-rx c2)))

(define (break-up-y c1 c2)
  (interval-break-up (cuboid-ry c1) (cuboid-ry c2)))

(define (break-up-z c1 c2)
  (interval-break-up (cuboid-rz c1) (cuboid-rz c2)))

(define (interval-break-up r1 r2)
  (define-values (low1 high1 low2 high2)
    (values (interval-low r1) (interval-high r1)
            (interval-low r2) (interval-high r2)))

  (cond [(and (<= low2 low1) (<= high1 high2))
         (list (interval low1 high1))]
        [(and (< low1 low2) (<= high1 high2))
         (list (interval low1 (sub1 low2))
               (interval low2 high1))]
        [(and (<= low2 low1) (< high2 high1))
         (list (interval low1 high2)
               (interval (add1 high2) high1))]
        [(and (< low1 low2) (< high2 high1))
         (list (interval low1 (sub1 low2))
               (interval low2 high2)
               (interval (add1 high2) high1))]
        [else
         (error "Logically hard to get here, I would have thought...")]))

(define (break-up c1 c2)
  ; break up the first cube into a list of non-overlapping cubes, so that each
  ; part is a cuboid and fully does or does not overlap with c2 update status of
  ; the part that overlaps with c2 to the status of c2 FIXME: The breakup is
  ; dumb as it splits existing cube into up to 27 cubes, even though I could get
  ; away with 6. E.g. cut into cuboids where x-dimension does not overlap at all
  ; and fully (at most three), and two of these regions need not be cut again --
  ; which I do do now. On the other hand, it's fast enough anyway, so...
  (filter (lambda (c)
            (not (intersect? c c2)))
          (for*/list ([rx (break-up-x c1 c2)]
                      [ry (break-up-y c1 c2)]
                      [rz (break-up-z c1 c2)])
            (define newc
              (cuboid rx ry rz (cuboid-status c1)))
            newc)))

(define (add-cube-to-list new-cube loc)
  (for/fold ([newloc (list new-cube)])
            ([c loc])
    (cond [(not (intersect? c new-cube))
           (cons c newloc)]
          [else
           (append (break-up c new-cube) newloc)])))

(define c1 (cuboid (interval 0 0) (interval 2 4) (interval -5 2) 0))
(define c2 (cuboid (interval -3 0) (interval 0 2) (interval -1 1) 1))

(define (range-smaller-than-50? r)
  (and (<= (abs (interval-low r)) 50)
       (<= (abs (interval-high r)) 50)))

(define (smaller-than-50? c)
  (and (range-smaller-than-50? (cuboid-rx c))
       (range-smaller-than-50? (cuboid-ry c))
       (range-smaller-than-50? (cuboid-rz c))))

(define (count-cuboids cuboids #:part1? (part1? #t))
  (define cubes
    (if part1?
        (filter smaller-than-50? cuboids)
        cuboids))
  (for/fold ([loc '()]
             #:result (count-cubes-on loc))
            ([nc cubes])
    (displayln "Adding new cube...")
    (add-cube-to-list nc loc)))

(define (part1 cuboids)
  (count-cuboids cuboids))

(define (part2 cuboids)
  (count-cuboids cuboids #:part1? #f))
