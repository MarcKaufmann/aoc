#lang racket

(require racket/list)

(define input "target area: x=155..182, y=-117..-67")

(define xmin 155)
(define xmax 182)
(define ymin -117)
(define ymax -67)

(define smax 300)

(define xrange
  (let ([rs (make-hash)])
    (lambda (vx)
      (cond [(hash-has-key? rs vx) (hash-ref rs vx)]
            [else
             (define new-range
               (for/list ([s (range 1 (add1 vx))])
                 (for/sum ([x (range vx (- vx s) -1)])
                   x)))
             (hash-set! rs vx new-range)
             new-range]))))

(define yrange
  (let ([rs (make-hash)])
    (lambda (vy)
      (cond [(hash-has-key? rs vy) (hash-ref rs vy)]
            [else
             (define new-range
               (for/list ([s (range 1 (add1 smax))])
                 (for/sum ([y (range vy (- vy s) -1)])
                   y)))
             (hash-set! rs vy new-range)
             new-range]))))

(define ((hit-x/steps xmin xmax) s)
  ; hit the range in exactly s steps
  (filter (lambda (vx)
            (define xr (xrange vx))
            (hits-target? xmin xmax
                          (if (< (length xr) s)
                              (last xr)
                              (list-ref xr (sub1 s)))))
          (range 1 (add1 xmax))))

(define ((hit-y/steps ymin ymax) s)
  ; hit the range in exactly s steps
  (filter (lambda (vy)
            (define yr (yrange vy))
            (hits-target? ymin ymax
                          (if (< (length yr) s)
                              (last yr)
                              (list-ref yr (sub1 s)))))
          (range (- (abs ymin)) (add1 (abs ymin)))))

(define (part2 (xmin xmin) (xmax xmax) (ymin ymin) (ymax ymax))
  (for/fold ([aims '()]
             #:result (remove-duplicates aims))
            ([s (range 1 (add1 smax))])
    (append
     (for*/list ([vx ((hit-x/steps xmin xmax) s)]
                 [vy ((hit-y/steps ymin ymax) s)])
       (list vx vy))
     aims)))

;; Clearly initial velocity has to be less than xmax, so just go through all the possibilities for those x's

; Find the largest step size to hit the target, since highest y is exactly proportional to the latest time y hits 0, and still hits the target
(define (hits-target? l h v)
  (<= l v h))

