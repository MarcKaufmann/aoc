#lang racket/base

(require racket/list)

(define p1-start 6)
(define p2-start 2)

(struct player (pos score)
  #:transparent)

(struct game (p1 p2)
  #:transparent)

(define (wrap-at n m)
  (add1 (remainder (sub1 n) m)))

(define (game-over? g)
  (or (>= (player-score (game-p1 g)) 1000)
      (>= (player-score (game-p2 g)) 1000)))

(define (loser-score g)
  (min (player-score (game-p1 g))
       (player-score (game-p2 g))))

(define (roll-d1)
  (let ([next-roll 1])
    (lambda ()
      (define res next-roll)
      (set! next-roll
            (+
             (remainder (add1 next-roll) 101)
             (quotient (add1 next-roll) 101)))
      res)))

(define (next-turn g d)
  (define p1 (game-p1 g))
  (define p2 (game-p2 g))
  (define rolls
    (+ (d) (d) (d)))
  (define pos1 (player-pos p1))
  (define pos2 (player-pos p2))
  (define sc1 (player-score p1))
  (define sc2 (player-score p2))
  (define new-pos
    (wrap-at (+ pos1 rolls) 10))
  (define new-p1
    (player new-pos
            (+ sc1 new-pos)))
  ;; Switch players to indicate whose turn it is.
  (game p2 new-p1))

(define (part1 p1-start p2-start)
  (define d1 (roll-d1))

  (for/fold ([g (game (player p1-start 0) (player p2-start 0))]
             [turns 0]
             #:result (* (* 3 turns) (loser-score g)))
            ([i (in-naturals)]
             #:break (game-over? g))
    (values (next-turn g d1)
            (add1 turns))))

(struct wins (p1 p2)
  #:transparent)

(define (+wins ws low)
  (cond [(empty? low) ws]
        [else
         (define next-ws (car low))
         (+wins (wins (+ (wins-p1 ws) (wins-p1 next-ws))
                           (+ (wins-p2 ws) (wins-p2 next-ws)))
                     (cdr low))]))

(define (*wins n ws)
  (wins (* n (wins-p1 ws))
        (* n (wins-p2 ws))))

(define (new-wins roll pos1 pos2 sc1 sc2 active)
  ; The `active` player rolled `roll`
  (cond [(= active 1)
         (define new-pos
           (wrap-at (+ pos1 roll) 10))
         (all-wins new-pos pos2 (- sc1 new-pos) sc2 2)]
        [(= active 2)
         (define new-pos
           (wrap-at (+ pos2 roll) 10))
         (all-wins pos1 new-pos sc1 (- sc2 new-pos) 1)]
        [else
         (error "Active player should be 1 or 2, instead received: " active)]))

(define all-wins
  (let ([mem (make-hash)])
    (lambda (pos1 pos2 sc1 sc2 active)
      (cond [(and (<= sc1 0) (<= sc2 0))
             (error "Should not get both players winning...")]
            [(<= sc1 0) (wins 1 0)]
            [(<= sc2 0) (wins 0 1)]
            [(hash-has-key? mem (list pos1 pos2 sc1 sc2 active))
             (hash-ref mem (list pos1 pos2 sc1 sc2 active))]
            [else
             (define res
               (+wins (wins 0 0)
                      (list
                       (*wins 1 (new-wins 3 pos1 pos2 sc1 sc2 active))
                       (*wins 3 (new-wins 4 pos1 pos2 sc1 sc2 active))
                       (*wins 6 (new-wins 5 pos1 pos2 sc1 sc2 active))
                       (*wins 7 (new-wins 6 pos1 pos2 sc1 sc2 active))
                       (*wins 6 (new-wins 7 pos1 pos2 sc1 sc2 active))
                       (*wins 3 (new-wins 8 pos1 pos2 sc1 sc2 active))
                       (*wins 1 (new-wins 9 pos1 pos2 sc1 sc2 active)))))
             (hash-set! mem (list pos1 pos2 sc1 sc2 active) res)
             res]))))

(define (part2)
  (all-wins 6 2 21 21 1))
