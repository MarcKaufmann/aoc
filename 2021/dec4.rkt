#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/vector)

(define bingo-input
  (file->lines "dec4.txt"))

(define nums
  (map
   string->number
   (string-split (car bingo-input) ",")))

(define bingo-list
  (cdr bingo-input))

(struct bingo (card [markers #:mutable])
  #:transparent)

(define (get-cards bl)
  (define (to-bingo l)
    (bingo
     (list->vector
      (map
       string->number
       (string-split (string-join l))))
     (make-vector 25)))    
  (cond [(< (length bl) 4) '()]
        [else
         (cons
          (to-bingo (take bl 6))
          (get-cards (drop bl 6)))]))

(define bingo-cards
  (get-cards bingo-list))

(define (won? a-bingo)
  (define (are-all-marked? lom)
    (= 5 (apply + (map (lambda (i)
                         (vector-ref (bingo-markers a-bingo) (sub1 i)))
                       lom))))
  (ormap are-all-marked?
         '(
           (1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25) ; check all rows
                       (1 6 11 16 21) (2 7 12 17 22) (3 8 13 18 23) (4 9 14 19 24) (5 10 15 20 25) ; check all columns
                       )))

(define (reset-markers bc)
  (vector-fill! (bingo-markers bc) 0))

(define (reset-all-markers lobc)
  (for ([bc lobc])
    (reset-markers bc)))

; Can't I just mutate the markers on a card?
(define (call-number/bingo n a-bingo)
  (define idx (vector-member n (bingo-card a-bingo)))
  (when idx
    (vector-set! (bingo-markers a-bingo) idx 1)))

(define (compute-score bc n)
  (define (card-score bc)
    (apply +
           (vector->list
            (vector-map (lambda (v i)
                          (* v (- 1 i)))
                        (bingo-card bc)
                        (bingo-markers bc)))))
  (* n (card-score bc)))

(define (part1 nums bingo-cards)
  (reset-all-markers bingo-cards)
  (for*/or ([n nums]
            [bc bingo-cards])
    (call-number/bingo n bc)
    (if (won? bc) (list (compute-score bc n) n bc) #f)))

(define (part2 nums bingo-cards)
  (reset-all-markers bingo-cards)
  (for/fold ([max-idx-to-win -1]
             [called-on-win -1]
             [longest-card '()]
             #:result (compute-score longest-card called-on-win))
            ([bc bingo-cards])
           (define won-on
             (for/or ([n nums])
               (call-number/bingo n bc)
               (if (won? bc) n #f)))
           (define idx-of-win (index-of nums won-on))
           (if (> idx-of-win max-idx-to-win)
               (values idx-of-win won-on bc)
               (values max-idx-to-win called-on-win longest-card))))
               
               
