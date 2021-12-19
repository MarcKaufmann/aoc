#lang racket/base

(require racket/list
         racket/port
         racket/pretty)

; TODO: How does Racket parse strings, i.e. could I just search and replace [] by () and ',' by ' ' and then let Racket do the parsing?
(struct regular (value parent)
  #:transparent
  #:mutable)

(struct sn (left right parent)
  #:transparent
  #:mutable)

(define (read-left-sn in)
  ; Assume all numbers are single digits
  (define c (string (peek-char in)))
  (cond [(string->number c)
         => (lambda (x)
              (read-char in)
              (regular x #f ))]
        [else
         (read-sn in)]))

(define (read-closing-brackets in)
  (for ([_ (in-naturals)]
        #:break (not (equal? (peek-char in) #\])))
    (read-char in)))

(define (read-right-sn in)
  (define c (string (peek-char in)))
  (cond [(string->number c)
         => (lambda (x)
              (read-char in)
              (read-closing-brackets in)
              (regular x #f ))]
        [else
         (define rn (read-sn in))
         (read-closing-brackets in)
         rn]))

(define (string->sn s)
  (call-with-input-string
   s
   read-sn))

(define (set-parent! x p)
  (cond [(regular? x) (set-regular-parent! x p)]
        [(sn? x) (set-sn-parent! x p)]))

(define (read-sn in)
  (define c1 (read-char in))
  (unless (equal? c1 #\[)
    (error "Expected a '[' to start a new snail-number, instead got" c1))
  (define l (read-left-sn in))
  (define c2 (read-char in))
  (unless (equal? c2 #\,)
    (error "Expected a ',' to separate the left from the right of the snail number, instead got " c2))
  (define r (read-right-sn in))
  (define root (sn l r #f ))
  (set-parent! r root)
  (set-parent! l root)
  root)

(define homework
  (call-with-input-file "dec18-test.txt"
    (lambda (in)
      (for/list ([r (in-lines in)])
        (string->sn r)))))

(define (get-self-setter/value! sn-or-r)
  (define parent
    (cond [(regular? sn-or-r) (regular-parent sn-or-r)]
          [(sn? sn-or-r) (sn-parent sn-or-r)]
          [else (error "Should receive regular or sn, received " sn-or-r)]))
  (if (eq? (sn-left parent) sn-or-r)
      (lambda (v) (set-sn-left! parent (regular v parent)))
      (lambda (v) (set-sn-right! parent (regular v parent)))))

(define (get-self-setter/sn! sn-or-r)
  (define parent
    (cond [(regular? sn-or-r) (regular-parent sn-or-r)]
          [(sn? sn-or-r) (sn-parent sn-or-r)]
          [else (error "Should receive regular or sn, received " sn-or-r)]))
  (if (eq? (sn-left parent) sn-or-r)
      (lambda (v)
        (set-parent! v parent)
        (set-sn-left! parent v))
      (lambda (v)
        (set-parent! v parent)
        (set-sn-right! parent v))))


(define (findf-for-explode n d)
  (cond [(regular? n) #f]
        [(sn? n)
         (cond [(and (regular? (sn-left n))
                     (regular? (sn-right n))
                     (>= d 4))
                n]
               [else
                (or (findf-for-explode (sn-left n) (add1 d))
                    (findf-for-explode (sn-right n) (add1 d)))])]
        [else
         (error "findf-for-explode: Expected `regular?` or `sn?`, instead received " n)]))

(define (findf-and-explode n)
  (define explode-this (findf-for-explode n 0))
  (cond [explode-this
         (log n)
         (explode explode-this n)
         (log n)
         #t]
        [else #f]))

(define (get-parent n)
  (if (regular? n)
      (regular-parent n)
      (sn-parent n)))

(define (climb-up/previous n)
  ; returns #f if there is no previous regular value
  (define p (get-parent n))
  (cond [(not p) #f]
        [(eq? (sn-right p) n) (sn-left p)]
        [else
         (climb-up/previous p)]))

(define (get-right-most n)
  (cond [(regular? n) n]
        [else
         (get-right-most (sn-right n))]))

(define (get-left-most n)
  (cond [(regular? n) n]
        [else
         (get-left-most (sn-left n))]))

(define (previous-regular n)
  (define pr (climb-up/previous n))
  (cond [(not pr) #f]
        [(regular? pr) pr]
        [(sn? pr) (get-right-most pr)]
        [else
         (error "Should receive #f or sn?, instead received " pr)]))

(define (climb-up/next n)
  (define p (get-parent n))
  (cond [(not p) #f]
        [(eq? (sn-left p) n) (sn-right p)]
        [else
         (climb-up/next p)]))

(define (next-regular n)
  (define nr (climb-up/next n))
  (cond [(not nr) #f]
        [(regular? nr) nr]
        [(sn? nr) (get-left-most nr)]
        [else
         (error "Should receive #f or sn?, instead received " nr)]))

(define (log n [mess ""])
  ;(displayln (format "Log: ~a : ~a" mess (if (sn? n) (pp-sn n) n)))
  (void))

(define (explode n full)
  (define l (regular-value (sn-left n)))
  (define r (regular-value (sn-right n)))
  (define prev (previous-regular n))
  (define next (next-regular n))
  (log full "Before changing prev")
  ;(displayln n)
  (when prev
    ((get-self-setter/value! prev) (+ (regular-value prev) l)))
  (log full "After changin prev, before next")
  (log next "Next is: ")
  (when next
    ((get-self-setter/value! next) (+ (regular-value next) r)))
  (log full "after next, before setting whole to 0")
  ((get-self-setter/value! n) 0)
  (log full "after setting whole to 0")
  #t)

(define (findf-and-split n)
  (define (split)
    (define v (regular-value n))
    (define p (regular-parent n))
    (define self-setter (get-self-setter/sn! n))
    (self-setter
     (make-sn-pair
      (quotient v 2)
      (+ (quotient v 2) (remainder v 2)))))

  (cond [(regular? n)
         (if (> (regular-value n) 9)
             (split)
             #f)]
        [else
         (or (findf-and-split (sn-left n) )
             (findf-and-split (sn-right n) ))]))

(define (reduce-sn! n)
  (define error-no-parent-setter
    (lambda (v) (error "Should not be called at top-level number")))
  (define (next-reduction)
    (or (findf-and-explode n)
        (findf-and-split n)))
  (let ([nr (next-reduction)])
    (log n)
    (if nr
        (reduce-sn! n)
        n)))

(define (add-sn sn1 sn2)
  (define new
    (sn sn1 sn2 #f))
  (set-parent! sn1 new)
  (set-parent! sn2 new)
  new)

(define (make-sn-pair n1 n2)
  (define r1 (regular n1 #f))
  (define r2 (regular n2 #f))
  (define t1 (sn r1 r2 #f))
  (set-parent! r1 t1)
  (set-parent! r2 t1)
  t1)

(define sn1 (string->sn "[[[[4,3],4],4],[7,[[8,4],9]]]"))
(define sn2 (make-sn-pair 1 1))
(define sn3 (add-sn sn1 sn2))

(define (magnitude n)
  (cond [(regular? n) (regular-value n)]
        [(+ (* 3 (magnitude (sn-left n)))
            (* 2 (magnitude (sn-right n))))]))

(define (part1 hw)
  (for/fold ([res (car hw)])
            ([n (cdr hw)])
    (reduce-sn! (add-sn res n))))

(define t1 (make-sn-pair 10 1))
(define t2 (string->sn "[[[[[1,2],3],4],5],6]"))

(define (pp-sn n)
  (cond [(regular? n) (regular-value n)]
        [else
         (format "[~a,~a]" (pp-sn (sn-left n)) (pp-sn (sn-right n)))]))

(define sn4 (string->sn "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"))

(define (part2)
  (define all-strings
    (call-with-input-file "dec18.txt"
      (lambda (in)
        (for/list ([r (in-lines in)])
          r))))
  (define res
    (for*/list ([r1 all-strings]
              [r2 all-strings]
              #:unless (string=? r1 r2))
    (define n1 (string->sn r1))
    (define n2 (string->sn r2))
    (magnitude (reduce-sn! (add-sn n1 n2)))))
  (apply max res))
