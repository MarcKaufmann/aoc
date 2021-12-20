#lang racket/base

(require racket/hash
         racket/list
         racket/string)

(struct posn (x y)
  #:transparent)

(struct image (top-left bottom-right content inf algo)
  #:transparent)

(define img
  (call-with-input-file "dec20.txt"
    (lambda (in)
      (define algo
        (list->vector
         (map (lambda (c)
                (if (equal? c #\.) 0 1))
              (string->list (read-line in)))))
      (read-line in)
      (for/fold ([img (hash)]
                 [h 0]
                 [w 0]
                 #:result (image (posn 0 0) (posn h w) img 0 algo))
                ([r (in-lines in)]
                 [i (in-naturals)])
        (values
         (hash-union img
                     (for/hash ([j (in-naturals)]
                                [c (in-string r)])
                       (values (posn i j) (if (equal? c #\.) 0 1))))
         i
         (sub1 (string-length r)))))))

(define (x-range img [expand 0])
  (range (- (posn-x (image-top-left img)) expand)
         (+ (add1 (posn-x (image-bottom-right img))) expand)))

(define (y-range img [expand 0])
  (range (- (posn-y (image-top-left img)) expand)
         (+ (add1 (posn-y (image-bottom-right img))) expand)))

(define (pp-img img)
  (define content (image-content img))
  (for ([i (x-range img)])
    (displayln
     (string-join
      (map (lambda (j)
             (case (hash-ref content (posn i j))
               [(0) "."]
               [(1) "#"]))
           (y-range img))
      ""))))

(define (pp-algo img)
  (define algo (image-algo img))
  (string-join
   (map (lambda (n) (if (= n 0) "." "#")) algo)
   ""))

; Need to store the light of the infinite grid as well, i.e. whether on or off.
; After two times, it should be off (else how to count?). So the last bit needs to be 0, or the first bit needs to be 0, i.e. a dot '.' . That's the case.

(define ((+posn p) i j)
  (posn (+ (posn-x p) i)
        (+ (posn-y p) j)))

(define (get-bit-string i j inf content)
  (define lop
    '((-1 -1)
      (-1 0)
      (-1 1)
      (0 -1)
      (0 0)
      (0 1)
      (1 -1)
      (1 0)
      (1 1)))
  (string-join
   (map (lambda (xy)
         (number->string
          (hash-ref content
                   (apply (+posn (posn i j)) xy)
                   inf)))
        lop)
   ""))

(define ((pixel img) i j)
  (define inf (image-inf img))
  (define content (image-content img))
  (define algo (image-algo img))
  (define code (get-bit-string i j inf content))
  (vector-ref algo (string->number code 2)))

(define (enhance img)
  (define algo
    (image-algo img))
  (define inf (image-inf img))
  (define pxl (pixel img))
  (define new-content
    (for*/hash ([i (x-range img 1)]
                [j (y-range img 1)])
      (values (posn i j) (pxl i j))))
  (define inf-code
    (string->number
     (string-join
      (map number->string (make-list 9 inf))
      "")
     2))
  (image ((+posn (image-top-left img)) -1 -1)
         ((+posn (image-bottom-right img)) 1 1)
         new-content
         (vector-ref algo inf-code)
         algo))

(define (count-lit final)
  (for*/sum ([i (x-range final)]
             [j (y-range final)])
    (hash-ref (image-content final) (posn i j))))

(define (part1 img [steps 2])
  (define final
    (for/fold ([img img])
              ([s steps])
      (enhance img)))
  (count-lit final))

(define (part2 img)
  (part1 img 50))
