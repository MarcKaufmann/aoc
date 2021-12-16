#lang racket/base

(require rackunit
         racket/match
         racket/port
         racket/string)

(define hex1 "D2FE28")
(define hex2 "38006F45291200")
(define hex3 "EE00D40C823060")

(define (hex->bin hx)
  (string-join
   (for/list ([c (in-string hx)])
     (match (string c)
       ["0" "0000"]
       ["1" "0001"]
       ["2" "0010"]
       ["3" "0011"]
       ["4" "0100"]
       ["5" "0101"]
       ["6" "0110"]
       ["7" "0111"]
       ["8" "1000"]
       ["9" "1001"]
       ["A" "1010"]
       ["B" "1011"]
       ["C" "1100"]
       ["D" "1101"]
       ["E" "1110"]
       ["F" "1111"]))
   ""))

(check-equal? (hex->bin hex1) "110100101111111000101000")
(check-equal? (hex->bin hex2) "00111000000000000110111101000101001010010001001000000000")
(check-equal? (hex->bin hex3) "11101110000000001101010000001100100000100011000001100000")

(define (to-bin s)
  (string->number s 2))

(check-equal? (to-bin "010") 2)

;; bs: bit-stream
(define (read-version bs)
  (to-bin (substring bs 0 3)))

(check-equal? (read-version (hex->bin hex1)) 6)
(check-equal? (read-version (hex->bin hex2)) 1)
(check-equal? (read-version (hex->bin hex3)) 7)

(define (read-type-ID bs)
  (to-bin (substring bs 3 6)))

(check-equal? (read-type-ID (hex->bin hex1)) 4)
(check-equal? (read-type-ID (hex->bin hex2)) 6)
(check-equal? (read-type-ID (hex->bin hex3)) 3)

(define (read-literal-packet bs)
  (define literals (substring bs 6))
  (define (read-literal a-bs)
    (if (string=? "0" (substring a-bs 0 1))
        ...
        (void))))

(define (read-next-packet bs)
  (define version (read-version bs))
  (define type-ID (read-type-ID bs))
  (cond [(= type-ID 4) (read-literal-packet bs)]
        [else
         (read-operator-packet bs)]))
