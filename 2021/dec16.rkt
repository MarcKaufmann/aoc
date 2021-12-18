#lang racket/base

(require rackunit
         racket/list
         racket/match
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

(define bin1 (hex->bin hex1))
(define bin2 (hex->bin hex2))
(define bin3 (hex->bin hex3))

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

(struct packet (version type content)
  #:transparent)

(define (read-literal-packet bs v tid)
  (define (next-literal a-bs)
    (substring a-bs 1 5))
  (define-values (lits remaining-bs _)
    (for/fold ([literals '()]
               [bs (substring bs 6)]
               [stop? #f])
              ([i (in-naturals)]
               #:break stop?)
      (define last-literal? (string=? "0" (substring bs 0 1)))
      (values (cons (next-literal bs) literals)
              (substring bs 5)
              last-literal?)))
  (values (packet v tid (string->number (string-join (reverse lits) "") 2))
          remaining-bs))

(define (read-op-bit-length a-bs)
  (string->number (substring a-bs 7 22) 2))
(define (read-op-num-subpackets a-bs)
  (string->number (substring a-bs 7 18) 2))
(define (read-op-length-type-ID a-bs)
  (substring a-bs 6 7))

(define (read-operator-packet bs v tid)
  (define ltid (read-op-length-type-ID bs))
  (cond [(string=? ltid  "0")
         (define l (read-op-bit-length bs))
         (values (packet v tid (read-packets (substring bs 22 (+ 22 l))))
                 (substring bs (+ 22 l)))]
        [else
         (define n (read-op-num-subpackets bs))
         (for/fold ([packets '()]
                    [remaining-bs (substring bs 18)]
                    #:result (values (packet v tid (reverse packets))
                                     remaining-bs))
                   ([n n])
           (define-values (next-packet rem-bs)
             (read-next-packet remaining-bs))
           (values (cons next-packet packets)
                   rem-bs))]))

(define (read-next-packet bs)
  (define version (read-version bs))
  (define type-ID (read-type-ID bs))
  (cond [(= type-ID 4)
         (read-literal-packet bs version type-ID)]
        [else
         (read-operator-packet bs version type-ID)]))

(define (bs-over? bs)
  (define (all-zeroes? a-bs)
    (for/and ([c (in-string a-bs)])
      (equal? c #\0)))
  ;; Check if we have reached the end of the bs
  (or (string=? "" bs)
      (all-zeroes? bs)))

(define (read-packets bs)
  (for/fold ([packets '()]
             [remaining-bs bs]
             #:result (reverse packets))
            ([i (in-naturals)]
             #:break (bs-over? remaining-bs))
    ;(displayln (list "packets: " packets "remaining-bs: " remaining-bs))
    (define-values (next-packet rem-bs)
      (read-next-packet remaining-bs))
    (values (cons next-packet packets)
            rem-bs)))

(define (version-sum ps)
  (for/sum ([p ps])
    (define c (packet-content p))
    (cond [(number? c) (packet-version p)]
          [else (+ (packet-version p) (version-sum c))])))

(define (part1 hexs)
  (version-sum (read-packets (hex->bin hexs))))

(check-equal? (part1 "8A004A801A8002F478") 16)
(check-equal? (part1 "620080001611562C8802118E34") 12)
(check-equal? (part1 "C0015000016115A2E0802F182340") 23)
(check-equal? (part1 "A0016C880162017C3686B18A3D4780") 31)

(define (hex-s)
  (call-with-input-file "dec16.txt"
    (lambda (in)
      (read-line in))))

(define (evaluate-packet p)
  (define (arguments p)
    (map evaluate-packet (packet-content p)))
  (case (packet-type p)
    [(0) (apply + (arguments p))]
    [(1) (apply * (arguments p))]
    [(2) (apply min (arguments p))]
    [(3) (apply max (arguments p))]
    [(4) (packet-content p)]
    [(5) (if (apply > (arguments p)) 1 0)]
    [(6) (if (apply < (arguments p)) 1 0)]
    [(7) (if (apply = (arguments p)) 1 0)]))

(define (evaluate-hex hexs)
  (evaluate-packet (car (read-packets (hex->bin hexs)))))

(check-equal? (evaluate-hex "C200B40A82") 3)
(check-equal? (evaluate-hex "04005AC33890") 54)
(check-equal? (evaluate-hex "880086C3E88112") 7)
(check-equal? (evaluate-hex "CE00C43D881120") 9)
(check-equal? (evaluate-hex "D8005AC2A8F0") 1)
(check-equal? (evaluate-hex "F600BC2D8F") 0)
(check-equal? (evaluate-hex "9C005AC2F8F0") 0)
(check-equal? (evaluate-hex "9C0141080250320F1802104A08") 1)

(define (part2)
  (evaluate-hex (hex-s)))
