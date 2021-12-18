#lang racket/base

(require racket/port)

; TODO: How does Racket parse strings, i.e. could I just search and replace [] by () and ',' by ' ' and then let Racket do the parsing?
(struct sn (left right)
  #:transparent)

(define (read-left-sn in)
  ; Assume all numbers are single digits
  (define c (string (peek-char in)))
  (cond [(string->number c)
         => (lambda (x)
              (read-char in)
              x)]
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
              x)]
        [else
         (define rn (read-sn in))
         (read-closing-brackets in)
         rn]))

(define (string->sn s)
  (call-with-input-string
   s
   read-sn))

(define (read-sn in)
  (define c1 (read-char in))
  (unless (equal? c1 #\[)
    (error "Expected a '[' to start a new snail-number, instead got" c1))
  (define l (read-left-sn in))
  (define c2 (read-char in))
  (unless (equal? c2 #\,)
    (error "Expected a ',' to separate the left from the right of the snail number, instead got " c2))
  (define r (read-right-sn in))
  (sn l r))

(define homework
  (call-with-input-file "dec18.txt"
    (lambda (in)
      (for/list ([r (in-lines in)])
        (string->sn r)))))
