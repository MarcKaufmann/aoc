#lang racket/base

(require racket/list
         racket/match)

(define code
  (call-with-input-file "dec10.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (for/list ([c (in-string line)])
          (string c))))))

(define corrupt-string
  (list "{" "(" "}"))

(define (is-corrupted? cs)
  (define (top-correct? stack c)
    (and (not (empty? stack))
         (string=? (car stack) c)))
  (for/fold ([stack '()]
             [first-mistake ""])
            ([c cs])
    #:break (not (string=? first-mistake ""))
    ;(displayln (list "push or pop c: " c " onto stack : " stack "  "))
    (match c
      ["(" (values (cons ")" stack) first-mistake)]
      ["[" (values (cons "]" stack) first-mistake)]
      ["{" (values (cons "}" stack) first-mistake)]
      ["<" (values (cons ">" stack) first-mistake)]
      [")" (if (top-correct? stack ")") (values (cdr stack) first-mistake) (values stack ")"))]
      ["]" (if (top-correct? stack "]") (values (cdr stack) first-mistake) (values stack "]"))]
      ["}" (if (top-correct? stack "}") (values (cdr stack) first-mistake) (values stack "}"))]
      [">" (if (top-correct? stack ">") (values (cdr stack) first-mistake) (values stack ">"))])))

(define (score c)
  (match c
    ["" 0]
    [")" 3]
    ["]" 57]
    ["}" 1197]
    [">" 25137]))

(define (part1 code)
  (for/sum ([cs code])
    (define-values (stack r) (is-corrupted? cs))
    (displayln (list r (score r)))
    (if r (score r) 0)))

(define (completion-score st)
  (for/fold ([sc 0])
            ([n st])
    (define pts (match n
                  [")" 1]
                  ["]" 2]
                  ["}" 3]
                  [">" 4]))
    (+ pts (* sc 5))))

(define (part2 code)
  (define scores
    (for/fold ([scores '()])
              ([cs code])
      (define-values (stack r) (is-corrupted? cs))
      (if (string=? r "")
          (cons (completion-score stack) scores)
          scores)))
  (list-ref (sort scores >) (quotient (length scores) 2)))
