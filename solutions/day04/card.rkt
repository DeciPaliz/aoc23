#lang racket/base

(require racket/list)
(require racket/string)

(provide make-card card-points card-winning-amount)

(struct card (id first-part second-part))

(define (make-card str)
  (let* ([id (string->number (second (regexp-match #px"Card.*(\\d+)" str)))]
         [split (string-trim (second (string-split str ":")))]
         [first-split (string-trim (first (string-split split "|")))]
         [second-split (string-trim (second (string-split split "|")))]
         [first-part (map string->number (string-split first-split))]
         [second-part (map string->number (string-split second-split))])
    (card id first-part second-part)))

(define (card-winning-numbers card)
  (filter (λ (n) (member n (card-first-part card))) (card-second-part card)))

(define (card-winning-amount card)
  (length (card-winning-numbers card)))

(define (card-points card)
  (if (> (card-winning-amount card) 0)
      (expt 2 (- (card-winning-amount card) 1))
      0))
