#lang racket/base

(require racket/list
         "../../solution.rkt"
         "card.rkt")

(define-solution 4 1 (input)
  (apply + (map card-points (map make-card input))))

(define-solution 4 2 (input)
  (let ([cards (map make-card input)]
        [amounts (make-vector (length input) 1)])

    (define (inner-func i)
      (if (>= i (length cards)) amounts
          (let ([current-amount (vector-ref amounts i)]
                [current-card (list-ref cards i)])

            (for ([j (map (λ (x) (+ x 1 i)) (range (card-winning-amount current-card)))])
              (when (< j (length cards))
                (vector-set! amounts j (+ (vector-ref amounts j)
                                          current-amount))))

            (inner-func (+ i 1)))))

    (apply + (vector->list (inner-func 0)))))

(module+ test
  (require rackunit)

  (check-eq?
   (solution 4 1 '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))
   13)

  (check-eq?
   (solution 4 2 '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))
   30))
