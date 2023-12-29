#lang racket/base

(require racket/string)
(require racket/list)

(provide turn turn-color turn-amount string->turn
         game game-id game-turns string->game game-possible? game-power)

(struct turn (color amount))
(struct game (id turns))

(define (string->turn input)
  (let* ([match (regexp-match #px"(\\d+) (red|green|blue)" input)]
         [color (third match)]
         [amount (string->number (second match))])
    (turn color amount)))

(define (string->game input)
  (game (string->number (second (regexp-match #px"Game (\\d+)" input)))
        (map string->turn (string-split (second (string-split input ":")) #px"(,|;)"))))

(define MAX-RED 12)
(define MAX-GREEN 13)
(define MAX-BLUE 14)

(define (game-possible? game)
  (andmap (λ (turn)
            (case (turn-color turn)
              [("red") (<= (turn-amount turn) MAX-RED)]
              [("green") (<= (turn-amount turn) MAX-GREEN)]
              [("blue") (<= (turn-amount turn) MAX-BLUE)]
              [else (error (format "invalid color: ~a~%" (turn-color turn)))]))
          (game-turns game)))

(define (game-power game)
  (define (inner-func lst red green blue)
    (if (empty? lst)
        (* red green blue)
        (inner-func (rest lst)
                    (if (string=? "red" (turn-color (first lst)))
                        (max (turn-amount (first lst)) red)
                        red)
                    (if (string=? "green" (turn-color (first lst)))
                        (max (turn-amount (first lst)) green)
                        green)
                    (if (string=? "blue" (turn-color (first lst)))
                        (max (turn-amount (first lst)) blue)
                        blue))))
  (inner-func (game-turns game) 0 0 0))
