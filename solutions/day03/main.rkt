#lang racket

(require "../../solution.rkt"
         "engine.rkt")

(define-solution 3 1 (input)
  (let ([engine (make-engine input)])
    (apply + (engine-numbers-adjacent-to-symbols engine))))

(define-solution 3 2 (input)
  (let ([engine (make-engine input)])
    (apply + (engine-gear-ratios engine))))

(module+ test
  (require rackunit)

  (check-eq?
   (solution 3 1 '("467..114.."
                   "...*......"
                   "..35..633."
                   "......#..."
                   "617*......"
                   ".....+.58."
                   "..592....."
                   "......755."
                   "...$.*...."
                   ".664.598.."))
   4361)

  (check-eq?
   (solution 3 2 '("467..114.."
                   "...*......"
                   "..35..633."
                   "......#..."
                   "617*......"
                   ".....+.58."
                   "..592....."
                   "......755."
                   "...$.*...."
                   ".664.598.."))
   467835))
