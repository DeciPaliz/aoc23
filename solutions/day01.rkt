#lang racket/base

(require racket/list
         racket/string
         "../solution.rkt"
         "../util.rkt")

(define-solution 1 1 (input)
  (apply + (map (λ (line)
                  (let ([digits (map char->number (filter char-numeric? (string->list line)))])
                    (+ (* 10 (first digits)) (last digits))))
                input)))

(define digits-list '("1" "2" "3" "4" "5" "6" "7" "8" "9"
                      "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(define (string->digit s)
  (case s
    [("1" "one") 1]
    [("2" "two") 2]
    [("3" "three") 3]
    [("4" "four") 4]
    [("5" "five") 5]
    [("6" "six") 6]
    [("7" "seven") 7]
    [("8" "eight") 8]
    [("9" "nine") 9]
    [else (error (format "invalid digit: ~a" s))]))

(define (find-first-digit line)
  (string->digit (first
                  (sort (filter (λ (digit) (string-contains? line digit)) digits-list)
                        (λ (a b) (< (string-index-of line a) (string-index-of line b)))))))

(define (find-last-digit line)
  (string->digit (first
                  (sort (filter (λ (digit) (string-contains? line digit)) digits-list)
                        (λ (a b) (< (string-last-index-of line b) (string-last-index-of line a)))))))

(define-solution 1 2 (input)
  (apply + (map (λ (line)
                  (+ (* 10 (find-first-digit line))
                     (find-last-digit line)))
                input)))

(module+ test
  (require rackunit)

  (check-eq?
   (solution 1 1 '("1abc2"
                   "pqr3stu8vwx"
                   "a1b2c3d4e5f"
                   "treb7uchet"))
   142)

  (check-eq?
   (solution 1 2 '("two1nine"
                   "eightwothree"
                   "abcone2threexyz"
                   "xtwone3four"
                   "4nineeightseven2"
                   "zoneight234"
                   "7pqrstsixteen"))
   281))
