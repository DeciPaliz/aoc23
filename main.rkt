#lang racket/base

(require racket/cmdline
         racket/file
         racket/string
         reprovide/require-transformer/glob-in
         "solution.rkt")

(require (glob-in "solutions/*.rkt")
         (glob-in "solutions/**/main.rkt"))

(command-line
 #:program "aoc23"
 #:args (day [part "1"])
 (let* ([input-file (format "input/day~a.txt"
                            (if (= 1 (string-length day))
                                (format "0~a" day)
                                day))]
        [day (string->number day)]
        [part (string->number part)]
        [input (filter non-empty-string? (file->lines input-file))])
   (solution day part input)))
