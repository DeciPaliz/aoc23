#lang racket/base

(require srfi/13)
(provide char->number
         string-index-of
         string-last-index-of)

(define (char->number c)
  (and (char-numeric? c) (- (char->integer c) (char->integer #\0))))

(define (string-index-of str substr)
  (string-contains str substr))

(define (string-last-index-of str substr)
  (let ([res (string-contains (list->string (reverse (string->list str)))
                              (list->string (reverse (string->list substr))))])
    (if (boolean? res) #f
        (- (string-length str) res (string-length substr)))))
