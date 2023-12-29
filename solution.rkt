#lang racket/base

(require (for-syntax racket/base))

(provide define-solution
         solution
         solutions)

(define solutions (make-hash))

(define-syntax (define-solution stx)
  (let* ([lst (syntax->datum stx)]
         [day (list-ref lst 1)]
         [part (list-ref lst 2)]
         [arg-list (list-ref lst 3)]
         [rest (cddddr lst)])
    (datum->syntax stx
                   `(hash-set! solutions '(,day ,part) ,(append '(λ) (list arg-list) rest)))))

(define-syntax (solution stx)
  (syntax-case stx ()
    [(_solution day part input) #'(apply (hash-ref solutions (list day part)) (list input))]
    [(solution day input) #'(solution day 1 input)]))
