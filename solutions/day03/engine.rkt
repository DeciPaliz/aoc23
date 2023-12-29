#lang racket/base

(require racket/list)
(require racket/set)

(require "../../util.rkt")

(provide make-engine engine-numbers-adjacent-to-symbols engine-gear-ratios)

(struct pos (x y))

(define (pos-inside-rect? a-pos b-pos b-width)
  (and (= (pos-y a-pos) (pos-y b-pos))
       (>= (pos-x a-pos) (pos-x b-pos))
       (< (pos-x a-pos) (+ (pos-x b-pos) b-width))))

(define (pos-rect position width)
  (define res '())
  (for ([y (range 3)])
    (for ([x (range (+ width 2))])
      (let ([x (+ (pos-x position) x -1)]
            [y (+ (pos-y position) y -1)])
        (unless (or (< y 0)
                    (< x 0)
                    (and (pos-inside-rect? (pos x y) position width)))
          (set! res (append res (list (pos x y))))))))
  res)

(define (pos-bounded-rect position width engine-width engine-height)
  (filter (λ (p) (and (< (pos-x p) engine-width)
                      (< (pos-y p) engine-height)))
          (pos-rect position width)))

(define (pos-box position width height)
  (pos-bounded-rect position 1 width height))

(struct entry (value pos))

(define (number-width n)
  (define (inner-func num width)
    (if (zero? num) width
        (inner-func (floor (/ num 10)) (+ width 1))))
  (inner-func n 0))

(define (entry-width entry)
  (number-width (entry-value entry)))

(define (entry-positions entry)
  (define (inner-func lst x)
    (if (>= x (+ (pos-x (entry-pos entry)) (entry-width entry))) lst
        (inner-func (append lst (list (pos x (pos-y (entry-pos entry))))) (+ x 1))))
  (inner-func '() (pos-x (entry-pos entry))))

(define (entry-adjacent-positions entry width height)
  (let ([entry-positions (entry-positions entry)])
    (filter (λ (p)
              (not (member p entry-positions)))
            (pos-bounded-rect (entry-pos entry) (entry-width entry) width height))))

(struct engine (entries numbers-map characters-map gears width height))

(define (2dvector-set! vec row col val)
  (vector-set! (vector-ref vec row) col val))

(define (2dvector-ref vec row col)
  (vector-ref (vector-ref vec row) col))

(define (make-engine input)
  (let* ([height (length input)]
         [width (string-length (first input))]
         [entries '()]
         [numbers-map (build-vector height (λ (row) (make-vector width #f)))]
         [characters-map (build-vector height (λ (row) (make-vector width #f)))]
         [gears (mutable-set)])

    (for ([y (range height)])
      (define last-number #f)
      (define (add-last-number x)
        (when last-number
          (let ([index (length entries)]
                [new-entry (entry last-number (pos (- x (number-width last-number)) y))])
            (set! entries (append entries (list new-entry)))
            (for ([p (entry-positions new-entry)])
              (2dvector-set! numbers-map (pos-y p) (pos-x p) index)))
          (set! last-number #f)))

      (for ([x (range width)])
        (let ([c (string-ref (list-ref input y) x)])
          (if (char-numeric? c)
              (if last-number (set! last-number (+ (* last-number 10) (char->number c)))
                  (set! last-number (char->number c)))
              
              (begin
                (add-last-number x)
                (unless (char=? c #\.)
                  (when (char=? c #\*) (set-add! gears (pos x y)))
                  (2dvector-set! characters-map y x c))))))

      (add-last-number width))

    (engine entries numbers-map characters-map gears width height)))

(define (engine-entry-has-adjacent-symbols? engine entry)
  (ormap (λ (p) (2dvector-ref (engine-characters-map engine) (pos-y p) (pos-x p)))
         (entry-adjacent-positions entry (engine-width engine) (engine-height engine))))

(define (engine-numbers-adjacent-to-symbols engine)
  (map entry-value
       (filter (λ (entry) (engine-entry-has-adjacent-symbols? engine entry))
               (engine-entries engine))))

(define (engine-get-adjacent-numbers engine pos)
  (set->list (list->set
              (filter (λ (x) x)
                      (map (λ (p) (2dvector-ref (engine-numbers-map engine) (pos-y p) (pos-x p)))
                           (pos-box pos (engine-width engine) (engine-height engine)))))))

(define (engine-gear-ratios engine)
  (map (λ (numbers) (* (entry-value (list-ref (engine-entries engine) (first numbers)))
                       (entry-value (list-ref (engine-entries engine) (second numbers)))))
       (filter (λ (numbers) (= (length numbers) 2))
               (map (λ (gear) (engine-get-adjacent-numbers engine gear))
                    (set->list (engine-gears engine))))))
