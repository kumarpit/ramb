#lang racket

;; `amb` (ambiguity operator) chooses one of the given options.
;; If an option (or its continuation) results in failure, amb backtracks and
;; tries the next. If all options fail, an error is raised.

;; This implementation is provides a 2 continuation model of backtracking, i.e
;; it implements backtracking using a "success" and a "failure" continuation.

;; The failure continuation that gets reset when all options are exhausted. This
;; may be set to different values during the search.
(define amb/fail #f)

(define amb/fail-initialize
  (λ ()
    (set! amb/fail (λ () (error "Amb search tree exhausted")))))

(amb/fail-initialize)

(define-syntax amb
  (syntax-rules ()
    [(_ opt ...)
     ;; Save the failure continuation at time of entry
     (let [(amb/fail-current amb/fail)]
       (call/cc
        ;; Captures the success continuation (i.e amb's entry continuation)
        (λ (sk)
          (call/cc
           (λ (fk) ;; Captures the failure continuation
             (begin
               (set!
                ;; Reset amb/fail to the failure continuation stored earlier
                ;; and try the next option
                amb/fail (λ ()
                           (begin
                             (set! amb/fail amb/fail-current)
                             (fk #f))))

               ;; Try each option left-to-right (chronological backtracking)
               (sk opt)))) ...
          ;; Trigger failure if all options are exhausted
          (amb/fail-current))))]))

(define-syntax amb/list
  (syntax-rules ()
    [(_ opt ...)
     ;; Save the failure continuation at time of entry
     (let [(amb/fail-current amb/fail)]
       (call/cc
        ;; Captures the success continuation (i.e amb's entry continuation)
        (λ (sk)
          (call/cc
           (λ (fk) ;; Captures the failure continuation
             (begin
               (set!
                ;; Reset amb/fail to the failure continuation stored earlier
                ;; and try the next option
                amb/fail (λ ()
                           (begin
                             (set! amb/fail amb/fail-current)
                             (fk #f))))

               ;; Try each option left-to-right (chronological backtracking)
               (sk opt)))) ...
          ;; Trigger failure if all options are exhausted
          (amb/fail-current))))]))


;; Collects all valid solutions for an `amb` expression
(define-syntax bag-of
  (syntax-rules ()
    [(_ expr)
     (let* ([amb/fail-current amb/fail]
            [results empty])
       (if (call/cc
            (λ (k)
              ;; Replace the entry failure continuation for expr with a call
              ;; to the else branch of this if statement
              (set! amb/fail (λ () (k #f)))
              (set! results (cons expr results))
              (k #t))) ;; Recursion!
           (amb/fail)
           (begin
             (set! amb/fail amb/fail-current)
             (reverse results))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UTILITIES
;; These methods make it easier to model problems using the `ramb` library
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Equivalent to (amb lo ... hi) with a step-size of 1
(define number-between
  (λ (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))

;; Evaluates a predicate and fails the current continuation if it evaluates
;; to false
(define assert
  (λ (pred)
    (if (not pred) (amb/fail) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXAMPLES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finds the pairs of numbers (x, y) such that x is in a and y is in b
;; and x + y = 8
(define solve/sum-to-8
  (λ ()
    (let ([a (amb 1 2 3 4 5)]
          [b (amb 6 7 8 9 10)])
      (assert (equal? (+ a b) 8))
      (list a b))))
(bag-of (solve/sum-to-8))


;; Solving the map-coloring problem described here:
;; https://www.metalevel.at/prolog/optimization
(define map-colors (list 'red 'green 'blue 'yellow))
(define adjacency-list
  (hash 'a '(b c d f)
        'b '(a c d)
        'c '(a b d e)
        'd '(a b c e f)
        'e '(c d f)
        'f '(a d e)))

(define solve/map-coloring
  (λ ()
    (let ([node-colors
           (hash 'a (amb/list map-colors)
                 'b (amb/list map-colors)
                 'c (amb/list map-colors)
                 'd (amb/list map-colors)
                 'e (amb/list map-colors)
                 'f (amb/list map-colors))])
      (assert (andmap
               (λ (kv)
                 (let* ([node (car kv)]
                        [node-color (cdr kv)]
                        [neighbour-colors
                         (map
                          (λ (neighbour)
                            (hash-ref node-colors neighbour))
                          (hash-ref adjacency-list node))])
                   (not (member node-color neighbour-colors)))) 
               (hash->list node-colors)))
      (displayln node-colors))))
(solve/map-coloring)
                 

;; Solving the eight queens problem
(define solve/8-queens
  (λ ()
    ;; only assign columns since the row assignments are implicit
    ;; (i.e must be 1, 2, 3,..., 8)
    (define queens (build-list 8 (λ (_) (number-between 1 8))))
    (assert (and
             ;; check for same rows
             (equal? (length queens) (length (remove-duplicates queens)))
             ;; check for same diagonals
             (andmap (λ (i)
                       (andmap (λ (j)
                                 (not
                                  (=
                                   (abs
                                    (- (list-ref queens i)
                                       (list-ref queens j)))
                                   (abs (- i j)))))
                               (range i)))
                     (range (length queens)))))
    (displayln queens)))
(solve/8-queens)
