#lang racket

(provide amb/fail-initialize amb)

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

;; Evaluates a predicate and fails the current continuation if it evaluates
;; to false
(define assert
  (λ (pred)
    (if (not pred) (amb/fail) #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXAMPLES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finds the pairs of numbers (x, y) such that x is in a and y is in b
;; and x + y = 8
(define sum-to-8
  (λ ()
    (let ([a (amb 1 2 3 4 5)]
          [b (amb 6 7 8 9 10)])
      (assert (equal? (+ a b) 8))
      (list a b))))
(bag-of (sum-to-8))
