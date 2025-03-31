#lang racket

;; ramb(iguous)
;; An implementation of the `amb` opeator and other utilities to provide
;; generalized backtracking search capabilities for Racket
;;
;;`amb` (ambiguity operator) chooses one of the given options. If an option
;; (or its continuation) results in failure, amb backtracks and tries the next.
;; If all options fail, an error is raised.
;;
;; This implementation is based on the implementation described in section 14.2
;; of "Teach Yourself Scheme in Fixnum Days" by Dorai Sitaram.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RAMB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The failure continuation that gets called when the current continuation
;; fails - it is defined as a global and mutated by every branch that the search
;; takes to keep track of where to return to in case of a failure
(define amb/fail
  (λ () (error "Amb search tree exhausted")))

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

;; Essentially the same as above, except that this allows you to pass a list
;; to amb, i.e (amb/list (list 1 2 3)) will choose between the elements of the
;; list while (amb (list 1 2 3)) treats the entire list as an option
(define-syntax amb/list
  (syntax-rules ()
    [(_ opts)
     (let [(amb/fail-current amb/fail)]
       (call/cc
        (λ (sk)
          (for-each
           (λ (opt)
             (call/cc
              (λ (fk)
                (begin
                  (set!
                   amb/fail (λ ()
                              (begin
                                (set! amb/fail amb/fail-current)
                                (fk #f))))
                  (sk opt)))))
           opts)
          (amb/fail-current))))]))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UTILITIES
;; These methods make it easier to model problems using the `ramb` library
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Collects all valid solutions for an `amb` expression or upto
;; `results-requested` solutions
(define-syntax bag-of
  (syntax-rules ()
    [(_ expr)
     (bag-of expr #f)]
    [(_ expr results-requested)
     (let* ([amb/fail-current amb/fail]
            [results empty])
       (if (call/cc
            (λ (k)
              ;; Replace the entry failure continuation for expr with a call
              ;; to the else branch of this if statement
              (set! amb/fail (λ () (k #f)))
              (set! results (cons expr results))
              (if (and results-requested
                       (= (length results) results-requested))
                  (k #f)
                  (k #t))))
           (amb/fail)
           (begin
             (set! amb/fail amb/fail-current)
             (reverse results))))]))

;; Equivalent to (amb lo lo+1 ... hi). Alternatively, you could use
;; (amb/list (range lo hi+1))
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
(bag-of (solve/sum-to-8) 2)


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
           (apply hash
                  (append-map (λ (node)
                                (list node (amb/list map-colors)))
                              '(a b c d e f)))])
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
      (values node-colors))))
(bag-of (solve/map-coloring) 3)
                 

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
    (values queens)))
(bag-of (solve/8-queens) 2)
