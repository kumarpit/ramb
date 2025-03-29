#lang racket

;; `amb` (ambiguity operator) chooses one of the given options.
;; If an option (or its continuation) results in failure, amb backtracks and
;; tries the next. If all options fail, an error is raised.

;; This implementation is provides a 2 continuation model of backtracking, i.e
;; it implements backtracking using a "success" and a "failure" continuation.

;; The failure continuation that gets reset when all options are exhausted. This
;; may be set to different values during the search.
(define amb/fail (λ () (error "Amb search tree exhausted")))

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
                ;; Reset amb/fail to the failure continuation  stored earlier
                ;; and try the next option
                amb/fail (λ ()
                           (begin
                             (set! amb/fail amb/fail-current)
                             (fk #f))))

               ;; Try each option left-to-right (chronological backtracking)
               (sk opt)))) ...
          ;; Trigger failure if all options are exhausted
          (amb/fail-current))))]))