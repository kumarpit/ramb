#lang racket

(define amb/fail (λ () (error "No possible solution")))
(define-syntax amb
  (syntax-rules ()
    [(_ opts ...)
     (let [(amb/fail-current amb/fail)]
       (call/cc
        (λ (sk)
          (begin
            (for-each (λ (opt)
                        (call/cc
                         (λ (fk)
                           (begin
                             (set! amb/fail (λ ()
                                              (begin
                                                (set! amb/fail amb/fail-current)
                                                (fk #f))))
                             
                             (sk opt)))))
                      (list opts ...))
            (amb/fail-current)))))]))

(amb (amb) 2 3)