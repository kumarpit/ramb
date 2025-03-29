#lang racket

(define amb/fail (λ () (error "No possible solution")))
(define-syntax amb
  (syntax-rules ()
    [(_ opt ...)
     (let [(amb/fail-current amb/fail)]
       (call/cc
        (λ (sk)
          (call/cc
           (λ (fk)
             (begin
               (set!
                amb/fail (λ ()
                           (begin
                             (set! amb/fail amb/fail-current)
                             (fk #f))))
                             
               (sk opt)))) ...
          (amb/fail-current))))]))

(amb (amb/fail) 2 3)