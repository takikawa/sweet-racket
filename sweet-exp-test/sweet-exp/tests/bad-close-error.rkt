#lang racket/base

(require rackunit
         (submod sweet-exp link-reader))

(define-values (sweet-read sweet-read-syntax)
  (sweet-link read read-syntax))

(check-exn exn:fail:read?
           (λ () (sweet-read (open-input-string ")"))))

