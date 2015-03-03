#lang racket/base

(require rackunit
         (submod "../main.rkt" link-reader))

(define-values (sweet-read sweet-read-syntax)
  (sweet-link read read-syntax))

(check-exn exn:fail:read?
           (λ () (sweet-read (open-input-string ")"))))

