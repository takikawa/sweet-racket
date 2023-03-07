#lang racket/base

(require rackunit
         sweet-exp/reader)

(check-exn exn:fail:read?
           (λ () (sweet-read (open-input-string ")"))))

