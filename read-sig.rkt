#lang racket/base

(require racket/unit)
(provide read^)

(define-signature read^ 
  (read read-syntax))
