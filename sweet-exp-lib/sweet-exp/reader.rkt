#lang racket/base

(provide sweet-read
         sweet-read-syntax)

(module link-reader racket/base

  ;; This module handles the linking of units to construct
  ;; a sweet expression reader.

  (require racket/unit
           "read-sig.rkt"
           "sweet.rkt")

  (provide sweet-link)

  ;; Procedure Procedure -> Procedure Procedure
  ;; Constructs sweet readers from the given underlying readers
  (define (sweet-link read read-syntax)

    (define-unit underlying-read@
      (import)
      (export (rename read^ [-read read]
                            [-read-syntax read-syntax]))

      (define -read read)
      (define -read-syntax read-syntax))

    (define-values/invoke-unit
      (compound-unit
        (import)
        (export S)

        (link [((R : read^)) underlying-read@]
              [((S : read^)) sweet@ R]))

      (import)
      (export (prefix sweet- read^)))

    (values sweet-read sweet-read-syntax)))

(require (submod "." link-reader))

(define-values (sweet-read sweet-read-syntax)
  (sweet-link read read-syntax))

