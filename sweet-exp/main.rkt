#lang racket/base

;; Sweet expressions

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

(module language-info racket/base
  (provide get-language-info)
  (define (get-language-info data)
    (lambda (key default)
      (case key
        [(configure-runtime)
         '(#((submod sweet-exp runtime-config) configure #f))]
        [else default]))))

(module runtime-config racket/base

  ;; This module provides runtime configuration, e.g., for
  ;; using sweet-exp based modules in DrRacket

  (require (submod ".." link-reader))

  (provide configure)

  (define (configure data)
    (define-values (_ sweet-read-syntax)
      (sweet-link read read-syntax))
    (current-read-interaction sweet-read-syntax)))

(module reader syntax/module-reader
  #:language read
  #:read sweet-read
  #:read-syntax sweet-read-syntax
  #:language-info #((submod sweet-exp language-info) get-language-info #f)

  (require (submod ".." link-reader))
  (define-values (sweet-read sweet-read-syntax)
    (sweet-link read read-syntax)))
