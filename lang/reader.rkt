#lang s-exp syntax/module-reader
#:language read
#:read -read
#:read-syntax -read-syntax

(require racket/unit
         "../read-sig.rkt"
         "../sweet.rkt")

(define-unit underlying-read@
  (import)
  (export (rename read^ [-read read]))

  (define -read read))

(define-values/invoke-unit
  (compound-unit
    (import)
    (export S)

    (link [((R : read^)) underlying-read@]
          [((S : read^)) sweet@ R]))

  (import)
  (export (prefix - read^)))

(define (-read-syntax src in)
  (define datum (-read in))
  (if (eof-object? datum)
      eof
      (datum->syntax #f datum)))
