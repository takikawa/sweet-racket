#lang s-exp syntax/module-reader
#:language read
#:read *read
#:read-syntax read-syntax0

(require racket/unit
         "../read-sig.rkt"
         "../sweet.rkt")

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
  (export (prefix * read^)))

(define (read-syntax0 src in)
  (define datum (*read in))
  ;(printf "~a ~n" datum)
  (if (eof-object? datum)
      eof
      (datum->syntax #f datum)))
