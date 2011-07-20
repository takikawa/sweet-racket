#lang s-exp syntax/module-reader
racket
#:read sweet-read
#:read-syntax sweet-read-syntax

(require "../sweet.rkt")

(define (sweet-read-syntax src in)
  (define datum (sweet-read in))
  (if (eof-object? datum)
      eof
      (datum->syntax #f datum)))
