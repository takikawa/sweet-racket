#lang s-exp syntax/module-reader
#:language read
#:read -read
#:read-syntax -read-syntax

(require (rename-in "../sweet.rkt" [sweet-read -read]))

(define (-read-syntax src in)
  (define datum (-read in))
  (if (eof-object? datum)
      eof
      (datum->syntax #f datum)))
