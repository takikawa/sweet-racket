#lang racket

(provide (all-defined-out))

(define current-source-name (make-parameter #f))

;; A syntax object that has the "original?" property
;; (borrowed from the scribble reader)
(define orig-stx (read-syntax #f (open-input-string "dummy")))

;; utility for construction syntaxes
(define (make-stx v ln col pos span)
  (datum->syntax #f v 
    (vector (current-source-name) ln col pos span)
    orig-stx))

(define (port-pos in)
  (define-values (_1 _2 pos) (port-next-location in))
  pos)

