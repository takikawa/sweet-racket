#lang sweet-exp racket

require rackunit syntax/parse

define
  foo
    #:a a
    #:b b
  hash 'a a 'b b

check-equal?
  foo
    #:a 1
    #:b 2
  hash 'a 1 'b 2

check-equal?
  foo
    #:a let ()
          define x 1
          x
    #:b 2
  hash 'a 1 'b 2

check-equal?
  syntax->datum
    syntax-parse #'(1 2 3)
      (a ...)
        #:with sum
        #'(+ a ...)
        #'(define x sum)
  '(define x (+ 1 2 3))

define-syntax-rule
  bar [#:a a]
  a

check-equal?
  bar
    [#:a 1]
  1

