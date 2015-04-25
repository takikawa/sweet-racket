#lang sweet-exp racket

require rackunit

define f
  case-lambda
    ()
      0

check-equal? f() 0

