#lang sweet-exp racket

require rackunit

define-syntax-rule((foo x ...) +(x ...))

check-equal?(foo(1 2 3) 6)
