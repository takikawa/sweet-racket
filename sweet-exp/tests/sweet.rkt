#lang reader (submod "../main.rkt" reader) racket

require rackunit

substring "Hello"
  1
  3

substring "Hello"
  1
  string-length("xyz")

if {7 < 5}
   {3 + 4}
   {5 * {2 + 3}}
abs{0 - 5}

let
  group
    x 5
    y 7
  {x + y}

begin-for-syntax
  define x 3
  add1 x

define (f . args) args

check-equal? (f 1 2 3) (list 1 2 3)

define (g a . args) list(a args)

check-equal? (g 1 2 3) (list 1 (list 2 3))

