#lang reader (submod "../main.rkt" reader) racket

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
