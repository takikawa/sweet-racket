#lang reader "../lang/reader.rkt"

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
