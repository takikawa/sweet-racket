#lang reader (submod "../main.rkt" reader) typed/racket

{x : Number}
define x 5

define: fact([n : Integer]) : Integer
  if zero?(n)
     1
     {n * fact{n - 1}}

fact(5)
