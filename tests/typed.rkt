#lang reader "../lang/reader.rkt" typed/racket
;#lang planet asumu/sweet typed/racket

define: fact([n : Integer]) : Integer
  if zero?(n)
     1
     {n * fact{n - 1}}

fact(5)
