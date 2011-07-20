#lang reader "../lang/reader.rkt" typed/racket

{x : Number}
define(x 5)

{fact : (Integer -> Integer)}
define fact(n)
  if zero?(n)
     1
     {n * fact{n - 1}}
