#lang reader (submod "../main.rkt" reader) racket

require(rackunit)

define fibfast(n)   ; Typical function notation
  if {n < 2}        ; Indentation, infix {...}
     n              ; Single expr = no new list
     fibup(n 2 1 0) ; Simple function calls
    
define fibup(max count n-1 n-2)
  if {max = count}
     {n-1 + n-2}
     fibup max {count + 1} {n-1 + n-2} n-1

define factorial(n)
  if {n <= 1}
     1
     {n * factorial{n - 1}} ; f{...} => f({...})

check-equal?(fibfast(9) 34)
check-equal?(factorial(5) 120)
