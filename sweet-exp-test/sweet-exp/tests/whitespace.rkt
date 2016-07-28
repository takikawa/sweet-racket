#lang sweet-exp racket

;; test with random strange whitespace thrown in
                         
define factorial(n)
  if {n <= 1}
     1
     {n * factorial{n - 1}}

;; tabs here
	
	
;; CR here

3
