#lang sweet-exp racket

require rackunit

substring "Hello"
  1
  3

substring "Hello"
  1
  string-length("xyz")

check-equal?
  substring "Hello" 1 $ string-length "xyz"
  "el"

check-equal?
  substring "Hello" 1 $ string-length("xyz")
  "el"

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

;; Check \\ grouping behavior
let  
  \\  
    a 1
      ;; This is a comment
      ;; This, too, is a comment
    b 2      
  check-equal? 
    * a b
    2

let
  group
    | | 5
    |;(]{ ,`'"# | 7
  check-equal? {| | + |;(]{ ,`'"# |} 12

(check-equal? '((a 1) (b 2) (c 3)) (list (list `a 1) (list 'b 2) (list 'c 3)))

