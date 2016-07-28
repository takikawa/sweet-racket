#lang sweet-exp racket

require rackunit syntax/parse

define
  foo #:c c d
    #:a a
    #:b b
  hash 'a a 'b b 'c c 'd d

check-equal?
  foo #:c 3 4
    #:a 1
    #:b 2
  hash 'a 1 'b 2 'c 3 'd 4

check-equal?
  foo #:c 3 4
    #:a let ()
          define x 1
          x
    #:b 2
  hash 'a 1 'b 2 'c 3 'd 4

check-equal?
  syntax->datum
    syntax-parse #'(1 2 3)
      (a ...)
        #:with sum
        #'(+ a ...)
        #'(define x sum)
  '(define x (+ 1 2 3))

define-syntax-rule
  bar [#:a a]
  a

check-equal?
  bar
    [#:a 1]
  1

define
  foo2 #:a a b
  hash 'a a 'b b

check-equal?
  foo2 #:a 1 2
  hash 'a 1 'b 2

struct posn (x y) #:transparent #:extra-constructor-name make-posn

define p posn(1 2)

check-pred posn? p
check-pred posn? make-posn(3 4)
check-equal? posn-x(p) 1
check-equal? posn-y(p) 2

