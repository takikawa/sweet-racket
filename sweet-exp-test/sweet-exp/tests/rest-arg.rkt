#lang sweet-exp racket

require rackunit

define thrush(. fs)
  apply compose reverse(fs)

check-equal?(thrush(+ add1)(3) 4)

define
  thrush2 . fs
  apply compose
    reverse fs

check-equal?
  thrush(+ add1) 3
  4

