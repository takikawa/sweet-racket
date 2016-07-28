#lang setup/infotab

(define collection 'multi)

(define deps
  '("base"
    "sweet-exp-lib"
    "sweet-exp-test"
    "scribble-lib"
    "racket-doc"
    "scribble-doc"
    "lazy"
    ))

(define implies
  '("sweet-exp-lib"
    "sweet-exp-test"
    ))

(define update-implies
  '("sweet-exp-lib"
    "sweet-exp-test"
    ))

