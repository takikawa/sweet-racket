#lang sweet-exp racket
(require "srcloc-helper.rkt")
srcloc-helper
  stx
    (I-am-a
      And-I-am-b
      f(x)
      g{y + z})
  stx2
    I-am-a
      And-I-am-b
      f(x)
      g{y + z}
  ;
  #:----------------------------------------------------------------
  ;
  (match-define (list a b c d) (syntax-e stx))
  (match-define (cons f xs) (syntax-e c))
  (match-define (list x) (syntax-e xs))
  (match-define (list g y+z) (syntax-e d))
  (match-define (list + y z) (syntax->list y+z))
  ;
  (match-define (list a2 b2 c2 d2) (syntax-e stx2))
  (match-define (cons f2 xs2) (syntax-e c2))
  (match-define (list x2) (syntax-e xs2))
  (match-define (list g2 y+z2) (syntax-e d2))
  (match-define (list +-2 y2 z2) (syntax->list y+z2))
  ;
  ;(highlight-stuff stx a b c d f xs x g y+z + y z
  ;                 stx2 a2 b2 c2 d2 f2 xs2 x2 g2 y+z2 +-2 y2 z2)
  ;
  (chk-srclocs
   [stx (srcloc _ 5 4 78 51)]
   [a   (srcloc _ 5 5 79 6)]
   [b   (srcloc _ 6 6 92 10)]
   [c   (srcloc _ 7 6 109 4)]
   [d   (srcloc _ 8 6 120 8)]
   [stx2 (srcloc _ 10 4 141 _)]
   [a2   (srcloc _ 10 4 141 6)]
   [b2   (srcloc _ 11 6 154 10)]
   [c2   (srcloc _ 12 6 171 4)]
   [d2   (srcloc _ 13 6 182 8)]
   )

