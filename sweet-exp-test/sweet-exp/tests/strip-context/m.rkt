#lang sweet-exp racket/base
provide m
require (for-syntax racket/base)
define-syntax (m stx)
  #'{1 + 2}
