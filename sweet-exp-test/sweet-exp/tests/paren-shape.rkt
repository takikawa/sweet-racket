#lang sweet-exp racket/base

require rackunit
        for-syntax racket/base
                   racket/match

define-syntax get-paren-shape
  lambda (stx)
    #`#,(or (syntax-property stx 'paren-shape) #\( )

check-equal? (get-paren-shape) #\(
check-equal? [get-paren-shape] #\[
check-equal? get-paren-shape() #\(
check-equal? get-paren-shape[] #\[
