#lang reader (submod "../main.rkt" reader) racket

(require rackunit)

{3 + 5}
'{3 + 5}
{2 + {3 * 4}}

cos(0)
substring("Hello" 1 3)
substring("Hello" 1 string-length("xyz"))
*(5 4)
not{#t and #f}

(define (f x)
  {x * x}
  ;
  )

(check-equal? (f 2) 4)
(check-equal? (f 3) 9)

(define (f2 x)
  {x * x}
  #;this(is a comment)
  )

(define (f3 x)
  {x * x}
  #|this is a comment|#
  )
