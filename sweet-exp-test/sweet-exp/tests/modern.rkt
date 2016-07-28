#lang sweet-exp racket

(require rackunit)

(check-equal? {3} 3)
(check-equal? {3 + 5} 8)
(check-equal? '{3 + 5} '(+ 3 5))
(check-equal? {2 + {3 * 4}} 14)

(check-equal? cos(0) 1)
(check-equal? substring("Hello" 1 3) "el")
(check-equal? substring("Hello" 1 string-length("xyz")) "el")
(check-equal? *(5 4) 20)
(check-equal? not{#t and #f} #t)

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

(check-equal? (syntax->datum #'{1 + 2}) '(+ 1 2))
(check-equal? (syntax->datum #`#,{1 + 2}) 3)
(check-equal? (syntax->datum #`(map #,@'{'(1 2) + '(3 4)})) '(map + '(1 2) '(3 4)))
(check-equal? (syntax->datum #'f(x)) '(f x))

(check-equal? (map . {'(1 2) + '(3 4)}) '(4 6))

