#lang sweet-exp lazy

define fibs
  cons 0 cons(1 map(+ fibs cdr(fibs)))

displayln list-ref(fibs 8)
