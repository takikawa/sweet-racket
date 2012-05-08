#lang reader (submod "../main.rkt" reader) lazy

define fibs
  cons 0 cons(1 map(+ fibs cdr(fibs)))

displayln list-ref(fibs 8)
