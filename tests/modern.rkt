#lang reader (submod "../main.rkt" reader) racket

{3 + 5}
'{3 + 5}
{2 + {3 * 4}}

cos(0)
substring("Hello" 1 3)
substring("Hello" 1 string-length("xyz"))
*(5 4)
not{#t and #f}
