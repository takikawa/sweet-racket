#lang scribble/doc

@(require scribble/manual (for-label sweet))

@title{Sweet: an alternative to s-expressions}
@author[(author+email "Asumu Takikawa" "asumu@racket-lang.org")]

This manual describes the sweet package, a Racket port of the
@hyperlink["http://www.dwheeler.com/readable/"]{sweet reader}
by David Wheeler. This packages provides the sweet
reader as a language mixin.

@defmodulelang[sweet]

To use sweet expressions, supply a @racketfont{#lang}
line like the following:

@codeblock|{
  #lang sweet racket

  printf("Hello")
}|

The third parameter on the @racketfont{#lang} line is the base language
used by the language mixin. The resulting language will use
the bindings from the base language, but support sweet
expression syntax. You can provide any language here
such as @racket[racket], @racket[typed/racket], or others.

For example:

@codeblock|{
  #lang sweet typed/racket

  define: fact([n : Integer]) : Integer
    if zero?(n)
       1
       {n * fact{n - 1}}
}|

Or alternatively:

@codeblock|{
  #lang sweet lazy

  define fibs
    cons 0 cons(1 map(+ fibs cdr(fibs)))

  displayln list-ref(fibs 8)
}|

Known issues: quasi-quotation combined with grouping does not
behave according to the specification.
