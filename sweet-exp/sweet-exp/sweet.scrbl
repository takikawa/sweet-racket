#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     racket/contract/base
                     sweet-exp
                     sweet-exp/reader))

@title{Sweet: an alternative to s-expressions}
@author[(author+email "Asumu Takikawa" "asumu@racket-lang.org")]

This manual describes the sweet package, a Racket port of the
@hyperlink["http://www.dwheeler.com/readable/"]{sweet reader}
by David Wheeler. This packages provides the sweet
reader as a language mixin, similar to the @racketmodname[at-exp]
or @racketmodname[s-exp] modules.

@defmodulelang[sweet-exp]

To use sweet expressions, supply a @racketfont{#lang}
line like the following:

@codeblock|{
  #lang sweet-exp racket

  printf("Hello")
}|

The third parameter on the @racketfont{#lang} line is the base language
used by the language mixin. The resulting language will use
the bindings from the base language, but support sweet
expression syntax. You can provide any language here
such as @racket[racket], @racket[typed/racket], or others.

For example:

@codeblock|{
  #lang sweet-exp typed/racket

  define: fact([n : Integer]) : Integer
    if zero?(n)
       1
       {n * fact{n - 1}}
}|

Or alternatively:

@codeblock|{
  #lang sweet-exp lazy

  define fibs
    cons 0 cons(1 map(+ fibs cdr(fibs)))

  displayln list-ref(fibs 8)
}|

Known issues: quasi-quotation combined with grouping does not
behave according to the specification.

@section{Sweet reading functions}

@defmodule[sweet-exp/reader]

@deftogether[[
  @defproc[(sweet-read [in input-port? (current-input-port)]) any]
  @defproc[(sweet-read-syntax [source-name any/c (object-name in)]
                              [in input-port? (current-input-port)])
           (or/c syntax? eof-object?)]]]{
These procedures implement the @racketmodname[sweet-exp] reader.

@examples[
  (require sweet-exp/reader)
  (sweet-read (open-input-string "printf(\"Hello\")"))
]}
