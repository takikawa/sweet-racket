#lang scribble/manual

@title{Sweet: an alternative to s-expressions}

This manual describes the sweet planet package, which is a port
of the @hyperlink["http://www.dwheeler.com/readable/"](sweet reader)
created by David Wheeler to Racket. In the port, you
can use the sweet reader as its own separate language.

To use sweet expressions, supply a #lang line like the following:

@codeblock{
  #lang planet asumu/sweet

  printf("Hello")
}

There are plans to modify the reader language to accept an
argument module to load so that you can use sweet expressions
with Racket, Typed Racket, or other languages.

Currently, the reader is limited because it does not read
Racket extensions to Scheme syntax such as hash literals, but
these limitations may be removed in future versions.
