Sweet expressions for Racket
============================

[![Build Status](https://travis-ci.org/takikawa/sweet-racket.svg?branch=master)](https://travis-ci.org/takikawa/sweet-racket)

This package provides a port of the [sweet
expression](http://readable.sourceforge.net/) reader, originally written by
David Wheeler, to [Racket](http://www.racket-lang.org).

To use the package, you have several options.

* With Racket 5.3.2 or later, you can install the package using `raco pkg`:
  - `raco pkg install sweet-exp`
  - set your language to `#lang sweet-exp <your-base-language>`

* You can also use Planet 2 package management manually:
  - `git clone git://github.com/takikawa/sweet-racket.git`
  - `raco pkg install sweet-racket/`

* With Racket 5.3.1 and earlier, you can use the
  [Planet package](http://planet.racket-lang.org/display.ss?package=sweet.plt&owner=asumu).
  - set your language to `#lang planet asumu/sweet <your-base-language>`

Note: the check syntax tool does work with this language, but
it may fail to activate when you first install it from PLaneT.
Changing the #lang line and then trying again should cause the
check syntax button to appear.

This package is released under the MIT license under the
same terms as the original implementation.

* * *

The following is an excerpt from the Scribble manual for this package. The
package provides the sweet reader as a language mixin, similar to the `at-exp`
or `s-exp` modules.

```racket
 #lang sweet-exp
```

To use sweet expressions, supply a `#lang` line like the following:

```racket
#lang sweet-exp racket

printf("Hello")
```

The third parameter on the `#lang` line is the base language used by the
language mixin. The resulting language will use the bindings from the
base language, but support sweet expression syntax. You can provide any
language here such as `racket`, `typed/racket`, or others.

For example:

```racket
#lang sweet-exp typed/racket

define: fact([n : Integer]) : Integer
  if zero?(n)
     1
     {n * fact{n - 1}}
```

Or alternatively:

```racket
#lang sweet-exp lazy

define fibs
  cons 0 cons(1 map(+ fibs cdr(fibs)))

displayln list-ref(fibs 8)
```

Known issues: quasi-quotation combined with grouping does not behave
according to the specification.
