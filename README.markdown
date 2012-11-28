Sweet expressions for Racket
============================

This package provides a port of the [sweet
expression](http://www.dwheeler.com/readable/) reader, originally written by
David Wheeler, to [Racket](http://www.racket-lang.org).

To use the package, you have several options.

* With a pre-release version of Racket, you can install the package using Planet 2:
  - `raco pkg install sweet-exp`
  - set your language to `#lang sweet-exp <your-base-language>`

* You can also use Planet 2 package management manually:
  - `git clone git://github.com/takikawa/sweet-racket.git`
  - `raco pkg install sweet-racket`

* With the current version of Racket, you can use the
  [Planet package](http://planet.racket-lang.org/display.ss?package=sweet.plt&owner=asumu).
  - set your language to `#lang planet asumu/sweet <your-base-language>`

Note: the check syntax tool does work with this language, but
it may fail to activate when you first install it from PLaneT.
Changing the #lang line and then trying again should cause the
check syntax button to appear.

This packaged is released under the MIT license under the
same terms as the original implementation.
