#lang racket

; modern.scm (Scheme), 2008-01-03
;
; NOTE: NOT READY FOR PRODUCTION USE.
;
; Implements "modern Lisp expressions", aka mod-expressions.
; These implement "curly infix" and term-prefixing rules. E.G.:
;  [x y z]     => (x y z)
;  {3 + 4 + 5} => (+ 3 4 5)
;  f(x)        => (f x)
;  f{x + 3}    => (f (+ x 3)
;  x[z]        => (bracketaccess x z)
;
; Call "modern-read" to read a "modern Lisp expression", aka mod-expression.
;
; Copyright (C) 2008 by David A. Wheeler.
;
; NOTE: This would be really easy to implement in Scheme, except for one
; quirk: most Scheme implementations' "read" function CONSUMES [, ], {, and },
; instead of treating them as delimiters like space, (, or ).
; This is even true when the Scheme standards don't permit such characters
; at all, such as at the end of a number.
; The only solution is to re-implement "read" in Scheme, but one that
; DOES treat them as delimiters.  So a simple re-implemention is provided.
; If your Scheme _does_ treat these characters as delimiters,
; you can eliminate all of that re-implementation code, and just use your
; built-in "read" function (which probably has additional extensions that
; this simple reader does not).
;
; If you DO use an ordinary Scheme reader, there is a limitation:
; the vector notation #(...) could not contain modern notation.
; In code, just use vector(...) instead.  The best solution, of course,
; is to build this into your Scheme reader.
;
; You _could_ in a pinch use a standard Scheme reader that didn't
; consider {} or [] as delimiters.  But then closing characters } and ]
; must be PRECEDED by a delimiter like a space, and you CANNOT invoke
; prefixed [] and {} at all.


; Released under the "MIT license":
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.

; Configuration:
(define modern-backwards-compatible #f) ; If true, "(" triggers old reader.
(define modern-bracketaccess #t) ; If true, "f[...]" => [bracketaccess f ...]
                                 ; if not, "f[...]" => [f ...].

; Preserve original read.
(define old-read read)

; A few useful utilities:

(define (ismember? item lyst)
  ; Returns true if item is member of lyst, else false.
  (pair? (member item lyst)))

(define (debug-result marker value)
  ; For debugging - you can insert this without adding let, etc., because
  ; after printing it returns the original value.
  (newline)
  (display "DEBUG: ")
  (display marker)
  (display " ")
  (write value)
  (newline)
  value)

; Define the tab character; a tab is immediately after the backslash.
; Unfortunately, this seems to be the only portable way to define the
; tab character in Scheme, so we'll do it once (here) and use it elsewhere.
(define tab #\	)

(define (my-is-whitespace c)
  (ismember? c `(#\space #\newline ,tab)))
; TODO: Possibly support other whitespace chars, e.g.:
;    #\return
;   (code-char 10) (code-char 11)     ; LF, VT
;   (code-char 12) (code-char 13)))))  ; FF, CR
;   If so, also modify the "delimiters" list above.
  

(define (skip-whitespace port)
  ; Consume whitespace.
  (cond
    ((my-is-whitespace (peek-char port))
      (read-char port)
      (skip-whitespace port))))


; Unfortunately, since most Scheme readers will consume [, {, }, and ],
; we have to re-implement our own Scheme reader.  Ugh.
; If you fix your Scheme's "read" so that [, {, }, and ] are considered
; delimiters (and thus not consumed when reading symbols, numbers, etc.),
; you can just call old-read instead of using underlying-read below,
; with the limitation noted above about vector constants #(...).
; We WILL call old-read on string reading (that DOES seem to work
; in common cases, and lets us use the implementation's string extensions).

(define modern-delimiters `(#\space #\newline #\( #\) #\[ #\] #\{ #\} ,tab))

(define (read-until-delim port delims)
  ; Read characters until eof or "delims" is seen; do not consume them.
  ; Returns a list of chars.
  (let ((c (peek-char port)))
    (cond
       ((eof-object? c) '())
       ((ismember? (peek-char port) delims) '())
       (#t (cons (read-char port) (read-until-delim port delims))))))

(define (read-error message)
  (display "Error: ")
  (display message)
  '())

(define (read-number port starting-lyst)
  (string->number (list->string
    (append starting-lyst
      (read-until-delim port modern-delimiters)))))

(define (process-char port)
  ; We've read #\ - returns what it represents.
  (cond
    ((eof-object? (peek-char port)) (peek-char port))
    (#t
      ; Not EOF. Read in the next character, and start acting on it.
      (let ((c (read-char port))
            (rest (read-until-delim port modern-delimiters)))
        (cond
          ((null? rest) c) ; only one char after #\ - so that's it!
          (#t
            (let ((rest-string (list->string (cons c rest))))
              (cond
                ((string-ci=? rest-string "space") #\space)
                ((string-ci=? rest-string "newline") #\newline)
                ((string-ci=? rest-string "ht") tab)  ; Scheme extension.
                ((string-ci=? rest-string "tab") tab) ; Scheme extension.
                (#t (read-error "Invalid character name"))))))))))


(define (process-sharp port)
  ; We've peeked a # character.  Returns what it represents.
  ; Note: Since we have to re-implement process-sharp anyway,
  ; the vector representation #(...) uses my-read-delimited-list, which in
  ; turn calls modern-read2. Thus, modern-expressions CAN be used inside
  ; a vector expression.
  (read-char port) ; Remove #
  (cond
    ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof. 
    (#t
      ; Not EOF. Read in the next character, and start acting on it.
      (let ((c (read-char port)))
        (cond
          ((char=? c #\t)  #t)
          ((char=? c #\f)  #f)
          ((ismember? c '(#\i #\e #\b #\o #\d #\x))
            (read-number port (list #\# c)))
          ((char=? c #\( )  ; Vector. 
            (list->vector (my-read-delimited-list #\) port)))
          ((char=? c #\\) (process-char port))
          (#t (read-error "Invalid #-prefixed string")))))))

(define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (process-period port)
  ; We've peeked a period character.  Returns what it represents.
  (read-char port) ; Remove .
  (let ((c (peek-char port)))
    (cond
      ((eof-object? c) '|.|) ; period eof; return period.
      ((ismember? c digits)
        (read-number port (list #\.)))  ; period digit - it's a number.
      (#t
        ; At this point, Scheme only requires support for "." or "...".
        ; As an extension we can support them all.
        (string->symbol (list->string (cons #\.
          (read-until-delim port modern-delimiters))))))))

(define (underlying-read port)
  ; This tiny reader implementation REQUIRES a port value.
  ; That way, while writing/modifying it, we
  ; won't forget to pass the port to it.
  ; Note: This reader is case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, and I
  ; _like_ case-sensitivity.
  (skip-whitespace port)
  (let ((c (peek-char port)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\")      ; old readers tend to handle strings okay, call it.
        (old-read port))   ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
      ((ismember? c digits) ; Initial digit.
        (read-number port '()))
      ((char=? c #\#) (process-sharp port))
      ((char=? c #\.) (process-period port))
      ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
        (read-char port)
        (if (ismember? (peek-char port) digits)
          (read-number port (list c))
          (string->symbol (list->string (cons c
            (read-until-delim port modern-delimiters))))))

      ; We'll reimplement abbreviations, (, and ;.
      ; These actually should be done by modern-read (and thus
      ; we won't see them), but redoing it here doesn't cost us anything,
      ; and it makes some kinds of testing simpler.  It also means that
      ; this function is a fully-usable Scheme reader, and thus perhaps
      ; useful for other purposes.
      ((char=? c #\')
        (read-char port)
        (list 'quote
          (underlying-read port)))
      ((char=? c #\`)
        (read-char port)
        (list 'quasiquote
          (underlying-read port)))
      ((char=? c #\,)
        (read-char port)
          (cond
            ((char=? #\@ (peek-char port))
              (read-char port)
              (list 'unquote-splicing
               (underlying-read port)))
           (#t 
            (list 'unquote
              (underlying-read port)))))
      ; The "(" calls modern-read, but since this one shouldn't normally
      ; be used anyway (modern-read will get first crack at it), it
      ; doesn't matter:
      ((char=? c #\( )
          (read-char port)
          (my-read-delimited-list #\) port))
      ((char=? c #\; )
        (skip-line port)
        (underlying-read port))
      ((char=? c #\| )   ; Scheme extension, |...| symbol (like Common Lisp)
        (read-char port) ; Skip |
        (let ((newsymbol
          (string->symbol (list->string
            (read-until-delim port '(#\|))))))
          (read-char port)
          newsymbol))
      (#t ; Nothing else.  Must be a symbol start.
        (string->symbol (list->string
          (read-until-delim port modern-delimiters)))))))


; End of Scheme reader re-implementation.




; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns false.
; If passed empty list, returns true (so recursion works correctly).
(define (even-and-op-prefix op lyst)
   (cond
     ((null? lyst) #t)
     ((not (pair? lyst)) #f) ; Not a list.
     ((not (eq? op (car lyst))) #f) ; fail - operators not all equal?.
     ((null? (cdr lyst)) #f) ; fail - odd # of parameters in lyst.
     (#t (even-and-op-prefix op (cddr lyst))))) ; recurse.

; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns NIL.
(define (simple-infix-listp lyst)
  (and
    (pair? lyst)           ; Must have list;  '() doesn't count.
    (pair? (cdr lyst))     ; Must have a second argument.
    (pair? (cddr lyst))    ; Must have a third argument (we check it
                           ; this way for performance)
    (symbol? (cadr lyst))  ; 2nd parameter must be a symbol.
    (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal??

; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(define (alternating-parameters lyst)
  (if (or (null? lyst) (null? (cdr lyst)))
    lyst
    (cons (car lyst) (alternating-parameters (cddr lyst)))))

; Transform a simple infix list - move the 2nd parameter into first position,
; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
(define (transform-simple-infix lyst)
   (cons (cadr lyst) (alternating-parameters lyst)))

(define (process-curly lyst)
  (if (simple-infix-listp lyst)
     (transform-simple-infix lyst) ; Simple infix expression.
     (cons 'nfx lyst))) ; Non-simple; prepend "nfx" to the list.


(define (my-read-delimited-list stop-char port)
  ; like read-delimited-list of Common Lisp, but calls modern-read instead.
  ; read the "inside" of a list until its matching stop-char, returning list.
  ; This implements a common extension: (. b) return b.
  ; That could be important for I-expressions, e.g., (. group)
  (skip-whitespace port)
  (let
    ((c (peek-char port)))
    (cond
      ((eof-object? c) (read-error "EOF in middle of list") c)
      ((char=? c stop-char)
        (read-char port)
        '()) ;(
      ((ismember? c '(#\) #\] #\}))  (read-error "Bad closing character") c)
      (#t
        (let ((datum (modern-read2 port)))
          (cond
             ((eq? datum '|.|)
               (let ((datum2 (modern-read2 port)))
                 (skip-whitespace port)
                 (cond
                   ((not (eqv? (peek-char port) stop-char))
                    (read-error "Bad closing character after . datum"))
                   (#t
                     (read-char port)
                     datum2))))
             (#t (cons datum
               (my-read-delimited-list stop-char port)))))))))

(define (modern-process-tail port prefix)
    ; See if we've just finished reading a prefix, and if so, process.
    ; This recurses, to handle formats like f(x)(y).
    ; This implements prefixed (), [], and {}
    (if (not (or (symbol? prefix) (pair? prefix)))
      prefix  ; Prefixes MUST be symbol or cons; return original value.
      (let ((c (peek-char port)))
        (cond
          ((eof-object? c) c)
          ((char=? c #\( ) ; ).  Implement f(x).
            (read-char port)
            (modern-process-tail port ;(
              (cons prefix (my-read-delimited-list #\) port))))
          ((char=? c #\[ )  ; Implement f[x]
            (read-char port)
            (modern-process-tail port
              (if modern-bracketaccess
                (cons 'bracketaccess (cons prefix
                  (my-read-delimited-list #\] port)))
                (cons prefix (my-read-delimited-list #\] port)))))
          ((char=? c #\{ )  ; Implement f{x}
            (read-char port)
            (modern-process-tail port
              (list prefix
                (process-curly
                  (my-read-delimited-list #\} port)))))
          (#t prefix)))))


(define (skip-line port)
  ; Skip every character in the line - end on EOF or newline.
  (let ((c (peek-char port)))
    (cond
      ((not (or (eof-object? c) (char=? c #\newline)))
        (read-char port)
        (skip-line port)))))

(define (modern-read2 port)
  ; Read using "modern Lisp notation".
  ; This implements unprefixed (), [], and {}
  (skip-whitespace port)
  (modern-process-tail port
    (let ((c (peek-char port)))
      ; (display "modern-read2 peeked at: ")
      ; (write c)
      (cond
        ; We need to directly implement abbreviations ', etc., so that
        ; we retain control over the reading process.
        ((eof-object? c) c)
        ((char=? c #\')
          (read-char port)
          (list 'quote
            (modern-read2 port)))
        ((char=? c #\`)
          (read-char port)
          (list 'quasiquote
            (modern-read2 port)))
        ((char=? c #\,)
          (read-char port)
            (cond
              ((char=? #\@ (peek-char port))
                (read-char port)
                (list 'unquote-splicing
                 (modern-read2 port)))
             (#t 
              (list 'unquote
                (modern-read2 port)))))
        ((char=? c #\( ) ; )
          (if modern-backwards-compatible
            (underlying-read port)
            (begin
              (read-char port) ; (
              (my-read-delimited-list #\) port))))
        ((char=? c #\[ )
            (read-char port)
            (my-read-delimited-list #\] port))
        ((char=? c #\{ )
          (read-char port)
          (process-curly
            (my-read-delimited-list #\} port)))
        ((char=? c #\; )  ; Handle ";" directly, so we don't lose control.
          (skip-line port)
          (modern-read2 port))
        (#t (let ((result (underlying-read port)))
                ; (display "DEBUG result = ")
                ; (write result)
                ; (display "\nDEBUG peek after= ")
                ; (write (peek-char port))
                result))))))


(define (modern-read . port)
  (if (null? port)
    (modern-read2 (current-input-port))
    (modern-read2 (car port))))


(define (modern-filter)
   (let ((result (modern-read (current-input-port))))
	(if (eof-object? result)
	    result
          (begin (write result) (newline) (modern-filter)))))

(define (modern-load filename)
  (define (load port)
    (let ((inp (modern-read port)))
	(if (eof-object? inp)
	    #t
	    (begin
	      (eval inp)
	      (load port)))))
  (load (open-input-file filename)))

(provide modern-read modern-load)
