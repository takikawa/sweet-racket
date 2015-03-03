#lang racket/unit

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

(require racket/contract
         syntax/parse
         syntax/readerr
         syntax/stx
         syntax/srcloc
         "read-sig.rkt"
         "util.rkt")

(import (prefix old- read^))
(export (rename read^ [modern-read read]
                      [modern-read-syntax read-syntax]))

; Configuration:
(define modern-backwards-compatible #f) ; If true, "(" triggers old reader.
(define modern-bracketaccess #f) ; If true, "f[...]" => [bracketaccess f ...]
                                 ; if not, "f[...]" => [f ...].
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
(define tab #\tab)

(define (skip-whitespace port)
  ; Consume whitespace.
  (define c (peek-char port))
  (cond
    [(and (char? c) (char-whitespace? c))
     (read-char port)
     (skip-whitespace port)]))

; Unfortunately, since most Scheme readers will consume [, {, }, and ],
; we have to re-implement our own Scheme reader.  Ugh.
; If you fix your Scheme's "read" so that [, {, }, and ] are considered
; delimiters (and thus not consumed when reading symbols, numbers, etc.),
; you can just call old-read instead of using underlying-read below,
; with the limitation noted above about vector constants #(...).
; We WILL call old-read on string reading (that DOES seem to work
; in common cases, and lets us use the implementation's string extensions).

(define modern-delimiters
  `(#\space #\newline #\return #\( #\) #\[ #\] #\{ #\} ,tab))

(define (read-until-delim port delims)
  ; Read characters until eof, "delims", or whitespace is seen; do not consume them.
  ; Returns a list of chars.
  (define (stop-char? c)
    (or (ismember? c delims)
        (char-whitespace? c)))
  (read-until port stop-char?))

(define (read-until port stop-char?)
  ; Read characters until eof or a character that passes stop-char? is seen; do not consume them.
  ; Returns a list of chars.
  (let ([c (peek-char port)])
    (cond [(eof-object? c) '()]
          [(stop-char? c) '()]
          [else (cons (read-char port) (read-until port stop-char?))])))

(define (read-error message)
  (display "Error: ")
  (display message)
  '())

(define (read-number port starting-lyst)
  (define-values (ln col pos) (port-next-location port))
  (define digits
    (append starting-lyst
      (read-until-delim port modern-delimiters)))
  (define n (string->number (list->string digits)))
  (define span (length digits))
  (make-stx n ln col pos span))

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
  (define-values (ln col pos) (port-next-location port))
  (let ((c (peek-char port)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\")      ; old readers tend to handle strings okay, call it.
        (old-read-syntax (current-source-name) port))
      ((ismember? c digits) ; Initial digit.
        (old-read-syntax (current-source-name) port))
      ((char=? c #\#)
       (old-read-syntax (current-source-name) port)) ; Racket's reader handles this
      ((char=? c #\.) (process-period port))
      ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
        (if (ismember? (peek-char port 1) digits)
            (begin (read-char) (read-number port (list c)))
            (read-symbol port)))
      ((char=? c #\; )
        (skip-line port)
        (underlying-read port))
      ((char=? c #\| )   ; Scheme extension, |...| symbol (like Common Lisp)
        (read-char port) ; Skip |
        (let ((newsymbol
          (string->symbol (list->string
            (read-until port (λ (c) (char=? c #\|)))))))
          (read-char port)
          newsymbol))
      [(ismember? c '(#\) #\] #\}))
       (raise-read-error (format "unexpected `~a`" c) (current-source-name) ln col pos 1)]
      (#t ; Nothing else.  Must be a symbol start.
        (read-symbol port)))))

(define (read-symbol port)
  (define-values (ln col pos) (port-next-location port))
  (define chars (read-until-delim port modern-delimiters))
  (define sym (string->symbol (list->string chars)))
  (make-stx sym ln col pos (length chars)))


; End of Scheme reader re-implementation.




; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns false.
; If passed empty list, returns true (so recursion works correctly).
(define (even-and-op-prefix op lyst)
  (cond
    [(stx-null? lyst) #t]
    [(not (stx-pair? lyst)) #f] ; Not a list.
    [(not (free-identifier=? op (stx-car lyst))) #f] ; fail - operators not all equal?.
    [(null? (stx-cdr lyst)) #f] ; fail - odd # of parameters in lyst.
    [#t (even-and-op-prefix op (stx-cdr (stx-cdr lyst)))])) ; recurse.

; syntax? -> any/c
; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns NIL.
(define (simple-infix-listp stx)
  (syntax-parse stx
    [(fst snd:id trd rst ...)
     (even-and-op-prefix #'snd
                         #'(rst ...))]
    [_ #f]))

;; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(define (alternating-parameters stx)
  (syntax-parse stx
    [() (syntax/loc stx ())]
    [(e) (syntax/loc stx (e))]
    [(fst snd rst ...)
     (quasisyntax/loc stx
       (fst #,@(alternating-parameters (syntax/loc stx (rst ...)))))]))

;; Transform a simple infix list - move the 2nd parameter into first position,
;; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
(define (transform-simple-infix stx)
  (syntax-parse stx
    [(fst snd rst ...)
     (quasisyntax/loc stx
       (snd #,@(alternating-parameters (syntax/loc stx (fst snd rst ...)))))]))

(define (process-curly stx)
  (define nfx (datum->syntax stx 'nfx stx))
  (if (simple-infix-listp stx)
      (transform-simple-infix stx)  ; Simple infix expression.
      (datum->syntax stx (cons nfx stx) stx)))       ; Non-simple; prepend "nfx" to the list.

;; my-read-delimited-list : char? input-port? -> (listof syntax?)
;; like read-delimited-list of Common Lisp, but calls modern-read-syntax instead.
;; read the "inside" of a list until its matching stop-char, returning list.
(define (my-read-delimited-list stop-char port)
  ;(define-values (_1 _2 start) (port-next-location port))
  
  ;; read-accum : (listof syntax?) e-n-i? -> (or/c eof (listof syntax?))
  ;; accum: lst accumulates the sub-expression syntaxes
  (define (read-accum subs)
    (skip-whitespace port)
    (define c (peek-char port))
    (define-values (ln col pos) (port-next-location port))
    (cond
      [(eof-object? c)
       (raise-read-eof-error "EOF in middle of list" #f ln col pos #f)
       c]
      [(char=? c stop-char)
       (read-char port)
       subs]
      [(ismember? c '(#\) #\] #\}))
       (raise-read-error "Bad closing character" #f ln col pos #f)
       c]
      [else
        (define datum (modern-read2 port))
        (cond [(eq? datum '|.|) (append subs (read-dot-extension))]
              [else (read-accum (append subs (list datum)))])]))

  ;; read-dot-extension : -> syntax?
  (define (read-dot-extension)
    (define datum2 (modern-read2 port))
    (define-values (ln col pos) (port-next-location port))
    (skip-whitespace port)
    (cond [(not (eqv? (peek-char port) stop-char))
           (raise-read-error "Bad closing character after . datum"
                             #f ln col pos #f)]
          [else (read-char port)
                datum2]))

  (read-accum '()))

;; modern-process-tail : input-port? syntax? -> syntax?
;; See if we've just finished reading a prefix, and if so, process.
;; This recurses, to handle formats like f(x)(y).
;; This implements prefixed (), [], and {}
(define (modern-process-tail port stx)
  (define prefix (if (syntax? stx) (syntax-e stx) stx))
  (define c (peek-char port))
  (cond [(not (or (symbol? prefix) (pair? prefix)))
         stx]  ; Prefixes MUST be symbol or cons; return original value.
        [(eof-object? c) stx]
        [(char=? c #\( ) ; ).  Implement f(x).
         (define args (modern-read2/no-process-tail port))
         (define end-pos (port-pos port))
         (modern-process-tail port
           (datum->syntax stx (cons stx args)
             (update-source-location stx #:span (- end-pos (syntax-position stx)))))]
        [(char=? c #\[ )  ; Implement f[x]
         (define args (modern-read2/no-process-tail port))
         (define end-pos (port-pos port))
         (modern-process-tail port
           (datum->syntax stx (cons stx args)
             (update-source-location stx #:span (- end-pos (syntax-position stx)))))]
        [(char=? c #\{ )  ; Implement f{x}
         (define arg (modern-read2/no-process-tail port))
         (define end-pos (port-pos port))
         (modern-process-tail port
           (datum->syntax stx (list stx arg)
             (update-source-location stx #:span (- end-pos (syntax-position stx)))))]
        [else stx]))

(define (skip-line port)
  ; Skip every character in the line - end on EOF or newline.
  (let ((c (peek-char port)))
    (cond
      ((not (or (eof-object? c) (char=? c #\newline)))
        (read-char port)
        (skip-line port)))))

;;
(define (modern-read-syntax [source-name #f]
                            [port (current-input-port)])
  (when (not source-name)
    (set! source-name (object-name port)))
  (parameterize ([current-source-name source-name]
                 [current-input-port port])
    (modern-read2 port)))

;; modern-read2 : input-port? -> syntax?
;; Read using "modern Lisp notation".
;; This implements unprefixed (), [], and {}
(define (modern-read2 port)
  (modern-process-tail port
    (modern-read2/no-process-tail port)))

(define (modern-read2/no-process-tail port)
  (skip-whitespace port)
  (define c (peek-char port))
  (define-values (ln col pos) (port-next-location port))
  ; (printf "modern-read-syntax peeked at: ~a ~n" c)
  (cond
    ; We need to directly implement abbreviations ', etc., so that
    ; we retain control over the reading process.
    [(eof-object? c) eof]
    [(char=? c #\')
     (read-char port)
     (define q (make-stx 'quote ln col pos 1))
     (define stx (modern-read2 port))
     (define end-pos (port-pos port))
     (datum->syntax stx (list q stx)
                    (update-source-location q #:span (- end-pos pos)))]
    [(char=? c #\`)
     (read-char port)
     (define q (make-stx 'quasiquote ln col pos 1))
     (define stx (modern-read2 port))
     (define end-pos (port-pos port))
     (datum->syntax stx (list q stx)
                    (update-source-location q #:span (- end-pos pos)))]
    [(char=? c #\,)
     (read-char port)
     (cond [(char=? #\@ (peek-char port))
            (read-char port)
            (define u (make-stx 'unquote-splicing ln col pos 2))
            (define stx (modern-read2 port))
            (define end-pos (port-pos port))
            (datum->syntax stx (list u stx)
                           (update-source-location u #:span (- end-pos pos)))]
           [else
            (define u (make-stx 'unquote ln col pos 1))
            (define stx (modern-read2 port))
            (define end-pos (port-pos port))
            (datum->syntax stx (list u stx)
                           (update-source-location u #:span (- end-pos pos)))])]
    [(char=? c #\( )
     (cond [modern-backwards-compatible (underlying-read port)]
           [else
            (read-char port)
            (define lst (my-read-delimited-list #\) port))
            (define end-pos (port-pos port))
            (make-stx lst ln col pos (- end-pos pos))])]
    [(char=? c #\[ )
     (read-char port)
     (define lst (my-read-delimited-list #\] port))
     (define end-pos (port-pos port))
     (make-stx lst ln col pos (- end-pos pos))]
    [(char=? c #\{ )
     (read-char port)
     (define lst (my-read-delimited-list #\} port))
     (define end-pos (port-pos port))
     (process-curly
      (make-stx lst ln col pos (- end-pos pos)))]
    [(char=? c #\; )  ; Handle ";" directly, so we don't lose control.
     (skip-line port)
     (modern-read2 port)]
    [else (define result (underlying-read port))
          ; (printf "DEBUG result = ~a ~n" result)
          ; (printf "DEBUG peek after = ~a ~n" (peek-char port))
          result]))

(define (modern-read [port (current-input-port)])
  (define stx (modern-read-syntax #f port))
  (if (eof-object? stx)
      eof
      (syntax->datum stx)))

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
