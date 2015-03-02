#lang racket/unit

;; Port of the original Guile code to Racket
;;
;; Implementation of a revision of SRFI 49, based on SRFI 49 at:
;; http://srfi.schemers.org/srfi-49/srfi-49.html
;; This provides "Indentation-sensitive syntax" for Scheme.
;; This SRFI descibes a new syntax for Scheme, called I-expressions,
;; with equal descriptive power as S-expressions. The syntax uses
;; indentation to group expressions, and has no special cases for
;;  semantic constructs of the language. It can be used both for
;;  program and data input.
;;
;; The following code implements I-expressions.

(require racket/match
         syntax/parse
         syntax/stx
         "read-sig.rkt"
         "util.rkt")

(import read^)
(export (rename read^
                [sugar-read read]
                [sugar-read-syntax read-syntax]))

(define treat-keywords-specially? #t)

(define (not-actual-parens stx)
  (syntax-property stx 'sweet-exp-not-actual-parens #t))

(define sugar-read-save read-syntax)

;; -> void?
;; Consumes chars to end of line, WITHOUT consume the ending newline/EOF
(define (consume-to-eol)
  (define c (peek-char))
  (cond [(eof-object? c) c]
        [(char=? c #\newline) c]
        [else (read-char)
              (consume-to-eol)]))

(define (maybe-syntax-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; string? symbol? -> stx?
;; read a quote item
(define (readquote level qt)
  (define char (peek-char))
  (cond [(char-whitespace? char) (datum->syntax #f qt #f orig-stx)]
        [else (datum->syntax #f (list qt (maybe-syntax-e (sugar-read-save))) #f orig-stx)]))

;; string? -> stx?
;; read in a single datum
(define (readitem level)
  (define char (peek-char))
  (cond [(eqv? char #\`)
         (read-char)
         (readquote level 'quasiquote)]
        [(eqv? char #\')
         (read-char)
         (readquote level 'quote)]
        [(eqv? char #\,)
         (read-char)
         (cond
           [(eqv? (peek-char) #\@)
            (read-char)
            (readquote level 'unquote-splicing)]
           [else (readquote level 'unquote)])]
     [else (sugar-read-save)]))

(define (indentation>? indentation1 indentation2)
  (let ([len1 (string-length indentation1)]
        [len2 (string-length indentation2)])
    (and (> len1 len2)
         (string=? indentation2 (substring indentation1 0 len2)))))

;; -> listof char?
(define (accumulate-hspace)
  (define c (peek-char))
  (cond [(and (char? c)
              (char-whitespace? c)
              (not (eqv? c #\newline)))
         (cons (read-char)
               (accumulate-hspace))]
        [else '()]))

;; -> string?
;; read the indentation of the next line
(define (indentationlevel)
  (define indent (accumulate-hspace))
  (cond [(eqv? (peek-char) #\;)
         (consume-to-eol) ; ALWAYS ignore comment-only lines.
         (when (eqv? (peek-char) #\newline) (read-char))
         (indentationlevel)]
        ; If ONLY whitespace on line, treat as "", because there's no way
        ; to (visually) tell the difference (preventing hard-to-find errors):
        [(eof-object? (peek-char)) ""]
        [(eqv? (peek-char) #\newline) ""]
        [else (list->string indent)]))

;; stx? -> stx?
;; clean up the block if necessary
(define (clean stx)
  (define-syntax-class quote-like
    (pattern (~or (~literal quote)
                  (~literal quasiquote)
                  (~literal unquote-splicing)
                  (~literal unquote))))

  (syntax-parse stx
    [((~literal group) e ...)
     (datum->syntax stx (stx-cdr stx) stx stx)]
    [(() e ...)
     (datum->syntax stx (stx-cdr stx) stx stx)]
    [((q:quote-like) e e1 ...)
     (datum->syntax stx (cons #'q (stx-cdr stx)) stx stx)]
    [((e ...) e1 ...)
     (datum->syntax stx (cons (clean (stx-car stx)) (stx-cdr stx)) stx stx)]
    [(e ...) stx]
    [e stx]
    [() stx]))

;; indent -> syntax?
;; Reads all subblocks of a block
(define (readblocks level)

  ;; indent -> (listof syntax?)
  (define (helper level)
    (define read (readblock-clean level))
    (define next-level (car read))
    (define stx (cdr read))
    (cond [(equal? next-level level)
           (define reads (helper level))
           (define next-next-level (car reads))
           (define next-blocks (cdr reads))
           (cond [(eq? (maybe-syntax-e stx) '|.|)
                  (if (pair? next-blocks)
                      (cons next-next-level (car next-blocks))
                      (cons next-next-level next-blocks))]
                 [(and treat-keywords-specially? (syntax-property stx 'ungroup-kw))
                  (syntax-parse stx
                    [(kw) (cons next-next-level (list* #'kw next-blocks))]
                    [(kw arg) (cons next-next-level (list* #'kw #'arg next-blocks))]
                    [(kw . args) (cons next-next-level
                                       (list* #'kw (not-actual-parens #'args) next-blocks))])]
                 [else (cons next-next-level (cons stx next-blocks))])]
          [else
           (cond [(and treat-keywords-specially? (syntax-property stx 'ungroup-kw))
                  (syntax-parse stx
                    [(kw) (cons next-level (list #'kw))]
                    [(kw arg) (cons next-level (list #'kw #'arg))]
                    [(kw . args) (cons next-level (list #'kw (not-actual-parens #'args)))])]
                 [else
                  (cons next-level (list stx))])]))

  (match (helper level)
    [(cons lvl lst)
     (cons lvl (datum->syntax #f lst #f orig-stx))]))

;; Read one block of input
(define (readblock level)
  (define char (peek-char))
  (cond
    [(eof-object? char)
      (cons -1 char)]
    [(eqv? char #\;)
      (consume-to-eol)
      (readblock level)]
    [(eqv? char #\newline)
      (read-char)
      (define next-level (indentationlevel))
      (if (indentation>? next-level level)
          (readblocks next-level)
          (cons next-level (datum->syntax #f '())))]
    [(char-whitespace? char)
      (read-char)
      (readblock level)]
    [else
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (define first (readitem level))
     (define rest  (readblock level))
     (define end-pos (port-pos (current-input-port)))
     (define new-level (car rest))
     (define stx (cdr rest))
     (define block (and (not (eof-object? stx))
                        (syntax->list stx)))
     (cond [(eq? (maybe-syntax-e first) '|.|)
            (if (pair? block)
                (cons new-level (car block))
                rest)]
           [(eof-object? first) (cons new-level first)]
           [(eof-object? stx) (cons new-level first)]
           [else (define new-stx (make-stx (cons first block) ln col pos (- end-pos pos)))
                 (cons new-level
                       (not-actual-parens
                        (cond [(keyword? (maybe-syntax-e first))
                               (syntax-property new-stx 'ungroup-kw #t)]
                              [else new-stx])))])]))

;; string? -> (string? . (U '|.| syntax?))
;; reads a block and handles group, (quote), (unquote),
;; (unquote-splicing) and (quasiquote).
(define (readblock-clean level)
  (define read (readblock level))
  (define next-level (car read))
  (define stx (cdr read))
  (define block (and (not (eof-object? stx))
                          (syntax->list stx)))
  (cond [(or (not block) (> (length block) 1))
         (cons next-level (clean stx))]
        [(= (length block) 1)
         (cons next-level (car block))]
        [else
         (cons next-level (datum->syntax stx '|.|))]))

;; Read single complete I-expression.
(define (sugar-start-expr)
  (define indentation (list->string (accumulate-hspace)))
  (define c (peek-char))
  (cond
    [(eof-object? c) c] ; EOF - return it, we're done.
    [(char-comment? c) => (λ (x) (read-comment))]
    [(eqv? c #\newline)
      (read-char)              ; Newline (with no preceding comment).
      (sugar-start-expr)]      ; Consume and again
    [else
     ; TODO: Handle  (> (string-length indentation) 0)
     (define read (readblock-clean ""))
     (define level (car read))
     (define stx (cdr read))
     (define block (maybe-syntax-e stx))
     (cond
       [(eq? block '|.|) (datum->syntax stx '())]
       [else stx])]))

;; predicate for comment characters
(define (char-comment? c) (eqv? c #\;))

;; read a commented line
(define (read-comment)
  (define d (consume-to-eol))
  (cond
    [(eof-object? d) d]         ; If EOF after comment, return it.
    [else
     (read-char)                ; Newline after comment.  Consume NL
     (sugar-start-expr)])) ; and try again

;; read and read-syntax functions
(define (sugar-read [port (current-input-port)])
  (define stx (sugar-read-syntax #f port))
  (if (eof-object? stx)
      eof
      (syntax->datum stx)))

(define (sugar-read-syntax [source-name #f]
                           [port (current-input-port)])
  (when (not source-name)
    (set! source-name (object-name port)))
  (parameterize ([current-source-name source-name]
                 [current-input-port port])
    (sugar-start-expr)))


(define (sugar-filter)
   (let ((result (sugar-read (current-input-port))))
        (if (eof-object? result)
            result
          (begin (write result) (newline) (sugar-filter)))))

(define (sugar-load filename)
  (define (load port)
    (let ((inp (sugar-read port)))
        (if (eof-object? inp)
            #t
            (begin
              (eval inp)
              (load port)))))
  (load (open-input-file filename)))


; ----{ sugar.scm }----
; Copyright (C) 2005-2008 by Egil Möller and David A. Wheeler.
; All Rights Reserved.
; 
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

