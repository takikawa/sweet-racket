#lang racket/unit

; Implementation of a revision of SRFI 49, based on SRFI 49 at:
; http://srfi.schemers.org/srfi-49/srfi-49.html
; This provides "Indentation-sensitive syntax" for Scheme.
; This SRFI descibes a new syntax for Scheme, called I-expressions,
; with equal descriptive power as S-expressions. The syntax uses
; indentation to group expressions, and has no special cases for
;  semantic constructs of the language. It can be used both for
;  program and data input.

; The following code implements I-expressions as a GNU Guile module
; that can be loaded using:
;     (use-modules (sugar))
; This command will only work if this file's directory is in %load-path; 
; if the file is not installed anywhere, you can do this with:
;      (set! %load-path (append %load-path '(".")))
; You can also use this (esp. for testing):
;    export GUILE_LOAD_PATH="."
; but beware; the GUILE_LOAD_PATH has higher precedence, so while this
; is useful for testing, setting GUILE_LOAD_PATH to include "." can have
; security problems if used in "real" programs.

; CHANGES:
; * 2008-01-08 David A. Wheeler <dwheeler at dwheeler dot com>
;   - Fixed comment processing.  Inline comments now (correctly) ignored;
;     all comment-only lines are completely skipped with indentation ignored.
;   - At the start of a new expression, whitespace-only lines are ignored.
;   - After the first line of a new expression, no-character lines and
;     horizontal-whitespace-only lines end the expression.
;   - Added support for unquote-splicing (,@).
;   - Fix sugar-filter - it should have called write, not display.
;   - Changed formatting to cuddle on left-hand-side, space-only, so that
;     it clearly is formatted in a way similar to what it reads
; * 2007-10-15 David A. Wheeler <dwheeler at dwheeler dot com>
;   - Changed "t" to "#t" (t is Common Lisp, #t is Scheme)
;   - Added sugar-filter.
; * 2006-06-10 David A. Wheeler <dwheeler at dwheeler dot com>
;   - Changed eq? to eqv? when comparing characters.
;     The "eq?" operator is not guaranteed to work
;     for comparing characters, or in comparing with end-of-file,
;     in the R5RS specification.
;   - sugar-load now calls sugar-read, not read;
;     that way, even if sugar has not been enabled, sugar-load will
;     correctly use sugar-read to read the contents.
;  ----{ sugar.scm }----


(require "read-sig.rkt")

(import read^)
(export (rename read^ [sugar-read read]))

(define sugar-read-save read)

(define group 'group)
; TODO: Need to NOT give "group" its special meaning if it doesn't
; sart with "g" or "G". This may be tricky to do with this design.

(define (consume-to-eol port)
  ; Consumes chars to end of line, WITHOUT consume the ending newline/EOF
  (let ((c (peek-char port)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\newline) c)
      (#t (read-char port) (consume-to-eol port)))))

(define (readquote level port qt)
  (let ((char (peek-char port)))
    (if (char-whitespace? char)
        (list qt)
        (list qt (sugar-read-save port)))))

(define (readitem level port)
  (let ((char (peek-char port)))
    (cond
     ((eqv? char #\`)
      (read-char port)
        (readquote level port 'quasiquote))
     ((eqv? char #\')
      (read-char port)
        (readquote level port 'quote))
     ((eqv? char #\,)
      (read-char port)
      (cond
        ((eqv? (peek-char port) #\@)
          (read-char port)
          (readquote level port 'unquote-splicing))
        (#t
          (readquote level port 'unquote))))
     (#t
        (sugar-read-save port)))))

(define (indentation>? indentation1 indentation2)
  (let ((len1 (string-length indentation1))
          (len2 (string-length indentation2)))
    (and (> len1 len2)
           (string=? indentation2 (substring indentation1 0 len2)))))

(define (accumulate-hspace port)
  (if (or (eqv? (peek-char port) #\space)
          (eqv? (peek-char port) #\return)
          (eqv? (peek-char port) #\tab))
      (cons (read-char port) (accumulate-hspace port))
      '()))

(define (indentationlevel port)
  (let ((indent (accumulate-hspace port)))
    (cond
      ((eqv? (peek-char port) #\;)
        (consume-to-eol port) ; ALWAYS ignore comment-only lines.
        (when (eqv? (peek-char port) #\newline) (read-char port))
        (indentationlevel port))
      ; If ONLY whitespace on line, treat as "", because there's no way
      ; to (visually) tell the difference (preventing hard-to-find errors):
      ((eof-object? (peek-char port)) "")
      ((eqv? (peek-char port) #\newline) "")
      (#t (list->string indent)))))

(define (clean line)
  (cond
   ((not (pair? line))
    line)
   ((null? line)
    line)
   ((eq? (car line) 'group)
    (cdr line))
   ((null? (car line))
    (cdr line))
   ((list? (car line))
    (if (or (equal? (car line) '(quote))
              (equal? (car line) '(quasiquote))
              (equal? (car line) '(unquote-splicing))
              (equal? (car line) '(unquote)))
          (if (and (list? (cdr line))
                   (= (length (cdr line)) 1))
              (cons
               (car (car line))
               (cdr line))
              (list
               (car (car line))
               (cdr line)))
          (cons
           (clean (car line))
           (cdr line))))
   (#t
    line)))

;; Reads all subblocks of a block
(define (readblocks level port)
  (let* ((read (readblock-clean level port))
           (next-level (car read))
           (block (cdr read)))
    (if (equal? next-level level)
          (let* ((reads (readblocks level port))
                 (next-next-level (car reads))
                 (next-blocks (cdr reads)))
            (if (eq? block '|.|)
                (if (pair? next-blocks)
                    (cons next-next-level (car next-blocks))
                    (cons next-next-level next-blocks))
                (cons next-next-level (cons block next-blocks))))
          (cons next-level (list block)))))

;; Read one block of input
(define (readblock level port)
  (let ((char (peek-char port)))
    (cond
     ((eof-object? char)
        (cons -1 char))
     ((eqv? char #\;)
        (consume-to-eol port)
        (readblock level port))
     ((eqv? char #\newline)
        (read-char port)
        (let ((next-level (indentationlevel port)))
          (if (indentation>? next-level level)
              (readblocks next-level port)
              (cons next-level '()))))
     ((or (char-whitespace? char))
        (read-char port)
        (readblock level port))
     (#t
        (let* ((first (readitem level port))
               (rest (readblock level port))
               (level (car rest))
               (block (cdr rest)))
          (if (eq? first '|.|)
              (if (pair? block)
                  (cons level (car block))
                  rest)
              (cons level (cons first block))))))))

;; reads a block and handles group, (quote), (unquote),
;; (unquote-splicing) and (quasiquote).
(define (readblock-clean level port)
  (let* ((read (readblock level port))
           (next-level (car read))
           (block (cdr read)))
    (if (or (not (list? block)) (> (length block) 1))
          (cons next-level (clean block))
          (if (= (length block) 1)
              (cons next-level (car block))
              (cons next-level '|.|)))))


(define (sugar-start-expr port)
  ; Read single complete I-expression.
  (let* ((indentation (list->string (accumulate-hspace port)))
         (c (peek-char port)))
    (cond
      ((eof-object? c) c) ; EOF - return it, we're done.
      ((eqv? c #\; )    ; comment - consume and see what's after it.
        (let ((d (consume-to-eol port)))
          (cond
            ((eof-object? d) d) ; If EOF after comment, return it.
            (#t  
              (read-char port) ; Newline after comment.  Consume NL
              (sugar-start-expr port))))) ; and try again
      ((eqv? c #\newline)
        (read-char port) ; Newline (with no preceding comment).
        (sugar-start-expr port)) ; Consume and again
      (#t
        ; TODO: Handle  (> (string-length indentation) 0)
        (let* ((read (readblock-clean "" port))
               (level (car read))
               (block (cdr read)))
          (cond
           ((eq? block '|.|)
              '())
           (#t
              block)))))))


(define (sugar-read . port)
  (if (null? port)
    (sugar-start-expr (current-input-port))
    (sugar-start-expr (car port))))


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

