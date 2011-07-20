#lang racket

; sweet.rkt
;
; Ported the original reader below for Racket.
;
; sweet.scm (Scheme), 2008-01-08
;
; Implements sweet-expression specification version 0.2.
; To use (in guile), type:
;   (use-modules (sweet))
; Your %load-path must be correctly set up; see the README file.
;
; Implements and enables "sweet-expressions", which are modern-expressions
; plus indentation (using I-expressions).  So we'll chain two modules
; that implement them (respectively).

; This version auto-enables.

; A lot of this is guile-specific, because it uses guile's module system,
; but it can be easily changed for other Schemes

;(define sweet-read-save read)

(require "sugar.rkt")

(define sweet-read-save read)

(define sweet-read sugar-read)

; Not needed here; see the file "sweet-filter":
; (define (sweet-filter)
;    (let ((result (sweet-read (current-input-port))))
; 	(if (eof-object? result)
; 	    result
;           (begin (write result) (newline) (sweet-filter)))))

(define (sweet-load filename)
  (define (load port)
    (let ((inp (sweet-read port)))
	(if (eof-object? inp)
	    #t
	    (begin
	      (eval inp)
	      (load port)))))
  (load (open-input-file filename)))

(provide sweet-read sweet-read-save sweet-load)
