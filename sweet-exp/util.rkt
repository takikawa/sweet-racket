#lang racket

(provide (all-defined-out))

(require racket/syntax
         syntax/srcloc
         syntax/readerr)

(define current-source-name (make-parameter #f))

;; A syntax object that has the "original?" property
;; (borrowed from the scribble reader)
(define orig-stx (read-syntax #f (open-input-string "dummy")))

;; utility for construction syntaxes
(define (make-stx v ln col pos span)
  (datum->syntax #f v 
    (vector (current-source-name) ln col pos span)
    orig-stx))

(define (paren-shape stx shape)
  (syntax-property stx 'paren-shape shape))

(define (port-pos in)
  (define-values (_1 _2 pos) (port-next-location in))
  pos)

(define dot (generate-temporary #'|.|))
(define (dot? x) (eq? x dot))

(define (read-err/srcloc msg srcloc)
  (apply raise-read-error msg (build-source-location-list srcloc)))

(define (rt-char=? c default-c [c=? char=?])
  (define-values (c2 _1 _2)
    (readtable-mapping (make-readtable (current-readtable)) c))
  (and (char? c2) (c=? c2 default-c)))

(define (rt-char-member? c default-cs)
  (rt-char=? c default-cs char-member?))

(define (char-member? c cs)
  (for/or ([c2 (in-list cs)])
    (char=? c c2)))

