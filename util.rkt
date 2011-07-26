#lang racket

(provide (all-defined-out))

(define current-source-name (make-parameter #f))

;; utility for construction syntaxes
(define (make-stx v ln col pos span)
  (datum->syntax #f v 
    (list (current-source-name) ln col pos span)))

;; syntax-cons : syntax? syntax? -> syntax?
;; take a syntax object and a syntax object that, when unwrapped, is
;; a list of syntax objects and create a new one with the former consed.
(define/contract (syntax-cons stx1 stx2)
  (-> syntax? syntax? syntax?)
  (define src (syntax-source stx1))
  (define line (syntax-line stx1))
  (define col (syntax-column stx1))
  (define start (syntax-position stx1))
  (define end (+ (syntax-position stx2) 
                 (syntax-span stx2)))
  (define span (- end start))
  (datum->syntax 
    #f
    (cons stx1 (syntax-e stx2))
    (list src line col start span)))

;; syntax-list : syntax? syntax? -> syntax?
;; take two syntax objects and put them in a syntax list
(define/contract (syntax-list stx1 stx2)
  (-> syntax? syntax? syntax?)
  (define src (syntax-source stx1))
  (define line (syntax-line stx1))
  (define col (syntax-column stx1))
  (define pos (syntax-position stx1))
  (define span (+ (syntax-span stx1)
                  (syntax-span stx2)))
  (datum->syntax 
    #f
    (list stx1 (syntax-e stx2))
    (list src line col pos span)))
