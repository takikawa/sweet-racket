#lang racket/base

(provide process-curly)

(require syntax/parse)

; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns false.
; If passed empty list, returns true (so recursion works correctly).
(define (even-and-op-prefix op lyst)
  (syntax-parse lyst
    [() #t]
    [(fst:id _ . rst)
     (and (free-identifier=? op #'fst)
          (even-and-op-prefix op #'rst))]
    [_ #f]))

; syntax? -> any/c
; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns #f.
(define (simple-infix-listp stx)
  (syntax-parse stx
    [(fst snd:id trd rst ...)
     (even-and-op-prefix #'snd
                         #'(rst ...))]
    [_ #f]))

;; Syntax -> (Listof Syntax)
;; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(define (alternating-parameters stx)
  (syntax-parse stx
    [() '()]
    [(e) (list #'e)]
    [(fst snd rst ...)
     (cons #'fst (alternating-parameters #'(rst ...)))]))

;; Transform a simple infix list - move the 2nd parameter into first position,
;; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
(define (transform-simple-infix stx)
  (syntax-parse stx
    [(fst snd rst ...)
     (datum->syntax stx
       (cons #'snd (alternating-parameters #'(fst snd rst ...)))
       stx)]))

(define (process-curly stx)
  (syntax-parse stx
    [(e) #'e]
    [(op:id e) stx]
    [_ #:when (simple-infix-listp stx)
       (transform-simple-infix stx)]  ; Simple infix expression.
    [_ (define nfx (datum->syntax stx 'nfx stx))
       (datum->syntax stx (cons nfx stx) stx)]  ; Non-simple; prepend "nfx" to the list.
    ))

