#lang racket/base

(provide indent)

(require racket/class
         framework
         )

;; indent :
;; (is-a?/c racket:text<%>) exact-nonnegative-integer? -> (or/c #f exact-nonnegative-integer?)
;; see:
;; http://plt.eecs.northwestern.edu/snapshots/current/doc/tools/adding-languages.html#%28idx._%28gentag._17._%28lib._scribblings%2Ftools%2Ftools..scrbl%29%29%29
;; This function is used to indent lines in DrRacket. It is called with the position containing
;; the line to be indented. It is expected to return the number of spaces that should appear
;; at the beginning of the line or #f. If #f is returned, DrRacket uses the standard s-expression
;; indentation rules.
(define (indent text pos)
  (define line-start
    (send text get-start-of-line pos))
  (define next-sexp-end
    (send text get-forward-sexp line-start))
  (define next-sexp-start
    (and next-sexp-end
         (send text get-backward-sexp next-sexp-end)))
  (define next-sexp-line-start
    (and next-sexp-start
         (send text get-start-of-line next-sexp-start)))
  (cond
    [next-sexp-line-start
     (- next-sexp-start next-sexp-line-start)]
    [else
     0]))
