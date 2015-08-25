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
  (define next-sexp-indentation
    (get-next-sexp-indentation text pos))
  next-sexp-indentation)

(define (get-start-of-line text pos)
  (and pos (send text get-start-of-line pos)))

(define (get-forward-sexp text pos)
  (and pos (send text get-forward-sexp pos)))

(define (get-backward-sexp text pos)
  (and pos (send text get-backward-sexp pos)))

(define (get-forward-sexp-start text pos)
  (define end (get-forward-sexp text pos))
  (and end (get-backward-sexp text end)))

(define (get-next-sexp-indentation text pos)
  (define line-start
    (get-start-of-line text pos))
  (define next-sexp-start
    (get-forward-sexp-start text line-start))
  (define next-sexp-line-start
    (get-start-of-line text next-sexp-start))
  (cond
    [next-sexp-line-start
     (- next-sexp-start next-sexp-line-start)]
    [else
     0]))

