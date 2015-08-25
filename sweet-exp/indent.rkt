#lang racket/base

;; This module provides an indenter for the sweet-exp language.
;; Within a normal s-expression, it uses normal s-expression indentation rules.
;; Otherwise, it leaves code indented the way it was, and indents blank lines
;; and comments to match the indentation of the next expression.

(provide indent)

(require racket/class)

;; indent :
;; (is-a?/c racket:text<%>) exact-nonnegative-integer? -> (or/c #f exact-nonnegative-integer?)
;; see:
;; http://plt.eecs.northwestern.edu/snapshots/current/doc/tools/adding-languages.html#%28idx._%28gentag._17._%28lib._scribblings%2Ftools%2Ftools..scrbl%29%29%29
;; This function is used to indent lines in DrRacket. It is called with the position containing
;; the line to be indented. It is expected to return the number of spaces that should appear
;; at the beginning of the line or #f. If #f is returned, DrRacket uses the standard s-expression
;; indentation rules.
(define (indent text pos)
  (cond
    [(within-sexp-not-first-line? text pos)
     #f] ; use normal racket s-expression indentation
    [else
     (get-next-sexp-indentation text pos)]))

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

(define (line=? text pos1 pos2)
  (and pos1 pos2
       (= (get-start-of-line text pos1)
          (get-start-of-line text pos2))))

(define (find-up-sexp text pos)
  (and pos (send text find-up-sexp pos)))

(define (within-sexp-not-first-line? text pos)
  (define up-sexp
    (find-up-sexp text (get-start-of-line text pos)))
  (cond [(not up-sexp) #f]
        [(line=? text up-sexp pos)
         (within-sexp-not-first-line? text up-sexp)]
        [else #t]))

