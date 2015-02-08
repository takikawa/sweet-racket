#lang sweet-exp racket

(require rackunit syntax/srcloc syntax/parse/define)

(define stx
 (syntax
   (I-am-a
      And-I-am-b
      f(x)
      g{y + z})))

define stx2
  syntax
    I-am-a
      And-I-am-b
      f(x)
      g{y + z}

(match-define (list a b c d) (syntax-e stx))
(match-define (cons f xs) (syntax-e c))
(match-define (list x) (syntax-e xs))
(match-define (list g y+z) (syntax-e d))
(match-define (list + y z) (syntax->list y+z))

(match-define (list a2 b2 c2 d2) (syntax-e stx2))
(match-define (cons f2 xs2) (syntax-e c2))
(match-define (list x2) (syntax-e xs2))
(match-define (list g2 y+z2) (syntax-e d2))
(match-define (list +-2 y2 z2) (syntax->list y+z2))

(define (display-syntax-warning a b c)
  (with-handlers ([exn:fail:syntax?
                   (λ (e)
                     (parameterize ([current-error-port (current-output-port)])
                       ((error-display-handler) (exn-message e) e)))])
    (raise-syntax-error a b c)))

(define-syntax-rule (highlight-stuff a ...)
  (begin (display-syntax-warning 'a (format "highlights ~a" 'a) a) ...))

;(highlight-stuff stx a b c d f xs x g y+z + y z
;                 stx2 a2 b2 c2 d2 f2 xs2 x2 g2 y+z2 +-2 y2 z2)

(define (syntax-srcloc stx)
  (build-source-location stx))

(begin-for-syntax
  (define-syntax-class cls
    [pattern [stx:expr srcloc-pat:expr]
             #:with norm (syntax/loc this-syntax (check-match (syntax-srcloc stx) srcloc-pat))]))
(define-simple-macro (chk-srclocs :cls ...)
  (begin norm ...))

(chk-srclocs
 [stx (srcloc _ 7 3 103 51)]
 [a   (srcloc _ 7 4 104 6)]
 [b   (srcloc _ 8 6 117 10)]
 [c   (srcloc _ 9 6 134 4)]
 [d   (srcloc _ 10 6 145 8)]
 [stx2 (srcloc _ 14 4 183 50)]
 [a2   (srcloc _ 14 4 183 6)]
 [b2   (srcloc _ 15 6 196 10)]
 [c2   (srcloc _ 16 6 213 4)]
 [d2   (srcloc _ 17 6 224 8)]
 )

