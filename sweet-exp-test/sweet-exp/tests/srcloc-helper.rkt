#lang racket/base

(provide srcloc-helper
         (for-meta 1
                   display-syntax-warning
                   highlight-stuff
                   chk-srclocs
                   (all-from-out rackunit racket/match syntax/srcloc)
                   )
         (for-meta 2
                   cls
                   (all-from-out racket/base)
                   ))

(require syntax/parse/define
         (for-syntax racket/base
                     rackunit
                     racket/match
                     syntax/parse
                     syntax/srcloc 
                     syntax/parse/define
                     (for-syntax racket/base
                                 syntax/parse
                                 )))

(define-simple-macro
  (srcloc-helper [stx-id:id stx-expr:expr] ...
                 #:----------------------------------------------------------------
                 body-expr:expr ...+)
  #:with [stx ...] (generate-temporaries #'[stx-id ...])
  (begin
    (define-syntax macro
      (syntax-parser
        [(_ stx ...)
         (define stx-id #'stx) ...
         body-expr ...
         #'(void)]))
    (macro stx-expr ...)))

(begin-for-syntax
  (define (display-syntax-warning a b c)
    (with-handlers ([exn:fail:syntax?
                     (λ (e)
                       (parameterize ([current-error-port (current-output-port)])
                         ((error-display-handler) (exn-message e) e)))])
      (raise-syntax-error a b c)))
  
  (define-syntax-rule (highlight-stuff a ...)
    (begin (display-syntax-warning 'a (format "highlights ~a" 'a) a) ...))
  
  (define (syntax-srcloc stx)
    (build-source-location stx))
  
  (begin-for-syntax
    (define-syntax-class cls
      [pattern [stx:expr srcloc-pat:expr]
        #:with norm (syntax/loc this-syntax (check-match (syntax-srcloc stx) srcloc-pat))]))
  
  (define-simple-macro (chk-srclocs :cls ...)
    (begin norm ...))
  
  )
