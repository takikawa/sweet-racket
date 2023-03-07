#lang racket/base

;; Sweet expressions

(module language-info racket/base
  (provide get-language-info)
  (define (get-language-info data)
    (lambda (key default)
      (case key
        [(configure-runtime)
         '(#((submod sweet-exp runtime-config) configure #f))]
        [else default]))))

(module runtime-config racket/base

  ;; This module provides runtime configuration, e.g., for
  ;; using sweet-exp based modules in DrRacket

  (require "reader.rkt")

  (provide configure)

  (define (configure data)
    (current-read-interaction sweet-read-syntax)))

(module reader syntax/module-reader
  #:language read
  #:read sweet-read
  #:read-syntax sweet-read-syntax
  #:info get-info
  #:language-info #((submod sweet-exp language-info) get-language-info #f)

  (require "reader.rkt")

  (define (get-info key default f)
    (define (fallback) (f key default))
    (case key
      [(drracket:indentation)
       (dynamic-require 'sweet-exp/indent 'indent)]
      [else (fallback)])))
