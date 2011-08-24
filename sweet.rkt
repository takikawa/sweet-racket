#lang racket

;; sweet.rkt
;;
;; Ported the original reader to Racket.
;; parametrized by the read in the underlying language

(require "read-sig.rkt"
         "sugar.rkt"
         "modern.rkt")

(provide sweet@)

(define-compound-unit sweet@
  (import (R : read^))
  (export S)

  (link [((M : read^)) modern@ R]
        [((S : read^)) sugar@ M]))
