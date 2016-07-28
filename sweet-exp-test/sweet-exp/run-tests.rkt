#lang racket

(require racket/runtime-path
         "tests/2htdp.rkt"
         "tests/335.rkt"
         "tests/bad-close-error.rkt"
         "tests/case-lambda.rkt"
         "tests/define-syntax-rule.rkt"
         "tests/fib.rkt"
         "tests/hash.rkt"
         "tests/kw.rkt"
         "tests/lazy.rkt"
         "tests/modern.rkt"
         "tests/paren-shape.rkt"
         "tests/rest-arg.rkt"
         "tests/return.rkt"
         "tests/srcloc.rkt"
         "tests/sweet.rkt"
         "tests/typed.rkt"
         "tests/typed2.rkt"
         "tests/whitespace.rkt"
         "tests/strip-context/use-m.rkt"
         "tests/sweet-test-run.rkt")

(define-runtime-path sweet-testsuite "tests/sweet-testsuite")

(sweet-test sweet-testsuite)
