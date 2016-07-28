#lang racket

;; Test driver ported from Guile to Racket

(require sweet-exp/read-sig
         sweet-exp/sweet)

(provide sweet-test)

;; Use Racket's reader as the base
(define-unit underlying-read@
  (import)
  (export (rename read^ [-read read]
                        [-read-syntax read-syntax]))

  (define -read read)
  (define -read-syntax read-syntax))

(define-values/invoke-unit
  (compound-unit
    (import)
    (export S)

    (link [((R : read^)) underlying-read@]
          [((S : read^)) sweet@ R]))

  (import)
  (export (prefix sweet- read^)))

(define old-read read)

(define sweet-test-error 0)
(define sweet-test-correct 0)

; Load a test file, which should have pairs of these:
;    s-expr (correct answer)
;    sweet-expression (which should match it)
(define (sweet-test filename)
  (define (load port)
    (let ((correct (old-read port)))
      (cond
	((eof-object? correct)
            (display "TEST COMPLETE\n")
            (display "Correct answers:  ")
            (display sweet-test-correct)
            (display " test errors: ")
            (display sweet-test-error)
            (newline)
	    #t)
	(#t
	  (let ((sweet-value (sweet-read port)))
            (cond
              ((equal? correct sweet-value)
                (set! sweet-test-correct (+ sweet-test-correct 1))
                (display "Correct for: ")
                (write correct)
                (newline))
              (#t
                (set! sweet-test-error (+ sweet-test-error 1))
                (display "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ERROR \n")
                (display " Should be:")
                (write correct)
                (display "\n Got: ")
                (write sweet-value)
                (newline))))
	  (load port)))))
  (load (open-input-file filename)))
