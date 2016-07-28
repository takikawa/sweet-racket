#lang sweet-exp racket/init
(require racket/init (submod sweet-exp runtime-config))
(provide (all-from-out racket/init))
(configure #f)
