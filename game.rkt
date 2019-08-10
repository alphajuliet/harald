#lang racket
; game.rkt
; AndrewJ 2019-08-09
; [i:019824]

; Imports
(require racket/hash
         threading
         hash-ext
         "util.rkt"
         "actions.rkt"
         "state.rkt")

; Exports
(provide (all-defined-out))

;-----------------------
; Helper functions

; Combinations of keys based on frequency
(define (key-combinations h n)
  (remove-duplicates (combinations (hash-enumerate h) n)))

;-----------------------
; List the available initial actions for a player p, given current state st
; available-actions :: Integer -> State -> [Action]
(define (available-actions p st)
  (define c-all (key-combinations (view (_hand p) st) 2))
  (for/list ([c c-all])
    `(play-cards 0 ',(first c) ',(second c))))


(define s0 (init-game 3 #:seed 1))


;========================
(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define harald-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)))

  (run-tests harald-tests))
; The End