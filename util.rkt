#lang racket/base
; util.rkt
; AndrewJ 2019-08-10

; Imports
(require racket/hash
         racket/list
         racket/function
         hash-ext
         lens)

; Exports
(provide (all-defined-out))

;-----------------------
; Utility functions

; Reverse subtraction, i.e. b -a == -1 * (a - b)
; hash-sub-neg :: ∀ a. Hash a Integer -> Hash a Integer -> Hash a Integer
(define hash-sub-neg
  (curry hash-combine hash-union (compose (curry * -1) -)))

; random-element :: ∀ a. [a] -> a
(define random-element
  (compose car shuffle))

; find-index :: ∀ a. a -> [a] -> Integer | #f
(define (find-index v lst)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (- (length lst)
       (length (member v lst)))))

;-----------------------
; Composable versions of lens functions. Similar to PureScript lens library functions.
(define (view _lens x) (lens-view _lens x))
(define (at _lens x y) (lens-set _lens y x))
(define (over _lens f x) (lens-transform _lens x f))
(define >>> lens-thrush)
(define <<< lens-compose)
(define (view* _lens x)
  (with-handlers ([exn:fail:contract? (λ (e) 0)])
    (lens-view _lens x)))

; The End