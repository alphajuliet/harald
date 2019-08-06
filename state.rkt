#lang racket
; state.rkt
; AndrewJ

; Imports
(require racket/hash
         lens/common
         lens/data/list
         lens/data/hash
         threading
         hash-ext)

; Exports
(provide (all-defined-out))

;-----------------------
; Cards are:
; - Blacksmith/Wild Boar -> Blk (red)
; - Warrior/Bear         -> War (blue)
; - Bard/Fox             -> Brd (yellow)
; - Seafarer/Goat        -> Sea (cyan)
; - Merchant/Lynx        -> Mer (purple)
; - Scout/Wolf           -> Sct (green)

(define card-types '(Blk War Brd Sea Mer Sct))

; A hand is a collection of zero or more cards for each character. Stored as a hash table.
; Examples of hands are: the council, each player's village, each player's hand, and the
; pile available for restocking a hand.

; Constructor
; make-hand :: Card -> Integer -> Hand 
(define (make-Hand c n)
  (hash c n))

;-----------------------
; Composable versions of lens functions. Similar to PureScript lens library functions.
(define (view _lens x) (lens-view _lens x))
(define (at _lens x y) (lens-set _lens y x))
(define (over _lens f x) (lens-transform _lens x f))
(define >>> lens-thrush)
(define <<< lens-compose)

; Lenses for the game state
(define _council (hash-ref-lens 'Council))
(define (_council-card t) (>>> _council (hash-ref-lens t)))
(define (_hand n) (>>> (hash-ref-lens 'Hands) (list-ref-lens n)))
(define (_hand-card n t) (>>> (_hand n) (hash-ref-lens t)))
(define (_village n) (>>> (hash-ref-lens 'Villages) (list-ref-lens n)))
(define (_village-card n t) (>>> (_village n) (hash-ref-lens t)))
(define _reserve (hash-ref-lens 'Reserve))
(define (_reserve-card t) (>>> _reserve (hash-ref-lens t)))

;-----------------------
; Score the current state
; score-states :: State -> [Integer]
(define (score-state st)
  (scores (hash-ref st 'Council)
          (hash-ref st 'Villages)))

;-----------------------
; Score a player against a reference hand; by default, the council
; score :: Hand -> Hand -> Integer
(define (score ref player)
  (define scoring-multiplier
    (hash 'Blk 1 'War 1 'Brd 1 'Sea 1 'Mer 1 'Sct 1
          'BlkX 0 'WarX 0 'BrdX 0 'SeaX 0 'MerX 0 'SctX 0))
  (hash-sum
   (hash-mul scoring-multiplier
             (hash-mul ref player))))

; Show player scores
; scores :: Hand -> [Hand] -> [Integer]
(define (scores ref villages)
  (for/list ([v villages])
    (score ref v)))


;-----------------------
(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define state-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
     (check-equal? (score (hash 'Blk 1 'War 3 'Sea 2)
                          (hash 'Blk 1 'War 2 'Brd 3))
                   7)
     (check-equal? (score (hash 'BlkX 1 'War 3 'Sea 2)
                          (hash 'Blk 1 'War 2 'Brd 3))
                   6)))

  (run-tests state-tests))

; The End