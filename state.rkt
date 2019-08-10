#lang racket
; state.rkt
; AndrewJ

; Imports
(require racket/hash
         lens/common
         lens/data/list
         lens/data/hash
         threading
         hash-ext
         "util.rkt")

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

(define all-cards '(Blk War Brd Sea Mer Sct BlkX WarX BrdX SeaX MerX SctX))
(define null-hand (hash 'Blk 0 'War 0 'Brd 0 'Sea 0 'Mer 0 'Sct 0
                        'BlkX 0 'WarX 0 'BrdX 0 'SeaX 0 'MerX 0 'SctX 0))

; A Hand is a collection of zero or more cards for each character. Stored as a hash table.
; Examples of hands are: the council, each player's village, each player's own hand, and the
; reserve pile available for restocking a player's hand.


; Game state
; State :: Hash Card Hand
(define (empty-state nplayers)
  (hash 'Council (hash)
        'Villages (for/list ([i (in-range nplayers)]) (hash))
        'Hands (for/list ([i (in-range nplayers)]) (hash))
        'Reserve (hash)))

; Lenses for the game state
(define _card hash-ref-lens)
(define _council (hash-ref-lens 'Council))
(define _hands (hash-ref-lens 'Hands))
(define (_hand p) (>>> _hands (list-ref-lens p)))
(define _villages (hash-ref-lens 'Villages))
(define (_village n) (>>> _villages (list-ref-lens n)))
(define _reserve (hash-ref-lens 'Reserve))

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
; Score the current state
; score-states :: State -> [Integer]
(define (score-state st)
  (scores (view _council st)
          (view _villages st)))

;-----------------------
; Encode the state as a vector
(define (encode-hand h)
  (hash-values (hash-add null-hand h)))

(define (encode-state s)
  (append (encode-hand (hash-ref s 'Council))
          (flatten (map encode-hand (hash-ref s 'Hands)))
          (flatten (map encode-hand (hash-ref s 'Villages)))
          (encode-hand (hash-ref s 'Reserve))))
  
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
                   6)
     (check-equal? (score-state (empty-state 4))
                   '(0 0 0 0))))

  (run-tests state-tests))

; The End