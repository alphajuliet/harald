#lang typed/racket
; cards.rkt
; AndrewJ 2018-09-06

; Cards are:
; - Blacksmith/Wild Boar -> Blk
; - Warrior/Bear         -> War
; - Bard/Fox             -> Brd
; - Seafarer/Goat        -> Sea
; - Merchant/Lynx        -> Mer
; - Scout/Wolf           -> Sct

(require racket/hash)

; Annotate untyped package
(require/typed hash-ext
               [hash-add (-> Hand Hand Hand)]
               [hash-mul (-> Hand Hand Hand)]
               [hash-sum (-> Hand Integer)])

(provide score
         scores)

;-----------------------
; Card :: (Hash Symbol Integer)
(define-type Card (U 'Blk 'War 'Brd 'Sea 'Mer 'Sct))
(define card-types '(Blk War Brd Sea Mer Sct))

; A hand is a collection of zero or more cards for each character.
; Examples of hands are: the council, each player's village, each player's hand, and the
; cards available for refreshing the hand.
(define-type Hand (Immutable-HashTable Card Integer))

(define enpty-hand hash)

;-----------------------
; Score a player against a reference hand; by default, the council
(: score (->* (Hand) (Hand) Integer))
(define (score player [ref *council])
  (hash-sum
   (hash-mul player ref)))

; Show player scores
(: scores (-> (Listof Integer)))
(define (scores)
  (list (score *village-1 *council)
        (score *village-2 *council)))

;-----------------------
; Utility function
; random-element :: ∀ a => List a -> a
(: random-element (All (a) (-> (Listof a) a)))
(define (random-element lst)
  (list-ref lst (random (length lst))))

; Deal a new random card
; deal-card :: Card
(: deal-card (-> Hand))
(define (deal-card)
  (hash (random-element card-types) 1))

; Deal n random cards to a given hand
(: deal-n-cards (-> Integer Hand Hand))
(define (deal-n-cards n init-h)
  (for/fold: : Hand ([h : Hand init-h]) ([i (range 0 n)])
    (hash-add (deal-card) h)))

;-----------------------
; For a 2-player game, the game state is captured by the following hands:
; - Council -> 'Council
; - Player 1's village -> 'Village-1
; - Player 1's hand cards -> 'Hand-1
; - Player 2's village -> 'Village-2
; - Player 2's hand cards -> 'Hand-2

; Define the hands active in a game
(define-type Game (Immutable-HashTable Symbol Hand))

; Test data
(define *council   : Hand (hash 'Blk 1 'War 2 'Brd 3 'Sea 0 'Mer 0 'Sct 0))
(define *village-1 : Hand (hash 'Blk 1 'War 2 'Sea 2))
(define *village-2 : Hand (hash 'War 2 'Brd 1 'Mer 1))

; e.g.
(: initial-state Game)
(define initial-state (hash 'Council *council
                            'Village-1 *village-1
                            'Village-2 *village-2))


;(define-language harald)

;========================
; Unit tests

(module+ test
  (require typed/rackunit
           typed/rackunit/text-ui)
  
  (define cards-tests
    (test-suite "Tests"
                (check-equal? (+ 2 2) 4)
                (check-equal? (score (hash 'Blk 1 'War 3 'Sea 2)
                                     (hash 'Blk 1 'War 2 'Brd 3))
                              7)))

  (run-tests cards-tests))

; The End