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

; A hand is a collection of zero or more cards for each character
(define-type Hand (Immutable-HashTable Card Integer))

; Test data
(define *council : Hand (hash 'Blk 1 'War 2 'Brd 3 'Sea 0 'Mer 0 'Sct 0))
(define *hand-1  : Hand (hash 'Blk 1 'War 2 'Sea 2))
(define *hand-2  : Hand (hash 'War 2 'Brd 1 'Mer 1))

;-----------------------
; Score a player against a reference hand; by default, the council
#;(: score (-> Hand Hand Integer))
(define (score [player : Hand] [ref : Hand *council]) : Integer
  (hash-sum
   (hash-mul player ref)))

; Show player scores
(: scores (-> (Listof Integer)))
(define (scores)
    (list (score *hand-1 *council)
          (score *hand-2 *council)))

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

; Add a new card to an existing hand
(: deal-to-hand (-> Hand Hand))
(define (deal-to-hand h)
  (hash-add (deal-card) h))

;-----------------------
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