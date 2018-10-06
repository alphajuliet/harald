#lang typed/racket
; cards.rkt
; AndrewJ 2018-09-06

; Imports
(require racket/hash)
(require/typed hash-ext ; Annotate untyped package
               [hash-add (-> Hand Hand Hand)]
               [hash-mul (-> Hand Hand Hand)]
               [hash-sum (-> Hand Integer)])

; Exports
(provide (all-defined-out))

;-----------------------
; Cards are:
; - Blacksmith/Wild Boar -> Blk
; - Warrior/Bear         -> War
; - Bard/Fox             -> Brd
; - Seafarer/Goat        -> Sea
; - Merchant/Lynx        -> Mer
; - Scout/Wolf           -> Sct

(define-type Card (U 'Blk 'War 'Brd 'Sea 'Mer 'Sct))
(define card-types '(Blk War Brd Sea Mer Sct))

; A hand is a collection of zero or more cards for each character. Stored as a hash table.
; Examples of hands are: the council, each player's village, each player's hand, and the
; pile available for restocking a hand.
; It doesn't yet handle cards turned over.
(define-type Hand (Immutable-HashTable Card Integer))

; Constructor
(: make-Hand (-> Card Integer Hand))
(define (make-Hand c n)
  (hash c n))

;-----------------------
; Utility function
; random-element :: ∀ a => [a] -> a
(: random-element (All (a) (-> (Listof a) a)))
(define (random-element lst)
  (list-ref lst (random (length lst))))

(: add-hand (-> Hand Hand Hand))
(define (add-hand h1 h2)
  (hash-add h1 h2))

; Deal a new random card
(: deal-card (-> Hand))
(define (deal-card)
  (hash (random-element card-types) 1))

; Deal n random cards to a given hand
(: deal-n-cards (-> Integer Hand Hand))
(define (deal-n-cards n init-h)
  (for/fold: : Hand ([h : Hand init-h]) ([i (range 0 n)])
    (hash-add (deal-card) h)))

;-----------------------
; Score a player against a reference hand; by default, the council
(: score (-> Hand Hand Integer))
(define (score ref player)
  (hash-sum
   (hash-mul ref player)))

; Show player scores
(: scores (-> Hand (Listof Hand) (Listof Integer)))
(define (scores ref villages)
  (for/list ([v : Hand villages])
    (score ref v)))


;-----------------------
; Example data

(define council : Hand
  (deal-n-cards 6 (hash)))

(define villages : (Listof Hand)
  (list
   (deal-n-cards 5 (hash))
   (deal-n-cards 5 (hash))))

(define hands : (Listof Hand)
  (list
   (deal-n-cards 4 (hash))
   (deal-n-cards 4 (hash))))

(define reserve : Hand
  (deal-n-cards 4 (hash)))


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
