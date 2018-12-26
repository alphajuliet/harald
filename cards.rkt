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

(define card-types '(Blk War Brd Sea Mer Sct))
(define-type Card (U 'Blk 'War 'Brd 'Sea 'Mer 'Sct
                     'BlkX 'WarX 'BrdX 'SeaX 'MerX 'SctX))


; A hand is a collection of zero or more cards for each character. Stored as a hash table.
; Examples of hands are: the council, each player's village, each player's hand, and the
; pile available for restocking a hand.
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

(: empty-hand (-> Hand))
(define (empty-hand)
  (deal-n-cards 0 (hash)))

;-----------------------
; Score a player against a reference hand; by default, the council
(: score (-> Hand Hand Integer))
(define (score ref player)
  (define scoring-multiplier (hash 'Blk 1 'War 1 'Brd 1 'Sea 1 'Mer 1 'Sct 1
                                   'BlkX 0 'WarX 0 'BrdX 0 'SeaX 0 'MerX 0 'SctX 0))
  (hash-sum
   (hash-mul scoring-multiplier
             (hash-mul ref player))))

; Show player scores
(: scores (-> Hand (Listof Hand) (Listof Integer)))
(define (scores ref villages)
  (for/list ([v : Hand villages])
    (score ref v)))

;-----------------------
; Define game state

(define-type State (Mutable-HashTable
                    (U 'Council 'Hands 'Reserve 'Villages)
                    (U (Listof Hand) Hand)))

; Define the initial state
(: init-game (-> Integer State))
(define (init-game nplayers)
  (let ([st : State (make-hash)])
    (hash-set! st 'Council (deal-n-cards 0 (empty-hand)))
    (hash-set! st 'Villages (for/list : (Listof Hand) ([i (in-range nplayers)])
                              (deal-n-cards 0 (empty-hand))))
    (hash-set! st 'Hands (for/list : (Listof Hand) ([i (in-range nplayers)])
                           (deal-n-cards 4 (empty-hand))))
    (hash-set! st 'Reserve (deal-n-cards 4 (empty-hand)))
    st))

; Score the current state
(: score-game (-> State (Listof Integer)))
(define (score-game st)
  (scores (cast (hash-ref st 'Council) Hand)
          (cast (hash-ref st 'Villages) (Listof Hand))))

; Define moves
; (: send-card-to-council (-> ))

;-----------------------
; Example data
(define s0 (init-game 3))

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
