#lang racket
; harald.rkt
; AndrewJ 2018-09-06

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
#;(define-type Card (U 'Blk 'War 'Brd 'Sea 'Mer 'Sct
                       'BlkX 'WarX 'BrdX 'SeaX 'MerX 'SctX))


; A hand is a collection of zero or more cards for each character. Stored as a hash table.
; Examples of hands are: the council, each player's village, each player's hand, and the
; pile available for restocking a hand.
#;(define-type Hand (Immutable-HashTable Card Integer))

; Constructor
#;(: make-Hand (-> Card Integer Hand))
(define (make-Hand c n)
  (hash c n))

;-----------------------
; Utility functions

; random-element :: ∀ a. [a] -> a
#;(: random-element (All (a) (-> (Listof a) a)))
(define (random-element lst)
  (list-ref lst (random (length lst))))

; Deal a new random card
; deal-card :: Card
#;(: deal-card (-> Hand))
(define (deal-card)
  (hash (random-element card-types) 1))

; Deal n random cards to a given hand
; deal-n-cards :: Integer -> Hand -> Hand
#;(: deal-n-cards (-> Integer Hand Hand))
(define (deal-n-cards n init-h)
  (for/fold ([h init-h]) ([i (range 0 n)])
    (hash-add (deal-card) h)))

#;(: empty-hand (-> Hand))
(define (empty-hand)
  (deal-n-cards 0 (hash)))

;-----------------------
; Score a player against a reference hand; by default, the council
; score :: Hand -> Hand -> Integer
#;(: score (-> Hand Hand Integer))
(define (score ref player)
  (define scoring-multiplier
    (hash 'Blk 1 'War 1 'Brd 1 'Sea 1 'Mer 1 'Sct 1
          'BlkX 0 'WarX 0 'BrdX 0 'SeaX 0 'MerX 0 'SctX 0))
  (hash-sum
   (hash-mul scoring-multiplier
             (hash-mul ref player))))

; Show player scores
; scores :: Hand -> [Hand] -> [Integer]
#;(: scores (-> Hand (Listof Hand) (Listof Integer)))
(define (scores ref villages)
  (for/list ([v villages])
    (score ref v)))

;-----------------------
; Define game state

#;(define-type State (Immutable-HashTable
                      (U 'Council 'Hands 'Reserve 'Villages)
                      (U (Listof Hand) Hand)))

; Define the initial state
; init-game :: Integer -> State
#;(: init-game (-> Integer State))
(define (init-game nplayers)
  (hash
   'Council (deal-n-cards 0 (empty-hand))
   
   'Villages (for/list ([i (in-range nplayers)])
               (deal-n-cards 0 (empty-hand)))

   'Hands (for/list ([i (in-range nplayers)])
            (deal-n-cards 4 (empty-hand)))

   'Reserve (deal-n-cards 4 (empty-hand))))

;-----------------------
; Lenses for the game state

(define (council) (hash-ref-lens 'Council))
(define (council-card t) (lens-compose (hash-ref-lens t) (council)))
(define (hand n) (lens-compose (list-ref-lens n) (hash-ref-lens 'Hands)))
(define (hand-card n t) (lens-compose (hash-ref-lens t) (hand n)))
(define (village n) (lens-compose (list-ref-lens n) (hash-ref-lens 'Villages)))
(define (village-card n t) (lens-compose (hash-ref-lens t) (village n)))
(define (reserve) (hash-ref-lens 'Reserve))
(define (reserve-card t) (lens-compose (hash-ref-lens t) (reserve)))


;-----------------------
; Score the current state
; score-states :: State -> [Integer]
#;(: score-state (-> State (Listof Integer)))
(define (score-state st)
  (scores (hash-ref st 'Council)
          (hash-ref st 'Villages)))

;-----------------------
; Define moves

; Reverse subtraction, i.e. b -a == -1 * (a - b)
(define hash-sub-neg (curry hash-combine hash-union (compose (curry * -1) -))) 

; Move a card of type t from the src hand to the dest hand
; e.g. (move-card 'Blk (hand 1) (council) s0)
; move-card :: Card -> Lens Hand -> Lens Hand -> State -> State
(define (move-card t src-lens dest-lens st)
  (if (and (hash-has-key? (lens-view src-lens st) t)
           (> (lens-view (lens-compose (hash-ref-lens t) src-lens) st)
              0))
      (lens-transform dest-lens
                      (lens-transform src-lens st (hash-sub-neg (hash t 1)))
                      (hash-add (hash t 1)))
      ;else
      (error "### Error: card not available to move")))


; Swap two cards between two hands
; i.e. exchange t1 from lens1 with t2 from lens2
; swap-cards :: Card -> Lens Hand -> Card -> Lens Hand -> State -> State
; @@TODO
(define (swap-cards t1 lens1 t2 lens2 st)
  (let* ([s1 (move-card t1 lens1 lens2 st)]
         [s2 (move-card t2 lens2 lens1 s1)])
    s2))

; Deal a card into the Reserve
; @@TODO
(define (deal-reserve st) '())

;-----------------------
; Fold a list of moves over an initial state
; e.g. (apply-moves moves s0)
; apply-moves :: [(State -> State)] -> State -> State
(define (apply-moves lst init-state)
  (foldl (λ (f st) (f st)) init-state lst))

;-----------------------
; Example data

(define s0 #hash((Council . #hash())
                 (Hands . (#hash((Blk . 2) (Mer . 1) (War . 1))
                           #hash((Mer . 1) (Sea . 1) (War . 2))
                           #hash((Brd . 1) (Mer . 1) (War . 2))))
                 (Reserve . #hash((Brd . 1) (Mer . 1) (Sea . 2)))
                 (Villages . (#hash() #hash() #hash()))))

(define moves
  (list (curry move-card 'Blk (hand 0) (council))
        (curry move-card 'Blk (hand 0) (village 0))
        (curry move-card 'Sea (reserve) (hand 0))
        (curry move-card 'War (hand 1) (council))
        (curry move-card 'War (hand 1) (village 1))
        (curry move-card 'Mer (reserve) (hand 1))))

;========================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define harald-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
     (check-equal? (score (hash 'Blk 1 'War 3 'Sea 2)
                          (hash 'Blk 1 'War 2 'Brd 3))
                   7)
     (check-equal? (score (hash 'BlkX 1 'War 3 'Sea 2)
                          (hash 'Blk 1 'War 2 'Brd 3))
                   6)
     (test-suite
      "Move cards"
      (let* ([s0 #hash((Council . #hash())
                       (Hands . (#hash((Blk . 2) (Mer . 1) (War . 1))
                                 #hash((Mer . 1) (Sea . 1) (War . 2))
                                 #hash((Brd . 1) (Mer . 1) (War . 2))))
                       (Reserve . #hash((Brd . 1) (Mer . 1) (Sea . 2)))
                       (Villages . (#hash() #hash() #hash())))]
             [s1 (move-card 'War (hand 0) (council) s0)]
             [s2 (move-card 'War (hand 1) (village 1) s1)]
             [s3 (swap-cards 'Blk (hand 0) 'Sea (hand 1) s0)])

        ; Test move-card
        (check-equal? (score-state s0) '(0 0 0))
        (check-equal? (lens-view (council-card 'War) s1) 1)
        (check-equal? (score-state s2) '(0 1 0))
        (check-exn exn:fail? (λ () (move-card 'Sea (hand 0) (council) s0)))

        ; Test swap-cards
        (check-equal? (hash-count (lens-view (hand 0) s3)) 4)
        (check-equal? (hash-count (lens-view (hand 1) s3)) 4)
        (check-equal? (lens-view (hand-card 0 'Blk) s3) 1)
        (check-equal? (lens-view (hand-card 0 'Sea) s3) 1)))
    
     ))

  (run-tests harald-tests))

; The End