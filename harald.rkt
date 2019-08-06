#lang racket
; harald.rkt
; AndrewJ 2018-09-06
; [i:019824]

; Imports
(require racket/hash
         lens/common
         lens/data/list
         lens/data/hash
         threading
         hash-ext
         "state.rkt")

; Exports
(provide (all-defined-out))


;-----------------------
; Utility functions

; Reverse subtraction, i.e. b -a == -1 * (a - b)
(define hash-sub-neg (curry hash-combine hash-union (compose (curry * -1) -))) 

; random-element :: ∀ a. [a] -> a
(define (random-element lst)
  (list-ref lst (random (length lst))))

; deal-card :: Card
(define (deal-card)
  (hash (random-element card-types) 1))

; Deal n random cards to a given hand
; deal-n-cards :: Integer -> Hand -> Hand
(define (deal-n-cards n init-h)
  (for/fold ([h init-h]) ([i (range 0 n)])
    (hash-add (deal-card) h)))

; empty-hand :: Hand
(define (empty-hand)
  (deal-n-cards 0 (hash)))


; Move a card of type t from the src hand to the dest hand
; e.g. (move-card 'Blk (hand 1) (council) s0)
; move-card :: Card -> Lens Hand -> Lens Hand -> State -> State
(define (move-card t _src _dest st)

  ; Ensure the card is available
  (if (and (hash-has-key? (view _src st) t)
           (> (view (<<< (hash-ref-lens t) _src) st)
              0))
      (over _dest
            (hash-add (hash t 1))
            (over _src (hash-sub-neg (hash t 1)) st))
      ;else
      (error "### Error: card not available to move")))


; Swap two cards between two hands
; i.e. exchange t1 from lens1 with t2 from lens2
; swap-cards :: Card -> Lens Hand -> Card -> Lens Hand -> State -> State
(define (swap-cards t1 _lens1 t2 _lens2 st)
  (let* ([s1 (move-card t1 _lens1 _lens2 st)]
         [s2 (move-card t2 _lens2 _lens1 s1)])
    s2))

; Deal a card into the Reserve
; deal-reserve :: State -> State
(define (deal-reserve st)
  (over _reserve (hash-add (deal-card)) s0))


;===============================
; Game actions
; - Init game
; - Play hand card to council
; - Play hand card to village
; - Take card from reserve
; - Swap card
; - Replace card


;-----------------------
; Define the initial state
; init-game :: Integer -> State
(define (init-game nplayers #:seed [seed 0])

  (cond [(> seed 0) (random-seed seed)])
  
  (hash
   'Council (deal-n-cards 0 (empty-hand))
   
   'Villages (for/list ([i (in-range nplayers)])
               (deal-n-cards 0 (empty-hand)))

   'Hands (for/list ([i (in-range nplayers)])
            (deal-n-cards 4 (empty-hand)))

   'Reserve (deal-n-cards 4 (empty-hand))))



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
  (list (curry move-card 'Blk (_hand 0) _council)
        (curry move-card 'Blk (_hand 0) (_village 0))
        (curry move-card 'Sea _reserve (_hand 0))
        (curry move-card 'War (_hand 1) _council)
        (curry move-card 'War (_hand 1) (_village 1))
        (curry move-card 'Mer _reserve (_hand 1))))

;========================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define harald-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
   
     (test-suite
      "Move cards"
      (let* ([s0 #hash((Council . #hash())
                       (Hands . (#hash((Blk . 2) (Mer . 1) (War . 1))
                                 #hash((Mer . 1) (Sea . 1) (War . 2))
                                 #hash((Brd . 1) (Mer . 1) (War . 2))))
                       (Reserve . #hash((Brd . 1) (Mer . 1) (Sea . 2)))
                       (Villages . (#hash() #hash() #hash())))]
             [s1 (move-card 'War (_hand 0) _council s0)]
             [s2 (move-card 'War (_hand 1) (_village 1) s1)]
             [s3 (swap-cards 'Blk (_hand 0) 'Sea (_hand 1) s0)]
             [s4 (deal-reserve s0)])

        ; Test move-card
        (check-equal? (score-state s0) '(0 0 0))
        (check-equal? (view (_council-card 'War) s1) 1)
        (check-equal? (score-state s2) '(0 1 0))
        (check-exn exn:fail? (λ () (move-card 'Sea (_hand 0) _council s0)))

        ; Test swap-cards
        (check-equal? (hash-sum (view (_hand 0) s3)) 4)
        (check-equal? (hash-sum (view (_hand 1) s3)) 4)
        (check-equal? (view (_hand-card 0 'Blk) s3) 1)
        (check-equal? (view (_hand-card 0 'Sea) s3) 1)

        ; Test deal-reserve
        (check-equal? (hash-sum (view _reserve s4)) 5)))
     ))

  (run-tests harald-tests))

; The End
