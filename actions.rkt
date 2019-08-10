#lang racket
; actions.rkt
; AndrewJ 2018-09-06
; [i:019824]

; Imports
(require racket/hash
         threading
         hash-ext
         "util.rkt"
         "state.rkt")

; Exports
(provide init-game
         play-cards
         turn-over-cards
         return-card
         swap-hand-card
         swap-council-card
         swap-village-card
         apply-moves)


;-----------------------
; Helper functions

; deal-card :: Card
(define (deal-card)
  (hash (random-element card-types) 1))

; Deal n random cards to a given hand
; deal-n-cards :: Integer -> Hand -> Hand
(define (deal-n-cards n init-h)
  (for/fold ([h init-h]) ([i (range 0 n)])
    (hash-add (deal-card) h)))

; Deal a card into the Reserve
; deal-reserve :: State -> State
(define (deal-reserve st)
  (over _reserve (hash-add (deal-card)) st))

; empty-hand :: Hand
(define (empty-hand)
  (deal-n-cards 0 (hash)))

; Move a card of type t from the src hand to the dest hand
; e.g. (move-card 'Blk (_hand 1) (_council) s0)
; move-card :: Card -> Hand -> Hand -> State -> State
(define (move-card t _src _dest st)

  ; Ensure the card is available
  (cond [(or (not (hash-has-key? (view _src st) t))
             (<= (view (>>> _src (_card t)) st) 0))
         (raise-user-error 'move-card
                           (format "### Error: card ~a not available to move"
                                   (symbol->string t)))])
        
  (~>> st
       (over _dest (curry hash-add (hash t 1)))
       (over _src (curry hash-sub-neg (hash t 1)))))


; Swap two cards between two hands
; i.e. exchange t1 from lens1 with t2 from lens2
; swap-cards :: Card -> Lens Hand -> Card -> Lens Hand -> State -> State
(define (swap-cards t1 _lens1 t2 _lens2 st)
  (~>> st
       (move-card t1 _lens1 _lens2)
       (move-card t2 _lens2 _lens1)))


; Turn over a card
; e.g. (invert 'Sea) => 'SeaX
; invert :: Card -> Card
(define (invert c)
  (list-ref all-cards
            (modulo (+ 6 (find-index c all-cards)) 12)))

; turn-over-card :: Card -> Lens Hand -> State -> State
(define (turn-over-card c _h st)
  (cond [(= 0 (view* (>>> _h (_card c)) st))
         (raise-user-error 'turn-over-card (format "Card ~a doesn't exist to turn over." c))])

  (~>> st
       (over _h (λ (h) (hash-sub h (hash c 1))))
       (over _h (λ (h) (hash-add h (hash (invert c) 1))))))


;===============================
; Game actions
; - Init game
; - Player turn
;   1. Play hand card to council
;   2. Play hand card to village
;   3. Apply effect from the played village card
;   4. Take card from reserve
;   5. Deal new card to reserve
;   6. Take card from reserve
;
; Effects
; - Turn over 0-2 village cards (Blacksmith effect)
; - Replace any village card with a random card (Warrior effect)
; - Swap hand card with own village card (Bard effect)
; - Swap any village card with a council card (Seafarer effect)
; - Swap your village card with another village card (Merchant effect)
; - Apply effect from council card instead (Scout effect)


;-----------------------
; Define the initial state
; init-game :: Integer -> State
(define (init-game nplayers #:seed [seed 0])

  (cond [(or (< nplayers 2) (> nplayers 4))
         (raise-user-error 'init-game
                           "The game needs 2-4 players.")])
  
  (cond [(> seed 0) (random-seed seed)])
  
  (hash
   'Council (deal-n-cards 0 (empty-hand))
   'Villages (for/list ([i (in-range nplayers)])
               (deal-n-cards 0 (empty-hand)))
   'Hands (for/list ([i (in-range nplayers)])
            (deal-n-cards 4 (empty-hand)))
   'Reserve (deal-n-cards 4 (empty-hand))))

;-----------------------
; Moves 1 and 2: play one card to council (cc) and one to the village (cv) 
; make-turn :: Player -> Card -> Card -> State -> State
(define (play-cards p cc cv st)
  (~>> st
       (move-card cc (_hand p) _council)
       (move-card cv (_hand p) (_village p))))


;-----------------------
; (Blk effect) Turn over up to two cards in any hand
; e.g. (turn-over-cards '((Blk (_hand 0)) (Mer _council)) s0)
; turn-over-cards :: [(Card (Lens Hand)] -> State -> State
(define (turn-over-cards xs st)
  (cond [(> (length xs) 2)
         (raise-user-error 'turn-over-cards "Can only turn over max 2 cards.")])
  
  (for/fold ([state st])
            ([x xs])
    (turn-over-card (first x) (eval (second x)) state)))

;-----------------------
; (War effect) Throw away a card from any player's village and replace with a random card
; return-card :: Player -> Card -> State -> State
(define (return-card p c st)
  (~>> st
       (over (>>> (_village p) (_card c)) sub1)
       (over (_village p) (hash-add (deal-card)))))

;-----------------------
; (Brd effect) Swap a hand card with your village card
; e.g. (swap-hand-card 'Brd 0 'Mer s0)
; swap-hand-card :: Player -> Card -> Card -> State -> State
(define (swap-hand-card p ch cv st)
  (swap-cards ch (_hand p) cv (_village p) st))

;-----------------------
; (Sea effect) Swap any village card with a council card
; e.g. (swap-council-card 'Brd 0 'Mer s0)
; swap-council-card :: Player -> Card -> Card -> State -> State
(define (swap-council-card p cv cc st)
  (swap-cards cv (_village p) cc _council st))

;-----------------------
; (Mer effect) Swap two village cards
; swap-village-card :: Player -> Card -> Player -> Card -> State -> State
(define (swap-village-card p1 cv1 p2 cv2 st)
  (swap-cards cv1 (_village p1) cv2 (_village p2) st))


;-----------------------
; Fold a list of moves over an initial state
; e.g. (apply-moves moves s0)
; apply-moves :: [(m ... -> State -> State)] -> State -> State
(define (apply-moves lst init-state)
  (foldl (λ (f st) (f st)) init-state lst))

;-----------------------
; Example data

(define s0 (init-game 4 #:seed 1))

(define moves
  (list (curry play-cards 0 'Mer 'Sea)
        (curry swap-council-card 'Sea 0 'Mer)))

;========================
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)
  
  (define action-tests
    (test-suite
     "Unit tests"
     (check-equal? (+ 2 2) 4)
   
     (test-suite
      "Move cards"
      (let* ([s0 (init-game 3 #:seed 1)]
             [s1 (move-card 'War (_hand 1) _council s0)]
             [s2 (move-card 'War (_hand 2) (_village 0) s1)]
             [s3 (swap-cards 'Brd (_hand 0) 'Sea (_hand 1) s0)]
             [s4 (deal-reserve s0)]
             [s5 (turn-over-card 'War _council s2)])

        ; Test move-card
        (check-equal? (score-state s0) '(0 0 0))
        (check-equal? (view (>>> _council (_card 'War)) s1) 1)
        (check-equal? (score-state s2) '(1 0 0))
        (check-exn exn:fail? (λ () (move-card 'Blk (_hand 0) _council s0)))

        ; Test swap-cards
        (check-equal? (hash-sum (view (_hand 0) s3)) 4)
        (check-equal? (hash-sum (view (_hand 1) s3)) 4)
        (check-equal? (view (>>> (_hand 0) (_card 'Brd)) s3) 0)
        (check-equal? (view (>>> (_hand 0) (_card 'Sea)) s3) 2)

        ; Test deal-reserve
        (check-equal? (hash-sum (view _reserve s4)) 5)

        ; Test turn-over-card
        (check-equal? (invert 'Sea) 'SeaX)
        (check-equal? (score-state s5) '(0 0 0))

        ; Encode state to a vector
        (check-equal? (length (encode-state s0)) 96)))
     ))

  (run-tests action-tests))

; The End
