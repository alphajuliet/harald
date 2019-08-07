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
(define hash-sub-neg
  (curry hash-combine hash-union (compose (curry * -1) -)))

; random-element :: ∀ a. [a] -> a
(define random-element
  (compose car shuffle))

; find-index :: a -> List a -> Integer | #f
(define (find-index v lst)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (- (length lst)
       (length (member v lst)))))

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
;   - Play hand card to council
;   - Play hand card to village
;   - Take card from reserve
;
; - Turn over cards (Blacksmith effect)
; - Return village card (Warrior effect)
; - Swap hand card (Bard effect)
; - Swap council card (Seafarer effect)
; - Swap village card (Merchant effect)


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
; Make a turn with three actions: play one card to council (cc),
; one to the village (cv) and take one from the reserve (cr).
; make-turn :: Player -> Card -> Card -> Card -> State -> State
(define (make-turn p cc cv cr st)
  (~>> st
       (move-card cc (_hand p) _council)
       (move-card cv (_hand p) (_village p))
       (move-card cr _reserve (_hand p))
       (deal-reserve)))

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
; (War effect) Return a card from any village and replace with a random card
; return-card :: CardLoc -> State -> State
(define (return-card _c st)
  #f)

;-----------------------
; (Brd effect) Swap a hand card with a village card
; swap-hand-card :: CardLoc -> CardLoc -> State -> State
(define (swap-hand-card _ch _cv st)
  #f)

;-----------------------
; (Sea effect) Swap a village card with a council card
; swap-council-card :: CardLoc -> CardLoc -> State -> State
(define (swap-council-card _cv _cc st)
  #f)

;-----------------------
; (Mer effect) Swap two village cards
; swap-village-card :: CardLoc -> CardLoc -> State -> State
(define (swap-village-card _cv1 _cv2 st)
  #f)


;-----------------------
; Fold a list of moves over an initial state
; e.g. (apply-moves moves s0)
; apply-moves :: [(State -> State)] -> State -> State
(define (apply-moves lst init-state)
  (foldl (λ (f st) (f st)) init-state lst))

;-----------------------
; Example data

(define s0 (init-game 4 #:seed 1))

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
      (let* ([s0 (init-game 3 #:seed 1)]
             [s1 (move-card 'War (_hand 1) _council s0)]
             [s2 (move-card 'War (_hand 2) (_village 0) s1)]
             [s3 (swap-cards 'Brd (_hand 0) 'Sea (_hand 1) s0)]
             [s4 (deal-reserve s0)])

        ; Test move-card
        (check-equal? (score-state s0) '(0 0 0))
        (check-equal? (view (_council-card 'War) s1) 1)
        (check-equal? (score-state s2) '(1 0 0))
        (check-exn exn:fail? (λ () (move-card 'Blk (_hand 0) _council s0)))

        ; Test swap-cards
        (check-equal? (hash-sum (view (_hand 0) s3)) 4)
        (check-equal? (hash-sum (view (_hand 1) s3)) 4)
        (check-equal? (view (_hand-card 0 'Brd) s3) 0)
        (check-equal? (view (_hand-card 0 'Sea) s3) 2)

        ; Test deal-reserve
        (check-equal? (hash-sum (view _reserve s4)) 5)))
     ))

  (run-tests harald-tests))

; The End
