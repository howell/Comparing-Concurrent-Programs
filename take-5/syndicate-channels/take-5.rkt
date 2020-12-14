#lang syndicate/actor

(require racket/list)
(require racket/set)

(require/activate syndicate/drivers/timestate)
(require/activate syndicate/drivers/tcp2)
(require/activate "../../tcp2-datum.rkt")

(require [only-in racket/port with-input-from-bytes with-output-to-bytes])

(require "deck.rkt")
(require "rules.rkt")
(require "logging.rkt")
(require "deal.rkt")
(require "struct.rkt")

(module+ test (require rackunit))

;; a Round is a (round-has-begun Nat [List-of Row]), where the Nat is between 1 and 10
(struct round-has-begun (number rows) #:transparent)

;; an InHand is an (in-hand PlayerID Card)
(struct in-hand (player card) #:transparent)

;; a Player is a (player PlayerID)
(struct player (id) #:transparent)

;; a GamePlayer is a function (Setof Card) Rows -> Card
;; that picks out a card to play based on a current state of the rows.

;; ===================================================================================================
;; Protocol

;; There is a conversation about the players participating in the game.
;; Players express interest in playing by making a Player assertion before
;; the game is started by the Dealer.

;; There is a conversation about playing a round in a game.
;; The Dealer begins a new round of the game with a Round assertion, containing
;; the round number and the current state of the rows. The Dealer also makes an
;; InHand assertion for each player in the game, containing that player's PlayerID
;; and the cards in their hand. Players respond with Move assertions, containing
;; that player's PlayerID, the round in which they are making their move, and the
;; card that they are choosing to play.

;; Invariants
;;
;; 1) No Takesies-Backsies: Once a player makes a move in a round, by asserting
;; (plays PlayerID Round Nat Card), that player never asserts a different card
;; for that round.
;; (plays-in-round pid r c1) && (plays-in-round pid r c2) ==> c1 == c2
;; or maybe
;; (plays-in-round pid r c1) ==> (always (plays-in-round pid r c2) ==> c1 == c2)
;; ^ this doesn't say that the player has to maintain the move assertion. Or in
;; other words, is it ok for a player to retract a move once they've made it,
;; say when the next round begins? This protocol doesn't say.
;;
;; 2) No Spying On Other Players Hands
;; Each player can only listen to (in-hand pid card) for their own pid
;;
;; 3) No Watching Other Players' Moves
;; Players do not subscribe to (plays-in-round _ _ _)
;;
;; 4) Players Only Play Cards Actually In Their Hand
;; (plays pid r c) ==> (in-hand pid c)
;; Note this is only true momentarily. Once the dealer processes the round,
;; the in-hand assertion goes away (maybe it should stay around. then the player
;; keeps track of what card they play, and we add a new invariant that a player
;; doesn't play a card multiple times).
;;
;; 5) No Impersonation
;; Player actors only make assertions with their own PlayerID
;;
;; 6) Timeliness
;; Players play a card in each round
;; (round-has-begun n) ==> (eventually (plays-in-round pid n c))
;; for all players
;;
;; 7) Patience
;; The dealer waits for each player before starting the next round
;; (round-has-begun n) ==>
;;     (round-has-begun (n+1)) ==>
;;         (forall pid. exists c. (plays-in-round pid n c)))
;;
;; 8) Orderly Succession
;; The dealer starts round in the right order (0, 1, 2, 3, 4, ...)
;; Does the number of the round matter? It's not being used for anything. Maybe
;; it should just be "the dealer starts 10 different rounds"

;; ---------------------------------------------------------------------------------------------------
;; The Dealer

;; Deck -> Dealer
(define (spawn-dealer deck)
  (spawn #:name 'dealer
    ;; Nat [Set-of PlayerID] [List-of Row] [Hash-of PID [List-of Card]] Scores -> void
    (define (run-round current-round players rows initial-hands initial-scores)
      (define one-second-from-now (+ (current-inexact-milliseconds) 1000))

      (react
        (field [hands initial-hands]
               [moves '()])

        (for ([(pid hand) (in-hash (hands))])
          (assert (in-hand pid hand)))

        (assert (round-has-begun current-round rows))

        ;; [List-of PlayerID] Scores -> Void
        (define (conclude-round? players curr-scores)
          (when (= (set-count players) (length (moves)))
            ;; have all the moves, play some cards!
            (define-values (new-rows new-scores) (play-round rows (moves) curr-scores))
            (log-rows new-rows)
            (log-scores new-scores)

            (cond
              [(< current-round 10)
               (stop-current-facet (run-round (add1 current-round) players new-rows (hands) new-scores))]
              [else ;; the game is over!
               (define winner/s (lowest-score/s new-scores))
               (log-winner/s winner/s)
               (stop-current-facet (react (on-start (send! (game-over winner/s)))))])))

        ;; ASSUME no player plays multiple cards
        (on (asserted (played-in-round $pid current-round $c))
            (define m (played-in-round pid current-round c))
            
            (log-move m)
            (moves (cons m (moves)))
            (hands (hash-update (hands) pid (λ (hand) (remove c hand))))
            (conclude-round? players initial-scores))

        (on (asserted (later-than one-second-from-now))
            (define players-that-moved
              (for/set ([move (moves)])
                       (match-define (played-in-round p _r _c) move)
                       p))
            (log-elimination (set->list (set-subtract players players-that-moved)))

            (define (filter-keys h valid-keys)
              (for/hash ([(key val) (in-hash h)]
                         #:when (set-member? valid-keys key))
                (values key val)))

            (define filtered-hands (filter-keys (hands) players-that-moved))
            (define filtered-scores (filter-keys initial-scores players-that-moved))

            (hands filtered-hands)
            (conclude-round? players-that-moved filtered-scores))))

    (on-start
      (react
  
        (define/query-set players (player $id) id)

        (define registration-timeout (+ (current-inexact-milliseconds) CONN-DURATION))

        (on (asserted (later-than registration-timeout))
            (define num-players (set-count (players)))
            (unless (and (>= num-players 2) (<= num-players 10))
              (error "Take-5 is played with 2-10 players"))

            (let*-values ([(initial-hands deck) (deal (players) deck)]
                          ;; you really seem to need a draw-four 
                          [(r1-start deck) (draw-one deck)]
                          [(r2-start deck) (draw-one deck)]
                          [(r3-start deck) (draw-one deck)]
                          [(r4-start _) (draw-one deck)])

              (define initial-rows (list (row (list r1-start))
                                         (row (list r2-start))
                                         (row (list r3-start))
                                         (row (list r4-start))))

              (define initial-scores (for/hash ([pid (in-set (players))]) (values pid 0)))
              (run-round 1 (players) initial-rows initial-hands initial-scores)))))))

;; ---------------------------------------------------------------------------------------------------
;; Player Agents, AI

;; it seems like there could be a race s.t. the player sees the start of a round
;; before their hand updates.

(define (spawn-player pid)
  (spawn
    (define/query-value my-hand '() (in-hand pid $c) c)

    (assert (player pid))

    (during (round-has-begun $n $rows)
            (on-start (send! (move-request n (my-hand) rows)))
            (on (message (played-in-round pid n $card))
                (react (assert (played-in-round pid n card)))))))

;; Execute and ferry commands between clients and player components
;; -> Void
(define (spawn-tcp-translator)
  ;; Readable/Writable -> Bytes
  (define (convert-msg-to-bytes msg)
    (with-output-to-bytes 
      (λ () (write msg))))

  (spawn
    (during/spawn (tcp-connection $conn-id (tcp-listener CONNECT-PORT))
            (assert (tcp-accepted conn-id))

            (on (message (tcp-in-datum conn-id $v))
                (match v
                  [(declare-player pid) (spawn-player pid)]
                  [(played-in-round pid round-no card)
                   (send! (played-in-round pid round-no card))]))

            ;; FIXME I don't like that input and output aren't symmetrically processed
            (on (message (move-request $n $hand $rows))
                (define bs (convert-msg-to-bytes (move-request n hand rows)))
                (send! (tcp-out conn-id bs)))

            (on (message (game-over $ws))
                (send! (tcp-out conn-id (convert-msg-to-bytes (game-over ws))))
                (stop-current-facet)))

    (on (message (game-over $ws))
        (stop-current-facet))))


;; ===================================================================================================
;; Test Game


(spawn-dealer (shuffle the-deck))
(spawn-tcp-translator)

(module+ main )
