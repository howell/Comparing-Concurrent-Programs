#lang racket

(require [only-in racket/random random-ref])
(require "deck.rkt")
(require "deal.rkt")
(require "logging.rkt")
(require "rules.rkt")

;; a PlayerID is a Symbol
;; a Hand is a [List-of Card]
;; a Score is a Nat
;; a Scores is a [Hash-of PlayerID Score]
;; Scores must contain an entry for each existing PlayerID

;; a Round is a (round Nat [List-of Card] [List-of Row] [Chan-of Move])
(struct round (number hand rows move-chan) #:transparent)

;; a PlayerStruct is a (player PlayerID [Chan-of Round])
(struct player (id chan) #:transparent)

;; a DeclaredWinners is a (declared-winner/s [List-of PlayerID])
(struct declared-winner/s (player/s) #:transparent)

;;;;;;;;;;; Protocol ;;;;;;;;;;;;;;;

;; There are two roles in this protocol:
;; 1. The Dealer, which manages the play of the game. There is one Dealer per
;;    per game instance.
;;
;; 2. The Player, which acts in the game and tries to win while obeying the
;;    rules. There can be between 2 and 10 Players.

;; FIXME specify the channels that messages are sent to/received from.
;; Conversations
;; There is a conversation about playing one round of the game. The Dealer starts a round
;; by sending each participating Player a Round message, notifying the Player of their hand,
;; the current state of the rows, and the channel to send Move messages to. A Player makes
;; a move by sending a Move message to the Dealer, containing the PlayerID of the player, the
;; current round number, and the card that the player has selected from their hand to play.
;;
;; The Dealer leads 10 rounds, in sequence, in this way.
;;
;; Rules/Assumptions for this conversation:
;; 1. Players only play cards in their Hand
;; 2. Players send Move messages with their own PlayerID only
;; 3. Players only send one Move message per round

;; Deck [List-of PlayerStruct] [Chan-of DeclaredWinners] -> void
(define (make-dealer initial-deck players main-chan)
  (define moves-channel (make-channel))

  (define num-players (length players))
  (unless (and (>= num-players 2) (<= num-players 10))
    (error "Take-5 is played with 2-10 players"))

  ;; [List-of Row] [Hash-of PlayerID Hand] [Hash-of PlayerID Score] -> void
  (define (run-rounds rows hands scores)
    ;; Loop once for each round
    (let loop ([rows rows]
               [hands hands]
               [scores scores]
               [round-count 1])

      (for ([player players])
        (channel-put (player-chan player)
                     (round round-count (hash-ref hands (player-id player)) rows moves-channel)))

      ;; Get all moves for the round
      (define moves
        (for/list ([_ (in-range num-players)])
          (define m (channel-get moves-channel))
          (log-move m)
          m))

      (define-values (new-rows new-scores) (play-round rows moves scores))
      (log-rows new-rows)
      (log-scores new-scores)

      (define new-hands
        (for/hash ([move moves])
          (define pid (played-in-round-player move))
          (values pid (remove (played-in-round-card move) (hash-ref hands pid)))))

      (cond
        [(< round-count 10)
         (loop new-rows new-hands new-scores (+ round-count 1))]
        [else ;; Game is over, determine a winner
          (define winner/s (lowest-score/s scores))
          (log-winner/s winner/s)
          (channel-put main-chan (declared-winner/s winner/s))])))

  (thread
    (thunk
      (define initial-scores (for/hash ([player players]) (values (player-id player) 0)))
      (define-values (initial-hands deck) (deal (map player-id players) initial-deck))
      (define-values (starting-rows _)
        (for/fold ([rows '()]
                   [curr-deck deck])
                  ([_ (in-range 5)])
          (define-values (starting-card new-deck) (draw-one curr-deck))
          (values (cons (row (list starting-card)) rows) new-deck)))

      (log-rows starting-rows)

      (run-rounds starting-rows initial-hands initial-scores))))

;; [List-of Symbol] -> [List-of PlayerStruct]
(define (make-player* players)
  ;; Hand [List-of Row] -> Card
  (define (random-player hand rows)
    (random-ref hand))

  (for/list ([player (in-list players)])
    (make-player player random-player)))

;; PlayerID (Hand [List-of Row] -> Card) -> PlayerStruct
(define (make-player pid make-decision)
  (define round-chan (make-channel))
  (thread
    (thunk
      (let loop ()
        (define round-info (channel-get round-chan))
        (match round-info
          [(round number hand rows move-chan)
           (define player-decision (make-decision hand rows))
           (log-player-decision pid player-decision hand)
           (channel-put move-chan (played-in-round pid number player-decision))
           (loop)]))))

  (player pid round-chan))

;; definitely rename, might make sense to create it in the dealer
;; this _should_ be described in the protocol
(define main-chan (make-channel))

(define all-players (make-player* '(a b c d)))
(make-dealer (shuffle the-deck) all-players main-chan)

(channel-get main-chan)
