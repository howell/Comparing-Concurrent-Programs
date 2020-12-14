#lang racket

(require "deck.rkt")
(require "deal.rkt")
(require "logging.rkt")
(require "rules.rkt")
(require "struct.rkt")

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
;;
;; 3. The Game Observer, which monitors and waits for the result of a run
;;    of the game. There is one per game instance.

;; Conversations
;; There is a conversation about playing one round of the game. The Dealer starts a round
;; by sending each participating Player a Round message on the corresponding Player's
;; personal channel, containing the Player's hand, the current rows, and the a channel
;; provided by the Dealer for posting Move messages to. A Player makes a move by sending
;; a Move message to that channel, containing the PlayerID of the Player, the current
;; round number, and the card that the Player has selected from their hand to play.
;;
;; The Dealer leads 10 rounds, in sequence, in this way.
;;
;; Rules/Assumptions for this conversation:
;; 1. Players only play cards in their Hand
;; 2. Players send Move messages with their own PlayerID only
;; 3. Players only send one Move message per round
;; 
;; There is a conversation about the result of the game. The Game Observer observes a channel
;; for a DeclaredWinners message, containing the players from that game with the lowest scores,
;; indicating the end of and results for a game instance. The Dealer posts a DeclaredWinners message
;; to the channel that the Game Observer is listening to when the game instance ends.

;; Deck [List-of PlayerStruct] -> void
(define (make-dealer initial-deck players)
  (define game-result-chan (make-channel))
  (define moves-channel (make-channel))

  (struct all-moves (moves) #:prefab)

  (define num-players (length players))
  (unless (and (>= num-players 2) (<= num-players 10))
    (error "Take-5 is played with 2-10 players"))

  ;; Runs all 10 rounds of the game and posts the winners to the game results channel
  ;; [List-of PlayerStruct] [List-of Row] [Hash-of PlayerID Hand] [Hash-of PlayerID Score] -> void
  (define (run-rounds initial-players initial-rows initial-hands initial-scores)

    (define (handle-moves round-number num-players dealer-chan)
      (define send-moves-chan (make-channel))
      (thread
        (thunk
          (define round-deadline (+ (current-inexact-milliseconds) 1000))
          (define round-timeout (alarm-evt round-deadline))

          (define (send-moves-to-dealer moves)
            (channel-put dealer-chan (all-moves moves)))

          ;; send timeout message here
          (let loop ([moves '()])
            (sync
              (handle-evt
                send-moves-chan
                (match-lambda
                  ;; TODO check equality on n?
                  [(and m (played-in-round pid n c))
                   (log-move m)
                   (define new-moves (cons m moves))
                   (if (= (length new-moves) num-players)
                     (send-moves-to-dealer new-moves)
                     (loop new-moves))]))
              (handle-evt
                round-timeout
                (λ (_)
                   (send-moves-to-dealer moves)))))))
      send-moves-chan)

    ;; Loop once for each round
    (let loop ([players initial-players]
               [rows initial-rows]
               [hands initial-hands]
               [scores initial-scores]
               [round-count 1])

      (define send-moves-chan (handle-moves round-count (length players) moves-channel))

      ;; Send a message to players in a separate thread
      ;; (PlayerStruct -> Any) -> Void
      (define (send-to-players msg-ctor)
        (for ([player players])
          (thread
            (thunk
              (channel-put (player-chan player)
                           (msg-ctor player))))))

      ;; notify all players that they must move
      (send-to-players
        (λ (p)
           (round round-count (hash-ref hands (player-id p)) rows send-moves-chan)))

      ;; Get all moves for the round
      (define moves
        (match (channel-get moves-channel)
          [(all-moves moves) moves]))

      (define players-that-moved
        (for/set ([m moves])
          (played-in-round-player m)))

      (define filtered-scores
        (for/hash ([(pid score) (in-hash scores)]
                   #:when (set-member? players-that-moved pid))
          (values pid score)))

      (define-values (new-rows new-scores) (play-round rows moves filtered-scores))
      (log-rows new-rows)
      (log-scores new-scores)

      (cond
        [(< round-count 10)
         (define new-hands
           (for/hash ([move moves])
             (define pid (played-in-round-player move))
             (values pid (remove (played-in-round-card move) (hash-ref hands pid)))))

         (define new-players
           (for/list ([p players]
                     #:when (set-member? players-that-moved (player-id p)))
             p))

         (loop new-players new-rows new-hands new-scores (+ round-count 1))]
        [else ;; Game is over, determine a winner
          (define winner/s (lowest-score/s new-scores))
          (log-winner/s winner/s)
          (send-to-players (λ (_) (game-over winner/s)))
          (channel-put game-result-chan (declared-winner/s winner/s))])))

  (thread
    (thunk
      (define initial-scores (for/hash ([player players]) (values (player-id player) 0)))
      (define-values (initial-hands deck) (deal (map player-id players) initial-deck))
      (define-values (starting-rows _)
        (for/fold ([rows '()]
                   [curr-deck deck])
                  ([_ (in-range 4)])
          (define-values (starting-card new-deck) (draw-one curr-deck))
          (values (cons (row (list starting-card)) rows) new-deck)))

      (log-rows starting-rows)

      (run-rounds players starting-rows initial-hands initial-scores)))
  game-result-chan)

;; Listener -> [List-of Ports]
(define (accept-connections listener)
  (define accept-deadline (+ (current-inexact-milliseconds) CONN-DURATION))
  (define accept-timeout (alarm-evt accept-deadline))

  (let loop ([connections '()])
    (define accept-evt (tcp-accept-evt listener))
    (sync
      (handle-evt
        accept-evt
        (match-lambda
          [(list input output)
           (log-connection)
           (remove-tcp-buffer input output)
           (loop (cons (ports input output) connections))]))
      (handle-evt 
        accept-timeout
        (λ (_) connections)))))

;; Listener -> [List-of PlayerStruct]
(define (initialize-players listener)
  (define connections (accept-connections listener))

  ;; TODO add some timeout here, not sure how to give everybody a chance
  ;; loop through one at a time and see if they're ready to read?
  ;; or, wait one second, then check which ones are ready to read and accept those
  (for/list ([conn connections])
    (match-define (ports input output) conn)
    (match (read input)
      [(declare-player name)
       (log-registration name)
       (make-player name input output)])))

;; Create a Player component that communicates with the client over TCP
;; PlayerID Port Port -> PlayerStruct
(define (make-player name input-port output-port)
  (define round-chan (make-channel))
  (thread
    (thunk
      (let loop ()
        (define dealer-msg (channel-get round-chan))
        (match dealer-msg
          [(round number hand rows move-chan)
           (write (move-request number hand rows) output-port)
           (define move (read input-port))
           (match move
             [(played-in-round _ _ _)
              (channel-put move-chan move)
              (loop)])]
          [(game-over _)
           (write dealer-msg output-port)
           (close-ports input-port output-port)]))))
  (player name round-chan))

;; -> void
(define (play-game)
  (define server (tcp-listen CONNECT-PORT))
  (define players (initialize-players server))

  (define game-result-chan (make-dealer (shuffle the-deck) players))
  (channel-get game-result-chan)

  (sleep 1)
  (tcp-close server))

(play-game)