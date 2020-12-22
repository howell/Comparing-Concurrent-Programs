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

;; a UserRegister is a (user-register PlayerID [Chan-of (U Registered UserLoggedIn)])
(struct user-register (id chan) #:transparent)

;; FIXME need to define LobbyMsg
;; a UserLoggedIn is a (user-logged-in [Chan-of LobbyMsg])
(struct user-logged-in (lobby-chan) #:transparent)

;; a UserListRooms is a (user-list-rooms [Chan-of LobbyMsg])
(struct user-list-rooms (chan) #:transparent)

;; a UserGetResults is a (user-get-results UserID [Chan-of LobbyMsg])
(struct user-get-results (id chan) #:transparent)

;; I don't know what RoomMsg means anymore
;; a UserCreateRoom is a (user-create-room UserID [Chan-of RoomMsg]) ;; FIXME should it be RoomMsg or LobbyMsg?
(struct user-create-room (id chan) #:transparent)

;; a UserJoinRoom is a (user-join-room UserID RoomID [Chan-of RoomMsg])
(struct user-join-room (user id chan) #:transparent)

;; a UserRoom is a (user-room RoomID [Chan-of RoomMsg]) ;; FIXME should there be a difference in the reply to Host vs. Guest?
(struct user-room (id chan) #:transparent)

;; a DeclaredWinners is a (declared-winner/s [List-of PlayerID])
(struct declared-winner/s (player/s) #:transparent)

;;;;;;;;;;; Protocol ;;;;;;;;;;;;;;;

;; NOTE should messages fail silently or always some ack?

;; There is a conversation about registration.
;; Users register for a user account by sending a UserRegister message to the 
;; Authentication Manager, including the UserID of the User and the Channel on 
;; which to receive authentication replies from the Authentication Manager.
;; The Authentication Manager replies with a Registered message, containing the
;; UserToken corresponding to the User that the User must use to log into their
;; user account.
;; 
;; There is a conversation about Login.
;; Users send a Login message to the Authentication Manager containing their UserID
;; and their UserToken. If the UserToken correctly corresponds to the UserID, then
;; the Authentication Manager responds with a UserLoggedIn message, containing the
;; Channel of the Lobby.
;;
;; To view rooms available to join in the lobby, a User sends a UserListRooms message
;; to the Lobby, containing a Channel for receiving Lobby replies. The Lobby replies
;; with a Rooms message, containing a List of RoomIDs.
;;
;; To view the results of games played by a User, a User sends a UserGetResults to
;; the Lobby, containing the UserID of the User and a Channel for the Lobby to send
;; replies to. The Lobby replies with a Results message, containing a List-of Result
;; structs, where each Scores hash contains the UserID as a key (representing a game
;; the User participated in).


;;
;; There are multiple conversations between Users and the Lobby.
;; To view rooms available to join in the Lobby, Users send a List Rooms message to
;; the Lobby, containing the channel on which to send replies. The Lobby responds
;; with a Rooms message, containing a list of unique Room IDs.
;; 
;; To view the results of games played by the User, the User sends a View Results
;; message to the Lobby, containing the channel to send replies on. The Lobby replies
;; with a Results message, containing a list of the Results of games played by the User.
;;
;; To create a room, Users send a Create Room message to the Lobby, containing
;; the User's channel. The Lobby creates a new Room component associated
;; with a unique ID, and the Room sends a NewRoom message to the client, containing
;; the ID of the Room and the channel of communication for the Room.
;; 
;; To join a room, Users send a Join Room message to the Lobby, containing the channel
;; of the User. The Lobby forwards the message to the Room, and the Room replies to
;; the User with a JoinedRoom message, containing the channel of the Room for communication.
;; The User is designated as the Guest.
;; 
;; To Log Out, Users send a Log Out message to the Lobby. The Lobby stops all future
;; communication with the User until they have logged back in via the Authentication
;; Manager.
;;
;; There are multiple conversations between the Room and its Host and Guests.
;; To cancel a game, the Host sends a Cancel Game message to the Room. The Room
;; sends a Cancelled Game message to the Host and Guests, and can no longer be
;; communicated with.
;;
;; To start a game, the Host sends a Start Game message to the Room. The Room
;; spawns a Dealer which sends a Started Game message to the Host and Guests.
;; Host and Guests are then designated as Players.
;;
;; A Guest may leave the game by sending a Leave Game message to the room.
;; They can no longer communicate with that Room unless they join it again.
;;
;; There are two conversations regarding playing the game:
;; To play a round of the game, the Dealer sends each Player a Round message
;; containing the Player's hand, the current rows, and a channel for posting
;; Move messages to. A Player makes a move by sending a Move message to that
;; channel, containing the PlayerID of the Player, the current round number, and
;; the card that the Player has selected from their hand to play.
;; The Dealer leads 10 rounds in sequence in this way.
;;
;; Rules/Assumptions for this conversation:
;; 1. Players only play cards in their Hand
;; 2. Players send Move messages with their own PlayerID only
;; 3. Players only send one Move message per round
;;
;; There is a conversation about the result of the game. Upon the conclusion of
;; the game, the Dealer sends a GameResult message to all Players and the Lobby.
;; The Dealer terminates and cannot be communicated with any further.

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
;; (define (initialize-players listener)
;;   (define connections (accept-connections listener))

;;   ;; TODO add some timeout here, not sure how to give everybody a chance
;;   ;; loop through one at a time and see if they're ready to read?
;;   ;; or, wait one second, then check which ones are ready to read and accept those
;;   (for/list ([conn connections])
;;     (match-define (ports input output) conn)
;;     (match (read input)
;;       [(declare-player name)
;;        (log-registration name)
;;        (make-player name input output)])))

;;;; OLD PLAYER-SERVER CODE ;;;;;;
  ;; (define round-chan (make-channel))
  ;; (thread
  ;;   (thunk
  ;;     (let loop ()
  ;;       (define dealer-msg (channel-get round-chan))
  ;;       (match dealer-msg
  ;;         [(round number hand rows move-chan)
  ;;          (write (move-request number hand rows) output-port)
  ;;          (define move (read input-port))
  ;;          (match move
  ;;            [(played-in-round _ _ _)
  ;;             (channel-put move-chan move)
  ;;             (loop)])]
  ;;         [(game-over _)
  ;;          (write dealer-msg output-port)
  ;;          (close-ports input-port output-port)]))))
  ;; (player name round-chan))

;; -> void
;; (define (play-game)
;;   (define server (tcp-listen CONNECT-PORT))
;;   (define players (initialize-players server))

;;   (define game-result-chan (make-dealer (shuffle the-deck) players))
;;   (channel-get game-result-chan)

;;   (sleep 1)
;;   (tcp-close server))


;;;;;;;;;;;;;;;;; EXTENDED VERSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-clients listener auth-chan)
  (define accept-deadline (+ (current-inexact-milliseconds) CONN-DURATION))
  (define accept-timeout (alarm-evt accept-deadline))

  (let loop ()
    (define accept-evt (tcp-accept-evt listener))
    (sync
      (handle-evt
        accept-evt
        (match-lambda
          [(list input output)
           (log-connection)
           (remove-tcp-buffer input output)
           (make-user input output auth-chan)
           (loop)]))
      (handle-evt 
        accept-timeout
        (λ (_) #f)))))

(define (make-room room-id lobby-chan host-chan)
  (define lobby-recv-chan (make-channel))
  (define host-recv-chan (make-channel))
  (define guest-recv-chan (make-channel))

  (thread
    (thunk
      (channel-put host-chan (user-room room-id host-recv-chan))

      (let loop ([guests '()]) ;; [Hash-of UserID Chan]
        (define guest-evts (apply choice-evt guests))
        (sync
          (handle-evt
            guest-evts
            (match-lambda
              [(leave-room user-id)
               (loop (hash-remove guests user-id))]))
          (handle-evt
            host-chan
            (match-lambda
              [(cancel-game)
               (for ([(user-id chan) guests])
                 (channel-put chan (game-cancelled room-id)))
               (channel-put lobby-chan (game-cancelled room-id))]))))))

  lobby-recv-chan)

(define (make-lobby)
  (define user-comm-chan (make-channel))
  (define room-comm-chan (make-channel))

  (thread
    (thunk
      (let loop ([room-lookup (hash)]   ;; [Hash-of RoomID Chan]
                 [score-lookup (hash)]) ;; [Hash-of RoomID Scores]
        (define msg (channel-get user-comm-chan))
        (match msg
          [(user-list-rooms resp-chan)
           (channel-put resp-chan (rooms (hash-keys room-lookup)))
           (loop room-lookup score-lookup)]
          [(user-get-results id resp-chan)
           (define user-results
             (for/list ([(room-id scores) (in-hash score-lookup)]
                        #:when (hash-has-key? scores id))
               (result room-id scores)))

           (channel-put resp-chan (results user-results))
           (loop room-lookup score-lookup)]
          [(user-create-room id resp-chan)
           (define room-id (intern-symbol (gensym id)))
           (define room-chan (make-room room-id room-comm-chan resp-chan))
           (loop (hash-set room-lookup room-id room-chan) score-lookup)]
          [(user-join-room user-id room-id resp-chan)
           (define room-chan (hash-ref room-lookup room-id))
           (channel-put room-chan msg)
           (loop room-lookup score-lookup)]))))
  user-comm-chan)


;; FIXME is this a good design?
;; Create a User component that communicates with the client over TCP
;; Port Port -> Void
(define (make-user input-port output-port auth-chan)
  (define recv-auth-chan (make-channel))
  (define recv-lobby-chan (make-channel))

  ;; Handle communication while client authenticating 
  ;; -> Void
  (define (handle-auth-comm)
    (define client-msg (read input-port))
    (match client-msg
      [(register user-id)
       (register-user user-id)
       (handle-auth-comm)]
      [(login user-id token)
       (define lobby-chan (log-in-user user-id token))
       (handle-lobby-comm lobby-chan)]))

  ;; Handle communication between client and lobby
  ;; Chan -> Void
  (define (handle-lobby-comm lobby-chan)
    (let loop ()
      (define client-msg (read input-port))
      (match client-msg
        [(list-rooms id)
         (log-list-rooms id)
         (channel-put lobby-chan (user-list-rooms recv-lobby-chan))
         (define server-msg (channel-get recv-lobby-chan))

         (match server-msg
           [(rooms items)
            (log-rooms items)
            (write server-msg output-port)
            (loop)])]

        [(get-results id)
         (channel-put lobby-chan (user-get-results id recv-lobby-chan))
         (define server-msg (channel-get recv-lobby-chan))

         (match server-msg
           [(results items)
            (write server-msg output-port)
            (loop)])]

        [(create-room id)
         (define room-recv-chan (make-channel))
         (channel-put lobby-chan (user-create-room id room-recv-chan))
         (define server-msg (channel-get room-recv-chan))

         (match server-msg
           [(user-room room-id room-chan)
            (write (room room-id) output-port)
            (close-ports input-port output-port)])]

        [(join-room user-id room-id)
         (define room-recv-chan (make-channel))
         (channel-put lobby-chan (user-join-room user-id room-id room-recv-chan))
         (define server-msg (channel-get room-recv-chan))

         (match server-msg
           [(user-room room-id room-chan)
            (write (room room-id) output-port)
            (close-ports input-port output-port)])])))

  ;; Register the client with a user account
  ;; UserID -> Void
  (define (register-user id)
    (channel-put auth-chan (user-register id recv-auth-chan))
    (define server-msg (channel-get recv-auth-chan))

    (match server-msg
      [(registered token)
       (log-registration id)
       (write server-msg output-port)]))

  ;; UserID UserToken -> Void
  (define (log-in-user id token)
    (channel-put auth-chan (login id token))
    (define server-msg (channel-get recv-auth-chan))

    (match server-msg
      [(user-logged-in lobby-chan)
       (log-login id)
       (write (logged-in) output-port)
       lobby-chan]))

  (thread
    (thunk
      (handle-auth-comm))))

;; Chan -> Chan
(define (make-authentication-manager lobby-chan auth-db)
  (define auth-chan (make-channel))
  (thread
    (thunk
      (let loop ([user-tokens (hash)] ;; [Hash-of UserID UserToken]
                 [user-comms (hash)]) ;; [Hash-of UserID [Chan-of (U Registered LoggedIn)]]
        (define msg (channel-get auth-chan))
        (match msg
          [(user-register id user-chan)
           (define user-token (intern-symbol (gensym id)))
           (channel-put user-chan (registered user-token))
           (define new-tokens (hash-set user-tokens id user-token))
           (write-to-file new-tokens auth-db #:exists 'replace)
           (loop new-tokens (hash-set user-comms id user-chan))]
          [(login id token)
           (when (symbol=? token (hash-ref user-tokens id))
             (channel-put (hash-ref user-comms id) (user-logged-in lobby-chan))
             (loop user-tokens user-comms))]))))
  auth-chan)

(define general-chan (make-channel))
(define lobby-chan (make-lobby))

(define server (tcp-listen CONNECT-PORT))

(define auth-chan (make-authentication-manager lobby-chan "login.info"))
(create-clients server auth-chan)

(channel-get lobby-chan)
