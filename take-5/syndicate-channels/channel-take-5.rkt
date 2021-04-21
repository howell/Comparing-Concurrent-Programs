#lang racket

(require "deck.rkt")
(require "deal.rkt")
(require "logging.rkt")
(require "rules.rkt")
(require "struct.rkt")

;; FIXME I suspect much of the below is wrong

;; a PlayerID is a Symbol
;; a Hand is a [List-of Card]
;; a Score is a Nat
;; a Scores is a [Hash-of PlayerID Score]
;; Scores must contain an entry for each existing PlayerID

;; an AuthMsg is one of:
;; - UserRegister
;; - Registered
;; - Login
;; - UserLoggedIn

;; a LobbyMsg is one of:
;; - ListRooms
;; - Rooms
;; - GetResults
;; - Results
;; - CreateRoom
;; - JoinRoom
;; - UserRoom
;; - Logout
;; - Ack

;; a RoomMsg is one of:
;; - CancelGame
;; - GameCancelled
;; - LeaveGame
;; - Ack

;; a DBMsg is one of:
;; - Ack
;; - Select
;; - Data
;; - Insert

;; a UserRegister is a (user-register PlayerID [Chan-of UserRegistered])
(struct user-register (id chan) #:transparent)

;; a UserRegistered is a (user-registered Token [Chan-of LobbyMsg])
(struct user-registered (token chan) #:transparent)

;; a CreateSession is a (create-session UserID)
(struct create-session (user) #:transparent)

;; a SessionCreated is a (session-created [Chan-of LobbyMsg])
(struct session-created (chan) #:transparent)

;; a UserLogin is a (user-login UserID [Chan-of UserLoggedIn])
(struct user-login (id token chan) #:transparent)

;; a UserLoggedIn is a (user-logged-in [Chan-of LobbyMsg])
(struct user-logged-in (lobby-chan) #:transparent)

;; an ApproveJoin is an (approve-join UserID [Chan-of LobbyMsg])
(struct approve-join (user chan) #:transparent)

;; a UserRoom is a (user-room RoomID [Chan-of RoomMsg])
(struct user-room (id chan) #:transparent)

;; a GuestRoom is a (guest-room RoomID [Chan-of RoomMsg] [Chan-of RoomMsg]) ;; FIXME two different types of channels
(struct guest-room (id chan broadcast) #:transparent)

;; a RoomTerminated is a (room-terminated RoomID)
(struct room-terminated (id) #:transparent)

;; a UserGameStarted is a (user-game-started Chan)
(struct user-game-started (chan) #:transparent)

;; a GameResults is a (game-results RoomID Score)
(struct game-results (room-id scores) #:transparent)

;; a Select is a (select Symbol Chan)
(struct select (key reply-chan) #:transparent)

;; a Data is a (data Any)
(struct data (contents) #:transparent)

;; an Insert is an (insert Symbol Any Chan)
(struct insert (key val reply-chan) #:transparent)

(define LOGIN-INFO 'login-info)
(define RESULTS-INFO 'results-info)

;; Create a repeatedly-synchronizable event that provides datums from an Input Port
;; Port -> Synchronizable Event 
(define (read-datum-evt in)
  (define chan (make-channel))

  (thread
    (thunk
      (let loop ()
        (when (not (port-closed? in))
          (define msg (read in))
          (channel-put chan msg)
          (loop)))))

  chan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authentication Manager Conversations
;; ------------------------------------
;; There is a conversation about registration.
;; Users register for a user account by sending a UserRegister message to the 
;; Authentication Manager, including the UserID of the User and the Channel on 
;; which to receive replies. If the UserID is unique, then the Authentication Manager
;; sends a CreateSession message to the Protected Component, containing the UserID
;; of the User registering. The Protected Component sends back a SessionCreated message,
;; containing a Channel for communication with a User. the Authentication Manager
;; forwards this channel to the user in a UserRegistered message, which also contains
;; a unique password for the new user. If the UserID was not unique, then the
;; Authentication Manager sends back a TakenId message, containing the already claimed
;; UserID.
;; 
;; There is a conversation about login.
;; Users send a Login message to the Authentication Manager containing their UserID,
;; their UserToken, and a Channel on which to receive replies. If the UserToken correctly
;; corresponds to the UserID, then the Authentication Manager sends a CreateSession
;; message to the Protected Component, containing the UserID of the User logging in.
;; The Protected Component sends back a SessionCreated message, containing a Channel for
;; communication with a User. The Authentication Manager forwards this channel to the
;; user in a UserLoggedIn message.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lobby Conversations
;; -------------------
;; There is a conversation about the open Rooms in the Lobby.
;; To view the rooms that are available to join in the Lobby, a User sends a
;; ListRooms message to the Lobby, containing the UserID of the User. The
;; Lobby responds with a Rooms message, containing a List of RoomIDs.
;;
;; There is a conversation about the Results of games played by a User.
;; To view past game results, a User sends a GetResults message to the Lobby,
;; containing the User's UserID. The Lobby replies with a Results message, containing
;; a List of Scores where each Scores contains the User's UserID as a key.
;;
;; There is a conversation about creating a room in the Lobby.
;; To create a room, Users send a CreateRoom message to the Lobby, containing
;; the UserID of the User. The newly-created Room uses that channel to send
;; a UserRoom message, containing the RoomID of the Room and the channel on
;; which the User and Room can communicate.
;;
;; There is a conversation about joining a room in the Lobby.
;; To join a room, Users send a JoinRoom message to the Lobby, containing the
;; UserID of the User and the RoomID of the room that they would like to join.
;; If the room exists, then the Lobby sends the corresponding Room an ApproveJoin
;; message, containing the UserID of the User and the Channel on which the User
;; and Lobby communicate, and the Room sends back along the aforementioned channel
;; a UserRoom message, containing the RoomID of the Room and the channel on which
;; the User and Room can Communicate.
;; If the room doesn't exist, the Lobby sends back a RoomNotFound message.
;;
;; There is a conversation about logging out from the Lobby.
;; To Log out and exit the Lobby, a User sends the lobby a Logout message, containing
;; the UserID of the User. The Lobby responds with an Ack message, and the User
;; can no longer contact the Lobby until they have logged back in.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence Conversations
;; -------------------------
;; There are two participants in a persistence conversation: the Database that stores,
;; updates and retrieves data, and a Client that makes requests of the Database.
;;
;; There is a conversation about fetching data.
;; A Client fetches data with a Select message, containing a symbol and the channel
;; on which to receive replies from the Database. The Database responds with a Data
;; message, containing the data associated with that symbol if it exists, and
;; #f otherwise.
;;
;; There is a conversation about updating data.
;; A Client inserts or updates data by sending an Insert message, containing a symbol,
;; data to associate with that symbol, and a channel on which to send replies. The
;; Database replies with an Acknowledgement message.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Room Conversations
;; ------------------
;; There are three participants in Room-related conversations. There is one Room,
;; which holds the players currently staged to play a game. There is one Host, who
;; created the game. And there are multiple Guests, who would also like to play the
;; game with the Host.
;;
;; There is a conversation about cancelling a game.
;; To cancel a game, the Host sends a CancelGame message to the Room. The Room sends
;; a CancelledGame message to the Host and Guests, and a TerminatedRoom message to
;; the Lobby, containing the Room's RoomID. The Room can no longer be communicated
;; with from that point onward.
;;
;; There is a conversation about starting the game.
;; To start a game, the Host sends a StartGame message to the room. The Room replies
;; with a GameNotStarted message if there aren't sufficient players to start the game.
;; Otherwise, the Room spawns a Dealer that broadcasts a UserGameStarted message to the
;; Host and Guests containing the new channel by which they will communicate with
;; the Dealer, and a GameHasBegun message to the Lobby. At that point, the Room
;; can no longer be communicated with.
;;
;; There is a conversation about leaving a game.
;; To leave a game, a Guest sends a LeaveGame message to the room. The Room replies
;; with an Acknowledgement message to the Guest. That Guest can no longer communicate
;; with that Room unless the Guest requests to re-join from the Lobby.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealer Conversations
;; --------------------
;; There is a conversation at the end of a game.
;; Upon the conclusion of the game, the Dealer sends all Players a GameOver message,
;; containing the player(s) with the lowest scores in the game. The Dealer sends
;; the Lobby a GameResults message, containing the Score for that game. Players
;; then resume conversations with the Lobby, and the Dealer can no longer be
;; communicated with.
;;
;; There is a conversation about playing a round of the game.
;; At the beginning of the round, the Dealer sends all active Players a MoveRequest
;; message, containing the current round number, the current hand of cards for
;; that Player, and the current rows. Players reply with a PlayedInRound message,
;; containing the Player's ID, the current round number, and the card the Player
;; has selected from their hand. If a Player hasn't replied within the time limit,
;; they are removed for the remainder of the game.
;; 
;; The Dealer expects Players to follow these rules:
;; 1. Players only select a card from their own Hand
;; 2. The RoundNumber in a player's reply matches the RoundNumber sent by the Dealer
;; 3. Players only play moves with their own PlayerID
;; 4. Players only make one move per round
;;
;; The Dealer leads 10 rounds of the game.

;; Listener [Chan-of UserRegister] -> Void
(define (create-clients listener auth-chan)
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
           (loop)])))))

;; RoomID [Hash-of UserID Chan] Chan
(define (make-dealer game-id initial-player-lookup lobby-chan)
  (thread
   (thunk

    (channel-put lobby-chan (game-has-begun game-id))

    (define player-lookup
      (for/hash ([(p-name _) (in-hash initial-player-lookup)])
        (values p-name (make-channel))))

    (for ([(p-name chan) (in-hash player-lookup)])
      (channel-put (hash-ref initial-player-lookup p-name) (user-game-started chan)))

    ;; Runs all 10 rounds of the game and posts the winners to the game results channel
    ;; [List-of UserID] [List-of Row] [Hash-of UserID Hand] [Hash-of UserID Score] -> Score
    (define (run-rounds initial-players initial-rows initial-hands initial-scores)

      (define (move-handler round-number dealer-chan p msg-ctor)
        (define send-moves-chan (make-channel))
        (thread
         (thunk
          (define player-chan (hash-ref player-lookup p))
          ;; send the message to the player
          (channel-put player-chan (msg-ctor p))
          (let ([m (channel-get player-chan)])
            (channel-put send-moves-chan m))))
        send-moves-chan)

      ;; Loop once for each round
      (let loop ([players initial-players]
                 [rows initial-rows]
                 [hands initial-hands]
                 [scores initial-scores]
                 [round-count 1])

        ;; Send a message to players in a separate thread
        ;; (PlayerStruct -> Any) -> Void
        (define (send-to-players msg-ctor)
          (for ([player players])
            (thread
              (thunk
                (channel-put (hash-ref player-lookup player)
                             (msg-ctor player))))))

        (define (process-results moves)
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
                          #:when (set-member? players-that-moved p))
                  p))

             (loop new-players new-rows new-hands new-scores (+ round-count 1))]
            [else ;; Game is over, determine a winner
             (define winner/s (lowest-score/s new-scores))
             (log-winner/s winner/s)
             ;; this is now the only place that uses this `send-to-players` function, maybe you should toss it.
             (send-to-players (λ (_) (game-over winner/s)))
             new-scores]))

        (define round-deadline (+ (current-inexact-milliseconds) 10000))
        (define round-timeout (alarm-evt round-deadline))

        (define move-handlers
          (for/list ([p players])
            (move-handler round-count moves-channel p
                          (λ (p) (move-request round-count (hash-ref hands p) rows)))))

        (let loop ([moves '()])

            (define (handle-move-evt msg)
              (match msg
                [(played-in-round pid n c)
                 (log-move msg)
                 (define new-moves (cons msg moves))
                 (if (= (length new-moves) num-players)
                   (process-results new-moves)
                   (loop new-moves))]))

            (define move-evts
              (apply choice-evt
                     (map (λ (handler-chan) (handle-evt handler-chan handle-move-evt))
                          move-handlers)))

            (sync move-evts
                  (handle-evt round-timeout
                              (λ (_) (process-results moves)))))))

    (define game-result-chan (make-channel))
    (define moves-channel (make-channel))

    (define players (hash-keys player-lookup))
    (define num-players (length players))

    (define initial-scores (for/hash ([player players]) (values player 0)))
    (define initial-deck (shuffle-deck (generate-deck)))

    (define-values (initial-hands deck) (deal players initial-deck))
    (define-values (starting-rows _)
      (for/fold ([rows '()]
                 [curr-deck deck])
                ([_ (in-range 4)])
        (define-values (starting-card new-deck) (draw-one curr-deck))
        (values (cons (row (list starting-card)) rows) new-deck)))

    ;; FIXME would be great to move this to the `run-rounds` function, or move the setup to a separate function
    (log-rows starting-rows)

    (define final-result (run-rounds players starting-rows initial-hands initial-scores))
    (channel-put lobby-chan (game-results game-id final-result)))))


;; RoomID [Chan-of LobbyMsg] [Chan-of Room] -> Void
(define (make-room room-id lobby-chan host-name temp-host-chan)
  (thread
    (thunk
      (define host-chan (make-channel))
      (channel-put temp-host-chan (user-room room-id host-chan))

      (let loop ([guests (hash)]) ;; [Hash-of UserID GuestRoom]
        (define (handle-guest-evt msg)
          (match msg
            [(leave-room user)
             (match-define (guest-room _ guest-chan _) (hash-ref guests user))
             (channel-put guest-chan (ack))
             (loop (hash-remove guests user))]))

        (define guest-evts
          (apply choice-evt
                 (map (λ (guest) 
                         (match-define (guest-room _ guest-chan _) guest)
                         (handle-evt guest-chan handle-guest-evt))
                      (hash-values guests))))

        (sync
          guest-evts
          (handle-evt
            lobby-chan
            (match-lambda
              [(approve-join user-id user-chan)
               (define guest-chan (make-channel))
               (define broadcast-guest-chan (make-channel))
               (define guest-room-msg (guest-room user-id guest-chan broadcast-guest-chan))
               (channel-put user-chan guest-room-msg)
               (loop (hash-set guests user-id guest-room-msg))]))

          (handle-evt
            host-chan
            (match-lambda
              [(cancel-game)
               (channel-put lobby-chan (room-terminated room-id))
               (channel-put host-chan (game-cancelled room-id))
               (for ([guest-room-struct (hash-values guests)])
                 (match-define (guest-room _user-id _guest-chan broadcast-chan) guest-room-struct)
                 (channel-put broadcast-chan (game-cancelled room-id)))]
              [(start-game)
               ;; FIXME game should also be prevented if we have > 10 players
               (cond
                 [(= (hash-count guests) 0)
                  (channel-put host-chan (game-not-started))
                  (loop guests)]
                 [else
                  (define guest-chan-lookup 
                    (for/hash ([(name guest-room-msg) (in-hash guests)])
                      (match-define (guest-room _ _ broadcast-guest-chan) guest-room-msg)
                      (values name broadcast-guest-chan)))
                  (define player-chan-lookup (hash-set guest-chan-lookup host-name host-chan))
                  (make-dealer room-id player-chan-lookup lobby-chan)])])))))))

;; -> Void
(define (make-lobby db-chan)
  (define auth-comm-chan (make-channel))

  (thread
    (thunk
      (define db-resp-chan (make-channel))
      (channel-put db-chan (select RESULTS-INFO db-resp-chan))
      (match-define (data saved-scores) (channel-get db-resp-chan))
      (let loop ([sessions (hash)]      ;; [Hash-of UserID Chan]
                 [room-lookup (hash)]   ;; [Hash-of RoomID Chan]
                 [game-lookup (hash)]   ;; [Hash-of RoomID Chan]
                 [score-lookup          ;; [Hash-of RoomID Scores]
                  (or saved-scores (hash))])

        (define (handle-user-evt msg)
          (match msg
            [(list-rooms user-id)
             (define room-list (hash-keys room-lookup))
             (log-list-rooms user-id room-list)
             (channel-put (hash-ref sessions user-id) (rooms room-list))
             (loop sessions room-lookup game-lookup score-lookup)]

            [(get-results user-id)
             (define user-results
               (for/list ([(room-id scores) (in-hash score-lookup)]
                           #:when (hash-has-key? scores user-id))
                 (result room-id scores)))

             ;; TODO add logging method
             (channel-put (hash-ref sessions user-id) (results user-results))
             (loop sessions room-lookup game-lookup score-lookup)]

            [(create-room user-id)
             (define room-id (intern-symbol (gensym user-id)))
             (define room-chan (make-channel))
             (make-room room-id room-chan user-id (hash-ref sessions user-id))
             (loop sessions (hash-set room-lookup room-id room-chan) game-lookup score-lookup)]

            [(join-room user-id room-id)
             (cond
               [(hash-has-key? room-lookup room-id)
                (channel-put (hash-ref room-lookup room-id)
                             (approve-join user-id (hash-ref sessions user-id)))]
               [else
                (channel-put (hash-ref sessions user-id)
                             (room-not-found))])
             (loop sessions room-lookup game-lookup score-lookup)]

            [(logout user-id)
             (channel-put (hash-ref sessions user-id) (ack))
             (loop (hash-remove sessions user-id) room-lookup game-lookup score-lookup)]))

        (define (handle-room-evt msg)
          (match msg
            [(room-terminated room-id)
             (loop sessions (hash-remove room-lookup room-id) game-lookup score-lookup)]
            [(game-has-begun room-id)
             (loop sessions
                   (hash-remove room-lookup room-id)
                   (hash-set game-lookup room-id (hash-ref room-lookup room-id))
                   score-lookup)]))

        (define (handle-game-evt msg)
          (match msg
            [(game-results room-id score)
             (let ([new-scores (hash-set score-lookup room-id score)]
                   [db-resp-chan (make-channel)])
               (channel-put db-chan (insert RESULTS-INFO new-scores db-resp-chan))
               (match (channel-get db-resp-chan)
                 [(ack)
                  (loop sessions
                   room-lookup
                   (hash-remove game-lookup room-id)
                   new-scores)]))]))
             ;; (channel-put db-chan (insert RESULTS-INFO ))
             ;; (loop sessions
             ;;       room-lookup
             ;;       (hash-remove game-lookup room-id)
             ;;       (hash-set score-lookup room-id score))]))

        (define user-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-user-evt))
                      (hash-values sessions))))

        (define room-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-room-evt))
                      (hash-values room-lookup))))

        (define game-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-game-evt))
                      (hash-values game-lookup))))

        (sync
          user-evts
          room-evts
          game-evts
          (handle-evt
            auth-comm-chan
            (match-lambda
              [(create-session user-id)
               (define user-comm-chan (make-channel))
               (channel-put auth-comm-chan (session-created user-comm-chan))
               (loop (hash-set sessions user-id user-comm-chan)
                     room-lookup
                     game-lookup
                     score-lookup)]))))))

  auth-comm-chan)


;; Create a User component that communicates with the client over TCP
;; Port Port -> Void
(define (make-user input-port output-port auth-chan)
  (thread
    (thunk

      (define recv-chan (make-channel))
      (define input-evt (read-datum-evt input-port))

      (define (handle-auth-comms)
        (define client-msg (channel-get input-evt))
        (match client-msg
          [(login id token)
           (channel-put auth-chan id token recv-chan)
           (define server-msg (channel-get recv-chan))
           (match server-msg
             ;; TODO can fail with invalid password
             [(user-logged-in lobby-chan)
              (write (logged-in) output-port)
              (handle-lobby-comms id auth-chan lobby-chan)])]
          [(register id)
           (channel-put auth-chan (user-register id recv-chan))
           (define server-msg (channel-get recv-chan))
           (match server-msg
             ;; TODO can fail with already taken UserID
             [(user-registered token lobby-chan)
              (write (registered token) output-port)
              (handle-lobby-comms id auth-chan lobby-chan)])]))

      ;; Ideally, we'd hide unrelated chans
      (define (handle-lobby-comms user-id auth-chan lobby-chan)
        (let loop ()

          (define client-msg (channel-get input-evt))
          (match client-msg
            [(list-rooms user-id)
             (channel-put lobby-chan client-msg)
             (write (channel-get lobby-chan) output-port)
             (loop)]
            [(get-results user-id)
             (channel-put lobby-chan client-msg)
             (write (channel-get lobby-chan) output-port)
             (loop)]
            [(create-room user-id)
             (channel-put lobby-chan client-msg)
             (define server-msg (channel-get lobby-chan))
             (match server-msg
               [(user-room id room-chan)
                (write (room id) output-port)
                (handle-room-comms user-id auth-chan lobby-chan room-chan)])]
            [(join-room user-id room-id)
             (channel-put lobby-chan client-msg)
             (define server-msg (channel-get lobby-chan))
             (match server-msg
               [(room-not-found)
                (write server-msg output-port)
                (loop)]
               [(guest-room id room-chan broadcast-chan)
                (write (room id) output-port)
                (handle-guest-comms user-id auth-chan lobby-chan room-chan broadcast-chan)])]
            [(logout user-id)
             (channel-put lobby-chan client-msg)
             (write (channel-get lobby-chan) output-port)
             (handle-auth-comms)])))

      (define (handle-room-comms user-id auth-chan lobby-chan room-chan)
        (define client-msg (channel-get input-evt))
        (match client-msg
          [(cancel-game)
           (channel-put room-chan client-msg)
           (write (channel-get room-chan) output-port)
           (handle-lobby-comms user-id auth-chan lobby-chan)]
          [(start-game)
           (channel-put room-chan client-msg)
           (define server-msg (channel-get room-chan))
           (match server-msg
             [(game-not-started)
              (write server-msg output-port)
              (handle-room-comms user-id auth-chan lobby-chan room-chan)] ;; FIXME is this better than a loop? worse?
             [(user-game-started dealer-chan)
              (write (game-started) output-port)
              (handle-player-comms user-id auth-chan lobby-chan dealer-chan)])]))

      (define (handle-guest-comms user-id auth-chan lobby-chan room-chan broadcast-chan)
        (sync
          (handle-evt
            input-evt
            (match-lambda
              [(leave-room user-id)
               (channel-put room-chan (leave-room user-id))
               (write (channel-get room-chan) output-port)
               (handle-lobby-comms user-id auth-chan lobby-chan)]))
          (handle-evt
            broadcast-chan
            (match-lambda
              [(game-cancelled room-id)
               (write (game-cancelled room-id) output-port)
               (handle-lobby-comms user-id auth-chan lobby-chan)]
              [(user-game-started dealer-chan)
               (write (game-started) output-port)
               (handle-player-comms user-id auth-chan lobby-chan dealer-chan)]))))

      (define (handle-player-comms user-id auth-chan lobby-chan dealer-chan)
        (define dealer-msg (channel-get dealer-chan))
        (match dealer-msg
          [(move-request _ _ _)
           (write dealer-msg output-port)
           (define client-msg (channel-get input-evt))
           (match client-msg
             [(played-in-round _ _ _)
              (channel-put dealer-chan client-msg)
              (handle-player-comms user-id auth-chan lobby-chan dealer-chan)])]
          [(game-over winners)
           (write dealer-msg output-port)
           (handle-lobby-comms user-id auth-chan lobby-chan)]))

      (handle-auth-comms))))

;; Chan -> Chan
(define (make-authentication-manager lobby-chan db-chan)
  (define auth-chan (make-channel))
  (define db-recv-chan (make-channel))

  (thread
    (thunk
      ;; -> [Hash-of UserID UserToken]
      (define (get-initial-tokens)
        (channel-put db-chan (select LOGIN-INFO db-recv-chan))
        (define db-resp (channel-get db-recv-chan))
        (match db-resp
          [(data d) (or d (hash))]))

      (define (establish-user-session id)
        (channel-put lobby-chan (create-session id))
        (define lobby-msg (channel-get lobby-chan))
        (match lobby-msg
          [(session-created user-lobby-chan) user-lobby-chan]))

      (define initial-tokens (get-initial-tokens))


      (let loop ([user-tokens initial-tokens]) ;; [Hash-of UserID UserToken]
        (define auth-msg (channel-get auth-chan))
        (match auth-msg
          [(user-login id token reply-chan)
           (log-login id)
           (when (symbol=? token (hash-ref user-tokens id))
             (define user-lobby-chan (establish-user-session id))
             (channel-put reply-chan (user-logged-in user-lobby-chan)))
           (loop user-tokens)]
          [(user-register id reply-chan)
           (define user-token (intern-symbol (gensym id)))
           (define new-tokens (hash-set user-tokens id user-token))
           (channel-put db-chan (insert LOGIN-INFO new-tokens db-recv-chan))

           (define db-msg (channel-get db-recv-chan))
           (match db-msg
             [(ack)
              (define user-lobby-chan (establish-user-session id))
              (channel-put reply-chan (user-registered user-token user-lobby-chan))
              (loop new-tokens)])]))))
  auth-chan)

;; Path -> [Chan-of DBMsg]
(define (make-database filename)
  (define starting-db
    (if (file-exists? filename)
      (file->value filename)
      (hash)))

  (define db-chan (make-channel))

  (thread
    (thunk
      (let loop ([db starting-db]) ;; [Hash-of Symbol Any]
        (define db-msg (channel-get db-chan))
        (match db-msg
          [(select key reply-chan)
           (channel-put reply-chan (data (hash-ref db key #f)))
           (loop db)]

          [(insert key val reply-chan)
           (define new-db (hash-set db key val))
           (write-to-file new-db filename #:exists 'replace)
           (channel-put reply-chan (ack))
           (loop new-db)]))))

  db-chan)

(define general-chan (make-channel))
(define db-chan (make-database "db.info"))
(define lobby-chan (make-lobby db-chan))
(define server (tcp-listen CONNECT-PORT))

(define auth-chan (make-authentication-manager lobby-chan db-chan))
(create-clients server auth-chan)

(channel-get general-chan)
