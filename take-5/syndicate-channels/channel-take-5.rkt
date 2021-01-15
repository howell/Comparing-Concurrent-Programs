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

;; a Round is a (round Nat [List-of Card] [List-of Row] [Chan-of Move])
(struct round (number hand rows move-chan) #:transparent)

;; a PlayerStruct is a (player PlayerID [Chan-of Round])
(struct player (id chan) #:transparent)

;; a UserRegister is a (user-register PlayerID [Chan-of UserRegistered])
(struct user-register (id chan) #:transparent)

;; a UserRegistered is a (user-registered Token [Chan-of Login])
(struct user-registered (token chan) #:transparent)

;; a CreateSession is a (create-session UserID)
(struct create-session (user) #:transparent)

;; a SessionCreated is a (session-created [Chan-of LobbyMsg])
(struct session-created (chan) #:transparent)

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

;; a DeclaredWinners is a (declared-winner/s [List-of PlayerID])
(struct declared-winner/s (player/s) #:transparent)

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
;; which to receive replies. The Authentication manager sends back a UserRegistered
;; message, containing the UserToken corresponding to the User that that User must
;; use to Log-in, and a new channel for all future communication.
;; 
;; There is a conversation about login.
;; Users send a Login message to the Authentication Manager containing their UserID
;; and their UserToken. If the UserToken correctly corresponds to the UserID, then the
;; Authentication Manager sends a CreateSession message to the Protected Component,
;; containing the UserID of the User logging in. The Protected Component sends back
;; a SessionCreated message, containing a Channel for communication with a User. The
;; Authentication Manager forwards this channel to the user in a UserLoggedIn message.
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
;; There is a conversation about leaving a game.
;; To leave a game, a Guest sends a LeaveGame message to the room. The Room replies
;; with an Acknowledgement message to the Guest. That Guest can no longer communicate
;; with that Room unless the Guest requests to re-join from the Lobby.


;; 
;; To Log Out, Users send a Log Out message to the Lobby. The Lobby stops all future
;; communication with the User until they have logged back in via the Authentication
;; Manager.
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

;; RoomID [Chan-of LobbyMsg] [Chan-of Room] -> Void
(define (make-room room-id lobby-chan temp-host-chan)
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
                 (channel-put broadcast-chan (game-cancelled room-id)))])))))))

;; -> Void
(define (make-lobby)
  (define auth-comm-chan (make-channel))

  (thread
    (thunk
      (let loop ([sessions (hash)]      ;; [Hash-of UserID Chan]
                 [room-lookup (hash)]   ;; [Hash-of RoomID Chan]
                 [score-lookup (hash)]) ;; [Hash-of RoomID Scores]

        (define (handle-user-evt msg)
          (match msg
            [(list-rooms user-id)
             (define room-list (hash-keys room-lookup))
             (log-list-rooms user-id room-list)
             (channel-put (hash-ref sessions user-id) (rooms room-list))
             (loop sessions room-lookup score-lookup)]

            [(get-results user-id)
             (define user-results
               (for/list ([(room-id scores) (in-hash score-lookup)]
                           #:when (hash-has-key? scores user-id))
                 (result room-id scores)))

             ;; TODO add logging method
             (channel-put (hash-ref sessions user-id) (results user-results))
             (loop sessions room-lookup score-lookup)]

            [(create-room user-id)
             (define room-id (intern-symbol (gensym user-id)))
             (define room-chan (make-channel))
             (make-room room-id room-chan (hash-ref sessions user-id))
             (loop sessions (hash-set room-lookup room-id room-chan) score-lookup)]

            [(join-room user-id room-id)
             (cond
               [(hash-has-key? room-lookup room-id)
                (channel-put (hash-ref room-lookup room-id)
                             (approve-join user-id (hash-ref sessions user-id)))]
               [else
                (channel-put (hash-ref sessions user-id)
                             (room-not-found))])
             (loop sessions room-lookup score-lookup)]

            [(logout user-id)
             (channel-put (hash-ref sessions user-id) (ack))
             (loop (hash-remove sessions user-id) room-lookup score-lookup)]))

        (define (handle-room-evt msg)
          (match msg
            [(room-terminated room-id)
             (loop sessions (hash-remove room-lookup room-id) score-lookup)]))

        (define user-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-user-evt))
                      (hash-values sessions))))

        (define room-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-room-evt))
                      (hash-values room-lookup))))

        (sync
          user-evts
          room-evts
          (handle-evt
            auth-comm-chan
            (match-lambda
              [(create-session user-id)
               (define user-comm-chan (make-channel))
               (channel-put auth-comm-chan (session-created user-comm-chan))
               (loop (hash-set sessions user-id user-comm-chan)
                     room-lookup
                     score-lookup)]))))))

  auth-comm-chan)


;; Create a User component that communicates with the client over TCP
;; Port Port -> Void
(define (make-user input-port output-port register-chan)
  (thread
    (thunk
      (define recv-chan (make-channel))

      (define input-evt (read-datum-evt input-port))

      ;; a ServerID is one of 'auth, 'lobby, 'room, 'room-broadcast, 'dealer

      ;; [Hash-of ServerID Chan]
      (define chan-hash (make-hash))

      ;; a MessageProcessor is a (msg-processor ServerID (Struct -> Struct))
      (struct msg-processor (target process) #:transparent)

      ;; Chan [Hash Symbol MessageProcessor] -> Chan
      ;; Requirements:
      ;; 1. all messages received on the channel must be prefab structs
      ;; 2. the keys of the hash must correspond to prefab struct keys
      (define (process-msg-evt-loop chan h)
        (define processed-msg-chan (make-channel))

        (thread
          (thunk
            (let loop ()
              (define msg (channel-get chan))
              (match-define (msg-processor target process)
                            (hash-ref h (prefab-struct-key msg)))

              (define comm-chan (hash-ref chan-hash target))

              (channel-put comm-chan msg)
              (define received-msg (channel-get comm-chan))

              (channel-put processed-msg-chan (process received-msg))
              (loop))))

        processed-msg-chan)

      (define/match (process-login-acceptance msg)
        [((user-logged-in lobby-chan))
         (hash-set! chan-hash 'lobby lobby-chan)
         (logged-in)])

      (define (process-logout msg)
        (hash-remove! chan-hash 'lobby)
        msg)

      (define/match (process-room-entry msg)
        [((user-room id room-chan))
         (hash-set! chan-hash 'room room-chan)
         (room id)])

      (define/match (process-room-join msg)
        [((room-not-found)) msg]
        [((guest-room id room-chan broadcast-chan)) 
         (hash-set! chan-hash 'room room-chan)
         (hash-set! chan-hash 'room-broadcast broadcast-chan)
         (room id)])

      (define (process-room-exit msg)
        (hash-remove! chan-hash 'room) 
        (when (hash-has-key? chan-hash 'room-broadcast) (hash-remove! chan-hash 'room-broadcast))
        msg)
    
      ;; registration and 'getting results' are the exceptions to the pattern
      (define msg-processor-hash
        (hash 'login       (msg-processor 'auth process-login-acceptance)
              'logout      (msg-processor 'lobby process-logout)
              'list-rooms  (msg-processor 'lobby identity)
              'get-results (msg-processor 'lobby identity)
              'create-room (msg-processor 'lobby process-room-entry)
              'join-room   (msg-processor 'lobby process-room-join)
              'cancel-game (msg-processor 'room process-room-exit)
              'leave-room  (msg-processor 'room process-room-exit)))

      (define msg-loop-evt (process-msg-evt-loop input-evt msg-processor-hash))

      (define client-msg (channel-get input-evt))
      (match client-msg
        [(register user-id)
         (channel-put register-chan (user-register user-id recv-chan))])

      (let loop ()
        ;; TODO does this fail b/c of the two-way comm when a guest?
        (define room-evt
          (if (hash-has-key? chan-hash 'room-broadcast)
            (hash-ref chan-hash 'room-broadcast)
            never-evt))

        (sync
          (handle-evt
            msg-loop-evt
            (λ (msg)
              (write msg output-port)
              (loop)))

          (handle-evt
            room-evt
            (match-lambda
              [(game-cancelled room-id)
               (write (game-cancelled room-id) output-port)
               (loop)]))

          (handle-evt
            recv-chan
            (match-lambda
              [(user-registered token user-auth-chan)
               (write (registered token) output-port)
               (hash-set! chan-hash 'auth user-auth-chan)
               (loop)])))))))

;; Chan -> Chan
(define (make-authentication-manager lobby-chan db-chan)
  (define register-chan (make-channel))
  (define db-recv-chan (make-channel))

  (thread
    (thunk
      ;; -> [Hash-of UserID UserToken]
      (define (get-initial-tokens)
        (channel-put db-chan (select LOGIN-INFO db-recv-chan))
        (define db-resp (channel-get db-recv-chan))
        (match db-resp
          [(data d) (or d (hash))]))

      (define initial-tokens (get-initial-tokens))

      (let loop ([user-tokens initial-tokens] ;; [Hash-of UserID UserToken]
                 [user-comms (hash)]) ;; [Hash-of UserID [Chan-of AuthMsg]]

        ;; Login -> Void
        (define (handle-login msg)
          (match msg
            [(login id token)
             (log-login id)
             (when (symbol=? token (hash-ref user-tokens id))
               (channel-put lobby-chan (create-session id))
               (define lobby-msg (channel-get lobby-chan))
               (match lobby-msg
                 [(session-created user-lobby-chan)
                  (channel-put (hash-ref user-comms id) (user-logged-in user-lobby-chan))
                  (loop user-tokens user-comms)]))]))

        (define login-evts
          (apply choice-evt
                 (map (λ (chan) (handle-evt chan handle-login))
                      (hash-values user-comms))))

        (sync
          login-evts
          (handle-evt
            register-chan
            (match-lambda
              [(user-register id user-chan)
               (log-registration id)

               (define user-token (intern-symbol (gensym id)))
               (define new-tokens (hash-set user-tokens id user-token))
               (channel-put db-chan (insert LOGIN-INFO new-tokens db-recv-chan))

               (define db-msg (channel-get db-recv-chan))
               (match db-msg
                 [(ack)
                  (define user-auth-chan (make-channel))
                  (channel-put user-chan (user-registered user-token user-auth-chan))
                  (loop new-tokens
                        (hash-set user-comms id user-auth-chan))])]))))))
  register-chan)

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
(define lobby-chan (make-lobby))
(define server (tcp-listen CONNECT-PORT))

(define auth-chan (make-authentication-manager lobby-chan db-chan))
(create-clients server auth-chan)

(channel-get general-chan)
