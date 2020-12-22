#lang racket

(provide
  ;; Structs
  (struct-out row)
  (struct-out played-in-round)
  (struct-out card)
  (struct-out ports)
  (struct-out declare-player)
  (struct-out move-request)
  (struct-out game-over)
  (struct-out register)
  (struct-out registered)
  (struct-out login)
  (struct-out logged-in)
  (struct-out list-rooms)
  (struct-out rooms)
  (struct-out get-results)
  (struct-out result)
  (struct-out results)

  ;; Constants
  CONNECT-PORT
  CONNECT-HOSTNAME
  CONN-DURATION
  REGISTER-DURATION

  ;; Functions
  remove-tcp-buffer
  close-ports)

;; Constants
(define CONNECT-PORT 8900) ;; the port number that the server listens to
(define CONNECT-HOSTNAME "localhost") ;; the hostname of the server
(define CONN-DURATION 20000) ;; the amount of time clients have to connect
(define REGISTER-DURATION 1000) ;; the amount of time clients have to register

;; ---------------------------------------------------------------------------------------------------
;; a UserID is a unique Symbol
;; a UserToken is a unique Symbol
;; a RoomID is a unique Symbol
;; a Hand is a [List-of Card]
;; a Score is a Nat
;; Scores is a [Hash-of UserID Score]
;;  - Scores must contain an entry for each existing PlayerID (i.e. it is safe to use hash-update)
;; a RoundNumber is a Nat in [1, 10]

;; a Register is a (register UserID)
(struct register (id) #:prefab)

;; a Registered is a (registered UserToken)
(struct registered (token) #:prefab)

;; a Login is a (log-in UserID UserToken)
(struct login (id token) #:prefab)

;; a LoggedIn is a (logged-in)
(struct logged-in () #:prefab)

;; a ListRooms is a (list-rooms)
(struct list-rooms (id) #:prefab)

;; a Rooms is a (rooms [List-of RoomID])
(struct rooms (items) #:prefab)

;; a GetResults is a (get-results UserID)
(struct get-results (player) #:prefab)

;; a Result is a (result RoomID Scores)
(struct result (room-id scores) #:prefab)

;; a Results is a (results [List-of Result])
(struct results (items) #:prefab) ;; FIXME I don't like `items`

;; a CreateRoom is a (create-room UserID)
(struct create-room (user) #:prefab)

;; a Room is a (room RoomID) --> FIXME should the room have the creating UserID? probably not
(struct room (id) #:prefab)

;; a DeclarePlayer is a (declare-player PlayerID)
(struct declare-player (id) #:prefab)

;; a Row is a (row [List-of Card]) where the length of the list is in [1, 5]
(struct row (cards) #:prefab)

;; a Rows is a [List-of Row] of length 4

;; a Move is a (played-in-round PlayerID RoundNumber Card)
(struct played-in-round (player round card) #:prefab)

;; a Card is a (card Nat Nat) where the first Nat is the value/rank of th card
;; and the second Nat is the number of "bulls"
(struct card (rank bulls) #:prefab) 

;; FIXME maybe this struct should be moved elsewhere?
;; a Ports is a (ports Port Port)
(struct ports (input output) #:transparent)

;; a MoveRequest is a (move-request RoundNumber Hand Rows)
(struct move-request (round hand rows) #:prefab)

;; a GameOver is a (game-over [List-of PlayerID]) where the PlayerID is the ID of the winning player
(struct game-over (winners) #:prefab)

;; a GamePlayer is a function [List-of Card] Rows -> Card

;; remove buffer from TCP ports
;; Port Port -> Void
(define (remove-tcp-buffer input output)
  (file-stream-buffer-mode input 'none)
  (file-stream-buffer-mode output 'none)
  #f)

;; Port Port -> Void
(define (close-ports input output)
  (close-input-port input)
  (close-output-port output))
