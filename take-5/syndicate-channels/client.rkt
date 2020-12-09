#lang racket

;; Client Protocol
;; There are multiple clients communicating with one server.
;;
;; Clients register to play the game with the server by connecting to the server and sending a
;; DeclarePlayer message over the output port, containing the player's ID.
;;
;; To play the game, the client waits for a MoveRequest message to be received from the input port,
;; containing the round number, the player's current hand, and the current state of the rows in 
;; the game. Clients respond with a Move message, containing the player's ID, the number of the round
;; this move is being made in, and the card they wish to play.
;; Invariant: the card selected by the client must be present in the hand the client the received. 

(require [only-in racket/random random-ref])
(require "struct.rkt")

;; -> [List-of Port]
(define (create-connection)
  (tcp-connect CONNECT-HOSTNAME CONNECT-PORT))

;; PlayerID GamePlayer -> void
(define (create-client player-name make-decision)
  (define-values (i o) (create-connection))
  (remove-tcp-buffer i o)

  (write (declare-player player-name) o)
  (flush-output o)

  (let loop ()
    (define contents (read i))
    (match contents
      [(move-request number hand rows)
       (let ([c (make-decision hand rows)])
         (write (played-in-round player-name number c) o)
         (flush-output o)
         (loop))]
      [(game-over winners)
       (if (member player-name winners)
         (printf "We won!\n")
         (printf "We lost :(\n"))
       (close-ports i o)])))

;; GamePlayer
(define (random-player hand rows)
  (random-ref hand))

(command-line #:args (name)
              (create-client (string->symbol name) random-player))
