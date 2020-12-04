#lang racket

(provide
  (struct-out row)
  (struct-out played-in-round)
  (struct-out card)
  (struct-out ports)
  (struct-out declare-player)
  (struct-out move-request)

  CONNECT-PORT
  CONNECT-HOSTNAME

  write-and-flush)

(define CONNECT-PORT 8900)
(define CONNECT-HOSTNAME "localhost")

;; ---------------------------------------------------------------------------------------------------
;; a PlayerID is a Symbol
;; a Hand is a [List-of Card]
;; a Score is a Nat
;; a RoundNumber is a Nat in [1, 10]
;; Scores is a [Hash-of PlayerID Score]
;;  - Scores must contain an entry for each existing PlayerID (i.e. it is safe to use hash-update)

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

;; a Ports is a (ports Port Port)
(struct ports (input output) #:prefab)

;; a MoveRequest is a (move-request RoundNumber Hand Rows)
(struct move-request (round hand rows) #:prefab)

;; a GamePlayer is a function [List-of Card] Rows -> Card

;; Any Port -> void
(define (write-and-flush contents port)
  (write contents port)
  (flush-output port))
