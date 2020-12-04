#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a PlayerID is a Symbol
;; a Hand is a [List-of Card]
;; a Score is a Nat
;; a RoundNumber is a Nat in [1, 10]
;; Scores is a [Hash-of PlayerID Score]
;;  - Scores must contain an entry for each existing PlayerID (i.e. it is safe to use hash-update)

(provide
  (struct-out row)
  (struct-out played-in-round)
  (struct-out card)
  (struct-out in-hand))

;; a Row is a (row [List-of Card]) where the length of the list is in [1, 5]
(struct row (cards) #:prefab)

;; a Rows is a [List-of Row] of length 4

;; a Move is a (played-in-round PlayerID RoundNumber Card)
(struct played-in-round (player round card) #:prefab)

;; a Card is a (card Nat Nat) where the first Nat is the value/rank of th card
;; and the second Nat is the number of "bulls"
(struct card (rank bulls) #:prefab) 

;; an InHand is an (in-hand PlayerID Card)
(struct in-hand (player card) #:transparent)
