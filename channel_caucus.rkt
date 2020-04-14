#lang racket

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a String

;; a Tax-Rate is a number

;; a candidate is a (candidate Name Tax-Rate)
(struct candidate (name tax-rate) #:transparent)

;; there is a presence-oriented conversation where candidates declare themselves
;; to their local candidate registry, so that they can be voted for.



;; what does the candidate registry do? 
;; it listens for candidate declarations
;; and upon receiving them, sends the full list of candidates
;; to all listeners
;; if there is a new subscriber,
;; the full list is sent to them immediately

(define (make-candidate-registry)
  (define registration-chan (make-channel))
