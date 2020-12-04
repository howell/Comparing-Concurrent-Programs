#lang racket

;; TODO maybe only require what you need?
(require "struct.rkt")

(provide
 ;; type Deck     = (Listof Card)
 ;; Deck 
 the-deck

 ;; Nat Deck -> (Values (Listof Card) Deck)
 ;; Remove cards from a deck
 draw
   
 ;; Deck -> (Values Card Deck)
 ;; Remove the first card from the deck
 draw-one)

;; -----------------------------------------------------------------------------
(require (only-in racket/random random-ref random-sample))

;; -----------------------------------------------------------------------------
  
(define (draw-one deck)
  (match deck
    ['() (error "tried to draw from empty deck")]
    [(cons c cs) (values c cs)]))
  
(define (draw n deck)
  (cond
    [(zero? n) (values '() deck)]
    [else
     (define-values (c new-deck) (draw-one deck))
     (define-values (cs new-new-deck) (draw (sub1 n) new-deck))
     (values (cons c cs) new-new-deck)]))

(define the-deck
  (for/list ([i (in-range 1 105)])
    (define bulls
      (cond
        [(= i 55) 7]
        [(= (modulo i 10) 0) 3]
        [(= (modulo i 5) 0) 2]
        [(member i '(11 22 33 44 66 77 88 99)) 5]
        [else 1]))
    (card i bulls)))
