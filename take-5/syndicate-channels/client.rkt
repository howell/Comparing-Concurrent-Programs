#lang racket

(require "channel-take-5.rkt")

(struct declare-player (id) #:prefab)

(define (create-client player-name make-decision)
  (define-values (i o) (tcp-connect "localhost" 8900))

  (write (declare-player player-name) o)

  (let loop ()
    (define contents (read i))
    (match contents
      [(round number hand rows)
       (let ([c (make-decision hand rows)])
          (write (played-in-round player-name number c) o)
          (loop))])))
