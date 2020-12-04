#lang racket

(require [only-in racket/random random-ref])
(require "struct.rkt")

(define (create-connection)
  (tcp-connect CONNECT-HOSTNAME CONNECT-PORT))

;; PlayerID GamePlayer -> void
(define (create-client player-name make-decision)
  (define-values (i o) (tcp-connect "localhost" 8900))

  (write-and-flush (declare-player player-name) o)

  (let loop ()
    (define contents (read i))
    (match contents
      [(move-request number hand rows)
       (let ([c (make-decision hand rows)])
         (write-and-flush (played-in-round player-name number c) o)
         (loop))]
      [eof
        (close-input-port i)
        (close-output-port o)])))

;; GamePlayer
(define (random-player hand rows)
  (random-ref hand))

(command-line #:args (name)
              (create-client (string->symbol name) random-player))
