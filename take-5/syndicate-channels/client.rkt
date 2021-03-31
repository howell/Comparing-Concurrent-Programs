#lang racket

(require [only-in racket/random random-ref])
(require [only-in racket/trace trace-define])
(require "struct.rkt")

;; -> [List-of Port]
(define (create-connection)
  (tcp-connect CONNECT-HOSTNAME CONNECT-PORT))


(define (handle-playing-game user-id input-port output-port)
  (define game-msg (read input-port))
  (match game-msg
    [(move-request round-no hand rows)
     (write (played-in-round user-id round-no (random-player hand rows)) output-port)
     (handle-playing-game user-id input-port output-port)]
    [(game-over winners)
     (printf "WOWOWOWOWOW the winners are ~a\n" winners)]))

;; UserID -> Void
(define (create-client user-id)
  (define-values (i o) (create-connection))
  (remove-tcp-buffer i o)

  ;; Idea:
  ;; create a loop where this player
  ;; 1. prints their results
  ;; 2. creates a room
  ;; 3. starts it once someone has joined
  ;; 4. plays
  ;; 5. goes back to lobby and restarts loop

  (sleep 1) ;; FIXME race due to spawning reader thread for syndicate driver

  (define token (authenticate user-id i o))
  (printf "login token is: ~a\n" token)

  (let loop ()
    (write (get-results user-id) o)
    (match (read i)
      [(results res)
       (printf "results from games including player ~a: ~a\n" user-id res)])

    (write (create-room user-id) o)
    (match (read i)
      [(room room-id)
       (printf "User ~a has successfully created room ~a\n" user-id room-id)])

    (sleep 10)

    (write (start-game) o)
    (match (read i)
      [(game-started) (printf "Game has started for player ~a\n" user-id)])

    (handle-playing-game user-id i o)
    (loop))

  ;; (write (create-room user-id) o)
  ;; (define resp (read i))

  ;; (printf "created room!\n")

  ;; (sleep 20)

  ;; (write (start-game) o)

  ;; (define hopefully-game-has-begun-msg (read i))

  ;; (match hopefully-game-has-begun-msg
  ;;   [(game-started) (printf "woohoo!\n") (handle-playing-game user-id i o)]
  ;;   [_ (printf "FAILURE\n")])

  (close-ports i o))


(define (create-joining-client user-id)
  (define-values (i o) (create-connection))
  (remove-tcp-buffer i o)

  (sleep 1) ;; FIXME race due to spawning reader thread for syndicate driver

  (define token (authenticate user-id i o))

  (let loop ()
    (write (get-results user-id) o)
    (match (read i)
      [(results res)
       (printf "results from games including player ~a: ~a\n" user-id res)])

    (sleep 5)

    (write (list-rooms user-id) o)
    (let* ([rooms-msg (read i)]
           [open-rooms (rooms-items rooms-msg)]
           [first-open-room (first open-rooms)])
      (printf "Open rooms: ~a\n" open-rooms)
      (printf "User ~a will join room ~a\n" user-id first-open-room)

      (write (join-room user-id first-open-room) o)
      (match (read i)
        [(room _) (printf "Room ~a successfully joined by user ~a\n" first-open-room user-id)])

      (match (read i)
        [(game-started) (printf "Game has started for player ~a\n" user-id)])

      (handle-playing-game user-id i o)
      (loop)))

  ;; (write (list-rooms user-id) o)
  ;; (define rooms-msg (read i))

  ;; (write (join-room user-id (first (rooms-items rooms-msg))) o)
  ;; (define confirmation-msg (read i))

  ;; (printf "Room ~a has been successfully joined!\n" (room-id confirmation-msg))

  ;; (define hopefully-game-has-begun-msg (read i))
  ;; (match hopefully-game-has-begun-msg
  ;;   [(game-started) (printf "woohoo!\n") (handle-playing-game user-id i o)]
  ;;   [_ (printf "FAILURE\n")])

  (close-ports i o))

;; register and log in the user
;; UserID Port Port -> UserToken
(define (authenticate id input output)
  ;; Register the User
  (write (register id) output)
  (define registered-msg (read input))
  (define token (registered-token registered-msg))

  ;; Log-in the User
  (write (login id token) output)
  (define logged-in-msg (read input))
  (match logged-in-msg
    [(logged-in) (printf "Yay! ~a is logged in with token ~a!\n" id token)])

  ;; Return the token for future use
  token)


;; GamePlayer
(define (random-player hand rows)
  (random-ref hand))

(command-line #:args (name creator?)
              (if (string=? creator? "yes")
                (create-client (string->symbol name))
                (create-joining-client (string->symbol name))))
