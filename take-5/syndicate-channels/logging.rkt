#lang racket

(provide
 ;; -> Void
 log-connection

 ;; UserID Any -> Void
 log-client-request

 ;; UserID -> Void
 log-registration

 ;; UserID -> Void
 log-login

 ;; UserID [List-of RoomID] -> Void
 log-list-rooms

 ;; Rows -> Void
 log-rows

 ;; (Hash-of PlayerId (List-of Card)) -> Void
 log-hands

 ;; Move -> Void
 log-move

 ;; Scores -> Void
 log-scores

 ;; (Listof PlayerId) -> Void
 log-winner/s

 ;; PlayerId Card (Listof Card) -> Void
 log-player-decision

 ;; (Setof PlayerId) -> Void
 log-elimination)

;; ---------------------------------------------------------------------------------------------------
(require "rules.rkt")
(require "struct.rkt")

;; ---------------------------------------------------------------------------------------------------
(define the-topic 'take-5)

(define-logger take-5)

(define (log-connection)
  (log-take-5-debug "Client has connected!"))

(define (log-client-request id request)
  (log-take-5-debug "User ~a is making a the request ~a!" id request)) 

(define (log-registration id)
  (log-take-5-debug "User ~a has registered for a user account!" id))

(define (log-login id)
  (log-take-5-debug "User ~a has logged into their account!" id))

(define (log-list-rooms id rooms)
  (log-take-5-debug "User ~a has requested to see the available rooms, which are: ~a" id rooms))

(define (log-move m)
  (match-define (played-in-round pid r c) m)
  (log-take-5-debug "player ~a plays card ~v in round ~v" pid c r))

(define (log-rows rows)
  (log-take-5-debug "The current rows are:\n\t~v\n\t~v\n\t~v\n\t~v"
                    (first rows) (second rows) (third rows) (fourth rows)))

(define (log-hands hands)
  (log-take-5-debug "The current hands are:")
  (for ([(p hand) (in-hash hands)])
    (log-take-5-debug "\n\t~a: ~v" p hand)))

(define (log-player-decision pid c hand)
  (log-take-5-debug "~a chooses card ~v from hand ~v" pid c hand))

;; should be able to abstract this with log-winner/s
(define (log-elimination pids)
  (cond
    [(= 0 (length pids))
     (log-take-5-debug "No one has been eliminated!")]
    [(= 1 (length pids))
     (log-take-5-debug "~a has been eliminated!" (first pids))]
    [else
      (define names (map symbol->string pids))
      (log-take-5-debug (string-join names ", "
                                     #:before-last " and "
                                     #:after-last " have been eliminated!"))]))

(define (log-scores scores)
  (log-take-5-debug "The current scores are:")
  (for ([(pid score) (in-hash scores)])
    (log-take-5-debug "~a has ~v bulls" pid score)))

(define (log-winner/s pids)
  (cond
    [(= 1 (length pids))
     (log-take-5-debug "~a wins!" (first pids))]
    [else
     (define names (map symbol->string pids))
     (log-take-5-debug (string-join names ", "
                                    #:before-last " and "
                                    #:after-last " tie!"))]))

