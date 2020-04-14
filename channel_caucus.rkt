#lang racket
(require racket/set)

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a string

;; a Tax-Rate is a number

;; a Chan is a channel

;; a candidate is a (candidate Name Tax-Rate)
(struct candidate (name tax-rate) #:transparent)

;; a subscribe is a (subscribe Chan)
(struct subscribe (chan) #:transparent)

;; there is a presence-oriented conversation where candidates declare themselves
;; to their local candidate registry, so that they can be voted for.

;; ENTITIES:
;; 1. Candidates
;; 2. Candidate Registry

;; a candidate registry holds two conversations:
;; 1. if a candidate registers, then an update is sent to all existing subscribers
;;    to this information, and the full set of available candidates is sent to them.
;; 2. if a new communicator subscribes to the candidate registry, then they are sent
;;    all available candidates and are updated as new candidates register.
(define (make-candidate-registry)
  (define registration-chan (make-channel))
  (thread
    (thunk
      (let loop ([candidates (set)]
                 [subscribers (set)])
        (sync
          (handle-evt
            registration-chan
            (match-lambda
              [(candidate name tax-rate)
               (define new-candidates (set-add candidates (candidate name tax-rate)))
               (for ([subscriber (set->list subscribers)])
                 (channel-put subscriber new-candidates))
               (loop new-candidates subscribers)]
              [(subscribe chan)
               (channel-put chan candidates)
               (loop candidates (set-add subscribers chan))]))))))
  registration-chan)

(define (make-candidate name tax-rate registration-chan)
  ;; TODO is this unnecessary?
  (thread 
    (thunk
      (channel-put registration-chan (candidate name tax-rate)))))

(define (make-dummy-subscriber pub-sub-chan)
  (define updates (make-channel))
  (thread
    (thunk
      (channel-put pub-sub-chan (subscribe updates))
      (let loop ()
        (sync
          (handle-evt
            updates
            (λ (evt) (printf "~a\n" evt) (loop))))))))

(define candidate-registry (make-candidate-registry))
(make-candidate "Bernie" 50 candidate-registry)
(make-candidate "Biden" 25 candidate-registry)
(make-dummy-subscriber candidate-registry)

(sleep 1)
