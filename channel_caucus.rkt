#lang racket

;; start by adding candidates and a leader who is able to consume candidate announcements

;; a Name is a String

(struct candidate (name) #:transparent)
(struct voter (name vote-chan) #:transparent)

;; There is a presence-oriented conversation where candidates announce themselves to their voting leader

;; there is a presence-oriented conversation where voters announce themselves to their voting leader

(define (make-candidate name leader) 
  (thread (thunk (channel-put leader (candidate name)))))

(define (make-voter name leader)
  (define voting-channel (make-channel))
  (thread (thunk (channel-put leader (voter name voting-channel)))))

(define (make-leader)
  (define leader-channel (make-channel))
  (thread
    (thunk
      (let loop ([candidates '()]
                 [voters '()])
        (printf "we entered here\n")
        (sync
          (handle-evt
            leader-channel
            (match-lambda
              ;; fix for tax-rate?
              [(candidate name)
               (printf "candidate made an announcement: ~a\n" name)
               (printf "total candidates: ~a\n" candidates)
               (loop (cons name candidates) voters)]
              [(voter name vote-chan)
               (printf "voter has registered: ~a\n" name)
               (loop candidates (cons (voter name vote-chan) voters))]
                ))))))
  leader-channel)

(define the-leader-channel (make-leader))
(make-candidate "Bernie" the-leader-channel)
(make-candidate "Biden" the-leader-channel)
(make-candidate "Tulsi" the-leader-channel)

(make-voter "Mitch" the-leader-channel)
(make-voter "Sam" the-leader-channel)

(sleep 1)
